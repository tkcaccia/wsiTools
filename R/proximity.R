wsi_empty_proximity_result <- function() {
  out <- data.frame(
    id = character(),
    label = character(),
    unit = character(),
    x = numeric(),
    y = numeric(),
    query_annotation_id = character(),
    query_annotation = character(),
    query_class = character(),
    target_annotation_id = character(),
    target_annotation = character(),
    target_class = character(),
    nearest_target_id = character(),
    nearest_target_label = character(),
    nearest_target_x = numeric(),
    nearest_target_y = numeric(),
    distance_px = numeric(),
    distance_um = numeric(),
    point_source = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_proximity_result", class(out))
  out
}

wsi_empty_proximity_stats_result <- function() {
  out <- data.frame(
    rank = integer(),
    feature = character(),
    method = character(),
    statistic = numeric(),
    correlation = numeric(),
    p_value = numeric(),
    MIC = numeric(),
    n_bins = integer(),
    n_points = integer(),
    distance_unit = character(),
    feature_source = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_proximity_stats_result", class(out))
  out
}

#' Create live viewer proximity-analysis context
#'
#' `wsi_proximity_context()` is an advanced helper used by [wsi_viewer_live()]
#' when a generic live viewer needs access to Seurat, Giotto,
#' SpatialExperiment, or CellPhenotyper coordinates for proximity analysis.
#' High-level spatial and CellPhenotyper viewer helpers set this automatically.
#'
#' @param spatial A linked spatial object returned by [wsi_link_seurat_image()]
#'   or the Giotto/SpatialExperiment linkers.
#' @param cellphenotyper_project A CellPhenotyper project returned by
#'   [wsi_read_cellphenotyper_project()].
#'
#' @return A list used internally by the live viewer proximity endpoint.
#' @export
wsi_proximity_context <- function(spatial = NULL, cellphenotyper_project = NULL) {
  list(
    spatial = spatial,
    cellphenotyper_project = cellphenotyper_project
  )
}

wsi_proximity_config <- function(seurat = NULL, cellphenotyper = NULL) {
  sources <- list()
  seurat <- seurat %||% list(enabled = FALSE)
  cellphenotyper <- cellphenotyper %||% list(enabled = FALSE)

  if (isTRUE(seurat$enabled) && as.integer(seurat$spot_count %||% 0L) > 0L) {
    source_name <- as.character(seurat$source_name %||% "Spatial")
    sources[[length(sources) + 1L]] <- list(
      id = "spatial:points",
      label = paste(source_name, "spots"),
      unit = "spot"
    )
  }

  if (isTRUE(cellphenotyper$enabled) && as.integer(cellphenotyper$cell_count %||% 0L) > 0L) {
    sources[[length(sources) + 1L]] <- list(
      id = "cellphenotyper:cells",
      label = "CellPhenotyper cells",
      unit = "cell"
    )
  }

  list(
    enabled = length(sources) > 0L,
    sources = sources
  )
}

wsi_proximity_validate_payload <- function(payload) {
  if (!is.list(payload)) {
    wsi_abort("Proximity request must be a JSON object.")
  }
  allowed <- c(
    "point_source", "query_annotations", "target_annotations", "rois",
    "max_pairs", "action", "feature_source", "method", "quantile_step",
    "max_features", "reduction_dims", "distance_unit"
  )
  unknown <- setdiff(names(payload), allowed)
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Proximity request contains unsupported field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  payload
}

wsi_proximity_feature_source <- function(point_source, feature_source = NULL) {
  feature_source <- as.character(feature_source %||% "")
  if (nzchar(feature_source) && !identical(feature_source, "auto")) {
    return(feature_source)
  }
  point_source <- as.character(point_source %||% "")
  if (startsWith(point_source, "cellphenotyper:")) {
    "cellphenotyper:numeric"
  } else {
    "spatial:raw"
  }
}

wsi_proximity_same_selection <- function(result, point_source, query_ids, target_ids) {
  if (!inherits(result, "wsi_proximity_result") || !nrow(result)) {
    return(FALSE)
  }
  existing_source <- as.character(attr(result, "point_source", exact = TRUE) %||%
    result$point_source[[1L]] %||% "")
  if (!identical(existing_source, as.character(point_source %||% ""))) {
    return(FALSE)
  }
  same_set <- function(a, b) {
    a <- sort(unique(as.character(a[nzchar(a) & !is.na(a)])))
    b <- sort(unique(as.character(b[nzchar(b) & !is.na(b)])))
    identical(a, b)
  }
  same_set(result$query_annotation_id, query_ids) &&
    same_set(result$target_annotation_id, target_ids)
}

wsi_proximity_result_from_payload <- function(context, state, payload, rois = NULL,
                                              force = FALSE) {
  rois <- rois %||% if (!is.null(payload$rois)) {
    wsi_rois_from_payload(payload$rois)
  } else {
    state$rois %||% wsi_empty_roi()
  }
  if (inherits(rois, "wsi_roi")) {
    state$rois <- rois
  }
  source <- payload$point_source %||% "spatial:points"
  query_ids <- payload$query_annotations %||% character()
  target_ids <- payload$target_annotations %||% character()
  if (!isTRUE(force) &&
      wsi_proximity_same_selection(state$proximity %||% NULL, source, query_ids, target_ids)) {
    return(list(result = state$proximity, rois = rois, source = source, recomputed = FALSE))
  }
  result <- wsi_proximity_run(
    context = context,
    rois = rois,
    point_source = source,
    query_ids = query_ids,
    target_ids = target_ids,
    pixel_size = state$pixel_size %||% NULL,
    max_pairs = payload$max_pairs %||% 5e6
  )
  layer <- wsi_proximity_layer(result)
  state$proximity <- result
  state$layers <- wsi_viewer_set_layer(state$layers, layer)
  wsi_viewer_queue_command(state, "add_layer", list(layer = layer))
  list(result = result, rois = rois, source = source, recomputed = TRUE)
}

wsi_proximity_roi_metadata <- function(rois, roi_id) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois) || is.na(roi_id) || !nzchar(roi_id)) {
    return(list(name = NA_character_, class = NA_character_))
  }
  idx <- match(roi_id, as.character(rois$roi_id))
  if (is.na(idx)) {
    return(list(name = NA_character_, class = NA_character_))
  }
  list(
    name = as.character(rois$name[[idx]] %||% roi_id),
    class = wsi_roi_class(rois$class[[idx]] %||% NA_character_)
  )
}

wsi_proximity_nearest <- function(query, target, max_pairs = 5e6) {
  if (!nrow(query) || !nrow(target)) {
    return(list(index = integer(), distance = numeric()))
  }
  max_pairs <- suppressWarnings(as.numeric(max_pairs %||% 5e6))
  if (!is.finite(max_pairs) || max_pairs < 1) {
    max_pairs <- 5e6
  }
  chunk <- max(1L, min(nrow(query), floor(max_pairs / max(1L, nrow(target)))))
  nearest_index <- integer(nrow(query))
  nearest_distance <- numeric(nrow(query))
  target_x <- as.numeric(target$x)
  target_y <- as.numeric(target$y)
  for (start in seq.int(1L, nrow(query), by = chunk)) {
    end <- min(nrow(query), start + chunk - 1L)
    rows <- seq.int(start, end)
    qx <- as.numeric(query$x[rows])
    qy <- as.numeric(query$y[rows])
    dist2 <- outer(qx, target_x, "-")^2 + outer(qy, target_y, "-")^2
    idx <- max.col(-dist2, ties.method = "first")
    nearest_index[rows] <- idx
    nearest_distance[rows] <- sqrt(dist2[cbind(seq_along(rows), idx)])
  }
  list(index = nearest_index, distance = nearest_distance)
}

wsi_proximity_colour <- function(distance) {
  distance <- as.numeric(distance)
  ok <- is.finite(distance)
  if (!any(ok)) {
    return(rep("#94A3B8", length(distance)))
  }
  rng <- range(distance[ok])
  if (!is.finite(diff(rng)) || diff(rng) <= .Machine$double.eps) {
    value <- rep(0.5, length(distance))
  } else {
    value <- (distance - rng[[1L]]) / diff(rng)
  }
  value[!is.finite(value)] <- 0.5
  grDevices::hcl.colors(101L, palette = "Inferno")[pmax(1L, pmin(101L, floor(value * 100) + 1L))]
}

wsi_proximity_stat <- function(x, fun) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) {
    return(NA_real_)
  }
  unname(fun(x))
}

wsi_proximity_legend <- function(result) {
  px <- as.numeric(result$distance_px %||% numeric())
  ok <- is.finite(px)
  if (!any(ok)) {
    return(NULL)
  }

  px_stops <- c(
    near = min(px[ok], na.rm = TRUE),
    median = stats::median(px[ok], na.rm = TRUE),
    far = max(px[ok], na.rm = TRUE)
  )

  um <- as.numeric(result$distance_um %||% rep(NA_real_, length(px)))
  use_um <- length(um) == length(px) && any(is.finite(um))
  values <- if (use_um) {
    c(
      near = min(um[is.finite(um)], na.rm = TRUE),
      median = stats::median(um[is.finite(um)], na.rm = TRUE),
      far = max(um[is.finite(um)], na.rm = TRUE)
    )
  } else {
    px_stops
  }

  colours <- wsi_proximity_colour(px_stops)
  stops <- lapply(seq_along(px_stops), function(i) {
    list(
      name = names(px_stops)[[i]],
      value = unname(as.numeric(values[[i]])),
      distance_px = unname(as.numeric(px_stops[[i]])),
      colour = colours[[i]]
    )
  })

  list(
    type = "continuous",
    title = "Distance to reference",
    unit = if (use_um) "um" else "px",
    palette = "Inferno",
    stops = stops
  )
}

wsi_proximity_layer <- function(result, radius = 8) {
  if (!is.data.frame(result) || !nrow(result)) {
    return(list(
      id = "wsi_proximity_distance",
      name = "Proximity distance",
      type = "vector",
      source_type = "proximity",
      visible = TRUE,
      opacity = 0.95,
      colour = "#F97316",
      replace = TRUE,
      count = 0L,
      items = list()
    ))
  }
  colours <- wsi_proximity_colour(result$distance_px)
  items <- lapply(seq_len(nrow(result)), function(i) {
    colour <- colours[[i]]
    list(
      id = paste0("proximity_", result$id[[i]] %||% i),
      name = result$label[[i]] %||% result$id[[i]] %||% paste0("point_", i),
      label = result$label[[i]] %||% result$id[[i]] %||% paste0("point_", i),
      class = result$query_class[[i]] %||% "query",
      type = "point",
      x = unname(as.numeric(result$x[[i]])),
      y = unname(as.numeric(result$y[[i]])),
      radius = radius,
      source = "proximity",
      colour = colour,
      fill = wsi_viewer_hex_to_rgba(colour, alpha = 0.36),
      proximity_distance_px = unname(as.numeric(result$distance_px[[i]])),
      proximity_distance_um = unname(as.numeric(result$distance_um[[i]]))
    )
  })
  list(
    id = "wsi_proximity_distance",
    name = "Proximity distance",
    type = "vector",
    source_type = "proximity",
    visible = TRUE,
    opacity = 0.95,
    colour = "#F97316",
    replace = TRUE,
    count = length(items),
    min_distance_px = wsi_proximity_stat(result$distance_px, min),
    max_distance_px = wsi_proximity_stat(result$distance_px, max),
    legend = wsi_proximity_legend(result),
    items = items
  )
}

wsi_proximity_run <- function(context, rois, point_source = "spatial:points",
                              query_ids, target_ids, pixel_size = NULL,
                              max_pairs = 5e6) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    wsi_abort("Draw or import at least two annotations before running proximity analysis.")
  }
  query_ids <- as.character(query_ids %||% character())
  target_ids <- as.character(target_ids %||% character())
  query_ids <- query_ids[nzchar(query_ids) & !is.na(query_ids)]
  target_ids <- target_ids[nzchar(target_ids) & !is.na(target_ids)]
  if (!length(query_ids)) {
    wsi_abort("Select at least one query annotation containing the spots/cells to measure.")
  }
  if (!length(target_ids)) {
    wsi_abort("Select at least one reference annotation to measure distance from.")
  }

  points <- wsi_prediction_points(context, point_source)
  ok <- is.finite(points$x) & is.finite(points$y)
  points <- points[ok, , drop = FALSE]
  if (!nrow(points)) {
    wsi_abort("No spatial spots or cells with finite coordinates are available for proximity analysis.")
  }

  query <- wsi_prediction_assign_points(points, rois, query_ids)
  target <- wsi_prediction_assign_points(points, rois, target_ids)
  query_rows <- which(!is.na(query$label) & nzchar(query$label))
  target_rows <- which(!is.na(target$label) & nzchar(target$label))
  if (!length(query_rows)) {
    wsi_abort("No spots/cells were found inside the selected query annotation(s).")
  }
  if (!length(target_rows)) {
    wsi_abort("No spots/cells were found inside the selected reference annotation(s).")
  }

  query_points <- points[query_rows, , drop = FALSE]
  target_points <- points[target_rows, , drop = FALSE]
  nearest <- wsi_proximity_nearest(query_points, target_points, max_pairs = max_pairs)
  target_nearest_rows <- target_rows[nearest$index]
  px <- tryCatch(wsi_pixel_size_xy(pixel_size), error = function(err) NULL)

  query_meta <- lapply(query$roi_id[query_rows], function(id) wsi_proximity_roi_metadata(rois, id))
  target_meta <- lapply(target$roi_id[target_nearest_rows], function(id) wsi_proximity_roi_metadata(rois, id))
  unit <- as.character(query_points$unit %||% points$unit %||% "point")

  out <- data.frame(
    id = as.character(query_points$id),
    label = as.character(query_points$label),
    unit = unit,
    x = as.numeric(query_points$x),
    y = as.numeric(query_points$y),
    query_annotation_id = as.character(query$roi_id[query_rows]),
    query_annotation = vapply(query_meta, `[[`, character(1), "name"),
    query_class = vapply(query_meta, `[[`, character(1), "class"),
    target_annotation_id = as.character(target$roi_id[target_nearest_rows]),
    target_annotation = vapply(target_meta, `[[`, character(1), "name"),
    target_class = vapply(target_meta, `[[`, character(1), "class"),
    nearest_target_id = as.character(target_points$id[nearest$index]),
    nearest_target_label = as.character(target_points$label[nearest$index]),
    nearest_target_x = as.numeric(target_points$x[nearest$index]),
    nearest_target_y = as.numeric(target_points$y[nearest$index]),
    distance_px = as.numeric(nearest$distance),
    distance_um = if (is.null(px)) NA_real_ else as.numeric(nearest$distance) * mean(px),
    point_source = as.character(point_source),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_proximity_result", class(out))
  attr(out, "query_count") <- length(query_rows)
  attr(out, "target_count") <- length(target_rows)
  attr(out, "point_source") <- as.character(point_source)
  out
}

wsi_proximity_stats_feature_matrix <- function(context, proximity, feature_source,
                                               point_source = NULL,
                                               max_features = 5000L,
                                               reduction_dims = NULL) {
  ids <- as.character(proximity$id %||% character())
  if (!length(ids)) {
    wsi_abort("Run proximity analysis before running proximity statistics.")
  }
  point_source <- as.character(point_source %||%
    attr(proximity, "point_source", exact = TRUE) %||%
    proximity$point_source[[1L]] %||% "spatial:points")
  points <- wsi_prediction_points(context, point_source)
  idx <- match(ids, as.character(points$id))
  if (!any(!is.na(idx))) {
    wsi_abort("Could not align proximity result IDs to the live spatial/cell table.")
  }
  keep <- !is.na(idx)
  points <- points[idx[keep], , drop = FALSE]
  ids <- ids[keep]
  proximity <- proximity[keep, , drop = FALSE]

  x <- wsi_prediction_feature_matrix(
    context,
    feature_source,
    ids,
    reduction_dims = reduction_dims,
    points = points
  )
  if (!is.matrix(x) || !nrow(x) || !ncol(x)) {
    wsi_abort("No numeric feature matrix was found for proximity statistics.")
  }
  storage.mode(x) <- "double"
  finite_col <- colSums(is.finite(x)) >= 2L
  x <- x[, finite_col, drop = FALSE]
  if (!ncol(x)) {
    wsi_abort("The feature matrix has no columns with at least two finite values.")
  }
  variance <- apply(x, 2L, stats::var, na.rm = TRUE)
  keep_col <- is.finite(variance) & variance > 0
  x <- x[, keep_col, drop = FALSE]
  variance <- variance[keep_col]
  if (!ncol(x)) {
    wsi_abort("The feature matrix has no variable numeric features.")
  }
  max_features <- suppressWarnings(as.integer(max_features %||% 5000L))
  if (is.finite(max_features) && max_features > 0L && ncol(x) > max_features) {
    ord <- order(variance, decreasing = TRUE)
    x <- x[, ord[seq_len(max_features)], drop = FALSE]
  }
  list(x = x, proximity = proximity)
}

wsi_proximity_binned_matrix <- function(proximity, x, quantile_step = 0.005,
                                        distance_unit = c("auto", "um", "px")) {
  distance_unit <- match.arg(distance_unit)
  px <- as.numeric(proximity$distance_px %||% numeric())
  um <- as.numeric(proximity$distance_um %||% rep(NA_real_, length(px)))
  use_um <- identical(distance_unit, "um") ||
    (identical(distance_unit, "auto") && length(um) == length(px) && any(is.finite(um)))
  y <- if (use_um) um else px
  unit <- if (use_um) "um" else "px"
  ok <- is.finite(y) & stats::complete.cases(x)
  if (sum(ok) < 3L) {
    wsi_abort("At least three complete spots/cells with finite distances are needed for proximity statistics.")
  }
  y <- y[ok]
  x <- x[ok, , drop = FALSE]
  quantile_step <- suppressWarnings(as.numeric(quantile_step %||% 0.005))
  if (!is.finite(quantile_step) || quantile_step <= 0 || quantile_step > 0.5) {
    quantile_step <- 0.005
  }
  probs <- unique(c(seq(0, 1, by = quantile_step), 1))
  breaks <- unique(as.numeric(stats::quantile(y, probs = probs, na.rm = TRUE, names = FALSE)))
  breaks <- breaks[is.finite(breaks)]
  if (length(breaks) < 3L) {
    breaks <- pretty(range(y, na.rm = TRUE), n = min(20L, max(3L, length(unique(y)))))
    breaks <- breaks[breaks >= min(y, na.rm = TRUE) & breaks <= max(y, na.rm = TRUE)]
    breaks <- unique(c(min(y, na.rm = TRUE), breaks, max(y, na.rm = TRUE)))
  }
  if (length(breaks) < 3L) {
    wsi_abort("Proximity distances do not contain enough variation to create distance bins.")
  }
  distance_binned <- cut(y, breaks = breaks, include.lowest = TRUE, ordered_result = TRUE)
  levels_used <- levels(distance_binned)
  gene_binned <- apply(x, 2L, function(value) {
    out <- tapply(value, distance_binned, mean, na.rm = TRUE)
    as.numeric(out[levels_used])
  })
  gene_binned <- as.matrix(gene_binned)
  rownames(gene_binned) <- levels_used
  lower <- breaks[-length(breaks)]
  upper <- breaks[-1L]
  centers <- (lower + upper) / 2
  list(
    x = gene_binned,
    distance = centers[seq_len(nrow(gene_binned))],
    breaks = breaks,
    unit = unit,
    n_points = length(y)
  )
}

wsi_proximity_cor_table <- function(gene_binned, distance, method) {
  method <- match.arg(method, c("spearman", "pearson"))
  rows <- lapply(seq_len(ncol(gene_binned)), function(j) {
    values <- as.numeric(gene_binned[, j])
    ok <- is.finite(values) & is.finite(distance)
    if (sum(ok) < 3L || stats::var(values[ok]) <= 0 || stats::var(distance[ok]) <= 0) {
      statistic <- NA_real_
      p_value <- NA_real_
      estimate <- NA_real_
    } else {
      test <- suppressWarnings(stats::cor.test(distance[ok], values[ok], method = method))
      statistic <- unname(as.numeric(test$statistic[[1L]] %||% NA_real_))
      p_value <- unname(as.numeric(test$p.value %||% NA_real_))
      estimate <- unname(as.numeric(test$estimate[[1L]] %||% NA_real_))
    }
    data.frame(
      feature = colnames(gene_binned)[[j]],
      method = method,
      statistic = statistic,
      correlation = estimate,
      p_value = p_value,
      MIC = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(abs(out$correlation), -log10(out$p_value), decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

wsi_proximity_mine_table <- function(gene_binned, distance) {
  if (!requireNamespace("clinical", quietly = TRUE)) {
    wsi_abort(
      "MINE proximity statistics need the optional package `clinical`. Install it or choose Spearman/Pearson.",
      class = "wsi_missing_dependency"
    )
  }
  ma <- clinical::multi_analysis(
    gene_binned,
    distance,
    FUN = "correlation.test",
    method = "MINE"
  )
  out <- as.data.frame(ma, stringsAsFactors = FALSE)
  if (!"feature" %in% names(out)) {
    rn <- rownames(out)
    out$feature <- if (length(rn)) rn else colnames(gene_binned)[seq_len(nrow(out))]
  }
  if (!"method" %in% names(out)) {
    out$method <- "MINE"
  }
  if (!"MIC" %in% names(out)) {
    out$MIC <- NA_real_
  }
  if (!"statistic" %in% names(out)) {
    out$statistic <- NA_real_
  }
  if (!"correlation" %in% names(out)) {
    out$correlation <- NA_real_
  }
  if (!"p_value" %in% names(out)) {
    p_col <- names(out)[tolower(names(out)) %in% c("p.value", "pvalue", "p")]
    out$p_value <- if (length(p_col)) suppressWarnings(as.numeric(out[[p_col[[1L]]]])) else NA_real_
  }
  out$MIC <- suppressWarnings(as.numeric(out$MIC))
  out <- out[order(out$MIC, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  out[, unique(c("feature", "method", "statistic", "correlation", "p_value", "MIC", names(out))), drop = FALSE]
}

wsi_proximity_stats_run <- function(context, proximity, feature_source = NULL,
                                    method = c("spearman", "pearson", "mine"),
                                    quantile_step = 0.005,
                                    max_features = 5000L,
                                    reduction_dims = NULL,
                                    distance_unit = c("auto", "um", "px")) {
  if (!inherits(proximity, "wsi_proximity_result") || !nrow(proximity)) {
    wsi_abort("Run proximity analysis before running proximity statistics.")
  }
  method <- match.arg(tolower(as.character(method %||% "spearman")), c("spearman", "pearson", "mine"))
  point_source <- as.character(attr(proximity, "point_source", exact = TRUE) %||%
    proximity$point_source[[1L]] %||% "spatial:points")
  feature_source <- wsi_proximity_feature_source(point_source, feature_source)
  features <- wsi_proximity_stats_feature_matrix(
    context = context,
    proximity = proximity,
    feature_source = feature_source,
    point_source = point_source,
    max_features = max_features,
    reduction_dims = reduction_dims
  )
  binned <- wsi_proximity_binned_matrix(
    proximity = features$proximity,
    x = features$x,
    quantile_step = quantile_step,
    distance_unit = match.arg(distance_unit)
  )
  table <- if (identical(method, "mine")) {
    wsi_proximity_mine_table(binned$x, binned$distance)
  } else {
    wsi_proximity_cor_table(binned$x, binned$distance, method = method)
  }
  table$rank <- seq_len(nrow(table))
  table$n_bins <- nrow(binned$x)
  table$n_points <- binned$n_points
  table$distance_unit <- binned$unit
  table$feature_source <- feature_source
  table <- table[, unique(c(
    "rank", "feature", "method", "statistic", "correlation", "p_value", "MIC",
    "n_bins", "n_points", "distance_unit", "feature_source", names(table)
  )), drop = FALSE]
  class(table) <- c("wsi_proximity_stats_result", class(table))
  attr(table, "gene_binned") <- binned$x
  attr(table, "break_points") <- binned$breaks[-length(binned$breaks)]
  attr(table, "distance_unit") <- binned$unit
  attr(table, "feature_source") <- feature_source
  table
}

wsi_proximity_stats_detail <- function(stats) {
  list(
    count = nrow(stats),
    method = as.character(stats$method[[1L]] %||% NA_character_),
    feature_source = as.character(attr(stats, "feature_source", exact = TRUE) %||%
      stats$feature_source[[1L]] %||% NA_character_),
    distance_unit = as.character(attr(stats, "distance_unit", exact = TRUE) %||%
      stats$distance_unit[[1L]] %||% NA_character_),
    n_bins = as.integer(stats$n_bins[[1L]] %||% NA_integer_),
    n_points = as.integer(stats$n_points[[1L]] %||% NA_integer_)
  )
}

wsi_proximity_stats_response <- function(context, state, payload) {
  payload <- wsi_proximity_validate_payload(payload)
  rois <- if (!is.null(payload$rois)) {
    wsi_rois_from_payload(payload$rois)
  } else {
    state$rois %||% wsi_empty_roi()
  }
  result_info <- wsi_proximity_result_from_payload(
    context = context,
    state = state,
    payload = payload,
    rois = rois,
    force = FALSE
  )
  wsi_viewer_state_record_event(
    state,
    "proximity_stats_started",
    list(
      point_source = result_info$source,
      feature_source = wsi_proximity_feature_source(result_info$source, payload$feature_source %||% NULL),
      method = payload$method %||% "spearman"
    )
  )
  stats <- wsi_proximity_stats_run(
    context = context,
    proximity = result_info$result,
    feature_source = payload$feature_source %||% NULL,
    method = payload$method %||% "spearman",
    quantile_step = payload$quantile_step %||% 0.005,
    max_features = payload$max_features %||% 5000L,
    reduction_dims = payload$reduction_dims %||% NULL,
    distance_unit = payload$distance_unit %||% "auto"
  )
  state$proximity_stats <- stats
  detail <- wsi_proximity_stats_detail(stats)
  detail$recomputed_proximity <- isTRUE(result_info$recomputed)
  wsi_viewer_state_record_event(state, "proximity_stats_finished", detail)
  response <- wsi_viewer_state_response(state)
  response$proximity_stats <- detail
  response$proximity_stats_rows <- stats
  response
}

wsi_proximity_response <- function(context, state, payload) {
  payload <- wsi_proximity_validate_payload(payload)
  action <- tolower(as.character(payload$action %||% "distance"))
  if (identical(action, "stats") || identical(action, "statistics")) {
    return(wsi_proximity_stats_response(context = context, state = state, payload = payload))
  }
  rois <- if (!is.null(payload$rois)) {
    wsi_rois_from_payload(payload$rois)
  } else {
    state$rois %||% wsi_empty_roi()
  }
  if (inherits(rois, "wsi_roi")) {
    state$rois <- rois
  }
  source <- payload$point_source %||% "spatial:points"
  wsi_viewer_state_record_event(state, "proximity_started", list(point_source = source))
  result <- wsi_proximity_result_from_payload(
    context = context,
    state = state,
    payload = payload,
    rois = rois,
    force = TRUE
  )$result
  detail <- list(
    count = nrow(result),
    point_source = source,
    query_count = attr(result, "query_count") %||% nrow(result),
    target_count = attr(result, "target_count") %||% NA_integer_,
    min_distance_px = wsi_proximity_stat(result$distance_px, min),
    median_distance_px = wsi_proximity_stat(result$distance_px, stats::median),
    max_distance_px = wsi_proximity_stat(result$distance_px, max),
    min_distance_um = wsi_proximity_stat(result$distance_um, min),
    median_distance_um = wsi_proximity_stat(result$distance_um, stats::median),
    max_distance_um = wsi_proximity_stat(result$distance_um, max)
  )
  wsi_viewer_state_record_event(state, "proximity_finished", detail)
  response <- wsi_viewer_state_response(state)
  response$proximity <- detail
  response
}
