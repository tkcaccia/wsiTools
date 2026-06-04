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
    "max_pairs"
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

wsi_proximity_response <- function(context, state, payload) {
  payload <- wsi_proximity_validate_payload(payload)
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
  result <- wsi_proximity_run(
    context = context,
    rois = rois,
    point_source = source,
    query_ids = payload$query_annotations %||% character(),
    target_ids = payload$target_annotations %||% character(),
    pixel_size = state$pixel_size %||% NULL,
    max_pairs = payload$max_pairs %||% 5e6
  )
  layer <- wsi_proximity_layer(result)
  state$proximity <- result
  state$layers <- wsi_viewer_set_layer(state$layers, layer)
  wsi_viewer_queue_command(state, "add_layer", list(layer = layer))
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
