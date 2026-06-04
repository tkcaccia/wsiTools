#' Check whether fastPLS is available
#'
#' `wsi_has_fastpls()` reports whether the optional GitHub package
#' `tkcaccia/fastPLS` is installed. It is only needed for the live viewer
#' **Prediction** menu; wsiTools can be installed and used without it.
#'
#' @return `TRUE` when `fastPLS` can be loaded, otherwise `FALSE`.
#' @export
wsi_has_fastpls <- function() {
  requireNamespace(wsi_fastpls_package(), quietly = TRUE)
}

wsi_fastpls_package <- function() {
  paste0("fast", "PLS")
}

wsi_empty_prediction_result <- function() {
  out <- data.frame(
    id = character(),
    label = character(),
    x = numeric(),
    y = numeric(),
    set = character(),
    train_annotation_id = character(),
    test_annotation_id = character(),
    observed = character(),
    predicted = character(),
    feature_source = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_prediction_result", class(out))
  out
}

wsi_prediction_config <- function(seurat = NULL, cellphenotyper = NULL) {
  sources <- list()
  seurat <- seurat %||% list(enabled = FALSE)
  cellphenotyper <- cellphenotyper %||% list(enabled = FALSE)

  if (isTRUE(seurat$enabled) && as.integer(seurat$spot_count %||% 0L) > 0L) {
    source_name <- as.character(seurat$source_name %||% "Spatial")
    sources[[length(sources) + 1L]] <- list(
      id = "spatial:raw",
      label = paste(source_name, "raw expression"),
      type = "raw_expression",
      unit = "spot"
    )
    plots <- seurat$plots %||% list()
    if (length(plots)) {
      for (i in seq_along(plots)) {
        plot <- plots[[i]]
        reduction <- as.character(plot$reduction %||% plot$label %||% paste0("reduction_", i))
        sources[[length(sources) + 1L]] <- list(
          id = sprintf("spatial:reduction:%d", i - 1L),
          label = paste(source_name, wsi_reduction_label(reduction)),
          type = "reduction",
          reduction = reduction,
          unit = "spot"
        )
      }
    }
  }

  if (isTRUE(cellphenotyper$enabled) && as.integer(cellphenotyper$cell_count %||% 0L) > 0L) {
    sources[[length(sources) + 1L]] <- list(
      id = "cellphenotyper:numeric",
      label = "CellPhenotyper numeric cell table",
      type = "cell_table",
      unit = "cell"
    )
  }

  list(
    enabled = length(sources) > 0L,
    sources = sources,
    fastpls_installed = wsi_has_fastpls(),
    fastpls_install = "remotes::install_github(\"tkcaccia/fastPLS\")"
  )
}

wsi_prediction_context_enabled <- function(context) {
  context <- context %||% list()
  spatial <- context$spatial %||% context$seurat %||% NULL
  cellphenotyper_project <- context$cellphenotyper_project %||% context$cellphenotyper %||% NULL
  isTRUE(inherits(spatial, "wsi_seurat_spatial") || inherits(spatial, "wsi_spatial_object")) ||
    isTRUE(inherits(cellphenotyper_project, "wsi_cellphenotyper_project"))
}

#' Create live viewer prediction context
#'
#' `wsi_prediction_context()` is a small advanced helper used by
#' [wsi_viewer_live()] to expose R-side feature matrices to the browser
#' **Prediction** menu without sending raw expression data to JavaScript. The
#' ordinary Seurat, Giotto, SpatialExperiment, and CellPhenotyper viewer helper
#' functions set this automatically.
#'
#' @param spatial A linked spatial object returned by
#'   [wsi_link_seurat_image()] or the Giotto/SpatialExperiment linkers.
#' @param cellphenotyper_project A CellPhenotyper project returned by
#'   [wsi_read_cellphenotyper_project()].
#'
#' @return A list used internally by the live viewer prediction endpoint.
#' @export
wsi_prediction_context <- function(spatial = NULL, cellphenotyper_project = NULL) {
  list(
    spatial = spatial,
    cellphenotyper_project = cellphenotyper_project
  )
}

wsi_prediction_spatial_points <- function(linked) {
  spots <- linked$spots %||% NULL
  if (!is.data.frame(spots) || !nrow(spots)) {
    return(data.frame(id = character(), label = character(), x = numeric(), y = numeric(), stringsAsFactors = FALSE))
  }
  id <- as.character(spots$id %||% spots$barcode %||% spots$label %||% seq_len(nrow(spots)))
  label <- as.character(spots$label %||% spots$barcode %||% spots$id %||% id)
  data.frame(
    id = id,
    label = label,
    x = as.numeric(spots$x),
    y = as.numeric(spots$y),
    stringsAsFactors = FALSE
  )
}

wsi_prediction_cell_points <- function(project) {
  cells <- project$cells %||% NULL
  if (!is.data.frame(cells) || !nrow(cells)) {
    return(data.frame(id = character(), label = character(), x = numeric(), y = numeric(), stringsAsFactors = FALSE))
  }
  id <- as.character(cells$id %||% cells$cell_id %||% cells$label %||% seq_len(nrow(cells)))
  label <- as.character(cells$label %||% cells$id %||% cells$cell_id %||% id)
  data.frame(
    id = id,
    label = label,
    x = as.numeric(cells$x %||% cells$cellphenotyper_x),
    y = as.numeric(cells$y %||% cells$cellphenotyper_y),
    stringsAsFactors = FALSE
  )
}

wsi_prediction_points <- function(context, source_id) {
  context <- context %||% list()
  source_id <- as.character(source_id %||% "")
  if (startsWith(source_id, "cellphenotyper:")) {
    project <- context$cellphenotyper_project %||% context$cellphenotyper %||% NULL
    if (!inherits(project, "wsi_cellphenotyper_project")) {
      wsi_abort("No live CellPhenotyper project is attached to this viewer session.")
    }
    points <- wsi_prediction_cell_points(project)
    points$unit <- "cell"
    return(points)
  }
  linked <- context$spatial %||% context$seurat %||% NULL
  if (!inherits(linked, "wsi_seurat_spatial") && !inherits(linked, "wsi_spatial_object")) {
    wsi_abort("No live Seurat/Giotto/SpatialExperiment object is attached to this viewer session.")
  }
  points <- wsi_prediction_spatial_points(linked)
  points$unit <- "spot"
  points
}

wsi_prediction_matrix_for_spatial_raw <- function(linked, ids) {
  source <- linked$expression_source %||% list()
  object <- source$object %||% NULL
  if (is.null(object)) {
    wsi_abort("Raw expression prediction needs the original live R object. Reopen the viewer from R with `live = TRUE`.")
  }
  matrices <- wsi_seurat_collect_expression_matrices(object)
  if (!length(matrices)) {
    wsi_abort("No expression matrix was found in the live spatial object.")
  }
  ids <- as.character(ids)
  for (entry in matrices) {
    mat <- entry$matrix
    rn <- tryCatch(rownames(mat), error = function(e) NULL)
    cn <- tryCatch(colnames(mat), error = function(e) NULL)
    if (!length(rn) || !length(cn)) {
      next
    }
    sample_idx <- match(ids, cn)
    if (any(!is.na(sample_idx))) {
      keep <- !is.na(sample_idx)
      out <- matrix(NA_real_, nrow = length(ids), ncol = length(rn))
      rownames(out) <- ids
      colnames(out) <- make.unique(as.character(rn))
      sub <- as.matrix(mat[, sample_idx[keep], drop = FALSE])
      storage.mode(sub) <- "double"
      out[keep, ] <- t(sub)
      attr(out, "source_name") <- entry$name %||% "expression"
      return(out)
    }
    sample_idx <- match(ids, rn)
    if (any(!is.na(sample_idx))) {
      keep <- !is.na(sample_idx)
      out <- matrix(NA_real_, nrow = length(ids), ncol = length(cn))
      rownames(out) <- ids
      colnames(out) <- make.unique(as.character(cn))
      sub <- as.matrix(mat[sample_idx[keep], , drop = FALSE])
      storage.mode(sub) <- "double"
      out[keep, ] <- sub
      attr(out, "source_name") <- entry$name %||% "expression"
      return(out)
    }
  }
  wsi_abort("Could not align expression matrix rows/columns to the viewer spot IDs.")
}

wsi_prediction_matrix_for_spatial_reduction <- function(linked, ids, source_id) {
  plots <- linked$plots %||% list()
  index <- suppressWarnings(as.integer(sub("^spatial:reduction:", "", source_id))) + 1L
  plot <- NULL
  if (is.finite(index) && index >= 1L && index <= length(plots)) {
    plot <- plots[[index]]
  }
  if (is.null(plot) && length(plots)) {
    plot <- plots[[1L]]
  }
  points <- plot$points %||% NULL
  if (!is.data.frame(points) || !nrow(points)) {
    wsi_abort("The selected dimensionality reduction source does not contain usable points.")
  }
  point_ids <- as.character(points$spot_id %||% points$label %||% points$id)
  idx <- match(ids, point_ids)
  out <- matrix(NA_real_, nrow = length(ids), ncol = 2L)
  rownames(out) <- ids
  colnames(out) <- c(
    as.character(plot$x_label %||% "dim1"),
    as.character(plot$y_label %||% "dim2")
  )
  keep <- !is.na(idx)
  out[keep, 1L] <- as.numeric(points$x[idx[keep]])
  out[keep, 2L] <- as.numeric(points$y[idx[keep]])
  attr(out, "source_name") <- plot$label %||% plot$reduction %||% "reduction"
  out
}

wsi_prediction_matrix_for_cells <- function(project, ids) {
  cells <- project$cells %||% NULL
  if (!is.data.frame(cells) || !nrow(cells)) {
    wsi_abort("No CellPhenotyper cell table is attached to this live viewer session.")
  }
  cell_ids <- as.character(cells$id %||% cells$cell_id %||% cells$label %||% seq_len(nrow(cells)))
  idx <- match(ids, cell_ids)
  if (!any(!is.na(idx))) {
    wsi_abort("Could not align CellPhenotyper cell table rows to viewer cell IDs.")
  }
  numeric_cols <- names(cells)[vapply(cells, is.numeric, logical(1))]
  excluded <- c(
    "x", "y", "x_orig", "y_orig", "global_x", "global_y", "slide_x", "slide_y",
    "centroid_x", "centroid_y", "center_x", "center_y", "centre_x", "centre_y",
    "cellphenotyper_x", "cellphenotyper_y"
  )
  numeric_cols <- setdiff(numeric_cols, excluded)
  if (!length(numeric_cols)) {
    wsi_abort("The CellPhenotyper cell table does not contain numeric predictor columns beyond coordinates.")
  }
  out <- matrix(NA_real_, nrow = length(ids), ncol = length(numeric_cols))
  rownames(out) <- ids
  colnames(out) <- make.unique(numeric_cols)
  sub <- as.matrix(cells[idx[!is.na(idx)], numeric_cols, drop = FALSE])
  storage.mode(sub) <- "double"
  out[!is.na(idx), ] <- sub
  attr(out, "source_name") <- "CellPhenotyper numeric cell table"
  out
}

wsi_prediction_feature_matrix <- function(context, source_id, ids) {
  context <- context %||% list()
  source_id <- as.character(source_id %||% "spatial:raw")
  if (identical(source_id, "cellphenotyper:numeric")) {
    return(wsi_prediction_matrix_for_cells(
      context$cellphenotyper_project %||% context$cellphenotyper,
      ids
    ))
  }
  linked <- context$spatial %||% context$seurat %||% NULL
  if (!inherits(linked, "wsi_seurat_spatial") && !inherits(linked, "wsi_spatial_object")) {
    wsi_abort("No live spatial object is attached to this viewer session.")
  }
  if (identical(source_id, "spatial:raw")) {
    return(wsi_prediction_matrix_for_spatial_raw(linked, ids))
  }
  if (startsWith(source_id, "spatial:reduction:")) {
    return(wsi_prediction_matrix_for_spatial_reduction(linked, ids, source_id))
  }
  wsi_abort(sprintf("Unsupported prediction feature source `%s`.", source_id))
}

wsi_prediction_feature_filter <- function(x, max_features = NULL) {
  if (!is.matrix(x) || !nrow(x) || !ncol(x)) {
    wsi_abort("Prediction feature matrix is empty.")
  }
  finite_col <- colSums(is.finite(x)) >= 2L
  x <- x[, finite_col, drop = FALSE]
  if (!ncol(x)) {
    wsi_abort("Prediction feature matrix contains no columns with enough finite values.")
  }
  means <- colMeans(x, na.rm = TRUE)
  for (j in seq_len(ncol(x))) {
    miss <- !is.finite(x[, j])
    if (any(miss)) {
      x[miss, j] <- means[[j]]
    }
  }
  variance <- apply(x, 2L, stats::var)
  keep <- is.finite(variance) & variance > 0
  x <- x[, keep, drop = FALSE]
  variance <- variance[keep]
  if (!ncol(x)) {
    wsi_abort("Prediction features have zero variance after filtering.")
  }
  max_features <- suppressWarnings(as.integer(max_features %||% 0L))
  if (is.finite(max_features) && max_features > 0L && ncol(x) > max_features) {
    ord <- order(variance, decreasing = TRUE)
    keep_idx <- ord[seq_len(max_features)]
    x <- x[, keep_idx, drop = FALSE]
  }
  x
}

wsi_prediction_roi_label <- function(rois, index) {
  label <- as.character(rois$class[[index]] %||% "")
  if (!nzchar(label) || is.na(label)) {
    label <- as.character(rois$name[[index]] %||% rois$roi_id[[index]] %||% sprintf("ROI %d", index))
  }
  label
}

wsi_prediction_selected_roi_indices <- function(rois, ids, include_all = FALSE) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    return(integer())
  }
  if (isTRUE(include_all)) {
    return(seq_len(nrow(rois)))
  }
  ids <- as.character(ids %||% character())
  ids <- ids[nzchar(ids) & !is.na(ids)]
  ids <- setdiff(ids, c("__all__", "__all_unlabelled__", "__all_non_training__"))
  if (!length(ids)) {
    return(integer())
  }
  keys <- cbind(
    as.character(rois$roi_id %||% ""),
    as.character(rois$name %||% ""),
    as.character(rois$class %||% "")
  )
  which(apply(keys, 1L, function(row) any(row %in% ids)))
}

wsi_prediction_assign_points <- function(points, rois, ids, include_all = FALSE) {
  label <- rep(NA_character_, nrow(points))
  roi_id <- rep(NA_character_, nrow(points))
  indices <- wsi_prediction_selected_roi_indices(rois, ids, include_all = include_all)
  if (!length(indices)) {
    return(list(label = label, roi_id = roi_id, indices = indices))
  }
  for (i in indices) {
    inside <- wsi_points_in_roi(rois, i, points$x, points$y)
    inside <- inside & is.na(label)
    if (!any(inside, na.rm = TRUE)) {
      next
    }
    label[inside] <- wsi_prediction_roi_label(rois, i)
    roi_id[inside] <- as.character(rois$roi_id[[i]] %||% i)
  }
  list(label = label, roi_id = roi_id, indices = indices)
}

wsi_prediction_palette <- function(labels) {
  labels <- sort(unique(as.character(labels[nzchar(labels) & !is.na(labels)])))
  if (!length(labels)) {
    return(stats::setNames(character(), character()))
  }
  base <- c(
    "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
    "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62",
    "#8DA0CB", "#E78AC3", "#A6D854", "#FFD92F"
  )
  if (length(labels) > length(base)) {
    extra <- grDevices::hcl.colors(length(labels) - length(base), palette = "Dark 3")
    base <- c(base, extra)
  }
  stats::setNames(base[seq_along(labels)], labels)
}

wsi_prediction_layer <- function(result, radius = 8) {
  if (!is.data.frame(result) || !nrow(result)) {
    return(list(
      id = "wsi_prediction_pls_lda",
      name = "PLS-LDA prediction",
      type = "vector",
      source_type = "prediction",
      visible = TRUE,
      opacity = 0.88,
      replace = TRUE,
      count = 0L,
      items = list()
    ))
  }
  palette <- wsi_prediction_palette(result$predicted)
  items <- lapply(seq_len(nrow(result)), function(i) {
    pred <- as.character(result$predicted[[i]] %||% "")
    colour <- palette[[pred]] %||% "#38BDF8"
    list(
      id = paste0("prediction_", result$id[[i]]),
      name = as.character(result$label[[i]] %||% result$id[[i]]),
      label = as.character(result$label[[i]] %||% result$id[[i]]),
      type = "point",
      x = unname(as.numeric(result$x[[i]])),
      y = unname(as.numeric(result$y[[i]])),
      radius = radius,
      colour = colour,
      fill = wsi_hex_to_rgba(colour, 0.33),
      predicted = pred,
      observed = as.character(result$observed[[i]] %||% NA_character_),
      feature_source = as.character(result$feature_source[[i]] %||% "")
    )
  })
  list(
    id = "wsi_prediction_pls_lda",
    name = "PLS-LDA prediction",
    type = "vector",
    source_type = "prediction",
    visible = TRUE,
    opacity = 0.88,
    colour = "#FACC15",
    replace = TRUE,
    count = length(items),
    items = items,
    metadata = list(
      classes = unname(names(palette)),
      colours = unname(palette)
    )
  )
}

wsi_hex_to_rgba <- function(hex, alpha = 0.3) {
  hex <- as.character(hex %||% "#38BDF8")
  hex <- sub("^#", "", hex)
  if (nchar(hex) == 3L) {
    hex <- paste0(strsplit(hex, "", fixed = TRUE)[[1L]], strsplit(hex, "", fixed = TRUE)[[1L]], collapse = "")
  }
  if (!grepl("^[0-9A-Fa-f]{6}$", hex)) {
    hex <- "38BDF8"
  }
  values <- grDevices::col2rgb(paste0("#", hex))[, 1L]
  sprintf(
    "rgba(%d,%d,%d,%.3f)",
    values[[1L]], values[[2L]], values[[3L]],
    max(0, min(1, as.numeric(alpha %||% 0.3)))
  )
}

wsi_prediction_extract_predicted <- function(model, x_test) {
  candidates <- list(
    model$Ypred,
    model$predicted,
    model$class,
    model$classes,
    model$prediction
  )
  for (candidate in candidates) {
    if (!is.null(candidate)) {
      pred <- candidate
      if (is.data.frame(pred) || is.matrix(pred)) {
        pred <- pred[, ncol(pred), drop = TRUE]
      }
      if (length(dim(pred)) == 3L) {
        pred <- pred[, dim(pred)[[2L]], dim(pred)[[3L]], drop = TRUE]
      }
      if (length(pred)) {
        return(as.character(pred))
      }
    }
  }
  predicted <- tryCatch(stats::predict(model, x_test), error = function(e) NULL)
  if (is.list(predicted)) {
    predicted <- predicted$Ypred %||% predicted$predicted %||% predicted$class %||% predicted$prediction
  }
  if (is.data.frame(predicted) || is.matrix(predicted)) {
    predicted <- predicted[, ncol(predicted), drop = TRUE]
  }
  if (!length(predicted)) {
    wsi_abort("fastPLS completed but no predicted class vector was found in the returned object.")
  }
  as.character(predicted)
}

wsi_prediction_fit <- function(x_train, y_train, x_test, ncomp = 2L,
                               method = "simpls", scaling = "autoscaling") {
  if (!wsi_has_fastpls()) {
    wsi_abort(
      "Prediction requires the optional GitHub package `fastPLS`. Install it with `remotes::install_github(\"tkcaccia/fastPLS\")`.",
      class = "wsi_missing_dependency"
    )
  }
  ncomp <- as.integer(wsi_check_scalar_number(ncomp, "ncomp", allow_zero = FALSE))
  ncomp <- max(1L, min(ncomp, nrow(x_train) - 1L, ncol(x_train)))
  method <- match.arg(as.character(method %||% "simpls"), c("simpls", "plssvd", "opls", "kernelpls"))
  scaling <- match.arg(as.character(scaling %||% "autoscaling"), c("autoscaling", "centering", "none"))
  pls_fun <- get("pls", envir = asNamespace(wsi_fastpls_package()), inherits = FALSE)
  model <- pls_fun(
    Xtrain = x_train,
    Ytrain = factor(y_train),
    Xtest = x_test,
    ncomp = ncomp,
    method = method,
    classifier = "lda",
    backend = "cpu",
    scaling = scaling,
    return_variance = FALSE,
    fit = FALSE,
    proj = FALSE
  )
  wsi_prediction_extract_predicted(model, x_test)
}

wsi_prediction_run <- function(context, rois, source_id, train_ids, test_ids = character(),
                               ncomp = 2L, method = "simpls", scaling = "autoscaling",
                               max_features = 5000L) {
  if (!inherits(rois, "wsi_roi") || !nrow(rois)) {
    wsi_abort("Draw or import annotations before running PLS-LDA prediction.")
  }
  source_id <- as.character(source_id %||% "spatial:raw")
  points <- wsi_prediction_points(context, source_id)
  points <- points[is.finite(points$x) & is.finite(points$y), , drop = FALSE]
  if (!nrow(points)) {
    wsi_abort("No spatial spots or cells with finite coordinates are available for prediction.")
  }
  train <- wsi_prediction_assign_points(points, rois, train_ids, include_all = FALSE)
  train_rows <- which(!is.na(train$label) & nzchar(train$label))
  if (length(train_rows) < 3L) {
    wsi_abort("Training annotations contain fewer than three spots/cells.")
  }
  classes <- unique(train$label[train_rows])
  if (length(classes) < 2L) {
    wsi_abort("Training annotations must include at least two annotation classes.")
  }
  all_unlabelled <- !length(test_ids) ||
    any(as.character(test_ids) %in% c("__all_unlabelled__", "__all_non_training__"))
  if (isTRUE(all_unlabelled)) {
    test_rows <- setdiff(seq_len(nrow(points)), train_rows)
    test_roi_id <- rep(NA_character_, nrow(points))
    test_label <- rep(NA_character_, nrow(points))
  } else {
    test <- wsi_prediction_assign_points(points, rois, test_ids, include_all = FALSE)
    test_rows <- which(!is.na(test$roi_id) & nzchar(test$roi_id))
    test_roi_id <- test$roi_id
    test_label <- test$label
  }
  if (!length(test_rows)) {
    wsi_abort("The selected test annotations contain no spots/cells. Select test annotations or choose all non-training points.")
  }

  x <- wsi_prediction_feature_matrix(context, source_id, points$id)
  x <- wsi_prediction_feature_filter(x, max_features = max_features)
  complete <- stats::complete.cases(x)
  train_rows <- train_rows[complete[train_rows]]
  test_rows <- test_rows[complete[test_rows]]
  if (length(train_rows) < 3L || length(unique(train$label[train_rows])) < 2L) {
    wsi_abort("Not enough complete training rows remain after feature filtering.")
  }
  if (!length(test_rows)) {
    wsi_abort("No complete test rows remain after feature filtering.")
  }

  y_train <- train$label[train_rows]
  predicted <- wsi_prediction_fit(
    x_train = x[train_rows, , drop = FALSE],
    y_train = y_train,
    x_test = x[test_rows, , drop = FALSE],
    ncomp = ncomp,
    method = method,
    scaling = scaling
  )
  if (length(predicted) != length(test_rows)) {
    predicted <- rep(predicted, length.out = length(test_rows))
  }
  result <- data.frame(
    id = points$id[test_rows],
    label = points$label[test_rows],
    x = points$x[test_rows],
    y = points$y[test_rows],
    set = "test",
    train_annotation_id = NA_character_,
    test_annotation_id = test_roi_id[test_rows],
    observed = test_label[test_rows],
    predicted = predicted,
    feature_source = source_id,
    stringsAsFactors = FALSE
  )
  class(result) <- c("wsi_prediction_result", class(result))
  attr(result, "source_name") <- attr(x, "source_name") %||% source_id
  attr(result, "ncomp") <- as.integer(ncomp)
  attr(result, "method") <- method
  attr(result, "scaling") <- scaling
  attr(result, "feature_count") <- ncol(x)
  result
}

wsi_prediction_validate_payload <- function(payload) {
  if (!is.list(payload)) {
    wsi_abort("Prediction request must be a JSON object.")
  }
  allowed <- c(
    "feature_source", "train_annotations", "test_annotations", "rois",
    "ncomp", "method", "scaling", "max_features"
  )
  unknown <- setdiff(names(payload), allowed)
  if (length(unknown)) {
    wsi_abort(sprintf(
      "Prediction request contains unsupported field%s: %s.",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  payload
}

wsi_prediction_response <- function(context, state, payload) {
  payload <- wsi_prediction_validate_payload(payload)
  rois <- if (!is.null(payload$rois)) {
    wsi_rois_from_payload(payload$rois)
  } else {
    state$rois %||% wsi_empty_roi()
  }
  if (inherits(rois, "wsi_roi")) {
    state$rois <- rois
  }
  wsi_viewer_state_record_event(state, "prediction_started", list(
    feature_source = payload$feature_source %||% "spatial:raw"
  ))
  result <- wsi_prediction_run(
    context = context,
    rois = rois,
    source_id = payload$feature_source %||% "spatial:raw",
    train_ids = as.character(payload$train_annotations %||% character()),
    test_ids = as.character(payload$test_annotations %||% character()),
    ncomp = payload$ncomp %||% 2L,
    method = payload$method %||% "simpls",
    scaling = payload$scaling %||% "autoscaling",
    max_features = payload$max_features %||% 5000L
  )
  layer <- wsi_prediction_layer(result, radius = 8)
  state$prediction <- result
  state$layers <- wsi_viewer_set_layer(state$layers, layer)
  wsi_viewer_queue_command(state, "add_layer", list(layer = layer))
  detail <- list(
    count = nrow(result),
    classes = sort(unique(as.character(result$predicted))),
    feature_source = payload$feature_source %||% "spatial:raw",
    feature_count = attr(result, "feature_count") %||% NA_integer_,
    ncomp = as.integer(payload$ncomp %||% 2L)
  )
  wsi_viewer_state_record_event(state, "prediction_finished", detail)
  response <- wsi_viewer_state_response(state)
  response$prediction <- detail
  response
}
