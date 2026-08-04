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

wsi_has_refine_svm <- function() {
  requireNamespace("e1071", quietly = TRUE)
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
    predicted_pls_lda = character(),
    svm_refined = logical(),
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
        dimension_count <- suppressWarnings(as.integer(plot$dimension_count %||%
          length(plot$component_names %||% character()) %||% 2L))
        if (!is.finite(dimension_count) || dimension_count < 1L) {
          dimension_count <- 2L
        }
        sources[[length(sources) + 1L]] <- list(
          id = sprintf("spatial:reduction:%d", i - 1L),
          label = paste(source_name, wsi_reduction_label(reduction)),
          type = "reduction",
          reduction = reduction,
          dimension_count = dimension_count,
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
    fastpls_install = "remotes::install_github(\"tkcaccia/fastPLS\")",
    svm_refinement_installed = wsi_has_refine_svm(),
    svm_refinement_install = "install.packages(\"e1071\")"
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
  out <- data.frame(
    id = id,
    label = label,
    x = as.numeric(spots$x),
    y = as.numeric(spots$y),
    stringsAsFactors = FALSE
  )
  if ("feature_id" %in% names(spots)) {
    out$feature_id <- as.character(spots$feature_id)
  } else if ("barcode" %in% names(spots)) {
    out$feature_id <- as.character(spots$barcode)
  }
  if ("original_id" %in% names(spots)) {
    out$original_id <- as.character(spots$original_id)
  }
  for (column in c("project_image_index", "project_section_index")) {
    if (column %in% names(spots)) {
      out[[column]] <- suppressWarnings(as.integer(spots[[column]]))
    }
  }
  wsi_prediction_add_point_metadata(out, spots)
}

wsi_prediction_cell_points <- function(project) {
  cells <- project$cells %||% NULL
  if (!is.data.frame(cells) || !nrow(cells)) {
    return(data.frame(id = character(), label = character(), x = numeric(), y = numeric(), stringsAsFactors = FALSE))
  }
  id <- as.character(cells$id %||% cells$cell_id %||% cells$label %||% seq_len(nrow(cells)))
  label <- as.character(cells$label %||% cells$id %||% cells$cell_id %||% id)
  out <- data.frame(
    id = id,
    label = label,
    x = as.numeric(cells$x %||% cells$cellphenotyper_x),
    y = as.numeric(cells$y %||% cells$cellphenotyper_y),
    stringsAsFactors = FALSE
  )
  wsi_prediction_add_point_metadata(out, cells)
}

wsi_prediction_first_metadata_column <- function(data, candidates) {
  for (candidate in candidates) {
    if (candidate %in% names(data)) {
      value <- data[[candidate]]
      if (length(value) == nrow(data)) {
        return(as.character(value))
      }
    }
  }
  rep(NA_character_, nrow(data))
}

wsi_prediction_add_point_metadata <- function(points, source) {
  metadata_columns <- list(
    project_key = c("project_key", "wsi_project_key"),
    project_image = c("project_image", "image", "image_id", "sample_id", "sample", "slide_id", "tissue"),
    project_section = c("project_section", "section", "section_id", "scene", "sample_id", "sample"),
    image_id = c("image_id", "sample_id", "sample", "slide_id", "project_image"),
    section_id = c("section_id", "section", "scene", "project_section"),
    sample_id = c("sample_id", "sample", "slide_id")
  )
  for (name in names(metadata_columns)) {
    value <- wsi_prediction_first_metadata_column(source, metadata_columns[[name]])
    if (any(nzchar(value) & !is.na(value))) {
      points[[name]] <- value
    }
  }
  points
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

wsi_prediction_expression_id_candidates <- function(ids, points = NULL) {
  ids <- as.character(ids)
  candidates <- list(ids = ids)
  if (is.data.frame(points) && nrow(points) == length(ids)) {
    for (field in c("feature_id", "barcode", "spot_id", "cell_id", "label", "id")) {
      if (field %in% names(points)) {
        value <- as.character(points[[field]])
        if (length(value) == length(ids)) {
          candidates[[field]] <- value
        }
      }
    }
  }
  seen <- character()
  out <- list()
  for (name in names(candidates)) {
    value <- candidates[[name]]
    if (length(value) != length(ids) || all(is.na(value) | !nzchar(value))) {
      next
    }
    key <- paste(value, collapse = "\r")
    if (key %in% seen) {
      next
    }
    seen <- c(seen, key)
    out[[name]] <- value
  }
  out
}

wsi_prediction_margin_variance <- function(x, margin) {
  margin <- as.integer(margin)
  if (requireNamespace("Matrix", quietly = TRUE) && inherits(x, "Matrix")) {
    n <- if (identical(margin, 1L)) ncol(x) else nrow(x)
    if (!is.finite(n) || n < 2L) {
      return(rep(0, if (identical(margin, 1L)) nrow(x) else ncol(x)))
    }
    mean_fun <- if (identical(margin, 1L)) Matrix::rowMeans else Matrix::colMeans
    mu <- mean_fun(x)
    x2 <- x
    squared <- FALSE
    if (isS4(x2) && "x" %in% methods::slotNames(x2)) {
      methods::slot(x2, "x") <- methods::slot(x2, "x") ^ 2
      squared <- TRUE
    }
    if (!squared) {
      x2 <- x2 ^ 2
    }
    mu2 <- mean_fun(x2)
    return((mu2 - mu ^ 2) * n / (n - 1))
  }
  dense <- as.matrix(x)
  apply(dense, margin, stats::var, na.rm = TRUE)
}

wsi_prediction_top_feature_indices <- function(x, max_features, margin) {
  max_features <- suppressWarnings(as.integer(max_features %||% 0L))
  feature_count <- if (identical(as.integer(margin), 1L)) nrow(x) else ncol(x)
  if (!is.finite(max_features) || max_features <= 0L || feature_count <= max_features) {
    return(seq_len(feature_count))
  }
  variance <- tryCatch(
    wsi_prediction_margin_variance(x, margin = margin),
    error = function(e) rep(NA_real_, feature_count)
  )
  keep <- which(is.finite(variance) & variance > 0)
  if (!length(keep)) {
    return(seq_len(min(max_features, feature_count)))
  }
  ord <- keep[order(variance[keep], decreasing = TRUE)]
  ord[seq_len(min(max_features, length(ord)))]
}

wsi_prediction_matrix_for_spatial_raw <- function(linked, ids, points = NULL,
                                                  max_features = NULL) {
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
  id_candidates <- wsi_prediction_expression_id_candidates(ids, points = points)
  for (entry in matrices) {
    mat <- entry$matrix
    rn <- tryCatch(rownames(mat), error = function(e) NULL)
    cn <- tryCatch(colnames(mat), error = function(e) NULL)
    if (!length(rn) || !length(cn)) {
      next
    }
    for (candidate_name in names(id_candidates)) {
      sample_ids <- id_candidates[[candidate_name]]
      sample_idx <- match(sample_ids, cn)
      if (any(!is.na(sample_idx))) {
        keep <- !is.na(sample_idx)
        feature_idx <- wsi_prediction_top_feature_indices(
          mat[, sample_idx[keep], drop = FALSE],
          max_features = max_features,
          margin = 1L
        )
        out <- matrix(NA_real_, nrow = length(ids), ncol = length(feature_idx))
        rownames(out) <- ids
        colnames(out) <- wsi_seurat_feature_display_names(rn[feature_idx], entry$feature_aliases)
        sub <- as.matrix(mat[feature_idx, sample_idx[keep], drop = FALSE])
        storage.mode(sub) <- "double"
        out[keep, ] <- t(sub)
        attr(out, "source_name") <- entry$name %||% "expression"
        attr(out, "id_source") <- candidate_name
        return(out)
      }
      sample_idx <- match(sample_ids, rn)
      if (any(!is.na(sample_idx))) {
        keep <- !is.na(sample_idx)
        feature_idx <- wsi_prediction_top_feature_indices(
          mat[sample_idx[keep], , drop = FALSE],
          max_features = max_features,
          margin = 2L
        )
        out <- matrix(NA_real_, nrow = length(ids), ncol = length(feature_idx))
        rownames(out) <- ids
        colnames(out) <- make.unique(as.character(cn[feature_idx]))
        sub <- as.matrix(mat[sample_idx[keep], feature_idx, drop = FALSE])
        storage.mode(sub) <- "double"
        out[keep, ] <- sub
        attr(out, "source_name") <- entry$name %||% "expression"
        attr(out, "id_source") <- candidate_name
        return(out)
      }
    }
  }
  wsi_abort("Could not align expression matrix rows/columns to the viewer spot IDs.")
}

wsi_prediction_feature_match <- function(feature, available) {
  feature <- as.character(feature %||% "")
  available <- as.character(available %||% character())
  if (!nzchar(feature) || !length(available)) {
    return(NA_integer_)
  }
  hit <- match(feature, available)
  if (!is.na(hit)) {
    return(hit)
  }
  lower <- tolower(available)
  hit <- match(tolower(feature), lower)
  if (!is.na(hit)) {
    return(hit)
  }
  aliases <- unique(c(
    feature,
    gsub("_", "-", feature, fixed = TRUE),
    gsub("-", "_", feature, fixed = TRUE),
    gsub("\\.", "-", feature),
    gsub("\\.", "_", feature)
  ))
  aliases <- aliases[nzchar(aliases) & !is.na(aliases)]
  hit <- match(tolower(aliases), lower)
  hit <- hit[!is.na(hit)]
  if (length(hit)) {
    return(hit[[1L]])
  }
  NA_integer_
}

wsi_prediction_vector_for_spatial_raw <- function(linked, ids, feature, points = NULL) {
  source <- linked$expression_source %||% list()
  object <- source$object %||% NULL
  if (is.null(object)) {
    wsi_abort("Raw expression lookup needs the original live R object. Reopen the viewer from R with `live = TRUE`.")
  }
  matrices <- wsi_seurat_collect_expression_matrices(object)
  if (!length(matrices)) {
    wsi_abort("No expression matrix was found in the live spatial object.")
  }
  ids <- as.character(ids)
  id_candidates <- wsi_prediction_expression_id_candidates(ids, points = points)
  for (entry in matrices) {
    mat <- entry$matrix
    rn <- tryCatch(rownames(mat), error = function(e) NULL)
    cn <- tryCatch(colnames(mat), error = function(e) NULL)
    if (!length(rn) || !length(cn)) {
      next
    }
    feature_idx <- wsi_seurat_gene_match_with_alias(feature, rn, entry$feature_aliases)
    if (!is.na(feature_idx)) {
      for (candidate_name in names(id_candidates)) {
        sample_ids <- id_candidates[[candidate_name]]
        sample_idx <- match(sample_ids, cn)
        if (any(!is.na(sample_idx))) {
          out <- rep(NA_real_, length(ids))
          keep <- !is.na(sample_idx)
          values <- as.numeric(as.matrix(mat[feature_idx, sample_idx[keep], drop = FALSE]))
          out[keep] <- values
          attr(out, "feature") <- wsi_seurat_feature_display_names(rn[[feature_idx]], entry$feature_aliases)[[1L]]
          attr(out, "source_name") <- entry$name %||% "expression"
          attr(out, "id_source") <- candidate_name
          return(out)
        }
      }
    }
    feature_idx <- wsi_prediction_feature_match(feature, cn)
    if (!is.na(feature_idx)) {
      for (candidate_name in names(id_candidates)) {
        sample_ids <- id_candidates[[candidate_name]]
        sample_idx <- match(sample_ids, rn)
        if (any(!is.na(sample_idx))) {
          out <- rep(NA_real_, length(ids))
          keep <- !is.na(sample_idx)
          values <- as.numeric(as.matrix(mat[sample_idx[keep], feature_idx, drop = FALSE]))
          out[keep] <- values
          attr(out, "feature") <- as.character(cn[[feature_idx]])
          attr(out, "source_name") <- entry$name %||% "expression"
          attr(out, "id_source") <- candidate_name
          return(out)
        }
      }
    }
  }
  wsi_abort(sprintf("Feature `%s` was not found or could not be aligned to the viewer spot IDs.", feature))
}

wsi_prediction_vector_for_spatial_project <- function(linked, ids, feature,
                                                      points = NULL) {
  if (!is.data.frame(points) || !nrow(points)) {
    wsi_abort("Project-scoped feature lookup needs point metadata.")
  }
  sections <- linked$project_sections
  ids <- as.character(ids)
  out <- rep(NA_real_, length(ids))
  actual <- character()
  source_names <- character()
  for (section in sections) {
    section_key <- as.character(section$project_key %||% NA_character_)
    if (is.na(section_key) || !nzchar(section_key)) {
      next
    }
    rows <- which(as.character(points$project_key %||% "") == section_key)
    if (!length(rows)) {
      next
    }
    feature_ids <- if ("feature_id" %in% names(points)) {
      as.character(points$feature_id[rows])
    } else {
      ids[rows]
    }
    value <- tryCatch(
      wsi_prediction_vector_for_spatial_raw(
        section,
        ids = feature_ids,
        feature = feature,
        points = points[rows, , drop = FALSE]
      ),
      error = function(e) NULL
    )
    if (is.null(value)) {
      next
    }
    out[rows] <- as.numeric(value)
    actual <- c(actual, attr(value, "feature", exact = TRUE) %||% character())
    source_names <- c(source_names, attr(value, "source_name", exact = TRUE) %||% character())
  }
  if (!any(is.finite(out))) {
    wsi_abort(sprintf("Feature `%s` was not found in the project-scoped spatial expression data.", feature))
  }
  actual <- unique(actual[nzchar(actual) & !is.na(actual)])
  source_names <- unique(source_names[nzchar(source_names) & !is.na(source_names)])
  attr(out, "feature") <- if (length(actual)) actual[[1L]] else as.character(feature)
  attr(out, "source_name") <- if (length(source_names)) paste(source_names, collapse = " / ") else "project-scoped expression"
  out
}

wsi_prediction_vector_for_cells <- function(project, ids, feature) {
  cells <- project$cells %||% NULL
  if (!is.data.frame(cells) || !nrow(cells)) {
    wsi_abort("No CellPhenotyper cell table is attached to this live viewer session.")
  }
  cell_ids <- as.character(cells$id %||% cells$cell_id %||% cells$label %||% seq_len(nrow(cells)))
  idx <- match(as.character(ids), cell_ids)
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
  feature_idx <- wsi_prediction_feature_match(feature, numeric_cols)
  if (is.na(feature_idx)) {
    wsi_abort(sprintf("Feature `%s` was not found in the CellPhenotyper numeric cell table.", feature))
  }
  out <- rep(NA_real_, length(ids))
  out[!is.na(idx)] <- as.numeric(cells[[numeric_cols[[feature_idx]]]][idx[!is.na(idx)]])
  attr(out, "feature") <- numeric_cols[[feature_idx]]
  attr(out, "source_name") <- "CellPhenotyper numeric cell table"
  out
}

wsi_prediction_feature_vector <- function(context, source_id, ids, feature,
                                          points = NULL,
                                          reduction_dims = NULL) {
  context <- context %||% list()
  source_id <- as.character(source_id %||% "spatial:raw")
  if (identical(source_id, "cellphenotyper:numeric")) {
    return(wsi_prediction_vector_for_cells(
      context$cellphenotyper_project %||% context$cellphenotyper,
      ids,
      feature
    ))
  }
  linked <- context$spatial %||% context$seurat %||% NULL
  if (!inherits(linked, "wsi_seurat_spatial") && !inherits(linked, "wsi_spatial_object")) {
    wsi_abort("No live spatial object is attached to this viewer session.")
  }
  if (wsi_prediction_is_spatial_project(linked) && identical(source_id, "spatial:raw")) {
    return(wsi_prediction_vector_for_spatial_project(linked, ids, feature, points = points))
  }
  if (identical(source_id, "spatial:raw")) {
    return(wsi_prediction_vector_for_spatial_raw(linked, ids, feature, points = points))
  }
  x <- wsi_prediction_feature_matrix(
    context,
    source_id,
    ids,
    reduction_dims = reduction_dims,
    points = points,
    max_features = NULL
  )
  idx <- wsi_prediction_feature_match(feature, colnames(x))
  if (is.na(idx)) {
    wsi_abort(sprintf("Feature `%s` was not found in `%s`.", feature, source_id))
  }
  out <- as.numeric(x[, idx])
  attr(out, "feature") <- colnames(x)[[idx]]
  attr(out, "source_name") <- attr(x, "source_name", exact = TRUE) %||% source_id
  out
}

wsi_prediction_feature_payload <- function(context, feature,
                                           source_id = "spatial:raw",
                                           point_source = NULL,
                                           reduction_dims = NULL) {
  feature <- as.character(feature %||% "")
  if (!nzchar(feature)) {
    wsi_abort("Provide a single non-empty feature or gene name.")
  }
  source_id <- as.character(source_id %||% "spatial:raw")
  point_source <- as.character(point_source %||% if (startsWith(source_id, "cellphenotyper:")) "cellphenotyper:cells" else "spatial:points")
  points <- wsi_prediction_points(context, point_source)
  ids <- as.character(points$id %||% character())
  if (!length(ids)) {
    wsi_abort("No point identifiers are available for live feature lookup.")
  }
  values <- wsi_prediction_feature_vector(
    context,
    source_id = source_id,
    ids = ids,
    feature = feature,
    points = points,
    reduction_dims = reduction_dims
  )
  actual <- attr(values, "feature", exact = TRUE) %||% feature
  feature_type <- if (startsWith(point_source, "cellphenotyper:") ||
    any(as.character(points$unit %||% "") == "cell")) "cell" else "spot"
  feature_label <- if (identical(feature_type, "cell")) "cell" else "spot"
  gene_expression <- list(
    enabled = TRUE,
    genes = as.character(actual),
    default_gene = as.character(actual),
    values = matrix(as.numeric(values), ncol = 1L, dimnames = list(NULL, as.character(actual))),
    ranges = wsi_seurat_gene_ranges(matrix(as.numeric(values), ncol = 1L, dimnames = list(NULL, as.character(actual))))
  )
  colours <- wsi_seurat_gene_colours(gene_expression, actual)
  point_value <- function(name, i) {
    if (name %in% names(points)) points[[name]][[i]] else NULL
  }
  point_rows <- lapply(seq_len(nrow(points)), function(i) {
    x <- suppressWarnings(as.numeric(point_value("x", i) %||% point_value("slide_x", i) %||% NA_real_))
    y <- suppressWarnings(as.numeric(point_value("y", i) %||% point_value("slide_y", i) %||% NA_real_))
    radius <- suppressWarnings(as.numeric(point_value("radius", i) %||% point_value("spot_radius", i) %||% NA_real_))
    list(
      id = as.character(point_value("id", i) %||% ""),
      label = as.character(point_value("label", i) %||% point_value("id", i) %||% ""),
      barcode = as.character(point_value("feature_id", i) %||% point_value("barcode", i) %||% point_value("id", i) %||% ""),
      feature_type = feature_type,
      x = if (is.finite(x)) x else NA_real_,
      y = if (is.finite(y)) y else NA_real_,
      slide_x = if (is.finite(x)) x else NA_real_,
      slide_y = if (is.finite(y)) y else NA_real_,
      radius = if (is.finite(radius)) radius else NA_real_,
      value = if (is.finite(values[[i]])) values[[i]] else NA_real_,
      colour = as.character(colours[[i]] %||% "#d1d5db")
    )
  })
  list(
    ok = TRUE,
    gene = as.character(actual),
    requested_gene = as.character(feature),
    feature_type = feature_type,
    feature_label = feature_label,
    feature_plural = paste0(feature_label, "s"),
    feature_source = source_id,
    point_source = point_source,
    source_name = attr(values, "source_name", exact = TRUE) %||% source_id,
    range = gene_expression$ranges[[as.character(actual)]] %||% list(min = NA_real_, max = NA_real_),
    count = length(point_rows),
    points = point_rows
  )
}

wsi_prediction_reduction_dim_count <- function(dimensions = NULL, max_dim = 2L, default = 2L) {
  max_dim <- suppressWarnings(as.integer(max_dim %||% default))
  if (!is.finite(max_dim) || max_dim < 1L) {
    max_dim <- 1L
  }
  if (is.null(dimensions) || !length(dimensions)) {
    dimensions <- default
  }
  n <- suppressWarnings(as.integer(dimensions[[1L]]))
  if (!is.finite(n) || n < 1L) {
    n <- min(default, max_dim)
  }
  min(n, max_dim)
}

wsi_prediction_stored_reduction_embeddings <- function(linked, reduction) {
  reduction <- as.character(reduction %||% linked$reduction %||% "")
  emb <- linked$reduction_embeddings %||% linked$reduction_matrix %||% linked$embeddings %||% NULL
  if (is.matrix(emb) || is.data.frame(emb)) {
    emb_name <- as.character(linked$reduction_embedding_name %||% linked$reduction %||% reduction)
    if (!nzchar(reduction) || !nzchar(emb_name) || identical(tolower(emb_name), tolower(reduction))) {
      return(emb)
    }
  }
  if (is.list(emb) && length(emb)) {
    nms <- names(emb) %||% character()
    hit <- nms[match(tolower(reduction), tolower(nms), nomatch = 0L)]
    if (length(hit) && nzchar(hit[[1L]])) {
      return(emb[[hit[[1L]]]])
    }
  }
  NULL
}

wsi_prediction_object_reduction_embeddings <- function(linked, reduction) {
  object <- linked$expression_source$object %||% linked$object %||% NULL
  if (is.null(object)) {
    return(NULL)
  }
  type <- tryCatch(wsi_spatial_object_type(object), error = function(e) "")
  tryCatch(
    switch(
      type,
      seurat = wsi_seurat_embeddings(object, reduction = reduction),
      giotto = wsi_giotto_embeddings(object, reduction = reduction),
      spatialexperiment = wsi_spatialexperiment_embeddings(object, reduction = reduction),
      NULL
    ),
    error = function(e) NULL
  )
}

wsi_prediction_reduction_embeddings <- function(linked, reduction) {
  wsi_prediction_stored_reduction_embeddings(linked, reduction) %||%
    wsi_prediction_object_reduction_embeddings(linked, reduction)
}

wsi_prediction_matrix_from_embeddings <- function(embeddings, ids, dimensions = NULL,
                                                  source_name = "reduction") {
  emb <- tryCatch(as.matrix(embeddings), error = function(e) NULL)
  if (is.null(emb) || length(dim(emb)) != 2L || !nrow(emb) || !ncol(emb)) {
    return(NULL)
  }
  storage.mode(emb) <- "double"
  dim_count <- wsi_prediction_reduction_dim_count(dimensions, ncol(emb), default = 2L)
  emb <- emb[, seq_len(dim_count), drop = FALSE]
  emb_ids <- rownames(emb) %||% character()
  idx <- if (length(emb_ids)) match(ids, emb_ids) else rep(NA_integer_, length(ids))
  if (!any(!is.na(idx)) && nrow(emb) == length(ids)) {
    idx <- seq_along(ids)
  }
  out <- matrix(NA_real_, nrow = length(ids), ncol = ncol(emb))
  rownames(out) <- ids
  colnames(out) <- colnames(emb) %||% paste0("dim", seq_len(ncol(emb)))
  keep <- !is.na(idx)
  if (any(keep)) {
    out[keep, ] <- emb[idx[keep], , drop = FALSE]
  }
  attr(out, "source_name") <- source_name
  attr(out, "reduction_dimensions") <- ncol(out)
  attr(out, "available_reduction_dimensions") <- ncol(as.matrix(embeddings))
  out
}

wsi_prediction_matrix_for_spatial_reduction <- function(linked, ids, source_id,
                                                        dimensions = NULL) {
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
  reduction <- as.character(plot$reduction %||% linked$reduction %||% "reduction")
  embeddings <- wsi_prediction_reduction_embeddings(linked, reduction)
  full <- wsi_prediction_matrix_from_embeddings(
    embeddings,
    ids = ids,
    dimensions = dimensions,
    source_name = plot$label %||% reduction
  )
  if (!is.null(full)) {
    return(full)
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
  attr(out, "reduction_dimensions") <- 2L
  attr(out, "available_reduction_dimensions") <- 2L
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

wsi_prediction_is_spatial_project <- function(linked) {
  inherits(linked, "wsi_spatial_project") &&
    is.list(linked$project_sections) &&
    length(linked$project_sections) > 0L
}

wsi_prediction_align_feature_matrices <- function(parts, ids) {
  parts <- parts[vapply(parts, function(part) is.matrix(part$x) && nrow(part$x) > 0L, logical(1))]
  if (!length(parts)) {
    wsi_abort("No project-scoped feature rows were found for the selected spots/cells.")
  }
  column_sets <- lapply(parts, function(part) {
    columns <- colnames(part$x)
    unique(columns[nzchar(columns) & !is.na(columns)])
  })
  columns <- Reduce(intersect, column_sets)
  if (!length(columns)) {
    wsi_abort("Project-scoped feature matrices do not share usable feature names across sections.")
  }
  out <- matrix(NA_real_, nrow = length(ids), ncol = length(columns))
  rownames(out) <- as.character(ids)
  colnames(out) <- make.unique(columns)
  source_names <- character()
  for (part in parts) {
    idx <- part$rows
    cols <- match(columns, colnames(part$x))
    out[idx, ] <- part$x[, cols, drop = FALSE]
    source_names <- c(source_names, attr(part$x, "source_name", exact = TRUE) %||% character())
  }
  source_names <- unique(source_names[nzchar(source_names) & !is.na(source_names)])
  attr(out, "source_name") <- if (length(source_names)) {
    paste(source_names, collapse = " / ")
  } else {
    "project-scoped features"
  }
  out
}

wsi_prediction_matrix_for_spatial_project <- function(linked, source_id, ids,
                                                      reduction_dims = NULL,
                                                      points = NULL,
                                                      max_features = NULL) {
  if (!is.data.frame(points) || !nrow(points)) {
    wsi_abort("Project-scoped prediction needs point metadata.")
  }
  sections <- linked$project_sections
  ids <- as.character(ids)
  parts <- list()
  for (section in sections) {
    section_key <- as.character(section$project_key %||% NA_character_)
    if (is.na(section_key) || !nzchar(section_key)) {
      next
    }
    rows <- which(as.character(points$project_key %||% "") == section_key)
    if (!length(rows)) {
      next
    }
    feature_ids <- if ("feature_id" %in% names(points)) {
      as.character(points$feature_id[rows])
    } else {
      ids[rows]
    }
    x <- if (identical(source_id, "spatial:raw")) {
      wsi_prediction_matrix_for_spatial_raw(
        section,
        feature_ids,
        max_features = NULL
      )
    } else if (startsWith(source_id, "spatial:reduction:")) {
      wsi_prediction_matrix_for_spatial_reduction(
        section, feature_ids, source_id,
        dimensions = reduction_dims
      )
    } else {
      wsi_abort(sprintf("Unsupported prediction feature source `%s`.", source_id))
    }
    rownames(x) <- ids[rows]
    parts[[length(parts) + 1L]] <- list(rows = rows, x = x)
  }
  wsi_prediction_align_feature_matrices(parts, ids)
}

wsi_prediction_feature_matrix <- function(context, source_id, ids,
                                          reduction_dims = NULL,
                                          points = NULL,
                                          max_features = NULL) {
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
  if (wsi_prediction_is_spatial_project(linked)) {
    return(wsi_prediction_matrix_for_spatial_project(
      linked, source_id, ids,
      reduction_dims = reduction_dims,
      points = points,
      max_features = max_features
    ))
  }
  if (identical(source_id, "spatial:raw")) {
    return(wsi_prediction_matrix_for_spatial_raw(
      linked,
      ids,
      points = points,
      max_features = max_features
    ))
  }
  if (startsWith(source_id, "spatial:reduction:")) {
    return(wsi_prediction_matrix_for_spatial_reduction(
      linked, ids, source_id,
      dimensions = reduction_dims
    ))
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
  prefixed_roi <- sub("^roi:", "", ids[startsWith(ids, "roi:")])
  prefixed_roi_index <- sub("^roi_index:", "", ids[startsWith(ids, "roi_index:")])
  prefixed_class <- sub("^class:", "", ids[startsWith(ids, "class:")])
  indices <- integer()
  if (length(prefixed_roi_index)) {
    roi_index <- suppressWarnings(as.integer(prefixed_roi_index))
    roi_index <- roi_index[is.finite(roi_index) & roi_index >= 0L & roi_index < nrow(rois)]
    indices <- c(indices, roi_index + 1L)
  }
  if (length(prefixed_roi)) {
    roi_keys <- cbind(
      as.character(rois$roi_id %||% ""),
      as.character(rois$name %||% "")
    )
    indices <- c(indices, which(apply(roi_keys, 1L, function(row) any(row %in% prefixed_roi))))
  }
  if (length(prefixed_class)) {
    class_keys <- wsi_roi_class(rois$class %||% rep(NA_character_, nrow(rois)))
    wanted <- wsi_roi_class(prefixed_class)
    indices <- c(indices, which(tolower(class_keys) %in% tolower(wanted)))
  }
  ids <- ids[!startsWith(ids, "roi:") & !startsWith(ids, "roi_index:") & !startsWith(ids, "class:")]
  keys <- cbind(
    as.character(rois$roi_id %||% ""),
    as.character(rois$name %||% ""),
    as.character(rois$class %||% "")
  )
  if (length(ids)) {
    indices <- c(indices, which(apply(keys, 1L, function(row) any(row %in% ids))))
  }
  sort(unique(indices))
}

wsi_prediction_assign_points <- function(points, rois, ids, include_all = FALSE) {
  label <- rep(NA_character_, nrow(points))
  roi_id <- rep(NA_character_, nrow(points))
  roi_index <- rep(NA_integer_, nrow(points))
  indices <- wsi_prediction_selected_roi_indices(rois, ids, include_all = include_all)
  if (!length(indices)) {
    return(list(label = label, roi_id = roi_id, roi_index = roi_index, indices = indices))
  }
  for (i in indices) {
    scope_keep <- wsi_prediction_scope_keep(points, wsi_prediction_roi_scope(rois, i))
    inside <- rep(FALSE, nrow(points))
    if (any(scope_keep, na.rm = TRUE)) {
      inside[scope_keep] <- wsi_points_in_roi(rois, i, points$x[scope_keep], points$y[scope_keep])
    }
    inside <- inside & is.na(label)
    if (!any(inside, na.rm = TRUE)) {
      next
    }
    label[inside] <- wsi_prediction_roi_label(rois, i)
    roi_id[inside] <- as.character(rois$roi_id[[i]] %||% i)
    roi_index[inside] <- i
  }
  list(label = label, roi_id = roi_id, roi_index = roi_index, indices = indices)
}

wsi_prediction_scope_scalar <- function(...) {
  values <- list(...)
  for (value in values) {
    value <- wsi_geojson_scalar(value, default = NA_character_)
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NA_character_
}

wsi_prediction_roi_scope <- function(rois, index) {
  properties <- if ("properties" %in% names(rois)) {
    rois$properties[[index]]
  } else {
    list()
  }
  properties <- wsi_geojson_list(properties)
  project <- wsi_geojson_list(properties$wsiToolsProject %||% properties$wsitools_project)
  list(
    project_key = wsi_prediction_scope_scalar(
      properties$project_key, properties$projectKey, project$key
    ),
    project_image = wsi_prediction_scope_scalar(
      properties$project_image, properties$projectImage,
      properties$image, properties$image_id, properties$sample_id, project$image
    ),
    project_section = wsi_prediction_scope_scalar(
      properties$project_section, properties$projectSection,
      properties$section, properties$section_id, project$section
    ),
    image_id = wsi_prediction_scope_scalar(
      properties$image_id, properties$sample_id, project$image_id
    ),
    section_id = wsi_prediction_scope_scalar(
      properties$section_id, properties$scene, project$section_id
    ),
    sample_id = wsi_prediction_scope_scalar(
      properties$sample_id, properties$sample, project$sample_id
    )
  )
}

wsi_prediction_scope_keep <- function(points, scope) {
  if (!is.data.frame(points) || !nrow(points)) {
    return(logical())
  }
  scope <- scope %||% list()
  candidates <- list(
    project_key = c("project_key", "wsi_project_key"),
    project_image = c("project_image", "image", "image_id", "sample_id", "sample", "slide_id", "tissue"),
    project_section = c("project_section", "section", "section_id", "scene", "sample_id", "sample"),
    image_id = c("image_id", "sample_id", "sample", "slide_id", "project_image"),
    section_id = c("section_id", "section", "scene", "project_section"),
    sample_id = c("sample_id", "sample", "slide_id")
  )
  keep <- rep(TRUE, nrow(points))
  matched_scope <- FALSE
  for (name in names(candidates)) {
    value <- as.character(scope[[name]] %||% NA_character_)
    if (length(value) != 1L || is.na(value) || !nzchar(value)) {
      next
    }
    cols <- intersect(candidates[[name]], names(points))
    if (!length(cols)) {
      next
    }
    field_keep <- rep(FALSE, nrow(points))
    value <- tolower(value)
    for (col in cols) {
      field_keep <- field_keep | tolower(as.character(points[[col]])) == value
    }
    keep <- keep & field_keep
    matched_scope <- TRUE
  }
  if (!matched_scope) {
    rep(TRUE, nrow(points))
  } else {
    keep
  }
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
  class_counts <- table(factor(as.character(result$predicted), levels = names(palette)))
  legend_entries <- lapply(seq_along(palette), function(i) {
    list(
      name = unname(names(palette)[[i]]),
      colour = unname(palette[[i]]),
      count = unname(as.integer(class_counts[[i]]))
    )
  })
  items <- lapply(seq_len(nrow(result)), function(i) {
    pred <- as.character(result$predicted[[i]] %||% "")
    colour <- palette[[pred]] %||% "#38BDF8"
    item <- list(
      id = paste0("prediction_", result$id[[i]]),
      name = as.character(result$label[[i]] %||% result$id[[i]]),
      label = as.character(result$label[[i]] %||% result$id[[i]]),
      type = "point",
      x = unname(as.numeric(result$x[[i]])),
      y = unname(as.numeric(result$y[[i]])),
      radius = radius,
      colour = colour,
      fill = wsi_hex_to_rgba(colour, 0.72),
      predicted = pred,
      predicted_pls_lda = as.character(result$predicted_pls_lda[[i]] %||% pred),
      svm_refined = isTRUE(result$svm_refined[[i]]),
      observed = as.character(result$observed[[i]] %||% NA_character_),
      feature_source = as.character(result$feature_source[[i]] %||% "")
    )
    scope_columns <- c(
      "project_key", "wsi_project_key", "project_image", "project_section",
      "image_id", "section_id", "sample_id", "project_image_index",
      "project_section_index", "feature_id", "original_id"
    )
    for (column in intersect(scope_columns, names(result))) {
      value <- result[[column]][[i]]
      item[[column]] <- if (is.numeric(value) || is.integer(value)) {
        unname(value)
      } else {
        as.character(value %||% "")
      }
    }
    item$project_scoped <- any(c(
      "project_key", "wsi_project_key", "project_image", "project_section",
      "image_id", "section_id", "sample_id", "project_image_index"
    ) %in% names(item))
    item
  })
  list(
    id = "wsi_prediction_pls_lda",
    name = "PLS-LDA prediction",
    type = "vector",
    source_type = "prediction",
    visible = TRUE,
    opacity = 0.96,
    colour = "#FACC15",
    replace = TRUE,
    count = length(items),
    items = items,
    legend = list(
      type = "categorical",
      title = "Predicted class",
      entries = legend_entries
    ),
    metadata = list(
      classes = unname(names(palette)),
      colours = unname(palette),
      vector_rendering = TRUE,
      coordinate_overlay = TRUE,
      lod = list(enabled = FALSE, full_coordinates = TRUE),
      svm_refined = any(as.logical(result$svm_refined %||% FALSE), na.rm = TRUE)
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

wsi_refine_SVM <- function(xy, labels, samples = NULL, newdata = NULL,
                           newsamples = NULL, tiles = NULL, newtiles = NULL, ...) {
  if (!wsi_has_refine_svm()) {
    wsi_abort(
      "SVM prediction refinement requires the optional package `e1071`. Install it with `install.packages(\"e1071\")`.",
      class = "wsi_missing_dependency"
    )
  }
  xy <- tryCatch(as.matrix(xy), error = function(e) NULL)
  if (is.null(xy) || length(dim(xy)) != 2L || !nrow(xy) || !ncol(xy)) {
    wsi_abort("SVM refinement needs a non-empty numeric matrix.")
  }
  storage.mode(xy) <- "double"
  labels <- factor(labels)
  if (length(labels) != nrow(xy)) {
    wsi_abort("SVM refinement labels must have one value per training row.")
  }
  if (is.null(rownames(xy))) {
    rownames(xy) <- paste0("row_", seq_len(nrow(xy)))
  }
  if (is.null(names(labels))) {
    names(labels) <- rownames(xy)
  }
  has_newdata <- !is.null(newdata)
  if (has_newdata) {
    newdata <- tryCatch(as.matrix(newdata), error = function(e) NULL)
    if (is.null(newdata) || length(dim(newdata)) != 2L || !nrow(newdata) || ncol(newdata) != ncol(xy)) {
      wsi_abort("SVM refinement `newdata` must be a non-empty matrix with the same columns as `xy`.")
    }
    storage.mode(newdata) <- "double"
    if (is.null(rownames(newdata))) {
      rownames(newdata) <- paste0("new_", seq_len(nrow(newdata)))
    }
  } else {
    newdata <- xy
  }

  samples <- if (is.null(samples)) rep("sample_1", nrow(xy)) else as.character(samples)
  if (length(samples) != nrow(xy)) {
    wsi_abort("SVM refinement `samples` must have one value per training row.")
  }
  newsamples <- if (is.null(newsamples)) {
    if (has_newdata) rep(samples[[1L]], nrow(newdata)) else samples
  } else {
    as.character(newsamples)
  }
  if (length(newsamples) != nrow(newdata)) {
    wsi_abort("SVM refinement `newsamples` must have one value per prediction row.")
  }
  use_tiles <- !is.null(tiles) && (!has_newdata || !is.null(newtiles))
  if (!is.null(newtiles) && is.null(tiles)) {
    wsi_abort("SVM refinement `newtiles` requires `tiles`.")
  }
  if (use_tiles) {
    tiles <- as.character(tiles)
    if (length(tiles) != nrow(xy)) {
      wsi_abort("SVM refinement `tiles` must have one value per training row.")
    }
    samples <- paste(samples, tiles, sep = "::tile::")
  }
  if (use_tiles && !is.null(newtiles)) {
    newtiles <- as.character(newtiles)
    if (length(newtiles) != nrow(newdata)) {
      wsi_abort("SVM refinement `newtiles` must have one value per prediction row.")
    }
    newsamples <- paste(newsamples, newtiles, sep = "::tile::")
  } else if (use_tiles && !has_newdata) {
    newsamples <- samples
  }

  out <- rep(NA_character_, nrow(newdata))
  names(out) <- rownames(newdata)
  svm_fun <- get("svm", envir = asNamespace("e1071"), inherits = FALSE)
  sample_levels <- unique(c(samples, newsamples))
  sample_levels <- sample_levels[nzchar(sample_levels) & !is.na(sample_levels)]
  for (sample_level in sample_levels) {
    train_idx <- which(samples == sample_level)
    pred_idx <- which(newsamples == sample_level)
    if (!length(train_idx) || !length(pred_idx)) {
      next
    }
    y <- droplevels(labels[train_idx])
    present <- !is.na(y)
    train_idx <- train_idx[present]
    y <- droplevels(y[present])
    if (!length(y)) {
      next
    }
    if (length(levels(y)) < 2L) {
      out[pred_idx] <- as.character(y)[[1L]]
      next
    }
    model <- svm_fun(x = xy[train_idx, , drop = FALSE], y = y, ...)
    out[pred_idx] <- as.character(stats::predict(model, newdata = newdata[pred_idx, , drop = FALSE]))
  }
  factor(out, levels = levels(labels))
}

wsi_prediction_spatial_refinement_matrix <- function(points, rows) {
  rows <- as.integer(rows)
  xy <- cbind(
    x = as.numeric(points$x[rows]),
    y = as.numeric(points$y[rows])
  )
  rownames(xy) <- as.character(points$id[rows] %||% rows)
  complete <- stats::complete.cases(xy)
  if (!all(complete)) {
    wsi_abort("SVM refinement needs finite spatial coordinates for all training and test points.")
  }
  storage.mode(xy) <- "double"
  xy
}

wsi_prediction_refinement_samples <- function(points, rows) {
  rows <- as.integer(rows)
  n <- length(rows)
  if (!n) {
    return(character())
  }
  text_value <- function(column) {
    if (!column %in% names(points)) {
      return(rep(NA_character_, n))
    }
    value <- as.character(points[[column]][rows])
    value[!nzchar(value) | is.na(value)] <- NA_character_
    value
  }
  for (column in c("project_key", "wsi_project_key")) {
    value <- text_value(column)
    if (any(!is.na(value))) {
      value[is.na(value)] <- "sample_1"
      return(value)
    }
  }
  if (all(c("project_image_index", "project_section_index") %in% names(points))) {
    image <- suppressWarnings(as.integer(points$project_image_index[rows]))
    section <- suppressWarnings(as.integer(points$project_section_index[rows]))
    if (any(is.finite(image) | is.finite(section))) {
      image[!is.finite(image)] <- -1L
      section[!is.finite(section)] <- -1L
      return(paste0("image_", image, "::section_", section))
    }
  }
  image <- text_value("project_image")
  section <- text_value("project_section")
  if (any(!is.na(image) | !is.na(section))) {
    image[is.na(image)] <- "image"
    section[is.na(section)] <- "section"
    return(paste0(image, "::", section))
  }
  for (column in c("image_id", "sample_id", "section_id")) {
    value <- text_value(column)
    if (any(!is.na(value))) {
      value[is.na(value)] <- "sample_1"
      return(value)
    }
  }
  rep("sample_1", n)
}

wsi_prediction_apply_svm_refinement <- function(points, train_rows, test_rows,
                                                y_train, predicted) {
  if (!length(test_rows)) {
    return(as.character(predicted))
  }
  refine_rows <- c(train_rows, test_rows)
  labels <- c(as.character(y_train), as.character(predicted))
  refine_xy <- wsi_prediction_spatial_refinement_matrix(points, refine_rows)
  samples <- wsi_prediction_refinement_samples(points, refine_rows)
  refined <- wsi_refine_SVM(refine_xy, labels, samples = samples, kernel = "radial")
  refined_ids <- rownames(refine_xy)[seq_along(refined)]
  wanted_ids <- as.character(points$id[test_rows] %||% test_rows)
  out <- as.character(refined[match(wanted_ids, refined_ids)])
  missing <- is.na(out) | !nzchar(out)
  if (any(missing)) {
    out[missing] <- as.character(predicted)[missing]
  }
  out
}

wsi_prediction_run <- function(context, rois, source_id, train_ids, test_ids = character(),
                               ncomp = 2L, method = "simpls", scaling = "autoscaling",
                               max_features = 5000L, reduction_dims = NULL,
                               refine_svm = FALSE) {
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

  x <- wsi_prediction_feature_matrix(
    context, source_id, points$id,
    reduction_dims = reduction_dims,
    points = points,
    max_features = max_features
  )
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
  predicted_pls_lda <- as.character(predicted)
  svm_refined <- isTRUE(refine_svm)
  if (svm_refined) {
    predicted <- wsi_prediction_apply_svm_refinement(
      points = points,
      train_rows = train_rows,
      test_rows = test_rows,
      y_train = y_train,
      predicted = predicted_pls_lda
    )
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
    predicted_pls_lda = predicted_pls_lda,
    svm_refined = svm_refined,
    feature_source = source_id,
    stringsAsFactors = FALSE
  )
  metadata_cols <- intersect(
    c("project_key", "project_image", "project_section", "image_id", "section_id", "sample_id"),
    names(points)
  )
  for (col in metadata_cols) {
    result[[col]] <- points[[col]][test_rows]
  }
  for (col in intersect(c("project_image_index", "project_section_index", "feature_id", "original_id"), names(points))) {
    result[[col]] <- points[[col]][test_rows]
  }
  class(result) <- c("wsi_prediction_result", class(result))
  attr(result, "source_name") <- attr(x, "source_name") %||% source_id
  attr(result, "ncomp") <- as.integer(ncomp)
  attr(result, "method") <- method
  attr(result, "scaling") <- scaling
  attr(result, "feature_count") <- ncol(x)
  attr(result, "point_unit") <- unique(as.character(points$unit %||% "point"))[[1L]] %||% "point"
  attr(result, "reduction_dimensions") <- attr(x, "reduction_dimensions", exact = TRUE) %||% NA_integer_
  attr(result, "svm_refined") <- svm_refined
  result
}

wsi_prediction_validate_payload <- function(payload) {
  if (!is.list(payload)) {
    wsi_abort("Prediction request must be a JSON object.")
  }
  allowed <- c(
    "feature_source", "train_annotations", "test_annotations", "rois",
    "ncomp", "method", "scaling", "max_features", "reduction_dims",
    "refine_svm"
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
    max_features = payload$max_features %||% 5000L,
    reduction_dims = payload$reduction_dims %||% NULL,
    refine_svm = isTRUE(payload$refine_svm)
  )
  layer <- wsi_prediction_layer(result, radius = 8)
  state$prediction <- result
  state$layers <- wsi_viewer_set_layer(state$layers, layer)
  wsi_viewer_queue_command(
    state,
    "add_layer",
    list(layer = layer),
    send_ws = FALSE
  )
  detail <- list(
    count = nrow(result),
    classes = sort(unique(as.character(result$predicted))),
    feature_source = payload$feature_source %||% "spatial:raw",
    feature_count = attr(result, "feature_count") %||% NA_integer_,
    ncomp = as.integer(payload$ncomp %||% 2L),
    reduction_dims = attr(result, "reduction_dimensions") %||% NA_integer_,
    svm_refined = isTRUE(attr(result, "svm_refined") %||% FALSE),
    point_unit = attr(result, "point_unit") %||% "point"
  )
  wsi_viewer_state_record_event(state, "prediction_finished", detail)
  response <- wsi_viewer_state_response(state)
  response$prediction <- detail
  response
}
