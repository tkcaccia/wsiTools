#' Associate spatial spots or cells with annotation polygons
#'
#' Assign each spot or cell coordinate to the first annotation polygon that
#' contains it. The default engine uses the package's native C++ point-in-polygon
#' routine when available and falls back to the R implementation otherwise.
#'
#' @param points A data.frame with point coordinates. Coordinate columns may be
#'   named `x`/`y`, `spot_x`/`spot_y`, `cell_x`/`cell_y`, or
#'   `centroid_x`/`centroid_y`.
#' @param rois A `wsi_roi` object, usually returned by [read_geojson()] or
#'   [wsi_read_geojson()].
#' @param ids Optional annotation selectors. Values may be annotation ids,
#'   names, classes, `roi:<id>`, `roi_index:<zero_based_index>`, or
#'   `class:<class>`.
#' @param include_all If `TRUE`, use all annotations when `ids` is empty.
#' @param output Return a long `data.frame` or a point-by-annotation `matrix`.
#' @param file Optional CSV path. When supplied, the long association table is
#'   written to this path.
#' @param engine Use the native C++ assignment engine, the R fallback, or choose
#'   automatically.
#'
#' @return A `wsi_annotation_association` data.frame, or an integer matrix when
#'   `output = "matrix"`.
#' @export
wsi_associate_annotations <- function(points, rois, ids = NULL, include_all = TRUE,
                                      output = c("data.frame", "matrix"),
                                      file = NULL,
                                      engine = c("auto", "native", "r")) {
  output <- match.arg(output)
  engine <- match.arg(engine)
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  points <- wsi_annotation_points(points)
  indices <- wsi_prediction_selected_roi_indices(rois, ids, include_all = include_all)
  assigned <- rep(NA_integer_, nrow(points))
  if (length(indices) && nrow(points)) {
    assigned <- wsi_annotation_assign_indices(points, rois, indices, engine = engine)
  }
  table <- wsi_annotation_association_table(points, rois, assigned)
  if (!is.null(file)) {
    wsi_write_annotation_associations(table, file)
  }
  if (identical(output, "matrix")) {
    return(wsi_annotation_association_matrix(table, rois = rois))
  }
  table
}

#' Convert an annotation association table to a matrix
#'
#' @param x A table returned by [wsi_associate_annotations()] or
#'   `viewer$get_spot_annotation_table()`.
#' @param rois Optional ROI table used to preserve annotation column order.
#' @param by Build columns by individual annotation or by annotation class.
#' @param include_unassigned Include an `unassigned` column for points outside
#'   all annotations.
#'
#' @return An integer matrix with one row per point and one column per
#'   annotation or class.
#' @export
wsi_annotation_association_matrix <- function(x, rois = NULL,
                                              by = c("annotation", "class"),
                                              include_unassigned = FALSE) {
  by <- match.arg(by)
  if (!is.data.frame(x)) {
    wsi_abort("`x` must be an annotation association data.frame.")
  }
  id_col <- if ("point_id" %in% names(x)) "point_id" else if ("spot_id" %in% names(x)) "spot_id" else NULL
  point_ids <- if (!is.null(id_col)) as.character(x[[id_col]]) else sprintf("point_%d", seq_len(nrow(x)))
  key <- if (identical(by, "class")) {
    as.character(x$annotation_class %||% NA_character_)
  } else {
    as.character(x$annotation_id %||% NA_character_)
  }
  key[!nzchar(key) | is.na(key)] <- NA_character_
  columns <- if (identical(by, "annotation") && inherits(rois, "wsi_roi")) {
    as.character(rois$roi_id %||% seq_len(nrow(rois)))
  } else {
    sort(unique(stats::na.omit(key)))
  }
  columns <- columns[nzchar(columns) & !is.na(columns)]
  if (isTRUE(include_unassigned)) {
    columns <- unique(c(columns, "unassigned"))
    key[is.na(key)] <- "unassigned"
  }
  mat <- matrix(0L, nrow = nrow(x), ncol = length(columns))
  rownames(mat) <- make.unique(point_ids)
  colnames(mat) <- columns
  matched <- match(key, columns)
  keep <- !is.na(matched)
  if (any(keep)) {
    mat[cbind(which(keep), matched[keep])] <- 1L
  }
  mat
}

#' Write annotation associations to CSV
#'
#' @param x A data.frame returned by [wsi_associate_annotations()].
#' @param file Output CSV path.
#'
#' @return The normalized output path, invisibly.
#' @export
wsi_write_annotation_associations <- function(x, file) {
  if (!is.data.frame(x)) {
    wsi_abort("`x` must be an annotation association data.frame.")
  }
  if (!is.character(file) || length(file) != 1L || is.na(file) || !nzchar(file)) {
    wsi_abort("`file` must be a single output path.")
  }
  dir <- dirname(file)
  if (!dir.exists(dir)) {
    wsi_abort(sprintf("Output directory does not exist: %s", dir))
  }
  utils::write.csv(x, file = file, row.names = FALSE)
  invisible(normalizePath(file, mustWork = FALSE))
}

wsi_annotation_points <- function(points) {
  if (!is.data.frame(points)) {
    wsi_abort("`points` must be a data.frame.")
  }
  x_name <- names(points)[tolower(names(points)) %in% c("x", "spot_x", "cell_x", "centroid_x")][1L]
  y_name <- names(points)[tolower(names(points)) %in% c("y", "spot_y", "cell_y", "centroid_y")][1L]
  if (is.na(x_name) || is.na(y_name)) {
    wsi_abort("`points` must contain coordinate columns such as `x`/`y`, `spot_x`/`spot_y`, or `cell_x`/`cell_y`.")
  }
  id_name <- names(points)[tolower(names(points)) %in% c("point_id", "spot_id", "cell_id", "barcode", "id")][1L]
  label_name <- names(points)[tolower(names(points)) %in% c("point_label", "spot_label", "cell_label", "label", "name")][1L]
  out <- data.frame(
    point_id = if (is.na(id_name)) sprintf("point_%d", seq_len(nrow(points))) else as.character(points[[id_name]]),
    point_label = if (is.na(label_name)) NA_character_ else as.character(points[[label_name]]),
    x = suppressWarnings(as.numeric(points[[x_name]])),
    y = suppressWarnings(as.numeric(points[[y_name]])),
    stringsAsFactors = FALSE
  )
  bad <- !is.finite(out$x) | !is.finite(out$y)
  if (any(bad)) {
    wsi_warn(sprintf("Dropping %d point(s) with invalid coordinates.", sum(bad)))
    out <- out[!bad, , drop = FALSE]
  }
  out
}

wsi_annotation_assign_indices <- function(points, rois, indices, engine = "auto") {
  if (identical(engine, "r")) {
    assigned <- rep(NA_integer_, nrow(points))
    for (i in indices) {
      inside <- wsi_points_in_roi(rois, i, points$x, points$y) & is.na(assigned)
      assigned[inside] <- i
    }
    return(assigned)
  }
  if (wsi_native_available("wsi_assign_points_to_polygons_cpp")) {
    polygons <- wsi_annotation_native_polygons(rois, indices)
    bbox <- cbind(rois$xmin[indices], rois$ymin[indices], rois$xmax[indices], rois$ymax[indices])
    local <- .Call(
      "wsi_assign_points_to_polygons_cpp",
      as.numeric(points$x),
      as.numeric(points$y),
      polygons,
      matrix(as.numeric(bbox), ncol = 4),
      PACKAGE = "wsiTools"
    )
    out <- rep(NA_integer_, length(local))
    keep <- !is.na(local)
    out[keep] <- indices[local[keep]]
    return(out)
  }
  if (identical(engine, "native")) {
    wsi_abort("Native annotation assignment is not available in this wsiTools build.")
  }
  wsi_annotation_assign_indices(points, rois, indices, engine = "r")
}

wsi_annotation_native_polygons <- function(rois, indices) {
  lapply(indices, function(i) {
    type <- tolower(as.character(rois$geometry_type[[i]] %||% ""))
    coords <- rois$coordinates[[i]]
    if (identical(type, "polygon")) {
      return(list(lapply(coords, wsi_ring_matrix)))
    }
    if (identical(type, "multipolygon")) {
      return(lapply(coords, function(poly) lapply(poly, wsi_ring_matrix)))
    }
    rect <- rbind(
      c(rois$xmin[[i]], rois$ymin[[i]]),
      c(rois$xmax[[i]], rois$ymin[[i]]),
      c(rois$xmax[[i]], rois$ymax[[i]]),
      c(rois$xmin[[i]], rois$ymax[[i]]),
      c(rois$xmin[[i]], rois$ymin[[i]])
    )
    list(list(matrix(as.numeric(rect), ncol = 2)))
  })
}

wsi_annotation_association_table <- function(points, rois, assigned) {
  annotation_id <- annotation_name <- annotation_class <- rep(NA_character_, nrow(points))
  keep <- !is.na(assigned)
  if (any(keep)) {
    idx <- assigned[keep]
    annotation_id[keep] <- as.character(rois$roi_id[idx] %||% idx)
    annotation_name[keep] <- as.character(rois$name[idx] %||% annotation_id[keep])
    annotation_class[keep] <- as.character(rois$class[idx] %||% annotation_name[keep])
  }
  out <- data.frame(
    point_id = points$point_id,
    point_label = points$point_label,
    x = points$x,
    y = points$y,
    annotation_index = assigned,
    annotation_id = annotation_id,
    annotation_name = annotation_name,
    annotation_class = annotation_class,
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_annotation_association", class(out))
  out
}
