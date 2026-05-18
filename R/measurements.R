wsi_pixel_size_xy <- function(pixel_size = NULL) {
  if (is.null(pixel_size)) {
    return(NULL)
  }
  if (is.list(pixel_size)) {
    pixel_size <- unlist(pixel_size, use.names = TRUE)
  }
  if (!is.numeric(pixel_size) || length(pixel_size) < 1L || anyNA(pixel_size) || any(!is.finite(pixel_size))) {
    wsi_abort("`pixel_size` must be NULL or numeric microns-per-pixel value(s).")
  }
  if (length(pixel_size) == 1L) {
    return(c(x = pixel_size[[1L]], y = pixel_size[[1L]]))
  }
  c(x = unname(pixel_size[[1L]]), y = unname(pixel_size[[2L]]))
}

wsi_distance_record <- function(dx, dy, pixel_size = NULL) {
  distance_px <- sqrt(dx^2 + dy^2)
  px <- wsi_pixel_size_xy(pixel_size)
  distance_um <- if (is.null(px)) NA_real_ else sqrt((dx * px[["x"]])^2 + (dy * px[["y"]])^2)
  data.frame(distance_px = distance_px, distance_um = distance_um)
}

#' Measure distance between two points
#'
#' @param point1,point2 Numeric coordinate pairs, matrices, or data frames with
#'   `x` and `y`.
#' @param pixel_size Optional microns per pixel. Supply one isotropic value or
#'   two values for x/y.
#'
#' @return A one-row data frame with pixel and micron distances.
#' @export
measure_distance <- function(point1, point2, pixel_size = NULL) {
  p1 <- wsi_points_matrix(point1, "point1")[1L, ]
  p2 <- wsi_points_matrix(point2, "point2")[1L, ]
  wsi_distance_record(p2[[1L]] - p1[[1L]], p2[[2L]] - p1[[2L]], pixel_size)
}

#' @rdname measure_distance
#' @export
wsi_measure_distance <- measure_distance

#' Measure nearest-neighbour distances between cells
#'
#' @param cells Data frame or matrix with `x` and `y` cell coordinates.
#' @param pixel_size Optional microns per pixel.
#'
#' @return A data frame with one row per cell.
#' @export
measure_nearest_cells <- function(cells, pixel_size = NULL) {
  pts <- wsi_points_matrix(cells, "cells")
  n <- nrow(pts)
  nearest <- rep(NA_integer_, n)
  distance_px <- rep(NA_real_, n)
  if (n > 1L) {
    for (i in seq_len(n)) {
      deltas <- sweep(pts, 2L, pts[i, ], "-")
      distances <- sqrt(rowSums(deltas^2))
      distances[[i]] <- Inf
      nearest[[i]] <- which.min(distances)
      distance_px[[i]] <- distances[[nearest[[i]]]]
    }
  }
  px <- wsi_pixel_size_xy(pixel_size)
  distance_um <- if (is.null(px)) NA_real_ else distance_px * mean(px)
  data.frame(
    cell_id = seq_len(n),
    x = pts[, 1L],
    y = pts[, 2L],
    nearest_cell_id = nearest,
    distance_px = distance_px,
    distance_um = distance_um,
    stringsAsFactors = FALSE
  )
}

#' @rdname measure_nearest_cells
#' @export
wsi_measure_nearest_cells <- measure_nearest_cells

#' Measure distances from cells to ROI boundaries
#'
#' @param cells Data frame or matrix with `x` and `y` cell coordinates.
#' @param roi A `wsi_roi` object.
#' @param pixel_size Optional microns per pixel.
#'
#' @return A data frame with nearest ROI boundary distances.
#' @export
measure_cells_to_roi <- function(cells, roi, pixel_size = NULL) {
  pts <- wsi_points_matrix(cells, "cells")
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a `wsi_roi` object.")
  }
  px <- wsi_pixel_size_xy(pixel_size)
  rows <- lapply(seq_len(nrow(pts)), function(i) {
    distances <- vapply(seq_len(nrow(roi)), function(j) {
      wsi_point_roi_boundary_distance(pts[i, ], roi, j)
    }, numeric(1))
    nearest <- which.min(distances)
    data.frame(
      cell_id = i,
      x = pts[i, 1L],
      y = pts[i, 2L],
      roi_id = roi$roi_id[[nearest]],
      roi_class = roi$class[[nearest]],
      inside = wsi_point_in_roi(pts[i, ], roi, nearest),
      distance_px = distances[[nearest]],
      distance_um = if (is.null(px)) NA_real_ else distances[[nearest]] * mean(px),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @rdname measure_cells_to_roi
#' @export
wsi_measure_cells_to_roi <- measure_cells_to_roi

#' Measure distance between ROI boundaries
#'
#' @param rois A `wsi_roi` object with at least two polygonal ROIs.
#' @param pixel_size Optional microns per pixel.
#'
#' @return A data frame with pairwise ROI distances.
#' @export
measure_roi_distance <- function(rois, pixel_size = NULL) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  if (nrow(rois) < 2L) {
    return(data.frame(
      roi_id_1 = character(),
      roi_id_2 = character(),
      distance_px = numeric(),
      distance_um = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  px <- wsi_pixel_size_xy(pixel_size)
  pairs <- utils::combn(seq_len(nrow(rois)), 2L)
  rows <- lapply(seq_len(ncol(pairs)), function(k) {
    i <- pairs[1L, k]
    j <- pairs[2L, k]
    pts_i <- do.call(rbind, wsi_roi_rings(rois, i))
    pts_j <- do.call(rbind, wsi_roi_rings(rois, j))
    d1 <- vapply(seq_len(nrow(pts_i)), function(p) {
      wsi_point_roi_boundary_distance(pts_i[p, ], rois, j)
    }, numeric(1))
    d2 <- vapply(seq_len(nrow(pts_j)), function(p) {
      wsi_point_roi_boundary_distance(pts_j[p, ], rois, i)
    }, numeric(1))
    distance_px <- min(c(d1, d2), na.rm = TRUE)
    data.frame(
      roi_id_1 = rois$roi_id[[i]],
      roi_id_2 = rois$roi_id[[j]],
      distance_px = distance_px,
      distance_um = if (is.null(px)) NA_real_ else distance_px * mean(px),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @rdname measure_roi_distance
#' @export
wsi_measure_roi_distance <- measure_roi_distance

#' Measure cell density inside ROIs
#'
#' @param cells Data frame or matrix with `x` and `y` cell coordinates.
#' @param roi A `wsi_roi` object.
#' @param pixel_size Optional microns per pixel.
#'
#' @return A data frame with counts, ROI area, and density.
#' @export
measure_cell_density <- function(cells, roi, pixel_size = NULL) {
  pts <- wsi_points_matrix(cells, "cells")
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a `wsi_roi` object.")
  }
  px <- wsi_pixel_size_xy(pixel_size)
  rows <- lapply(seq_len(nrow(roi)), function(i) {
    inside <- vapply(seq_len(nrow(pts)), function(j) wsi_point_in_roi(pts[j, ], roi, i), logical(1))
    area_px2 <- wsi_roi_area_px(roi, i)
    area_mm2 <- if (is.null(px)) NA_real_ else area_px2 * px[["x"]] * px[["y"]] / 1e6
    cell_count <- sum(inside)
    data.frame(
      roi_id = roi$roi_id[[i]],
      roi_class = roi$class[[i]],
      cell_count = cell_count,
      area_px2 = area_px2,
      area_mm2 = area_mm2,
      cells_per_px2 = if (area_px2 > 0) cell_count / area_px2 else NA_real_,
      cells_per_mm2 = if (!is.null(px) && area_mm2 > 0) cell_count / area_mm2 else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @rdname measure_cell_density
#' @export
wsi_measure_cell_density <- measure_cell_density

#' Summarise annotated tissue classes
#'
#' @param rois A `wsi_roi` object.
#' @param cells Optional data frame or matrix with `x` and `y` cell coordinates.
#' @param pixel_size Optional microns per pixel.
#' @param file Optional CSV output path.
#'
#' @return A data frame with area and optional cell-density summaries per class.
#' @export
summarise_rois <- function(rois, cells = NULL, pixel_size = NULL, file = NULL) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  px <- wsi_pixel_size_xy(pixel_size)
  roi_classes <- rois$class
  roi_classes[is.na(roi_classes) | !nzchar(roi_classes)] <- "unclassified"
  area_px2 <- vapply(seq_len(nrow(rois)), function(i) wsi_roi_area_px(rois, i), numeric(1))
  total_area <- sum(area_px2)
  base <- data.frame(class = roi_classes, area_px2 = area_px2, stringsAsFactors = FALSE)
  summary <- stats::aggregate(area_px2 ~ class, data = base, sum)
  summary$roi_count <- as.integer(tabulate(match(roi_classes, summary$class), nbins = nrow(summary)))
  summary$percent_area <- if (total_area > 0) 100 * summary$area_px2 / total_area else NA_real_
  summary$area_mm2 <- if (is.null(px)) NA_real_ else summary$area_px2 * px[["x"]] * px[["y"]] / 1e6

  if (!is.null(cells)) {
    density <- measure_cell_density(cells, rois, pixel_size = pixel_size)
    density_class <- density$roi_class
    density_class[is.na(density_class) | !nzchar(density_class)] <- "unclassified"
    counts <- stats::aggregate(density$cell_count, list(class = density_class), sum)
    names(counts)[[2L]] <- "cell_count"
    summary <- merge(summary, counts, by = "class", all.x = TRUE)
    summary$cell_count[is.na(summary$cell_count)] <- 0L
    summary$cells_per_mm2 <- if (is.null(px)) NA_real_ else ifelse(summary$area_mm2 > 0, summary$cell_count / summary$area_mm2, NA_real_)
    summary$cells_per_px2 <- ifelse(summary$area_px2 > 0, summary$cell_count / summary$area_px2, NA_real_)
  }

  if (!is.null(file)) {
    utils::write.csv(summary, file, row.names = FALSE)
  }
  summary
}

#' @rdname summarise_rois
#' @export
wsi_summarise_rois <- summarise_rois
