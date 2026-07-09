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

wsi_area_record <- function(area_px2, pixel_size = NULL) {
  px <- wsi_pixel_size_xy(pixel_size)
  area_um2 <- if (is.null(px)) NA_real_ else area_px2 * px[["x"]] * px[["y"]]
  area_mm2 <- if (is.null(px)) NA_real_ else area_um2 / 1e6
  data.frame(area_px2 = area_px2, area_um2 = area_um2, area_mm2 = area_mm2)
}

wsi_roi_class <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(x)] <- "unclassified"
  x
}

wsi_roi_measurement_table <- function(rois, pixel_size = NULL) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  area_px2 <- vapply(seq_len(nrow(rois)), function(i) wsi_roi_area_px(rois, i), numeric(1))
  area <- wsi_area_record(area_px2, pixel_size = pixel_size)
  total_area <- sum(area_px2, na.rm = TRUE)
  data.frame(
    roi_id = rois$roi_id,
    roi_name = rois$name,
    roi_class = wsi_roi_class(rois$class),
    object_type = rois$object_type,
    xmin = rois$xmin,
    ymin = rois$ymin,
    xmax = rois$xmax,
    ymax = rois$ymax,
    area,
    percent_total_area = if (total_area > 0) 100 * area_px2 / total_area else NA_real_,
    stringsAsFactors = FALSE
  )
}

wsi_measurement_origin <- function(image_origin = c(x = 0, y = 0)) {
  if (is.null(image_origin)) {
    image_origin <- c(x = 0, y = 0)
  }
  if (is.list(image_origin)) {
    image_origin <- unlist(image_origin, use.names = TRUE)
  }
  if (!is.numeric(image_origin) || length(image_origin) < 2L ||
      anyNA(image_origin[seq_len(2L)]) || any(!is.finite(image_origin[seq_len(2L)]))) {
    wsi_abort("`image_origin` must be a numeric x/y coordinate pair.")
  }
  if (all(c("x", "y") %in% names(image_origin))) {
    return(c(x = unname(image_origin[["x"]]), y = unname(image_origin[["y"]])))
  }
  c(x = unname(image_origin[[1L]]), y = unname(image_origin[[2L]]))
}

wsi_measurement_channels <- function(channels, channel = NULL) {
  if (!inherits(channels, "wsi_ihc_channels")) {
    wsi_abort("`channels` must be a `wsi_ihc_channels` object returned by `wsi_deconvolve_ihc()` or `wsi_deconvolve_multi_ihc()`.")
  }
  ids <- if (!is.null(channels$channel_metadata)) {
    vapply(channels$channel_metadata, `[[`, character(1), "id")
  } else {
    setdiff(wsi_channel_ids_from_output(channels), c("residual", "residual_1", "residual_2"))
  }
  names <- ids
  metadata <- channels$channel_metadata
  if (!is.null(metadata)) {
    names <- vapply(metadata, function(x) x$name %||% x$id, character(1))
  }
  if (!is.null(channel)) {
    requested <- tolower(as.character(channel))
    matches <- which(tolower(ids) %in% requested | tolower(names) %in% requested)
    if (!length(matches)) {
      wsi_abort("None of the requested stain `channel` values were found.")
    }
    ids <- ids[matches]
    names <- names[matches]
  }
  data.frame(channel_id = ids, channel_name = names, stringsAsFactors = FALSE)
}

wsi_stain_channel_dimensions <- function(channels, channel_ids) {
  dims <- dim(channels[[channel_ids[[1L]]]])
  if (length(dims) != 2L) {
    wsi_abort("Stain channel matrices must be two-dimensional.")
  }
  for (id in channel_ids[-1L]) {
    if (!identical(dim(channels[[id]]), dims)) {
      wsi_abort("All stain channel matrices must have the same dimensions.")
    }
  }
  dims
}

wsi_roi_pixel_mask <- function(rois, index, dims, image_origin = c(x = 0, y = 0), max_pixels = 5e6) {
  origin <- wsi_measurement_origin(image_origin)
  height <- dims[[1L]]
  width <- dims[[2L]]
  cols <- seq.int(
    max(1L, floor(rois$xmin[[index]] - origin[["x"]]) + 1L),
    min(width, ceiling(rois$xmax[[index]] - origin[["x"]]))
  )
  rows <- seq.int(
    max(1L, floor(rois$ymin[[index]] - origin[["y"]]) + 1L),
    min(height, ceiling(rois$ymax[[index]] - origin[["y"]]))
  )
  if (!length(cols) || !length(rows) || cols[[1L]] > cols[[length(cols)]] || rows[[1L]] > rows[[length(rows)]]) {
    return(list(rows = integer(), cols = integer(), mask = matrix(FALSE, nrow = 0L, ncol = 0L)))
  }
  candidate_pixels <- length(rows) * length(cols)
  if (candidate_pixels > max_pixels) {
    wsi_abort(sprintf(
      "ROI `%s` covers %s candidate pixels in the supplied stain image. Use a smaller region or increase `max_pixels`.",
      rois$roi_id[[index]],
      format(candidate_pixels, scientific = FALSE)
    ))
  }

  xs <- origin[["x"]] + cols - 0.5
  ys <- origin[["y"]] + rows - 0.5
  mask <- matrix(FALSE, nrow = length(rows), ncol = length(cols))
  for (i in seq_along(ys)) {
    y <- ys[[i]]
    mask[i, ] <- vapply(xs, function(x) wsi_point_in_roi(c(x, y), rois, index), logical(1))
  }
  list(rows = rows, cols = cols, mask = mask)
}

wsi_stain_summary_row <- function(values, positive_threshold = NULL) {
  values <- values[is.finite(values)]
  if (!length(values)) {
    return(data.frame(
      n_pixels = 0L,
      mean_intensity = NA_real_,
      median_intensity = NA_real_,
      sd_intensity = NA_real_,
      min_intensity = NA_real_,
      max_intensity = NA_real_,
      p10_intensity = NA_real_,
      p90_intensity = NA_real_,
      positive_fraction = NA_real_
    ))
  }
  data.frame(
    n_pixels = length(values),
    mean_intensity = mean(values),
    median_intensity = stats::median(values),
    sd_intensity = if (length(values) > 1L) stats::sd(values) else 0,
    min_intensity = min(values),
    max_intensity = max(values),
    p10_intensity = unname(stats::quantile(values, 0.1, names = FALSE, type = 7)),
    p90_intensity = unname(stats::quantile(values, 0.9, names = FALSE, type = 7)),
    positive_fraction = if (is.null(positive_threshold)) NA_real_ else mean(values >= positive_threshold)
  )
}

wsi_empty_ihc_intensity_summary <- function(level = c("roi", "class")) {
  level <- match.arg(level)
  base <- if (identical(level, "roi")) {
    data.frame(
      roi_id = character(),
      roi_name = character(),
      roi_class = character(),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      class = character(),
      stringsAsFactors = FALSE
    )
  }
  metrics <- data.frame(
    ihc_roi_count = integer(),
    ihc_n_pixels = integer(),
    ihc_roi_area_px2 = numeric(),
    ihc_dab_channel = character(),
    ihc_hematoxylin_channel = character(),
    ihc_dab_threshold = numeric(),
    ihc_dab_mean = numeric(),
    ihc_dab_median = numeric(),
    ihc_dab_positive_pixels = integer(),
    ihc_dab_positive_fraction = numeric(),
    ihc_dab_positive_area_px2 = numeric(),
    ihc_dab_positive_area_um2 = numeric(),
    ihc_dab_positive_area_mm2 = numeric(),
    ihc_hematoxylin_density = numeric(),
    ihc_hematoxylin_median = numeric(),
    ihc_dab_h_ratio = numeric(),
    stringsAsFactors = FALSE
  )
  if (identical(level, "roi")) {
    metrics$ihc_roi_count <- NULL
  }
  out <- cbind(base, metrics)
  class(out) <- c("wsi_ihc_intensity_summary", class(out))
  out
}

wsi_stain_channel_key <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", x))
}

wsi_ihc_channel_id <- function(channels, channel = NULL, candidates, name) {
  channel_table <- wsi_measurement_channels(channels)
  keys <- c(
    wsi_stain_channel_key(channel_table$channel_id),
    wsi_stain_channel_key(channel_table$channel_name)
  )
  ids <- rep(channel_table$channel_id, 2L)
  requested <- if (is.null(channel)) candidates else channel
  requested <- wsi_stain_channel_key(requested)
  for (key in requested) {
    idx <- match(key, keys)
    if (!is.na(idx)) {
      return(ids[[idx]])
    }
  }
  wsi_abort(sprintf(
    "Could not find the %s stain channel. Supply `%s_channel` explicitly.",
    name,
    name
  ))
}

wsi_safe_ratio <- function(numerator, denominator, epsilon = .Machine$double.eps) {
  ifelse(is.finite(denominator) & abs(denominator) > epsilon, numerator / denominator, NA_real_)
}

wsi_mean_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

wsi_median_or_na <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

wsi_weighted_mean_or_na <- function(values, weights) {
  ok <- is.finite(values) & is.finite(weights) & weights > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  stats::weighted.mean(values[ok], weights[ok])
}

wsi_sum_or_na <- function(values) {
  values <- values[is.finite(values)]
  if (length(values)) sum(values) else NA_real_
}

wsi_ihc_positive_area_record <- function(positive_pixels, pixel_size = NULL) {
  px <- wsi_pixel_size_xy(pixel_size)
  area_um2 <- if (is.null(px)) NA_real_ else positive_pixels * px[["x"]] * px[["y"]]
  data.frame(
    ihc_dab_positive_area_px2 = as.numeric(positive_pixels),
    ihc_dab_positive_area_um2 = area_um2,
    ihc_dab_positive_area_mm2 = if (is.null(px)) NA_real_ else area_um2 / 1e6
  )
}

wsi_ihc_intensity_row <- function(roi_id, roi_name, roi_class, roi_area_px2,
                                  dab_values, hematoxylin_values,
                                  dab_channel, hematoxylin_channel,
                                  dab_threshold, pixel_size = NULL) {
  dab_values <- dab_values[is.finite(dab_values)]
  hematoxylin_values <- hematoxylin_values[is.finite(hematoxylin_values)]
  n_pixels <- length(dab_values)
  positive_pixels <- if (n_pixels) sum(dab_values >= dab_threshold) else 0L
  dab_mean <- wsi_mean_or_na(dab_values)
  hematoxylin_density <- wsi_mean_or_na(hematoxylin_values)
  cbind(
    data.frame(
      roi_id = roi_id,
      roi_name = roi_name,
      roi_class = wsi_roi_class(roi_class),
      ihc_n_pixels = as.integer(n_pixels),
      ihc_roi_area_px2 = roi_area_px2,
      ihc_dab_channel = dab_channel,
      ihc_hematoxylin_channel = hematoxylin_channel,
      ihc_dab_threshold = dab_threshold,
      ihc_dab_mean = dab_mean,
      ihc_dab_median = wsi_median_or_na(dab_values),
      ihc_dab_positive_pixels = as.integer(positive_pixels),
      ihc_dab_positive_fraction = if (n_pixels > 0) positive_pixels / n_pixels else NA_real_,
      stringsAsFactors = FALSE
    ),
    wsi_ihc_positive_area_record(positive_pixels, pixel_size = pixel_size),
    data.frame(
      ihc_hematoxylin_density = hematoxylin_density,
      ihc_hematoxylin_median = wsi_median_or_na(hematoxylin_values),
      ihc_dab_h_ratio = wsi_safe_ratio(dab_mean, hematoxylin_density),
      stringsAsFactors = FALSE
    )
  )
}

wsi_ihc_intensity_class_summary <- function(roi_summary) {
  if (!is.data.frame(roi_summary) || !nrow(roi_summary)) {
    return(wsi_empty_ihc_intensity_summary("class"))
  }
  classes <- unique(wsi_roi_class(roi_summary$roi_class))
  rows <- lapply(classes, function(cls) {
    group <- roi_summary[wsi_roi_class(roi_summary$roi_class) == cls, , drop = FALSE]
    n_pixels <- sum(group$ihc_n_pixels, na.rm = TRUE)
    positive_pixels <- sum(group$ihc_dab_positive_pixels, na.rm = TRUE)
    dab_mean <- wsi_weighted_mean_or_na(group$ihc_dab_mean, group$ihc_n_pixels)
    hematoxylin_density <- wsi_weighted_mean_or_na(group$ihc_hematoxylin_density, group$ihc_n_pixels)
    cbind(
      data.frame(
        class = cls,
        ihc_roi_count = nrow(group),
        ihc_n_pixels = as.integer(n_pixels),
        ihc_roi_area_px2 = sum(group$ihc_roi_area_px2, na.rm = TRUE),
        ihc_dab_channel = group$ihc_dab_channel[[1L]],
        ihc_hematoxylin_channel = group$ihc_hematoxylin_channel[[1L]],
        ihc_dab_threshold = group$ihc_dab_threshold[[1L]],
        ihc_dab_mean = dab_mean,
        ihc_dab_median = wsi_weighted_mean_or_na(group$ihc_dab_median, group$ihc_n_pixels),
        ihc_dab_positive_pixels = as.integer(positive_pixels),
        ihc_dab_positive_fraction = if (n_pixels > 0) positive_pixels / n_pixels else NA_real_,
        stringsAsFactors = FALSE
      ),
      data.frame(
        ihc_dab_positive_area_px2 = sum(group$ihc_dab_positive_area_px2, na.rm = TRUE),
        ihc_dab_positive_area_um2 = wsi_sum_or_na(group$ihc_dab_positive_area_um2),
        ihc_dab_positive_area_mm2 = wsi_sum_or_na(group$ihc_dab_positive_area_mm2),
        stringsAsFactors = FALSE
      ),
      data.frame(
        ihc_hematoxylin_density = hematoxylin_density,
        ihc_hematoxylin_median = wsi_weighted_mean_or_na(group$ihc_hematoxylin_median, group$ihc_n_pixels),
        ihc_dab_h_ratio = wsi_safe_ratio(dab_mean, hematoxylin_density),
        stringsAsFactors = FALSE
      )
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("wsi_ihc_intensity_summary", class(out))
  out
}

wsi_write_report_table <- function(table, file, overwrite = FALSE) {
  file <- wsi_validate_output_path(file, overwrite = overwrite)
  utils::write.csv(table, file, row.names = FALSE)
  invisible(file)
}

wsi_report_table_names <- function(report) {
  intersect(
    c("roi_summary", "class_summary", "nearest_cells", "cell_boundary", "stain_summary", "ihc_summary", "ihc_class_summary"),
    names(report)
  )
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
    inside <- if (isTRUE(wsi_roi_polygonal(roi)[[i]])) {
      vapply(seq_len(nrow(pts)), function(j) wsi_point_in_roi(pts[j, ], roi, i), logical(1))
    } else {
      rep(FALSE, nrow(pts))
    }
    area_px2 <- wsi_roi_area_px(roi, i)
    area_um2 <- if (is.null(px)) NA_real_ else area_px2 * px[["x"]] * px[["y"]]
    area_mm2 <- if (is.null(px)) NA_real_ else area_um2 / 1e6
    cell_count <- sum(inside)
    data.frame(
      roi_id = roi$roi_id[[i]],
      roi_name = roi$name[[i]],
      roi_class = wsi_roi_class(roi$class[[i]]),
      cell_count = cell_count,
      area_px2 = area_px2,
      area_um2 = area_um2,
      area_mm2 = area_mm2,
      cells_per_px2 = if (is.finite(area_px2) && area_px2 > 0) cell_count / area_px2 else NA_real_,
      cells_per_mm2 = if (!is.null(px) && is.finite(area_mm2) && area_mm2 > 0) cell_count / area_mm2 else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' @rdname measure_cell_density
#' @export
wsi_measure_cell_density <- measure_cell_density

#' Summarise stain intensity in deconvolved IHC channels
#'
#' Summarises hematoxylin, HRP/DAB, or other deconvolved stain-channel
#' concentration matrices for a small image, thumbnail, or region. The function
#' works on a `wsi_ihc_channels` object and does not read whole-slide pixels.
#'
#' @param channels A `wsi_ihc_channels` object returned by
#'   [wsi_deconvolve_ihc()] or [wsi_deconvolve_multi_ihc()].
#' @param rois Optional `wsi_roi` object. When supplied, intensities are
#'   summarised inside each ROI.
#' @param image_origin Level-0 x/y coordinate of the top-left pixel in
#'   `channels`. Use this when `channels` came from a region crop but `rois`
#'   are in slide coordinates.
#' @param channel Optional channel ids or names to summarise.
#' @param positive_threshold Optional concentration threshold used to report
#'   positive pixel fraction.
#' @param file Optional CSV output path.
#' @param overwrite Whether to overwrite `file`.
#' @param max_pixels Maximum ROI candidate pixels to mask in one call.
#'
#' @return A data frame with per-channel or per-ROI stain intensity summaries.
#' @export
measure_stain_intensity <- function(channels, rois = NULL,
                                    image_origin = c(x = 0, y = 0),
                                    channel = NULL,
                                    positive_threshold = NULL,
                                    file = NULL,
                                    overwrite = FALSE,
                                    max_pixels = 5e6) {
  channel_table <- wsi_measurement_channels(channels, channel = channel)
  dims <- wsi_stain_channel_dimensions(channels, channel_table$channel_id)
  if (!is.null(positive_threshold)) {
    positive_threshold <- wsi_check_scalar_number(positive_threshold, "positive_threshold")
  }
  max_pixels <- as.integer(wsi_check_scalar_number(max_pixels, "max_pixels", allow_zero = FALSE))

  if (!is.null(rois) && !inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be NULL or a `wsi_roi` object.")
  }

  if (is.null(rois)) {
    rows <- lapply(seq_len(nrow(channel_table)), function(i) {
      id <- channel_table$channel_id[[i]]
      cbind(
        data.frame(
          roi_id = "whole_image",
          roi_name = "whole_image",
          roi_class = "whole_image",
          channel_id = id,
          channel_name = channel_table$channel_name[[i]],
          stringsAsFactors = FALSE
        ),
        wsi_stain_summary_row(as.vector(channels[[id]]), positive_threshold = positive_threshold)
      )
    })
  } else {
    masks <- lapply(seq_len(nrow(rois)), function(i) {
      wsi_roi_pixel_mask(rois, i, dims = dims, image_origin = image_origin, max_pixels = max_pixels)
    })
    rows <- lapply(seq_len(nrow(rois)), function(roi_index) {
      mask <- masks[[roi_index]]
      lapply(seq_len(nrow(channel_table)), function(channel_index) {
        id <- channel_table$channel_id[[channel_index]]
        values <- numeric()
        if (length(mask$rows) && length(mask$cols) && any(mask$mask)) {
          region <- channels[[id]][mask$rows, mask$cols, drop = FALSE]
          values <- region[mask$mask]
        }
        cbind(
          data.frame(
            roi_id = rois$roi_id[[roi_index]],
            roi_name = rois$name[[roi_index]],
            roi_class = wsi_roi_class(rois$class[[roi_index]]),
            channel_id = id,
            channel_name = channel_table$channel_name[[channel_index]],
            stringsAsFactors = FALSE
          ),
          wsi_stain_summary_row(values, positive_threshold = positive_threshold)
        )
      })
    })
    rows <- unlist(rows, recursive = FALSE)
  }

  out <- do.call(rbind, rows)
  if (!is.null(file)) {
    wsi_write_report_table(out, file, overwrite = overwrite)
  }
  out
}

#' @rdname measure_stain_intensity
#' @export
wsi_measure_stain_intensity <- measure_stain_intensity

#' Measure IHC intensity inside ROIs
#'
#' Computes practical hematoxylin/HRP-DAB measurements inside ROI polygons from
#' deconvolved IHC channels. The function works on already-read regions,
#' thumbnails, or crops represented by a `wsi_ihc_channels` object; it does not
#' read a whole-slide image into memory.
#'
#' Reported ROI metrics include DAB mean, DAB-positive area, hematoxylin
#' density, and the DAB/hematoxylin ratio. Class summaries use pixel-count
#' weighted means and summed DAB-positive area.
#'
#' @param channels A `wsi_ihc_channels` object returned by
#'   [wsi_deconvolve_ihc()] or [wsi_deconvolve_multi_ihc()].
#' @param rois Optional `wsi_roi` object. When omitted, the whole supplied
#'   channel image is summarised.
#' @param image_origin Level-0 x/y coordinate of the top-left pixel in
#'   `channels`.
#' @param dab_channel,hematoxylin_channel Optional channel ids or names. Defaults
#'   match the standard `hrp_dab` and `hematoxylin` channels.
#' @param dab_threshold DAB concentration threshold for positive-area reporting.
#'   This is assay-specific; tune it for your staining and scanner.
#' @param pixel_size Optional microns per pixel. Used to report positive area in
#'   square microns and square millimetres.
#' @param by Return level: `"roi"`, `"class"`, or `"both"`.
#' @param file Optional CSV output path. When `by = "both"`, this is treated as
#'   an output directory and two CSV files are written.
#' @param overwrite Whether to overwrite output files.
#' @param max_pixels Maximum ROI candidate pixels to mask in one call.
#'
#' @return A data frame, or a `wsi_ihc_intensity_report` list when
#'   `by = "both"`.
#' @export
#'
#' @examples
#' image <- array(0.8, dim = c(50, 50, 3))
#' channels <- wsi_deconvolve_ihc(image)
#' measure_ihc_intensity(channels, dab_threshold = 0.1)
measure_ihc_intensity <- function(channels, rois = NULL,
                                  image_origin = c(x = 0, y = 0),
                                  dab_channel = NULL,
                                  hematoxylin_channel = NULL,
                                  dab_threshold = 0.1,
                                  pixel_size = NULL,
                                  by = c("roi", "class", "both"),
                                  file = NULL,
                                  overwrite = FALSE,
                                  max_pixels = 5e6) {
  if (!inherits(channels, "wsi_ihc_channels")) {
    wsi_abort("`channels` must be a `wsi_ihc_channels` object returned by `wsi_deconvolve_ihc()` or `wsi_deconvolve_multi_ihc()`.")
  }
  by <- match.arg(by)
  dab_threshold <- wsi_check_scalar_number(dab_threshold, "dab_threshold", allow_zero = TRUE)
  max_pixels <- as.integer(wsi_check_scalar_number(max_pixels, "max_pixels", allow_zero = FALSE))
  dab_id <- wsi_ihc_channel_id(
    channels,
    channel = dab_channel,
    candidates = c("hrp_dab", "hrp/dab", "dab", "hrp"),
    name = "dab"
  )
  hematoxylin_id <- wsi_ihc_channel_id(
    channels,
    channel = hematoxylin_channel,
    candidates = c("hematoxylin", "haematoxylin", "h"),
    name = "hematoxylin"
  )
  dims <- wsi_stain_channel_dimensions(channels, c(dab_id, hematoxylin_id))

  if (!is.null(rois) && !inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be NULL or a `wsi_roi` object.")
  }

  if (is.null(rois)) {
    roi_summary <- wsi_ihc_intensity_row(
      roi_id = "whole_image",
      roi_name = "whole_image",
      roi_class = "whole_image",
      roi_area_px2 = prod(dims),
      dab_values = as.vector(channels[[dab_id]]),
      hematoxylin_values = as.vector(channels[[hematoxylin_id]]),
      dab_channel = dab_id,
      hematoxylin_channel = hematoxylin_id,
      dab_threshold = dab_threshold,
      pixel_size = pixel_size
    )
  } else if (!nrow(rois)) {
    roi_summary <- wsi_empty_ihc_intensity_summary("roi")
  } else {
    masks <- lapply(seq_len(nrow(rois)), function(i) {
      wsi_roi_pixel_mask(rois, i, dims = dims, image_origin = image_origin, max_pixels = max_pixels)
    })
    rows <- lapply(seq_len(nrow(rois)), function(i) {
      mask <- masks[[i]]
      dab_values <- numeric()
      hematoxylin_values <- numeric()
      if (length(mask$rows) && length(mask$cols) && any(mask$mask)) {
        dab_region <- channels[[dab_id]][mask$rows, mask$cols, drop = FALSE]
        hematoxylin_region <- channels[[hematoxylin_id]][mask$rows, mask$cols, drop = FALSE]
        dab_values <- dab_region[mask$mask]
        hematoxylin_values <- hematoxylin_region[mask$mask]
      }
      wsi_ihc_intensity_row(
        roi_id = rois$roi_id[[i]],
        roi_name = rois$name[[i]],
        roi_class = rois$class[[i]],
        roi_area_px2 = wsi_roi_area_px(rois, i),
        dab_values = dab_values,
        hematoxylin_values = hematoxylin_values,
        dab_channel = dab_id,
        hematoxylin_channel = hematoxylin_id,
        dab_threshold = dab_threshold,
        pixel_size = pixel_size
      )
    })
    roi_summary <- do.call(rbind, rows)
  }
  rownames(roi_summary) <- NULL
  class(roi_summary) <- c("wsi_ihc_intensity_summary", class(roi_summary))
  class_summary <- wsi_ihc_intensity_class_summary(roi_summary)

  out <- switch(
    by,
    roi = roi_summary,
    class = class_summary,
    both = structure(
      list(roi_summary = roi_summary, class_summary = class_summary, files = character()),
      class = c("wsi_ihc_intensity_report", "list")
    )
  )

  if (!is.null(file)) {
    if (identical(by, "both")) {
      if (!dir.exists(file) && !dir.create(file, recursive = TRUE, showWarnings = FALSE)) {
        wsi_abort(sprintf("Could not create output directory: %s", file))
      }
      roi_file <- file.path(file, "ihc_roi_summary.csv")
      class_file <- file.path(file, "ihc_class_summary.csv")
      wsi_write_report_table(out$roi_summary, roi_file, overwrite = overwrite)
      wsi_write_report_table(out$class_summary, class_file, overwrite = overwrite)
      out$files <- c(roi_summary = roi_file, class_summary = class_file)
    } else {
      wsi_write_report_table(out, file, overwrite = overwrite)
    }
  }
  out
}

#' @rdname measure_ihc_intensity
#' @export
wsi_measure_ihc_intensity <- measure_ihc_intensity

#' @rdname measure_ihc_intensity
#' @param x ROI-level IHC intensity data returned by
#'   [measure_ihc_intensity()].
#' @export
summarise_ihc_intensity <- function(x, file = NULL, overwrite = FALSE) {
  if (inherits(x, "wsi_ihc_intensity_report")) {
    x <- x$roi_summary
  }
  if (!is.data.frame(x) || !"roi_class" %in% names(x)) {
    wsi_abort("`x` must be ROI-level data returned by `measure_ihc_intensity()`.")
  }
  out <- wsi_ihc_intensity_class_summary(x)
  if (!is.null(file)) {
    wsi_write_report_table(out, file, overwrite = overwrite)
  }
  out
}

#' @rdname measure_ihc_intensity
#' @export
wsi_summarise_ihc_intensity <- summarise_ihc_intensity

#' Summarise annotated tissue classes
#'
#' @param rois A `wsi_roi` object.
#' @param cells Optional data frame or matrix with `x` and `y` cell coordinates.
#' @param pixel_size Optional microns per pixel.
#' @param file Optional CSV output path.
#' @param overwrite Whether to overwrite `file`.
#'
#' @return A data frame with area and optional cell-density summaries per class.
#' @export
summarise_rois <- function(rois, cells = NULL, pixel_size = NULL, file = NULL, overwrite = FALSE) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  px <- wsi_pixel_size_xy(pixel_size)
  roi_summary <- wsi_roi_measurement_table(rois, pixel_size = pixel_size)
  roi_classes <- roi_summary$roi_class
  area_px2 <- roi_summary$area_px2
  area_for_summary <- area_px2
  area_for_summary[is.na(area_for_summary)] <- 0
  total_area <- sum(area_for_summary)
  base <- data.frame(class = roi_classes, area_px2 = area_for_summary, stringsAsFactors = FALSE)
  summary <- stats::aggregate(area_px2 ~ class, data = base, sum)
  summary$roi_count <- as.integer(tabulate(match(roi_classes, summary$class), nbins = nrow(summary)))
  summary$percent_area <- if (total_area > 0) 100 * summary$area_px2 / total_area else NA_real_
  summary$area_um2 <- if (is.null(px)) NA_real_ else summary$area_px2 * px[["x"]] * px[["y"]]
  summary$area_mm2 <- if (is.null(px)) NA_real_ else summary$area_um2 / 1e6

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
    wsi_write_report_table(summary, file, overwrite = overwrite)
  }
  summary
}

#' @rdname summarise_rois
#' @export
wsi_summarise_rois <- summarise_rois

#' Create a pathology measurement report
#'
#' Combines ROI area summaries, per-class summaries, optional cell-density
#' measurements, nearest-neighbour distances, distance-to-boundary tables, and
#' optional hematoxylin/HRP-DAB stain intensity summaries. The function only
#' uses ROI coordinates, cell coordinates, and already-small/deconvolved stain
#' images supplied by the caller.
#'
#' @param rois A `wsi_roi` object.
#' @param cells Optional data frame or matrix with `x` and `y` cell coordinates.
#' @param stains Optional `wsi_ihc_channels` object returned by
#'   [wsi_deconvolve_ihc()] or [wsi_deconvolve_multi_ihc()].
#' @param pixel_size Optional microns per pixel.
#' @param image_origin Level-0 x/y coordinate of the top-left pixel in `stains`.
#' @param positive_threshold Optional concentration threshold used for stain
#'   positive-pixel fractions.
#' @param ihc_intensity Whether to add practical IHC ROI/class summaries with
#'   DAB mean, DAB-positive area, hematoxylin density, and DAB/H ratio when
#'   `stains` contains hematoxylin and HRP/DAB channels.
#' @param dab_threshold DAB threshold used for the IHC positive-area summary.
#'   Defaults to `positive_threshold` when supplied, otherwise `0.1`.
#' @param output_dir Optional directory where report CSV files should be written.
#' @param prefix File prefix used when `output_dir` is supplied.
#' @param overwrite Whether to overwrite existing CSV files.
#' @param max_pixels Maximum ROI candidate pixels to mask for stain summaries.
#'
#' @return A `wsi_measurement_report` list containing data frames.
#' @export
measurement_report <- function(rois, cells = NULL, stains = NULL,
                               pixel_size = NULL,
                               image_origin = c(x = 0, y = 0),
                               positive_threshold = NULL,
                               ihc_intensity = TRUE,
                               dab_threshold = positive_threshold %||% 0.1,
                               output_dir = NULL,
                               prefix = "wsi_measurements",
                               overwrite = FALSE,
                               max_pixels = 5e6) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  roi_summary <- wsi_roi_measurement_table(rois, pixel_size = pixel_size)
  density <- NULL
  nearest <- NULL
  boundary <- NULL
  if (!is.null(cells)) {
    density <- measure_cell_density(cells, rois, pixel_size = pixel_size)
    roi_summary <- merge(
      roi_summary,
      density[, c("roi_id", "cell_count", "cells_per_px2", "cells_per_mm2"), drop = FALSE],
      by = "roi_id",
      all.x = TRUE,
      sort = FALSE
    )
    roi_summary$cell_count[is.na(roi_summary$cell_count)] <- 0L
    nearest <- measure_nearest_cells(cells, pixel_size = pixel_size)
    boundary <- measure_cells_to_roi(cells, rois, pixel_size = pixel_size)
  }

  class_summary <- summarise_rois(rois, cells = cells, pixel_size = pixel_size)
  stain_summary <- NULL
  ihc_summary <- NULL
  ihc_class_summary <- NULL
  if (!is.null(stains)) {
    stain_summary <- measure_stain_intensity(
      stains,
      rois = rois,
      image_origin = image_origin,
      positive_threshold = positive_threshold,
      max_pixels = max_pixels
    )
    if (isTRUE(ihc_intensity)) {
      ihc_report <- measure_ihc_intensity(
        stains,
        rois = rois,
        image_origin = image_origin,
        dab_threshold = dab_threshold,
        pixel_size = pixel_size,
        by = "both",
        max_pixels = max_pixels
      )
      ihc_summary <- ihc_report$roi_summary
      ihc_class_summary <- ihc_report$class_summary
    }
  }

  report <- list(
    roi_summary = roi_summary,
    class_summary = class_summary,
    nearest_cells = nearest,
    cell_boundary = boundary,
    stain_summary = stain_summary,
    ihc_summary = ihc_summary,
    ihc_class_summary = ihc_class_summary,
    files = character()
  )
  class(report) <- c("wsi_measurement_report", class(report))

  if (!is.null(output_dir)) {
    report <- write_measurement_report(report, output_dir = output_dir, prefix = prefix, overwrite = overwrite)
  }
  report
}

#' @rdname measurement_report
#' @export
wsi_measurement_report <- measurement_report

#' Write measurement report tables to CSV files
#'
#' @param report A report returned by [measurement_report()].
#' @param output_dir Output directory. It is created if needed.
#' @param prefix CSV filename prefix.
#' @param overwrite Whether to overwrite existing CSV files.
#'
#' @return The report, with a `files` element containing written paths.
#' @export
write_measurement_report <- function(report, output_dir,
                                     prefix = "wsi_measurements",
                                     overwrite = FALSE) {
  if (!inherits(report, "wsi_measurement_report")) {
    wsi_abort("`report` must be a `wsi_measurement_report` object.")
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    wsi_abort("`output_dir` must be a single non-empty directory path.")
  }
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create output directory: %s", output_dir))
  }
  if (!is.character(prefix) || length(prefix) != 1L || is.na(prefix) || !nzchar(prefix)) {
    wsi_abort("`prefix` must be a single non-empty character value.")
  }

  tables <- wsi_report_table_names(report)
  files <- character()
  for (name in tables) {
    table <- report[[name]]
    if (!is.data.frame(table)) {
      next
    }
    file <- file.path(output_dir, sprintf("%s_%s.csv", prefix, name))
    wsi_write_report_table(table, file, overwrite = overwrite)
    files[[name]] <- file
  }
  report$files <- files
  report
}

#' @rdname write_measurement_report
#' @export
wsi_write_measurement_report <- write_measurement_report

#' @export
print.wsi_measurement_report <- function(x, ...) {
  cat("<wsi_measurement_report>\n")
  for (name in wsi_report_table_names(x)) {
    table <- x[[name]]
    if (is.data.frame(table)) {
      cat(sprintf("  %s: %d rows x %d columns\n", name, nrow(table), ncol(table)))
    }
  }
  if (length(x$files)) {
    cat(sprintf("  csv files: %d\n", length(x$files)))
  }
  invisible(x)
}
