wsi_cell_id_vector <- function(data, n) {
  if (is.data.frame(data)) {
    candidates <- c("cell_id", "id", "object_id", "objectId", "label")
    lower <- tolower(names(data))
    idx <- match(tolower(candidates), lower)
    idx <- idx[!is.na(idx)]
    if (length(idx)) {
      ids <- as.character(data[[idx[[1L]]]])
      ids[is.na(ids) | !nzchar(ids)] <- sprintf("cell_%d", which(is.na(ids) | !nzchar(ids)))
      return(make.unique(ids, sep = "_"))
    }
  }
  sprintf("cell_%d", seq_len(n))
}

wsi_ring_centroid <- function(ring) {
  if (nrow(ring) < 3L) {
    return(c(x = mean(ring[, 1L]), y = mean(ring[, 2L]), area = 0))
  }
  if (!all(ring[1L, ] == ring[nrow(ring), ])) {
    ring <- rbind(ring, ring[1L, ])
  }
  x <- ring[, 1L]
  y <- ring[, 2L]
  i <- seq_len(length(x) - 1L)
  cross <- x[i] * y[i + 1L] - x[i + 1L] * y[i]
  signed_area <- sum(cross) / 2
  if (abs(signed_area) <= .Machine$double.eps) {
    return(c(x = mean(x[i]), y = mean(y[i]), area = 0))
  }
  cx <- sum((x[i] + x[i + 1L]) * cross) / (6 * signed_area)
  cy <- sum((y[i] + y[i + 1L]) * cross) / (6 * signed_area)
  c(x = cx, y = cy, area = abs(signed_area))
}

wsi_roi_centroid_px <- function(rois, index) {
  polygons <- wsi_roi_polygons(rois, index)
  pieces <- lapply(polygons, function(rings) {
    if (!length(rings)) {
      return(c(x = NA_real_, y = NA_real_, area = 0))
    }
    wsi_ring_centroid(rings[[1L]])
  })
  mat <- do.call(rbind, pieces)
  ok <- is.finite(mat[, "x"]) & is.finite(mat[, "y"]) & mat[, "area"] > 0
  if (any(ok)) {
    weights <- mat[ok, "area"]
    return(c(
      x = stats::weighted.mean(mat[ok, "x"], weights),
      y = stats::weighted.mean(mat[ok, "y"], weights)
    ))
  }
  c(
    x = mean(c(rois$xmin[[index]], rois$xmax[[index]])),
    y = mean(c(rois$ymin[[index]], rois$ymax[[index]]))
  )
}

wsi_cell_class_from_data <- function(data, n) {
  if (!is.data.frame(data)) {
    return(rep(NA_character_, n))
  }
  candidates <- c("cell_class", "class", "classification", "type", "label")
  lower <- tolower(names(data))
  idx <- match(candidates, lower)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) {
    return(rep(NA_character_, n))
  }
  as.character(data[[idx[[1L]]]])
}

wsi_cell_table_from_points <- function(data) {
  pts <- wsi_points_matrix(data, "segmentation")
  n <- nrow(pts)
  base <- data.frame(
    cell_id = wsi_cell_id_vector(data, n),
    x = pts[, "x"],
    y = pts[, "y"],
    cell_class = wsi_cell_class_from_data(data, n),
    cell_area_px2 = NA_real_,
    stringsAsFactors = FALSE
  )
  if (is.data.frame(data)) {
    drop_names <- c("x", "y", "centroid_x", "centroid_y", "center_x", "center_y",
                    "centre_x", "centre_y", "cell_id", "id", "object_id",
                    "objectId", "label", "cell_class", "class", "classification")
    extras <- data[, setdiff(names(data), drop_names), drop = FALSE]
    if (ncol(extras)) {
      collisions <- intersect(names(extras), names(base))
      names(extras)[match(collisions, names(extras))] <- paste0("seg_", collisions)
      base <- cbind(base, extras)
    }
  }
  base
}

wsi_cell_table_from_rois <- function(rois) {
  if (!nrow(rois)) {
    return(data.frame(
      cell_id = character(),
      x = numeric(),
      y = numeric(),
      cell_class = character(),
      cell_area_px2 = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  centroids <- t(vapply(seq_len(nrow(rois)), function(i) wsi_roi_centroid_px(rois, i), numeric(2)))
  cell_name <- if ("name" %in% names(rois)) rois$name else as.character(rois$roi_id)
  object_type <- if ("object_type" %in% names(rois)) rois$object_type else rep(NA_character_, nrow(rois))
  data.frame(
    cell_id = make.unique(as.character(rois$roi_id), sep = "_"),
    x = centroids[, "x"],
    y = centroids[, "y"],
    cell_class = wsi_roi_class(rois$class),
    cell_area_px2 = vapply(seq_len(nrow(rois)), function(i) wsi_roi_area_px(rois, i), numeric(1)),
    cell_name = cell_name,
    cell_object_type = object_type,
    stringsAsFactors = FALSE
  )
}

wsi_cell_table_from_segmentation <- function(segmentation) {
  if (inherits(segmentation, "wsi_stardist_result")) {
    segmentation <- segmentation$segmentation
  }
  if (is.character(segmentation) && length(segmentation) == 1L) {
    segmentation <- import_segmentation(segmentation)
  }
  if (inherits(segmentation, "wsi_segmentation_table") && is.data.frame(segmentation$data)) {
    segmentation <- segmentation$data
  }
  if (inherits(segmentation, "wsi_roi")) {
    return(wsi_cell_table_from_rois(segmentation))
  }
  if (is.data.frame(segmentation) || is.matrix(segmentation) ||
      (is.numeric(segmentation) && length(segmentation) >= 2L)) {
    return(wsi_cell_table_from_points(segmentation))
  }
  wsi_abort("`segmentation` must be a centroid table, ROI/GeoJSON segmentation, StarDist result, or segmentation file path.")
}

wsi_cell_pixel_size <- function(slide = NULL, pixel_size = NULL) {
  if (!is.null(pixel_size)) {
    px <- wsi_pixel_size_xy(pixel_size)
    if (any(px <= 0)) {
      wsi_abort("`pixel_size` values must be greater than zero.")
    }
    return(px)
  }
  if (inherits(slide, "wsi_slide")) {
    mpp <- suppressWarnings(wsi_mpp(slide))
    if (length(mpp) >= 2L && all(is.finite(mpp[seq_len(2L)])) && all(mpp[seq_len(2L)] > 0)) {
      return(c(x = unname(mpp[[1L]]), y = unname(mpp[[2L]])))
    }
  }
  NULL
}

wsi_channels_as_ihc <- function(channels) {
  if (is.null(channels)) {
    return(NULL)
  }
  if (inherits(channels, "wsi_ihc_channels")) {
    return(channels)
  }
  if (!is.list(channels) || !length(channels)) {
    wsi_abort("`channels` must be NULL, a `wsi_ihc_channels` object, or a named list of numeric matrices.")
  }
  is_matrix <- vapply(channels, function(x) is.matrix(x) && is.numeric(x), logical(1))
  if (!all(is_matrix)) {
    wsi_abort("`channels` lists must contain only numeric matrices.")
  }
  ids <- names(channels)
  if (is.null(ids) || any(!nzchar(ids))) {
    ids <- sprintf("channel_%d", seq_along(channels))
  }
  ids <- wsi_clean_channel_id(ids, sprintf("channel_%d", seq_along(channels)))
  names(channels) <- ids
  channels$channel_metadata <- lapply(ids, function(id) {
    list(id = id, name = id, colour = "#FFFFFF", strength = 1, visible = TRUE)
  })
  class(channels) <- "wsi_ihc_channels"
  channels
}

wsi_cell_positive_thresholds <- function(positive_threshold, channel_table) {
  if (is.null(positive_threshold)) {
    return(NULL)
  }
  if (!is.numeric(positive_threshold) || anyNA(positive_threshold) || any(!is.finite(positive_threshold))) {
    wsi_abort("`positive_threshold` must be NULL or finite numeric threshold value(s).")
  }
  ids <- channel_table$channel_id
  if (length(positive_threshold) == 1L) {
    out <- rep(unname(positive_threshold), length(ids))
    names(out) <- ids
    return(out)
  }
  if (length(positive_threshold) != length(ids)) {
    wsi_abort("`positive_threshold` must have length 1 or one value per selected channel.")
  }
  values <- as.numeric(positive_threshold)
  names(values) <- names(positive_threshold)
  matched <- values
  if (!is.null(names(positive_threshold)) && all(nzchar(names(positive_threshold)))) {
    matched <- rep(NA_real_, length(ids))
    names(matched) <- ids
    keys <- wsi_stain_channel_key(names(positive_threshold))
    for (i in seq_along(ids)) {
      channel_keys <- wsi_stain_channel_key(c(ids[[i]], channel_table$channel_name[[i]]))
      idx <- match(channel_keys, keys)
      idx <- idx[!is.na(idx)]
      if (length(idx)) {
        matched[[i]] <- values[[idx[[1L]]]]
      } else {
        matched[[i]] <- values[[i]]
      }
    }
  } else {
    names(matched) <- ids
  }
  matched
}

wsi_safe_channel_column <- function(id, prefix = "channel") {
  key <- wsi_stain_channel_key(id)
  if (!nzchar(key)) {
    key <- "value"
  }
  paste(prefix, key, sep = "_")
}

wsi_cell_channel_values <- function(matrix, x, y, origin, downsample, radius) {
  height <- nrow(matrix)
  width <- ncol(matrix)
  col <- floor((x - origin[["x"]]) / downsample) + 1L
  row <- floor((y - origin[["y"]]) / downsample) + 1L
  if (!is.finite(row) || !is.finite(col) || row < 1L || row > height || col < 1L || col > width) {
    return(numeric())
  }
  radius_px <- radius / downsample
  if (radius_px <= 0) {
    return(matrix[row, col])
  }
  rows <- seq.int(max(1L, floor(row - radius_px)), min(height, ceiling(row + radius_px)))
  cols <- seq.int(max(1L, floor(col - radius_px)), min(width, ceiling(col + radius_px)))
  yy <- rows - row
  xx <- cols - col
  keep <- outer(yy^2, xx^2, "+") <= radius_px^2
  values <- matrix[rows, cols, drop = FALSE]
  values[keep]
}

wsi_cell_channel_table <- function(cells, channels = NULL,
                                   image_origin = c(x = 0, y = 0),
                                   channel_downsample = 1,
                                   channel = NULL,
                                   cell_radius = 0,
                                   positive_threshold = NULL) {
  channels <- wsi_channels_as_ihc(channels)
  if (is.null(channels)) {
    return(list(table = data.frame(stringsAsFactors = FALSE), matrix = matrix(numeric(), nrow = nrow(cells), ncol = 0L), channel_columns = character(), positive_columns = character()))
  }
  channel_table <- wsi_measurement_channels(channels, channel = channel)
  dims <- wsi_stain_channel_dimensions(channels, channel_table$channel_id)
  origin <- wsi_measurement_origin(image_origin)
  channel_downsample <- wsi_check_scalar_number(channel_downsample, "channel_downsample", allow_zero = FALSE)
  cell_radius <- wsi_check_scalar_number(cell_radius, "cell_radius", allow_zero = TRUE)
  thresholds <- wsi_cell_positive_thresholds(positive_threshold, channel_table)

  values <- matrix(NA_real_, nrow = nrow(cells), ncol = nrow(channel_table))
  colnames(values) <- channel_table$channel_id
  in_bounds <- rep(FALSE, nrow(cells))
  for (j in seq_len(nrow(channel_table))) {
    id <- channel_table$channel_id[[j]]
    mat <- channels[[id]]
    if (!identical(dim(mat), dims)) {
      wsi_abort("All channel matrices must have the same dimensions.")
    }
    for (i in seq_len(nrow(cells))) {
      sampled <- wsi_cell_channel_values(
        mat,
        x = cells$x[[i]],
        y = cells$y[[i]],
        origin = origin,
        downsample = channel_downsample,
        radius = cell_radius
      )
      if (length(sampled)) {
        values[i, j] <- mean(sampled, na.rm = TRUE)
        in_bounds[[i]] <- TRUE
      }
    }
  }

  out <- data.frame(channel_in_bounds = in_bounds, stringsAsFactors = FALSE)
  channel_columns <- character()
  positive_columns <- character()
  for (j in seq_len(nrow(channel_table))) {
    id <- channel_table$channel_id[[j]]
    col <- wsi_safe_channel_column(id)
    channel_columns <- c(channel_columns, col)
    out[[col]] <- values[, j]
    if (!is.null(thresholds)) {
      pcol <- paste0(col, "_positive")
      positive_columns <- c(positive_columns, pcol)
      out[[pcol]] <- is.finite(values[, j]) & values[, j] >= thresholds[[id]]
    }
  }
  rownames(values) <- cells$cell_id
  list(table = out, matrix = values, channel_columns = channel_columns, positive_columns = positive_columns)
}

wsi_assign_cells_to_rois <- function(cells, rois = NULL) {
  if (is.null(rois)) {
    return(data.frame(
      roi_id = rep(NA_character_, nrow(cells)),
      roi_name = rep(NA_character_, nrow(cells)),
      roi_class = rep(NA_character_, nrow(cells)),
      inside_roi = rep(FALSE, nrow(cells)),
      stringsAsFactors = FALSE
    ))
  }
  if (is.character(rois) && length(rois) == 1L) {
    rois <- read_geojson(rois)
  }
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be NULL, a `wsi_roi` object, or a GeoJSON file path.")
  }
  roi_index <- rep(NA_integer_, nrow(cells))
  if (nrow(rois)) {
    for (i in seq_len(nrow(cells))) {
      candidates <- which(
        cells$x[[i]] >= rois$xmin & cells$x[[i]] <= rois$xmax &
          cells$y[[i]] >= rois$ymin & cells$y[[i]] <= rois$ymax
      )
      if (length(candidates)) {
        hits <- candidates[vapply(candidates, function(j) wsi_point_in_roi(c(cells$x[[i]], cells$y[[i]]), rois, j), logical(1))]
        if (length(hits)) {
          roi_index[[i]] <- hits[[1L]]
        }
      }
    }
  }
  data.frame(
    roi_id = ifelse(is.na(roi_index), NA_character_, rois$roi_id[roi_index]),
    roi_name = ifelse(is.na(roi_index), NA_character_, rois$name[roi_index]),
    roi_class = ifelse(is.na(roi_index), NA_character_, wsi_roi_class(rois$class[roi_index])),
    inside_roi = !is.na(roi_index),
    stringsAsFactors = FALSE
  )
}

wsi_cell_group_summary <- function(cells, group_cols, channel_columns = character(),
                                   positive_columns = character()) {
  if (!nrow(cells)) {
    columns <- replicate(length(group_cols), character(), simplify = FALSE)
    names(columns) <- group_cols
    base <- as.data.frame(columns, stringsAsFactors = FALSE)
    base$cell_count <- integer()
    return(base)
  }
  groups <- unique(cells[, group_cols, drop = FALSE])
  rows <- lapply(seq_len(nrow(groups)), function(i) {
    keep <- rep(TRUE, nrow(cells))
    for (col in group_cols) {
      value <- groups[[col]][[i]]
      if (is.na(value)) {
        keep <- keep & is.na(cells[[col]])
      } else {
        keep <- keep & !is.na(cells[[col]]) & cells[[col]] == value
      }
    }
    subset <- cells[keep, , drop = FALSE]
    row <- groups[i, , drop = FALSE]
    row$cell_count <- nrow(subset)
    for (col in channel_columns) {
      row[[paste0(col, "_mean")]] <- wsi_mean_or_na(subset[[col]])
      row[[paste0(col, "_median")]] <- wsi_median_or_na(subset[[col]])
    }
    for (col in positive_columns) {
      positive <- subset[[col]]
      observed <- !is.na(positive)
      row[[paste0(col, "_count")]] <- sum(positive %in% TRUE)
      row[[paste0(col, "_fraction")]] <- if (any(observed)) mean(positive[observed] %in% TRUE) else NA_real_
    }
    row
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

wsi_cell_roi_counts <- function(cell_table, rois = NULL, pixel_size = NULL,
                                channel_columns = character(),
                                positive_columns = character()) {
  if (is.null(rois)) {
    out <- wsi_cell_group_summary(
      transform(cell_table, roi_id = "whole_slide", roi_name = "whole_slide", roi_class = "whole_slide"),
      c("roi_id", "roi_name", "roi_class"),
      channel_columns = channel_columns,
      positive_columns = positive_columns
    )
    out$area_px2 <- NA_real_
    out$area_um2 <- NA_real_
    out$area_mm2 <- NA_real_
    out$cells_per_px2 <- NA_real_
    out$cells_per_mm2 <- NA_real_
    return(out)
  }
  assigned <- cell_table[cell_table$inside_roi %in% TRUE, , drop = FALSE]
  grouped <- if (nrow(assigned)) {
    wsi_cell_group_summary(
      assigned,
      c("roi_id", "roi_name", "roi_class"),
      channel_columns = channel_columns,
      positive_columns = positive_columns
    )
  } else {
    data.frame(roi_id = character(), roi_name = character(), roi_class = character(), cell_count = integer(), stringsAsFactors = FALSE)
  }
  base <- wsi_roi_measurement_table(rois, pixel_size = pixel_size)
  out <- merge(base, grouped, by = c("roi_id", "roi_name", "roi_class"), all.x = TRUE, sort = FALSE)
  out$cell_count[is.na(out$cell_count)] <- 0L
  out$cells_per_px2 <- ifelse(out$area_px2 > 0, out$cell_count / out$area_px2, NA_real_)
  out$cells_per_mm2 <- ifelse(is.finite(out$area_mm2) & out$area_mm2 > 0, out$cell_count / out$area_mm2, NA_real_)
  out
}

wsi_cell_class_counts <- function(cell_table, rois = NULL, pixel_size = NULL,
                                  channel_columns = character(),
                                  positive_columns = character()) {
  if (is.null(rois)) {
    class_values <- cell_table$cell_class
    class_values[is.na(class_values) | !nzchar(class_values)] <- "all_cells"
    data <- cell_table
    data$class <- class_values
    return(wsi_cell_group_summary(data, "class", channel_columns, positive_columns))
  }
  assigned <- cell_table[cell_table$inside_roi %in% TRUE, , drop = FALSE]
  grouped <- if (nrow(assigned)) {
    data <- assigned
    data$class <- data$roi_class
    wsi_cell_group_summary(data, "class", channel_columns, positive_columns)
  } else {
    data.frame(class = character(), cell_count = integer(), stringsAsFactors = FALSE)
  }
  area <- summarise_rois(rois, pixel_size = pixel_size)
  out <- merge(area, grouped, by = "class", all.x = TRUE, sort = FALSE)
  out$cell_count[is.na(out$cell_count)] <- 0L
  out$cells_per_px2 <- ifelse(out$area_px2 > 0, out$cell_count / out$area_px2, NA_real_)
  out$cells_per_mm2 <- ifelse(is.finite(out$area_mm2) & out$area_mm2 > 0, out$cell_count / out$area_mm2, NA_real_)

  unassigned <- cell_table[!cell_table$inside_roi, , drop = FALSE]
  if (nrow(unassigned)) {
    unassigned$class <- "unassigned"
    extra <- wsi_cell_group_summary(
      unassigned,
      "class",
      channel_columns,
      positive_columns
    )
    for (name in setdiff(names(out), names(extra))) {
      extra[[name]] <- if (is.numeric(out[[name]])) NA_real_ else NA
    }
    for (name in setdiff(names(extra), names(out))) {
      out[[name]] <- if (is.numeric(extra[[name]])) NA_real_ else NA
    }
    extra <- extra[, names(out), drop = FALSE]
    out <- rbind(out, extra)
  }
  out
}

wsi_write_cell_counts <- function(result, output_dir = NULL, file = NULL,
                                  prefix = "wsi_cell_counts", overwrite = FALSE) {
  files <- character()
  if (!is.null(file)) {
    file <- wsi_validate_output_path(file, overwrite = overwrite)
    utils::write.csv(result$cell_table, file, row.names = FALSE)
    files[["cell_table"]] <- file
  }
  if (!is.null(output_dir)) {
    if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
      wsi_abort("`output_dir` must be a single non-empty directory path.")
    }
    if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
      wsi_abort(sprintf("Could not create output directory: %s", output_dir))
    }
    tables <- list(
      cell_table = result$cell_table,
      roi_counts = result$roi_counts,
      class_counts = result$class_counts,
      counts_matrix = as.data.frame(result$counts_matrix, stringsAsFactors = FALSE)
    )
    if (ncol(tables$counts_matrix)) {
      tables$counts_matrix <- cbind(cell_id = rownames(result$counts_matrix), tables$counts_matrix)
    } else {
      tables$counts_matrix <- data.frame(cell_id = result$cell_table$cell_id, stringsAsFactors = FALSE)
    }
    for (name in names(tables)) {
      path <- wsi_validate_output_path(file.path(output_dir, sprintf("%s_%s.csv", prefix, name)), overwrite = overwrite)
      utils::write.csv(tables[[name]], path, row.names = FALSE)
      files[[name]] <- path
    }
  }
  files
}

#' Build per-cell count and channel measurement tables
#'
#' Combines imported segmentation with optional deconvolved stain channels and
#' optional ROI annotations. The function works from centroid coordinates,
#' polygon cell segmentations, or imported StarDist/Cellpose-style outputs. It
#' samples only the already-supplied channel matrices and never loads a whole
#' slide into memory.
#'
#' @param slide Optional `wsi_slide` object. Used only to infer microns per
#'   pixel via [wsi_mpp()] when `pixel_size` is not supplied.
#' @param segmentation Cell segmentation as a centroid table, `wsi_roi`
#'   polygons, imported segmentation object, StarDist result, or file path.
#' @param channels Optional `wsi_ihc_channels` object, or a named list of
#'   numeric channel matrices, representing a patch/region/thumbnail already in
#'   memory.
#' @param rois Optional `wsi_roi` annotations, or a GeoJSON file path. Cells are
#'   assigned to the first ROI containing their centroid.
#' @param image_origin Level-0 x/y coordinate of the top-left pixel in
#'   `channels`.
#' @param channel_downsample Slide pixels represented by one channel-matrix
#'   pixel. Use values greater than 1 when `channels` came from a thumbnail or
#'   lower-resolution pyramid level.
#' @param channel Optional channel ids or names to measure.
#' @param cell_radius Radius, in slide pixels, used to average channel values
#'   around each centroid. The default `0` samples the centroid pixel only.
#' @param positive_threshold Optional numeric threshold(s) used to add per-cell
#'   channel-positive flags.
#' @param pixel_size Optional microns per pixel. When omitted and `slide` is a
#'   slide object, [wsi_mpp()] is used when available.
#' @param file Optional CSV path for the per-cell table.
#' @param output_dir Optional directory where per-cell, ROI, class, and counts
#'   matrix CSV files are written.
#' @param prefix File prefix used with `output_dir`.
#' @param overwrite Whether to overwrite existing CSV files.
#'
#' @return A `wsi_cell_counts` list with `cell_table`, `counts_matrix`,
#'   `roi_counts`, `class_counts`, and `files`.
#' @export
#'
#' @examples
#' cells <- data.frame(cell_id = c("c1", "c2"), x = c(5.5, 20.5), y = c(5.5, 6.5))
#' channels <- structure(
#'   list(
#'     hematoxylin = matrix(0.2, nrow = 32, ncol = 32),
#'     hrp_dab = matrix(0.6, nrow = 32, ncol = 32),
#'     channel_metadata = list(
#'       list(id = "hematoxylin", name = "Hematoxylin"),
#'       list(id = "hrp_dab", name = "HRP/DAB")
#'     )
#'   ),
#'   class = "wsi_ihc_channels"
#' )
#' counts <- wsi_cell_counts(segmentation = cells, channels = channels)
wsi_cell_counts <- function(slide = NULL, segmentation, channels = NULL, rois = NULL,
                            image_origin = c(x = 0, y = 0),
                            channel_downsample = 1,
                            channel = NULL,
                            cell_radius = 0,
                            positive_threshold = NULL,
                            pixel_size = NULL,
                            file = NULL,
                            output_dir = NULL,
                            prefix = "wsi_cell_counts",
                            overwrite = FALSE) {
  if (missing(segmentation)) {
    wsi_abort("`segmentation` is required.")
  }
  if (!is.null(slide) && !inherits(slide, "wsi_slide")) {
    wsi_abort("`slide` must be NULL or a `wsi_slide` object.")
  }
  if (!is.null(rois) && is.character(rois) && length(rois) == 1L) {
    rois <- read_geojson(rois)
  }
  if (!is.null(rois) && !inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be NULL, a `wsi_roi` object, or a GeoJSON file path.")
  }

  px <- wsi_cell_pixel_size(slide = slide, pixel_size = pixel_size)
  cell_table <- wsi_cell_table_from_segmentation(segmentation)
  roi_assignment <- wsi_assign_cells_to_rois(cell_table, rois = rois)
  channel_data <- wsi_cell_channel_table(
    cell_table,
    channels = channels,
    image_origin = image_origin,
    channel_downsample = channel_downsample,
    channel = channel,
    cell_radius = cell_radius,
    positive_threshold = positive_threshold
  )
  cell_table <- cbind(cell_table, roi_assignment, channel_data$table)
  if (!is.null(px)) {
    cell_table$pixel_size_x_um <- px[["x"]]
    cell_table$pixel_size_y_um <- px[["y"]]
  }

  roi_counts <- wsi_cell_roi_counts(
    cell_table,
    rois = rois,
    pixel_size = px,
    channel_columns = channel_data$channel_columns,
    positive_columns = channel_data$positive_columns
  )
  class_counts <- wsi_cell_class_counts(
    cell_table,
    rois = rois,
    pixel_size = px,
    channel_columns = channel_data$channel_columns,
    positive_columns = channel_data$positive_columns
  )

  result <- structure(
    list(
      cell_table = cell_table,
      counts_matrix = channel_data$matrix,
      roi_counts = roi_counts,
      class_counts = class_counts,
      pixel_size = px,
      files = character()
    ),
    class = "wsi_cell_counts"
  )
  result$files <- wsi_write_cell_counts(result, output_dir = output_dir, file = file, prefix = prefix, overwrite = overwrite)
  result
}

#' @rdname wsi_cell_counts
#' @export
cell_counts <- wsi_cell_counts

#' @export
print.wsi_cell_counts <- function(x, ...) {
  cat("<wsi_cell_counts>\n")
  cat(sprintf("  cells:   %d\n", nrow(x$cell_table)))
  cat(sprintf("  channels:%d\n", ncol(x$counts_matrix)))
  cat(sprintf("  rois:    %d\n", nrow(x$roi_counts)))
  cat(sprintf("  classes: %d\n", nrow(x$class_counts)))
  if (length(x$files)) {
    cat(sprintf("  csv files: %d\n", length(x$files)))
  }
  invisible(x)
}

#' @export
as.data.frame.wsi_cell_counts <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame(x$cell_table, row.names = row.names, optional = optional, ...)
}
