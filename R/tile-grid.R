wsi_tissue_fraction_for_tile <- function(mask, x, y, width, height, downsample) {
  if (is.null(mask)) {
    return(NA_real_)
  }
  if (!inherits(mask, "wsi_tissue_mask")) {
    wsi_abort("`tissue_mask` must be a `wsi_tissue_mask` object returned by `wsi_tissue_mask()`.")
  }
  mat <- mask$mask
  coverage_width <- width * downsample
  coverage_height <- height * downsample

  x1 <- max(1L, floor(x / mask$scale_x) + 1L)
  x2 <- min(ncol(mat), ceiling((x + coverage_width) / mask$scale_x))
  y1 <- max(1L, floor(y / mask$scale_y) + 1L)
  y2 <- min(nrow(mat), ceiling((y + coverage_height) / mask$scale_y))
  if (x2 < x1 || y2 < y1) {
    return(NA_real_)
  }
  mean(mat[y1:y2, x1:x2, drop = FALSE], na.rm = TRUE)
}

#' Generate a tile grid
#'
#' Generates coordinates only. No image pixels are read.
#'
#' @param slide A `wsi_slide` object.
#' @param tile_size Tile width and height in pixels at `level`.
#' @param overlap Overlap in pixels at `level`.
#' @param level Pyramid level.
#' @param region Optional level-0 region (`x`, `y`, `width`, `height`).
#' @param tissue_mask Optional `wsi_tissue_mask` object.
#' @param include_partial Include edge tiles smaller than `tile_size`.
#'
#' @return A data frame of tile coordinates and metadata.
#' @export
wsi_tile_grid <- function(slide, tile_size = 512, overlap = 0, level = 0,
                          region = NULL, tissue_mask = NULL,
                          include_partial = FALSE) {
  wsi_check_slide(slide)
  level_info <- wsi_level_row(slide, level)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  overlap <- as.integer(wsi_check_scalar_number(overlap, "overlap"))
  if (overlap >= tile_size) {
    wsi_abort("`overlap` must be smaller than `tile_size`.")
  }

  region <- wsi_normalize_region(region, slide)
  downsample <- level_info$downsample[[1L]]
  tile_coverage <- tile_size * downsample
  step_coverage <- (tile_size - overlap) * downsample
  x_end <- region[["x"]] + region[["width"]]
  y_end <- region[["y"]] + region[["height"]]

  if (isTRUE(include_partial)) {
    x_starts <- seq(region[["x"]], x_end - 1e-6, by = step_coverage)
    y_starts <- seq(region[["y"]], y_end - 1e-6, by = step_coverage)
  } else {
    last_x <- x_end - tile_coverage
    last_y <- y_end - tile_coverage
    x_starts <- if (last_x >= region[["x"]]) seq(region[["x"]], last_x, by = step_coverage) else numeric()
    y_starts <- if (last_y >= region[["y"]]) seq(region[["y"]], last_y, by = step_coverage) else numeric()
  }

  if (!length(x_starts) || !length(y_starts)) {
    out <- data.frame(
      tile_id = character(),
      x = numeric(),
      y = numeric(),
      width = integer(),
      height = integer(),
      level = integer(),
      row = integer(),
      col = integer(),
      downsample = numeric(),
      tissue_fraction = numeric(),
      output_file = character(),
      stringsAsFactors = FALSE
    )
    return(out)
  }

  rows <- vector("list", length(x_starts) * length(y_starts))
  k <- 0L
  for (row in seq_along(y_starts)) {
    for (col in seq_along(x_starts)) {
      x <- as.integer(round(x_starts[[col]]))
      y <- as.integer(round(y_starts[[row]]))
      width_level <- if (isTRUE(include_partial)) min(tile_size, ceiling((x_end - x) / downsample)) else tile_size
      height_level <- if (isTRUE(include_partial)) min(tile_size, ceiling((y_end - y) / downsample)) else tile_size
      if (width_level <= 0L || height_level <= 0L) {
        next
      }
      k <- k + 1L
      tile_id <- sprintf("L%d_R%05d_C%05d", level, row, col)
      rows[[k]] <- data.frame(
        tile_id = tile_id,
        x = x,
        y = y,
        width = as.integer(width_level),
        height = as.integer(height_level),
        level = as.integer(level),
        row = as.integer(row),
        col = as.integer(col),
        downsample = downsample,
        tissue_fraction = wsi_tissue_fraction_for_tile(tissue_mask, x, y, width_level, height_level, downsample),
        output_file = sprintf("%s.png", tile_id),
        stringsAsFactors = FALSE
      )
    }
  }

  do.call(rbind, rows[seq_len(k)])
}

#' Extract tiles to disk
#'
#' Generates a tile grid, optionally filters by tissue, writes tiles to disk, and
#' returns a manifest.
#'
#' @param slide A `wsi_slide` object.
#' @param output_dir Output directory.
#' @param tile_size,overlap,level,region Arguments passed to [wsi_tile_grid()].
#' @param roi Optional ROI object; use [wsi_tile_roi()] for ROI-specific tiling.
#' @param tissue_mask Whether to estimate and filter by tissue mask.
#' @param tissue_threshold Minimum tissue fraction when `tissue_mask = TRUE`.
#' @param format Tile image format.
#' @param prefix Optional filename prefix.
#' @param overwrite Whether to overwrite existing tiles.
#' @param workers Reserved for future parallel processing.
#'
#' @return A tile manifest data frame.
#' @export
wsi_tile <- function(slide, output_dir, tile_size = 512, overlap = 0, level = 0,
                     region = NULL, roi = NULL, tissue_mask = FALSE,
                     tissue_threshold = 0.1, format = c("png", "jpeg", "tiff"),
                     prefix = NULL, overwrite = FALSE, workers = 1) {
  wsi_check_slide(slide)
  format <- match.arg(format)
  if (!identical(as.integer(workers), 1L)) {
    wsi_warn("Parallel tile extraction is planned but not implemented yet; using `workers = 1`.")
  }

  if (!is.null(roi)) {
    wsi_warn("Use `wsi_tile_roi()` for ROI-aware tiling. This call will use the ROI bounding box in the first milestone.")
    region <- c(x = roi$xmin[[1L]], y = roi$ymin[[1L]], width = roi$xmax[[1L]] - roi$xmin[[1L]], height = roi$ymax[[1L]] - roi$ymin[[1L]])
  }

  mask <- NULL
  if (isTRUE(tissue_mask)) {
    mask <- wsi_tissue_mask(slide)
  }

  grid <- wsi_tile_grid(
    slide,
    tile_size = tile_size,
    overlap = overlap,
    level = level,
    region = region,
    tissue_mask = mask,
    include_partial = FALSE
  )

  if (isTRUE(tissue_mask)) {
    grid <- grid[is.na(grid$tissue_fraction) | grid$tissue_fraction >= tissue_threshold, , drop = FALSE]
  }

  if (!is.null(prefix) && nrow(grid)) {
    grid$output_file <- sprintf("%s_%s.%s", prefix, tools::file_path_sans_ext(grid$tile_id), wsi_format_extension(format))
  }

  manifest <- wsi_export_tiles(slide, grid, output_dir = output_dir, format = format, overwrite = overwrite)
  class(manifest) <- c("wsi_tile_manifest", setdiff(class(manifest), "wsi_tile_manifest"))
  manifest
}
