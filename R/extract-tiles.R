wsi_stride_to_overlap <- function(tile_size, stride) {
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  stride <- as.integer(wsi_check_scalar_number(stride, "stride", allow_zero = FALSE))
  if (stride > tile_size) {
    wsi_abort("`stride` greater than `tile_size` is not implemented in this milestone because it creates gaps between tiles.")
  }
  tile_size - stride
}

#' Extract tiles with fixed tile size and stride
#'
#' Convenience wrapper around [wsi_tile_grid()] and [wsi_tile()]. When
#' `save_images = FALSE`, only tile coordinates are returned and no pixels are
#' read. When `save_images = TRUE`, tiles are exported through the existing
#' region-based tile pipeline.
#'
#' @param image A `wsi_slide` object.
#' @param roi Optional ROI object. The first milestone uses the ROI bounding box.
#' @param tile_size Tile width and height in pixels at `level`.
#' @param stride Step size in pixels at `level`.
#' @param output_dir Optional output directory.
#' @param save_images Whether to save tile image files. Defaults to `TRUE` when
#'   `output_dir` is supplied.
#' @param level Pyramid level.
#' @param tissue_mask Whether to estimate and filter by tissue mask when saving
#'   images.
#' @param tissue_threshold Minimum tissue fraction when `tissue_mask = TRUE`.
#' @param format Tile image format.
#' @param prefix Optional filename prefix.
#' @param overwrite Whether to overwrite existing tile files.
#' @param include_partial Include partial edge tiles when returning coordinates.
#' @param ... Reserved for future extensions.
#'
#' @return A tile grid or tile manifest data frame.
#' @export
extract_tiles <- function(image, roi = NULL, tile_size = 512, stride = 512,
                          output_dir = NULL, save_images = !is.null(output_dir),
                          level = 0, tissue_mask = FALSE,
                          tissue_threshold = 0.1,
                          format = c("png", "jpeg", "tiff"),
                          prefix = NULL, overwrite = FALSE,
                          include_partial = FALSE, ...) {
  wsi_check_slide(image)
  format <- match.arg(format)
  overlap <- wsi_stride_to_overlap(tile_size, stride)
  region <- NULL
  if (!is.null(roi)) {
    region <- wsi_roi_bbox(roi)
  }

  if (!isTRUE(save_images)) {
    mask <- if (isTRUE(tissue_mask)) wsi_tissue_mask(image) else NULL
    grid <- wsi_tile_grid(
      image,
      tile_size = tile_size,
      overlap = overlap,
      level = level,
      region = region,
      tissue_mask = mask,
      include_partial = include_partial
    )
    if (isTRUE(tissue_mask)) {
      grid <- grid[is.na(grid$tissue_fraction) | grid$tissue_fraction >= tissue_threshold, , drop = FALSE]
    }
    return(grid)
  }

  if (is.null(output_dir)) {
    wsi_abort("`output_dir` is required when `save_images = TRUE`.")
  }

  wsi_tile(
    image,
    output_dir = output_dir,
    tile_size = tile_size,
    overlap = overlap,
    level = level,
    region = region,
    tissue_mask = tissue_mask,
    tissue_threshold = tissue_threshold,
    format = format,
    prefix = prefix,
    overwrite = overwrite,
    workers = 1
  )
}

#' @rdname extract_tiles
#' @export
wsi_extract_tiles <- extract_tiles
