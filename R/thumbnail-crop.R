#' Create a slide thumbnail
#'
#' Uses libvips when available so the thumbnail is generated through the backend
#' instead of by loading level 0 into R.
#'
#' @param slide A `wsi_slide` object.
#' @param width Desired thumbnail width in pixels.
#' @param height Optional maximum height in pixels.
#' @param format Return format.
#' @param level Reserved for future explicit pyramid-level thumbnailing.
#'
#' @return A magick image, array, or raster.
#' @export
wsi_thumbnail <- function(slide, width = 1024, height = NULL,
                          format = c("magick", "array", "raster"), level = "auto") {
  wsi_check_slide(slide)
  format <- match.arg(format)
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  if (!is.null(height)) {
    height <- as.integer(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
  }

  if (identical(slide$backend, "mock")) {
    thumb_height <- height %||% max(1L, as.integer(round(width * slide$dimensions[["height"]] / slide$dimensions[["width"]])))
    array <- wsi_blank_array(width, thumb_height, colour = c(0.98, 0.98, 0.98, 1))
    if (format == "array") {
      return(array)
    }
    if (format == "raster") {
      return(wsi_array_to_raster(array))
    }
    wsi_require_magick("create a mock thumbnail")
    return(magick::image_blank(width, thumb_height, color = "grey98"))
  }

  if (identical(slide$backend, "imagemagick") && !wsi_has_vips()) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    wsi_imagemagick_thumbnail_file(slide, tmp, width = width, height = height)
    return(wsi_read_image_file(tmp, format))
  }

  if (!wsi_has_vips()) {
    wsi_abort(
      wsi_backend_action_message(
        "Thumbnail generation requires libvips for this milestone.",
        backend = "vips"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  args <- c("thumbnail", slide$path, tmp, as.character(width))
  if (!is.null(height)) {
    args <- c(args, "--height", as.character(height))
  }
  wsi_run_command("vips", args = args, error_message = "libvips failed to create a thumbnail.")
  wsi_read_image_file(tmp, format)
}

#' Crop a slide region
#'
#' Crops only the requested region. When `output` is supplied, the crop is
#' written to disk; otherwise it is returned to R through [wsi_read_region()].
#'
#' @param slide A `wsi_slide` object.
#' @param x,y,width,height,level Region coordinates; see [wsi_read_region()].
#' @param output Optional output path.
#' @param format Output file format when `output` is supplied.
#' @param backend Backend for file export.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return A returned image object when `output = NULL`, otherwise the output
#'   path invisibly.
#' @export
wsi_crop <- function(slide, x, y, width, height, level = 0, output = NULL,
                     format = c("tiff", "png", "jpeg", "ome-tiff"),
                     backend = c("auto", "vips", "openslide", "bioformats", "imagemagick"),
                     overwrite = FALSE) {
  format <- match.arg(format)
  backend <- match.arg(backend)
  region <- wsi_validate_region(slide, x, y, width, height, level)

  if (is.null(output)) {
    return(wsi_read_region(slide, x, y, width, height, level = level, format = "array"))
  }

  output <- wsi_validate_output_path(output, overwrite = overwrite)
  actual_output <- output
  if (format %in% c("tiff", "ome-tiff") && (backend == "vips" || (backend == "auto" && wsi_has_vips()))) {
    actual_output <- wsi_vips_tiff_target(
      output,
      tile_size = min(region$width, region$height, 512L),
      compression = "lzw",
      pyramid = identical(format, "ome-tiff"),
      bigtiff = identical(format, "ome-tiff"),
      ome = identical(format, "ome-tiff")
    )
  }

  chosen <- wsi_choose_region_backend(slide, backend)
  if (chosen == "openslide" && format != "png") {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    wsi_openslide_read_region_file(slide, region, tmp)
    if (wsi_has_vips()) {
      wsi_vips_copy(tmp, actual_output)
    } else {
      wsi_require_magick("convert OpenSlide PNG output to the requested format")
      magick::image_write(magick::image_read(tmp), path = output, format = format)
    }
  } else {
    wsi_region_to_file(slide, region, actual_output, backend = chosen)
  }

  invisible(output)
}
