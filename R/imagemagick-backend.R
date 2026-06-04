#' @rdname wsi_backends
#' @export
wsi_has_imagemagick <- function() {
  wsi_command_exists("magick") || requireNamespace("magick", quietly = TRUE)
}

wsi_imagemagick_cli <- function() {
  wsi_command_exists("magick")
}

wsi_imagemagick_input <- function(path) {
  paste0(path, "[0]")
}

wsi_imagemagick_identify <- function(path) {
  if (wsi_imagemagick_cli()) {
    out <- wsi_run_command(
      "magick",
      args = c("identify", "-quiet", "-format", "%w\n%h\n%m", wsi_imagemagick_input(path)),
      error_message = sprintf("ImageMagick could not read metadata from `%s`.", path)
    )
    out <- trimws(out)
    out <- out[nzchar(out)]
    if (length(out) >= 2L) {
      return(list(
        width = suppressWarnings(as.numeric(out[[1L]])),
        height = suppressWarnings(as.numeric(out[[2L]])),
        format = out[[3L]] %||% NA_character_
      ))
    }
  }

  wsi_require_magick("read image metadata with the ImageMagick fallback backend")
  info <- magick::image_info(magick::image_read(path))
  list(width = info$width[[1L]], height = info$height[[1L]], format = info$format[[1L]])
}

wsi_imagemagick_open <- function(path) {
  if (!wsi_has_imagemagick()) {
    wsi_abort(
      wsi_backend_action_message(
        "ImageMagick fallback could not open the image because ImageMagick is not installed.",
        backend = "imagemagick"
      ),
      class = "wsi_backend_unavailable"
    )
  }
  info <- wsi_imagemagick_identify(path)
  width <- info$width
  height <- info$height
  if (is.na(width) || is.na(height) || width <= 0 || height <= 0) {
    wsi_abort("ImageMagick did not report image dimensions for this file.")
  }
  levels <- data.frame(
    level = 0L,
    width = width,
    height = height,
    downsample = 1,
    stringsAsFactors = FALSE
  )
  properties <- list(
    "imagemagick.format" = info$format %||% NA_character_,
    "imagemagick.version" = wsi_first_available_version(
      wsi_command_version("magick"),
      wsi_optional_package_version("magick")
    )
  )
  wsi_make_slide(
    path = path,
    backend = "imagemagick",
    dimensions = c(width = width, height = height),
    levels = levels,
    properties = properties,
    metadata = list(vendor = "imagemagick"),
    associated_images = character()
  )
}

wsi_imagemagick_thumbnail_file <- function(slide, output, width, height = NULL) {
  if (wsi_imagemagick_cli()) {
    geometry <- if (is.null(height)) {
      sprintf("%dx", as.integer(width))
    } else {
      sprintf("%dx%d>", as.integer(width), as.integer(height))
    }
    wsi_run_command(
      "magick",
      args = c(wsi_imagemagick_input(slide$path), "-auto-orient", "-thumbnail", geometry, output),
      error_message = "ImageMagick failed to create a viewer thumbnail."
    )
    return(invisible(output))
  }

  wsi_require_magick("create an ImageMagick fallback thumbnail")
  image <- magick::image_read(slide$path)
  geometry <- if (is.null(height)) sprintf("%dx", as.integer(width)) else sprintf("%dx%d>", as.integer(width), as.integer(height))
  image <- magick::image_resize(image, geometry)
  magick::image_write(image, output, format = "png")
  invisible(output)
}

wsi_imagemagick_read_region_file <- function(slide, region, output) {
  if (region$level != 0L) {
    wsi_abort("The ImageMagick fallback backend only exposes level 0; install libvips or OpenSlide for pyramid-aware region reads.")
  }
  geometry <- sprintf(
    "%dx%d+%d+%d",
    as.integer(region$width),
    as.integer(region$height),
    as.integer(floor(region$x)),
    as.integer(floor(region$y))
  )
  if (wsi_imagemagick_cli()) {
    wsi_run_command(
      "magick",
      args = c(wsi_imagemagick_input(slide$path), "-auto-orient", "-crop", geometry, "+repage", output),
      error_message = "ImageMagick failed to crop the requested region."
    )
    return(invisible(output))
  }

  wsi_require_magick("crop a region with the ImageMagick fallback backend")
  image <- magick::image_read(slide$path)
  image <- magick::image_crop(image, geometry)
  magick::image_write(image, output)
  invisible(output)
}
