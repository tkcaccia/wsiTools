wsi_region_to_file <- function(slide, region, output, backend = c("auto", "vips", "openslide", "bioformats", "imagemagick")) {
  backend <- wsi_choose_region_backend(slide, backend)

  if (identical(slide$backend, "mock")) {
    wsi_abort("Mock slides do not support file-backed region export.")
  }

  if (backend == "vips") {
    return(wsi_vips_read_region_file(slide, region, output))
  }
  if (backend == "openslide") {
    return(wsi_openslide_read_region_file(slide, region, output))
  }
  if (backend == "bioformats") {
    return(wsi_bioformats_read_region_file(slide, region, output))
  }
  if (backend == "imagemagick") {
    return(wsi_imagemagick_read_region_file(slide, region, output))
  }

  wsi_abort(sprintf("Backend `%s` does not yet support region reading.", backend))
}

#' Read a rectangular slide region
#'
#' Coordinates are level-0 coordinates. `width` and `height` are output pixels at
#' the requested pyramid `level`, matching OpenSlide's region-read convention.
#' Only the requested region is read.
#'
#' @param slide A `wsi_slide` object.
#' @param x,y Level-0 coordinate of the top-left corner.
#' @param width,height Region size in pixels at `level`.
#' @param level Pyramid level.
#' @param format Return format.
#'
#' @return An array, raster, magick image, or native region-file object.
#' @export
wsi_read_region <- function(slide, x, y, width, height, level = 0,
                            format = c("array", "raster", "magick", "native")) {
  format <- match.arg(format)
  region <- wsi_validate_region(slide, x, y, width, height, level)

  if (identical(slide$backend, "mock")) {
    array <- wsi_blank_array(region$width, region$height, colour = c(0.95, 0.95, 0.95, 1))
    if (format == "array") {
      return(array)
    }
    if (format == "raster") {
      return(wsi_array_to_raster(array))
    }
    if (format == "magick") {
      wsi_require_magick("create a mock magick image")
      return(magick::image_blank(region$width, region$height, color = "grey95"))
    }
    return(structure(list(array = array, format = "mock-array"), class = "wsi_region_file"))
  }

  tmp <- tempfile(fileext = ".png")
  if (format != "native") {
    on.exit(unlink(tmp), add = TRUE)
  }
  wsi_region_to_file(slide, region, tmp, backend = "auto")
  wsi_read_image_file(tmp, format)
}

#' Read or export a grid of regions
#'
#' @param slide A `wsi_slide` object.
#' @param grid A data frame with `x`, `y`, `width`, and `height` columns.
#' @param level Default pyramid level when `grid$level` is absent.
#' @param output_dir Optional directory for image output.
#' @param format Output image format, or `"array"` when `output_dir = NULL`.
#' @param overwrite Whether to overwrite existing files.
#'
#' @return A list of arrays when `output_dir = NULL`; otherwise a tile manifest
#'   data frame.
#' @export
wsi_read_region_grid <- function(slide, grid, level = 0, output_dir = NULL,
                                 format = c("png", "jpeg", "tiff", "array"),
                                 overwrite = FALSE) {
  wsi_check_slide(slide)
  format <- match.arg(format)
  needed <- c("x", "y", "width", "height")
  if (!is.data.frame(grid) || !all(needed %in% names(grid))) {
    wsi_abort("`grid` must be a data frame with `x`, `y`, `width`, and `height` columns.")
  }
  if (!"level" %in% names(grid)) {
    grid$level <- level
  }

  if (is.null(output_dir)) {
    if (format != "array") {
      wsi_abort("When `output_dir = NULL`, `format` must be `\"array\"`.")
    }
    return(lapply(seq_len(nrow(grid)), function(i) {
      wsi_read_region(
        slide,
        x = grid$x[[i]],
        y = grid$y[[i]],
        width = grid$width[[i]],
        height = grid$height[[i]],
        level = grid$level[[i]],
        format = "array"
      )
    }))
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  files <- character(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    suggestion <- if ("output_file" %in% names(grid)) grid$output_file[[i]] else sprintf("tile_%05d.%s", i, wsi_format_extension(format))
    stem <- tools::file_path_sans_ext(basename(suggestion))
    file <- file.path(output_dir, sprintf("%s.%s", stem, wsi_format_extension(format)))
    wsi_export_region(
      slide,
      x = grid$x[[i]],
      y = grid$y[[i]],
      width = grid$width[[i]],
      height = grid$height[[i]],
      level = grid$level[[i]],
      output = file,
      format = format,
      overwrite = overwrite
    )
    files[[i]] <- file
  }

  manifest <- data.frame(
    tile_id = if ("tile_id" %in% names(grid)) grid$tile_id else seq_len(nrow(grid)),
    file = files,
    x = grid$x,
    y = grid$y,
    width = grid$width,
    height = grid$height,
    level = grid$level,
    row = grid$row %||% NA_integer_,
    col = grid$col %||% NA_integer_,
    tissue_fraction = grid$tissue_fraction %||% NA_real_,
    roi_id = grid$roi_id %||% NA_character_,
    stringsAsFactors = FALSE
  )
  extra_columns <- setdiff(names(grid), c(names(manifest), "output_file"))
  for (column in extra_columns) {
    manifest[[column]] <- grid[[column]]
  }
  class(manifest) <- c("wsi_tile_manifest", class(manifest))
  manifest
}

#' Export a single region
#'
#' @param slide A `wsi_slide` object.
#' @param x,y,width,height,level Region coordinates; see [wsi_read_region()].
#' @param output Output file path.
#' @param format Output format.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return The output path, invisibly.
#' @export
wsi_export_region <- function(slide, x, y, width, height, level = 0, output,
                              format = c("png", "jpeg", "tiff"), overwrite = FALSE) {
  format <- match.arg(format)
  wsi_crop(
    slide,
    x = x,
    y = y,
    width = width,
    height = height,
    level = level,
    output = output,
    format = format,
    overwrite = overwrite
  )
  invisible(output)
}

#' Export tiles from a grid
#'
#' @param slide A `wsi_slide` object.
#' @param grid Tile grid from [wsi_tile_grid()].
#' @param output_dir Output directory.
#' @param format Output format.
#' @param overwrite Whether to overwrite existing files.
#'
#' @return A tile manifest data frame.
#' @export
wsi_export_tiles <- function(slide, grid, output_dir, format = c("png", "jpeg", "tiff"),
                             overwrite = FALSE) {
  format <- match.arg(format)
  wsi_read_region_grid(slide, grid, output_dir = output_dir, format = format, overwrite = overwrite)
}
