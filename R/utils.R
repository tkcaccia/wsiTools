`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

.wsi_native_cache <- new.env(parent = emptyenv())

wsi_native_available <- function(name) {
  cached <- .wsi_native_cache[[name]]
  if (!is.null(cached)) {
    return(isTRUE(cached))
  }
  available <- tryCatch(
    {
      getNativeSymbolInfo(name, PACKAGE = "wsiTools")
      TRUE
    },
    error = function(err) FALSE
  )
  .wsi_native_cache[[name]] <- available
  available
}

wsi_abort <- function(message, class = "wsi_error", call = NULL) {
  cli::cli_abort(message, class = class, call = call)
}

wsi_warn <- function(message, class = "wsi_warning", call = NULL) {
  cli::cli_warn(message, class = class, call = call)
}

wsi_command_exists <- function(command) {
  if (!is.character(command) || length(command) != 1L || is.na(command) || !nzchar(command)) {
    return(FALSE)
  }
  if (file.exists(command)) {
    return(file.access(command, mode = 1L) == 0L)
  }
  nzchar(Sys.which(command))
}

wsi_system2_args <- function(args) {
  if (!length(args)) {
    return(character())
  }
  shQuote(as.character(args))
}

wsi_run_command <- function(command, args = character(), error_message = NULL) {
  if (!wsi_command_exists(command)) {
    wsi_abort(sprintf("Required command-line tool `%s` is not installed or not on PATH.", command))
  }

  output <- tryCatch(
    suppressWarnings(system2(command, args = wsi_system2_args(args), stdout = TRUE, stderr = TRUE)),
    error = function(err) {
      wsi_abort(
        sprintf(
          "%s\nCommand failed before completion: %s",
          error_message %||% sprintf("Command `%s` failed.", command),
          conditionMessage(err)
        )
      )
    }
  )

  status <- attr(output, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L)) {
    details <- paste(output, collapse = "\n")
    wsi_abort(
      paste0(
        error_message %||% sprintf("Command `%s` failed.", command),
        if (nzchar(details)) paste0("\n", details) else ""
      )
    )
  }

  output
}

wsi_command_version <- function(command, args = "--version") {
  if (!wsi_command_exists(command)) {
    return(NA_character_)
  }
  out <- tryCatch(
    suppressWarnings(system2(command, args = wsi_system2_args(args), stdout = TRUE, stderr = TRUE)),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L) || length(out) == 0) {
    return(NA_character_)
  }
  out <- trimws(out)
  out <- out[nzchar(out)]
  out <- out[!grepl("WARNING|VIPS-WARNING", out)]
  if (!length(out)) {
    return(NA_character_)
  }
  out[[1L]]
}

wsi_parse_key_value <- function(lines) {
  lines <- lines[nzchar(lines)]
  pos <- regexpr(":\\s*", lines)
  keep <- pos > 1
  lines <- lines[keep]
  pos <- pos[keep]

  keys <- trimws(substr(lines, 1L, pos - 1L))
  values <- trimws(sub("^[^:]+:\\s*", "", lines))
  stats::setNames(as.list(values), keys)
}

wsi_validate_input_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    wsi_abort("`path` must be a single non-empty file path.")
  }
  if (!file.exists(path)) {
    wsi_abort(sprintf("Input file does not exist: %s", path), class = "wsi_file_not_found")
  }
  normalizePath(path, mustWork = TRUE)
}

wsi_validate_output_path <- function(output, overwrite = FALSE) {
  if (!is.character(output) || length(output) != 1L || is.na(output) || !nzchar(output)) {
    wsi_abort("`output` must be a single non-empty file path.")
  }
  if (file.exists(output) && !isTRUE(overwrite)) {
    wsi_abort(
      sprintf("Output file already exists and `overwrite = FALSE`: %s", output),
      class = "wsi_output_exists"
    )
  }
  parent <- dirname(output)
  if (!dir.exists(parent)) {
    wsi_abort(sprintf("Output directory does not exist: %s", parent))
  }
  output
}

wsi_check_slide <- function(slide) {
  if (!inherits(slide, "wsi_slide")) {
    wsi_abort("`slide` must be a `wsi_slide` object created by `wsi_open()`.")
  }
  if (isTRUE(slide$closed)) {
    wsi_abort("This slide has been closed.")
  }
  invisible(slide)
}

wsi_check_scalar_number <- function(x, name, allow_zero = TRUE) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x)) {
    wsi_abort(sprintf("`%s` must be a single finite number.", name))
  }
  if (allow_zero) {
    if (x < 0) {
      wsi_abort(sprintf("`%s` must be greater than or equal to zero.", name))
    }
  } else if (x <= 0) {
    wsi_abort(sprintf("`%s` must be greater than zero.", name))
  }
  x
}

wsi_level_row <- function(slide, level = 0) {
  levels <- wsi_levels(slide)
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level %% 1 != 0) {
    wsi_abort("`level` must be a single integer pyramid level.")
  }
  idx <- match(as.integer(level), levels$level)
  if (is.na(idx)) {
    wsi_abort(sprintf("Pyramid level %s is not available for this slide.", level))
  }
  levels[idx, , drop = FALSE]
}

wsi_validate_region <- function(slide, x, y, width, height, level = 0) {
  wsi_check_slide(slide)
  level_info <- wsi_level_row(slide, level)
  x <- wsi_check_scalar_number(x, "x")
  y <- wsi_check_scalar_number(y, "y")
  width <- wsi_check_scalar_number(width, "width", allow_zero = FALSE)
  height <- wsi_check_scalar_number(height, "height", allow_zero = FALSE)

  downsample <- level_info$downsample[[1]]
  coverage_width <- width * downsample
  coverage_height <- height * downsample
  slide_width <- slide$dimensions[["width"]]
  slide_height <- slide$dimensions[["height"]]

  if (x + coverage_width > slide_width + 1e-6 || y + coverage_height > slide_height + 1e-6) {
    wsi_abort(
      "Requested region is outside slide bounds. Coordinates are level-0 coordinates; width and height are pixels at the requested level.",
      class = "wsi_region_out_of_bounds"
    )
  }

  list(
    x = as.integer(round(x)),
    y = as.integer(round(y)),
    width = as.integer(round(width)),
    height = as.integer(round(height)),
    level = as.integer(level),
    downsample = downsample
  )
}

wsi_array_to_raster <- function(array) {
  dims <- dim(array)
  if (length(dims) != 3L || dims[[3]] < 3L) {
    wsi_abort("Image array must have dimensions height x width x channels with at least RGB channels.")
  }
  arr <- array
  if (max(arr, na.rm = TRUE) > 1) {
    arr <- arr / 255
  }
  alpha <- if (dims[[3]] >= 4L) arr[, , 4L] else 1
  colours <- grDevices::rgb(arr[, , 1L], arr[, , 2L], arr[, , 3L], alpha = alpha)
  dim(colours) <- dims[1:2]
  grDevices::as.raster(colours)
}

wsi_blank_array <- function(width, height, colour = c(1, 1, 1, 1)) {
  array(rep(colour, each = width * height), dim = c(height, width, length(colour)))
}

wsi_require_magick <- function(reason = "read image data into R") {
  if (!requireNamespace("magick", quietly = TRUE)) {
    wsi_abort(
      sprintf("The optional package `magick` is required to %s. Install it or request a file output instead.", reason),
      class = "wsi_missing_dependency"
    )
  }
  invisible(TRUE)
}

wsi_magick_to_array <- function(image) {
  wsi_require_magick("convert image data to an R array")
  raw <- magick::image_data(image, channels = "rgba")
  dims <- dim(raw)
  arr <- as.integer(as.vector(raw))
  dim(arr) <- dims
  if (length(dims) != 3L) {
    wsi_abort("Could not convert magick image data to an RGB/RGBA array.")
  }
  if (dims[[1L]] %in% 3:4) {
    # magick commonly returns channel x width x height.
    arr <- aperm(arr, c(3L, 2L, 1L))
  } else if (dims[[3L]] %in% 3:4) {
    # Some magick/ImageMagick builds return width x height x channel.
    arr <- aperm(arr, c(2L, 1L, 3L))
  } else {
    wsi_abort("Could not identify the channel dimension in magick image data.")
  }
  arr / 255
}

wsi_read_image_file <- function(path, format = c("array", "raster", "magick", "native")) {
  format <- match.arg(format)
  if (format == "native") {
    return(structure(list(path = path, format = tools::file_ext(path)), class = "wsi_region_file"))
  }

  wsi_require_magick("read backend image output into R")
  image <- magick::image_read(path)
  if (format == "magick") {
    return(image)
  }
  array <- wsi_magick_to_array(image)
  if (format == "array") {
    return(array)
  }
  wsi_array_to_raster(array)
}

wsi_normalize_region <- function(region, slide) {
  if (is.null(region)) {
    return(c(
      x = 0,
      y = 0,
      width = unname(slide$dimensions[["width"]]),
      height = unname(slide$dimensions[["height"]])
    ))
  }

  if (is.data.frame(region)) {
    region <- region[1L, , drop = FALSE]
    region <- unlist(region[c("x", "y", "width", "height")], use.names = TRUE)
  }

  if (is.list(region) && !is.data.frame(region)) {
    region <- unlist(region[c("x", "y", "width", "height")], use.names = TRUE)
  }

  needed <- c("x", "y", "width", "height")
  if (!is.numeric(region) || !all(needed %in% names(region))) {
    wsi_abort("`region` must provide numeric `x`, `y`, `width`, and `height` values in level-0 coordinates.")
  }

  region <- region[needed]
  x <- wsi_check_scalar_number(region[["x"]], "region$x")
  y <- wsi_check_scalar_number(region[["y"]], "region$y")
  width <- wsi_check_scalar_number(region[["width"]], "region$width", allow_zero = FALSE)
  height <- wsi_check_scalar_number(region[["height"]], "region$height", allow_zero = FALSE)

  if (x + width > slide$dimensions[["width"]] + 1e-6 || y + height > slide$dimensions[["height"]] + 1e-6) {
    wsi_abort("`region` is outside slide bounds.", class = "wsi_region_out_of_bounds")
  }

  c(x = x, y = y, width = width, height = height)
}

wsi_make_slide <- function(path, backend, dimensions, levels, properties = list(),
                           metadata = list(), associated_images = character(),
                           ptr = NULL) {
  dimensions <- c(width = as.numeric(dimensions[["width"]]), height = as.numeric(dimensions[["height"]]))
  levels <- as.data.frame(levels, stringsAsFactors = FALSE)
  levels$level <- as.integer(levels$level)
  levels$width <- as.numeric(levels$width)
  levels$height <- as.numeric(levels$height)
  levels$downsample <- as.numeric(levels$downsample)

  structure(
    list(
      path = path,
      backend = backend,
      dimensions = dimensions,
      levels = levels,
      properties = properties,
      metadata = metadata,
      associated_images = associated_images,
      ptr = ptr,
      closed = FALSE
    ),
    class = c("wsi_slide", sprintf("wsi_%s_slide", backend))
  )
}

wsi_format_extension <- function(format) {
  switch(
    format,
    "jpeg" = "jpg",
    "ome-tiff" = "ome.tiff",
    "pyramidal-tiff" = "tif",
    "tiff" = "tif",
    format
  )
}
