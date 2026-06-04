wsi_vips_field <- function(path, field) {
  if (!wsi_has_vips()) {
    return(NA_character_)
  }
  out <- tryCatch(
    suppressWarnings(system2("vipsheader", args = wsi_system2_args(c("-f", field, path)), stdout = TRUE, stderr = TRUE)),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L) || length(out) == 0L) {
    return(NA_character_)
  }
  out <- trimws(out)
  out <- out[nzchar(out)]
  out <- out[!grepl("VIPS-WARNING", out, fixed = TRUE)]
  if (!length(out)) {
    return(NA_character_)
  }
  out[[length(out)]]
}

wsi_vips_properties <- function(path) {
  out <- wsi_run_command(
    "vipsheader",
    args = c("-a", path),
    error_message = sprintf("libvips could not read metadata from `%s`.", path)
  )
  out <- out[!grepl("VIPS-WARNING", out, fixed = TRUE)]
  wsi_parse_key_value(out)
}

wsi_vips_open <- function(path) {
  if (!wsi_has_vips()) {
    wsi_abort(
      wsi_backend_action_message(
        "libvips could not open the image because the libvips backend is not installed.",
        backend = "vips"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  width <- suppressWarnings(as.numeric(wsi_vips_field(path, "width")))
  height <- suppressWarnings(as.numeric(wsi_vips_field(path, "height")))
  if (is.na(width) || is.na(height)) {
    wsi_abort("libvips did not report image dimensions for this file.")
  }

  properties <- wsi_vips_properties(path)
  levels <- wsi_properties_to_levels(properties)
  if (nrow(levels) == 0L || is.na(levels$width[[1L]]) || is.na(levels$height[[1L]])) {
    levels <- data.frame(level = 0L, width = width, height = height, downsample = 1, stringsAsFactors = FALSE)
  }

  metadata <- list(
    vendor = properties[["openslide.vendor"]] %||% NA_character_,
    backend_version = wsi_command_version("vips")
  )

  wsi_make_slide(
    path = path,
    backend = "vips",
    dimensions = c(width = width, height = height),
    levels = levels,
    properties = properties,
    metadata = metadata,
    associated_images = wsi_openslide_associated_images(properties)
  )
}

wsi_vips_input_for_level <- function(path, level) {
  if (level == 0L) {
    return(path)
  }
  paste0(path, "[level=", level, "]")
}

wsi_vips_read_region_file <- function(slide, region, output) {
  if (!wsi_has_vips()) {
    wsi_abort(
      wsi_backend_action_message(
        "libvips region export could not start because the libvips backend is not installed.",
        backend = "vips"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  input <- wsi_vips_input_for_level(slide$path, region$level)
  crop_x <- as.integer(floor(region$x / region$downsample))
  crop_y <- as.integer(floor(region$y / region$downsample))
  args <- c(
    "crop",
    input,
    output,
    as.character(crop_x),
    as.character(crop_y),
    as.character(region$width),
    as.character(region$height)
  )
  wsi_run_command("vips", args = args, error_message = "libvips failed to crop the requested region.")
  invisible(output)
}

wsi_vips_tiff_target <- function(output, tile_size = 512, compression = "lzw",
                                 pyramid = TRUE, bigtiff = TRUE, ome = FALSE,
                                 subifd = ome, properties = ome,
                                 depth = "onepixel", region_shrink = "mean",
                                 predictor = NULL, quality = NULL,
                                 compression_level = NULL) {
  options <- c(
    "tile",
    sprintf("tile-width=%d", as.integer(tile_size)),
    sprintf("tile-height=%d", as.integer(tile_size))
  )
  if (isTRUE(pyramid)) {
    options <- c(options, "pyramid")
  }
  if (isTRUE(bigtiff)) {
    options <- c(options, "bigtiff")
  }
  if (isTRUE(ome)) {
    options <- c(options, "subifd")
  } else if (isTRUE(subifd)) {
    options <- c(options, "subifd")
  }
  if (isTRUE(properties)) {
    options <- c(options, "properties")
  }
  if (!identical(compression, "none")) {
    options <- c(options, sprintf("compression=%s", compression))
  } else {
    options <- c(options, "compression=none")
  }
  if (isTRUE(pyramid) && !is.null(depth)) {
    options <- c(options, sprintf("depth=%s", depth))
  }
  if (isTRUE(pyramid) && !is.null(region_shrink)) {
    options <- c(options, sprintf("region-shrink=%s", region_shrink))
  }
  if (!is.null(predictor)) {
    options <- c(options, sprintf("predictor=%s", predictor))
  }
  if (!is.null(quality)) {
    options <- c(options, sprintf("Q=%d", as.integer(quality)))
  }
  if (!is.null(compression_level)) {
    options <- c(options, sprintf("level=%d", as.integer(compression_level)))
  }
  sprintf("%s[%s]", output, paste(options, collapse = ","))
}

wsi_vips_copy <- function(input, output_target) {
  wsi_run_command(
    "vips",
    args = c("copy", input, output_target),
    error_message = "libvips conversion failed."
  )
  invisible(output_target)
}
