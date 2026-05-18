#' @noRd
wsi_normalize_stain_vector <- function(x, name) {
  if (!is.numeric(x) || length(x) != 3L || anyNA(x) || any(!is.finite(x))) {
    wsi_abort(sprintf("`%s` must be a numeric RGB optical-density vector of length 3.", name))
  }
  norm <- sqrt(sum(x^2))
  if (norm <= 0) {
    wsi_abort(sprintf("`%s` must not be the zero vector.", name))
  }
  unname(as.numeric(x) / norm)
}

wsi_colour_to_hex <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    wsi_abort(sprintf("`%s` must be a single R colour value.", name))
  }
  rgb <- tryCatch(
    grDevices::col2rgb(x),
    error = function(err) {
      wsi_abort(sprintf("`%s` is not a valid R colour value.", name))
    }
  )
  grDevices::rgb(rgb[1L, 1L], rgb[2L, 1L], rgb[3L, 1L], maxColorValue = 255)
}

wsi_image_to_array <- function(image) {
  if (inherits(image, "magick-image")) {
    return(wsi_magick_to_array(image))
  }

  if (inherits(image, "raster")) {
    dims <- dim(image)
    rgba <- grDevices::col2rgb(as.vector(image), alpha = TRUE) / 255
    arr <- array(aperm(array(rgba, dim = c(4L, dims[1L], dims[2L])), c(2L, 3L, 1L)),
                 dim = c(dims[1L], dims[2L], 4L))
    return(arr)
  }

  dims <- dim(image)
  if (!is.array(image) || length(dims) != 3L || dims[[3L]] < 3L) {
    wsi_abort("`image` must be an RGB/RGBA array, raster, or magick image.")
  }
  arr <- image
  storage.mode(arr) <- "double"
  if (max(arr, na.rm = TRUE) > 1) {
    arr <- arr / 255
  }
  pmin(pmax(arr, 0), 1)
}

wsi_ihc_stain_matrix <- function(hematoxylin = c(0.650, 0.704, 0.286),
                                 hrp = c(0.268, 0.570, 0.776)) {
  hematoxylin <- wsi_normalize_stain_vector(hematoxylin, "hematoxylin")
  hrp <- wsi_normalize_stain_vector(hrp, "hrp")
  residual <- c(
    hematoxylin[[2L]] * hrp[[3L]] - hematoxylin[[3L]] * hrp[[2L]],
    hematoxylin[[3L]] * hrp[[1L]] - hematoxylin[[1L]] * hrp[[3L]],
    hematoxylin[[1L]] * hrp[[2L]] - hematoxylin[[2L]] * hrp[[1L]]
  )
  residual <- wsi_normalize_stain_vector(residual, "residual")
  cbind(hematoxylin = hematoxylin, hrp = hrp, residual = residual)
}

wsi_recolour_ihc <- function(channels, hematoxylin_colour = "#4b3f99",
                             hrp_colour = "#8b5a2b",
                             hematoxylin_strength = 1,
                             hrp_strength = 1) {
  hematoxylin_colour <- grDevices::col2rgb(wsi_colour_to_hex(hematoxylin_colour, "hematoxylin_colour"))[, 1L] / 255
  hrp_colour <- grDevices::col2rgb(wsi_colour_to_hex(hrp_colour, "hrp_colour"))[, 1L] / 255
  hematoxylin_strength <- wsi_check_scalar_number(hematoxylin_strength, "hematoxylin_strength")
  hrp_strength <- wsi_check_scalar_number(hrp_strength, "hrp_strength")

  h <- channels$hematoxylin
  d <- channels$hrp
  height <- nrow(h)
  width <- ncol(h)
  th <- pmin(1, pmax(0, 1 - exp(-h * hematoxylin_strength)))
  td <- pmin(1, pmax(0, 1 - exp(-d * hrp_strength)))

  out <- array(1, dim = c(height, width, 4L))
  for (channel in seq_len(3L)) {
    out[, , channel] <- out[, , channel] * (1 - th) + hematoxylin_colour[[channel]] * th
    out[, , channel] <- out[, , channel] * (1 - td) + hrp_colour[[channel]] * td
  }
  if (!is.null(channels$alpha)) {
    out[, , 4L] <- channels$alpha
  }
  out
}

wsi_format_ihc_output <- function(channels, format, hematoxylin_colour, hrp_colour,
                                  hematoxylin_strength, hrp_strength) {
  if (identical(format, "channels")) {
    return(channels)
  }

  image <- wsi_recolour_ihc(
    channels,
    hematoxylin_colour = hematoxylin_colour,
    hrp_colour = hrp_colour,
    hematoxylin_strength = hematoxylin_strength,
    hrp_strength = hrp_strength
  )
  if (identical(format, "array")) {
    return(image)
  }
  raster <- wsi_array_to_raster(image)
  if (identical(format, "raster")) {
    return(raster)
  }
  wsi_require_magick("return a deconvolved IHC image as a magick object")
  magick::image_read(raster)
}

#' Deconvolve hematoxylin and HRP/DAB stains from an IHC image
#'
#' Performs color deconvolution on an already-small image object such as a
#' thumbnail or a region read with [wsi_read_region()]. The implementation uses
#' the standard H-DAB optical-density vectors by default and returns separate
#' hematoxylin and HRP/DAB concentration channels, or a recolored pseudo-image.
#'
#' This function is intended for patches, regions, and thumbnails. It does not
#' read a whole-slide image into memory.
#'
#' @param image RGB/RGBA array, raster object, or magick image.
#' @param format Output format. `"channels"` returns numeric concentration
#'   matrices for `hematoxylin` and `hrp`; image formats return a recolored
#'   two-channel visualization.
#' @param hematoxylin,hrp RGB optical-density vectors for hematoxylin and
#'   HRP/DAB. Defaults are conventional H-DAB stain vectors.
#' @param hematoxylin_colour,hrp_colour Display colours for recolored outputs.
#' @param hematoxylin_strength,hrp_strength Display gains for recolored outputs.
#' @param epsilon Lower bound used before taking optical-density logarithms.
#'
#' @return A `wsi_ihc_channels` object, array, raster, or magick image.
#' @export
#' @examples
#' patch <- array(0.8, dim = c(32, 32, 3))
#' channels <- wsi_deconvolve_ihc(patch)
wsi_deconvolve_ihc <- function(image,
                               format = c("channels", "array", "raster", "magick"),
                               hematoxylin = c(0.650, 0.704, 0.286),
                               hrp = c(0.268, 0.570, 0.776),
                               hematoxylin_colour = "#4b3f99",
                               hrp_colour = "#8b5a2b",
                               hematoxylin_strength = 1,
                               hrp_strength = 1,
                               epsilon = 1 / 255) {
  format <- match.arg(format)
  epsilon <- wsi_check_scalar_number(epsilon, "epsilon", allow_zero = FALSE)
  if (epsilon >= 1) {
    wsi_abort("`epsilon` must be less than 1.")
  }

  arr <- wsi_image_to_array(image)
  dims <- dim(arr)
  rgb <- pmax(arr[, , seq_len(3L), drop = FALSE], epsilon)
  od <- -log(rgb)
  od_mat <- cbind(as.vector(od[, , 1L]), as.vector(od[, , 2L]), as.vector(od[, , 3L]))
  stain_matrix <- wsi_ihc_stain_matrix(hematoxylin = hematoxylin, hrp = hrp)
  inverse <- tryCatch(
    solve(stain_matrix),
    error = function(err) {
      wsi_abort("The supplied stain vectors are not linearly independent.")
    }
  )
  concentration <- od_mat %*% t(inverse)

  channels <- structure(
    list(
      hematoxylin = matrix(pmax(0, concentration[, 1L]), nrow = dims[[1L]], ncol = dims[[2L]]),
      hrp = matrix(pmax(0, concentration[, 2L]), nrow = dims[[1L]], ncol = dims[[2L]]),
      residual = matrix(pmax(0, concentration[, 3L]), nrow = dims[[1L]], ncol = dims[[2L]]),
      alpha = if (dims[[3L]] >= 4L) arr[, , 4L] else NULL,
      stain_matrix = stain_matrix
    ),
    class = "wsi_ihc_channels"
  )

  wsi_format_ihc_output(
    channels,
    format = format,
    hematoxylin_colour = hematoxylin_colour,
    hrp_colour = hrp_colour,
    hematoxylin_strength = hematoxylin_strength,
    hrp_strength = hrp_strength
  )
}

#' Deconvolve hematoxylin and HRP/DAB stains from a slide region
#'
#' Reads only the requested region, then applies [wsi_deconvolve_ihc()]. This is
#' the preferred R-side workflow for WSI patches because the full slide is never
#' loaded into memory.
#'
#' @param slide A `wsi_slide` object.
#' @param x,y,width,height,level Region coordinates; see [wsi_read_region()].
#' @param ... Arguments passed to [wsi_deconvolve_ihc()].
#'
#' @return See [wsi_deconvolve_ihc()].
#' @export
wsi_deconvolve_region <- function(slide, x, y, width, height, level = 0, ...) {
  patch <- wsi_read_region(
    slide,
    x = x,
    y = y,
    width = width,
    height = height,
    level = level,
    format = "array"
  )
  wsi_deconvolve_ihc(patch, ...)
}

wsi_ihc_stain_config <- function(stain = c("none", "ihc"),
                                 hematoxylin = c(0.650, 0.704, 0.286),
                                 hrp = c(0.268, 0.570, 0.776),
                                 hematoxylin_colour = "#4b3f99",
                                 hrp_colour = "#8b5a2b",
                                 hematoxylin_strength = 1,
                                 hrp_strength = 1) {
  stain <- match.arg(stain)
  if (identical(stain, "none")) {
    return(list(enabled = FALSE))
  }
  hematoxylin_strength <- wsi_check_scalar_number(hematoxylin_strength, "hematoxylin_strength")
  hrp_strength <- wsi_check_scalar_number(hrp_strength, "hrp_strength")
  list(
    enabled = TRUE,
    type = "H-DAB",
    hematoxylin = wsi_normalize_stain_vector(hematoxylin, "hematoxylin"),
    hrp = wsi_normalize_stain_vector(hrp, "hrp"),
    hematoxylin_colour = wsi_colour_to_hex(hematoxylin_colour, "hematoxylin_colour"),
    hrp_colour = wsi_colour_to_hex(hrp_colour, "hrp_colour"),
    hematoxylin_strength = hematoxylin_strength,
    hrp_strength = hrp_strength
  )
}

#' View an IHC slide with interactive hematoxylin and HRP/DAB deconvolution
#'
#' Writes an HTML viewer with H-DAB color deconvolution controls. The viewer can
#' show the original image or a two-channel recolored display, with separate
#' toggles, colours, and gain sliders for hematoxylin and HRP/DAB. In tiled
#' mode, the browser recolors only the visible Deep Zoom tiles, so the full WSI
#' is not loaded into R memory.
#'
#' @param slide A `wsi_slide` object.
#' @param mode Viewer mode passed to [wsi_viewer()]. `"tiles"` is recommended
#'   for full-resolution inspection.
#' @param hematoxylin,hrp RGB optical-density vectors.
#' @param hematoxylin_colour,hrp_colour Initial display colours.
#' @param hematoxylin_strength,hrp_strength Initial display gains.
#' @param ... Additional arguments passed to [wsi_viewer()], such as `output`,
#'   `tile_dir`, `open`, `roi`, or `overwrite`.
#'
#' @return The HTML viewer path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' html <- wsi_viewer_ihc(slide, output = "ihc_viewer.html", open = FALSE)
#' wsi_close(slide)
#' }
wsi_viewer_ihc <- function(slide, mode = c("tiles", "thumbnail"),
                           hematoxylin = c(0.650, 0.704, 0.286),
                           hrp = c(0.268, 0.570, 0.776),
                           hematoxylin_colour = "#4b3f99",
                           hrp_colour = "#8b5a2b",
                           hematoxylin_strength = 1,
                           hrp_strength = 1,
                           ...) {
  mode <- match.arg(mode)
  wsi_viewer(
    slide,
    mode = mode,
    stain = "ihc",
    hematoxylin = hematoxylin,
    hrp = hrp,
    hematoxylin_colour = hematoxylin_colour,
    hrp_colour = hrp_colour,
    hematoxylin_strength = hematoxylin_strength,
    hrp_strength = hrp_strength,
    ...
  )
}

#' @export
print.wsi_ihc_channels <- function(x, ...) {
  dims <- dim(x$hematoxylin)
  cat("<wsi_ihc_channels>\n")
  cat("  size: ", dims[[2L]], " x ", dims[[1L]], " px\n", sep = "")
  cat("  channels: hematoxylin, hrp, residual\n")
  invisible(x)
}
