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

wsi_clean_channel_id <- function(x, fallback) {
  id <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  id <- gsub("^_+|_+$", "", id)
  empty <- is.na(id) | !nzchar(id)
  id[empty] <- fallback[empty]
  make.unique(id, sep = "_")
}

wsi_recycle_to <- function(x, n, name) {
  if (length(x) == n) {
    return(x)
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  wsi_abort(sprintf("`%s` must have length 1 or length %s.", name, n))
}

wsi_stain_palette <- function(n) {
  palette <- c(
    "#4b3f99", "#8b5a2b", "#d73027", "#008fd5", "#2ca25f",
    "#c51b8a", "#fdae61", "#7b3294"
  )
  rep(palette, length.out = n)
}

#' Define stain channels for IHC and multi-IHC color deconvolution
#'
#' Creates channel metadata used by [wsi_deconvolve_multi_ihc()] and by the
#' interactive viewer. Each channel is an RGB optical-density vector with an
#' initial display colour, gain, and visibility state.
#'
#' RGB brightfield deconvolution can resolve at most three independent stain
#' vectors at a time. For assays with more markers, create different channel
#' sets for the combinations you want to inspect.
#'
#' @param name Character vector of channel names.
#' @param vector List of numeric RGB optical-density vectors, one per channel.
#' @param colour Display colours for the channels.
#' @param strength Initial display gain for each channel.
#' @param visible Initial visibility for each channel.
#' @param opacity Initial display opacity for each channel.
#' @param contrast_min,contrast_max Initial concentration display window.
#'
#' @return A `wsi_stain_channels` object.
#' @export
#' @examples
#' channels <- wsi_stain_channels(
#'   name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
#'   vector = list(c(0.650, 0.704, 0.286), c(0.268, 0.570, 0.776),
#'                 c(0.213, 0.851, 0.477)),
#'   colour = c("#4b3f99", "#8b5a2b", "#d73027")
#' )
wsi_stain_channels <- function(name = c("Hematoxylin", "HRP/DAB"),
                               vector = list(c(0.650, 0.704, 0.286),
                                             c(0.268, 0.570, 0.776)),
                               colour = wsi_stain_palette(length(vector)),
                               strength = 1,
                               visible = TRUE,
                               opacity = 1,
                               contrast_min = 0,
                               contrast_max = 1) {
  if (is.matrix(vector) || is.data.frame(vector)) {
    if (ncol(vector) != 3L) {
      wsi_abort("`vector` must have three columns when supplied as a matrix or data frame.")
    }
    vector <- lapply(seq_len(nrow(vector)), function(i) as.numeric(vector[i, ]))
  }
  if (is.numeric(vector) && length(vector) == 3L) {
    vector <- list(vector)
  }
  if (!is.list(vector) || length(vector) == 0L) {
    wsi_abort("`vector` must be a non-empty list of RGB optical-density vectors.")
  }
  if (length(vector) > 3L) {
    wsi_abort("RGB brightfield color deconvolution supports at most three stain channels at a time.")
  }

  n <- length(vector)
  name <- as.character(wsi_recycle_to(name, n, "name"))
  colour <- as.character(wsi_recycle_to(colour, n, "colour"))
  strength <- as.numeric(wsi_recycle_to(strength, n, "strength"))
  visible <- wsi_recycle_to(visible, n, "visible")
  opacity <- as.numeric(wsi_recycle_to(opacity, n, "opacity"))
  contrast_min <- as.numeric(wsi_recycle_to(contrast_min, n, "contrast_min"))
  contrast_max <- as.numeric(wsi_recycle_to(contrast_max, n, "contrast_max"))
  if (!is.logical(visible) || anyNA(visible)) {
    wsi_abort("`visible` must be TRUE or FALSE for each stain channel.")
  }

  ids <- wsi_clean_channel_id(name, paste0("channel_", seq_len(n)))
  channels <- lapply(seq_len(n), function(i) {
    list(
      id = ids[[i]],
      name = name[[i]],
      vector = wsi_normalize_stain_vector(vector[[i]], sprintf("vector[[%s]]", i)),
      colour = wsi_colour_to_hex(colour[[i]], sprintf("colour[[%s]]", i)),
      strength = wsi_check_scalar_number(strength[[i]], sprintf("strength[[%s]]", i)),
      visible = isTRUE(visible[[i]]),
      opacity = wsi_channel_opacity(opacity[[i]]),
      contrast_min = unname(wsi_channel_contrast(contrast_min[[i]], contrast_max[[i]])[["min"]]),
      contrast_max = unname(wsi_channel_contrast(contrast_min[[i]], contrast_max[[i]])[["max"]])
    )
  })
  class(channels) <- "wsi_stain_channels"
  channels
}

wsi_default_multi_ihc_channels <- function() {
  wsi_stain_channels(
    name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
    vector = list(
      c(0.650, 0.704, 0.286),
      c(0.268, 0.570, 0.776),
      c(0.213, 0.851, 0.477)
    ),
    colour = c("#4b3f99", "#8b5a2b", "#d73027"),
    strength = c(1, 1, 1),
    visible = c(TRUE, TRUE, TRUE)
  )
}

wsi_as_stain_channels <- function(channels = NULL) {
  if (is.null(channels)) {
    return(wsi_stain_channels())
  }
  if (inherits(channels, "wsi_stain_channels")) {
    return(channels)
  }
  if (is.data.frame(channels)) {
    name <- channels$name %||% channels$channel %||% rownames(channels)
    vector <- channels$vector %||% channels$od
    if (is.null(vector)) {
      rgb_names <- list(
        c("r", "g", "b"),
        c("red", "green", "blue"),
        c("od_r", "od_g", "od_b")
      )
      for (cols in rgb_names) {
        if (all(cols %in% names(channels))) {
          vector <- as.matrix(channels[, cols, drop = FALSE])
          break
        }
      }
    }
    if (is.null(vector)) {
      wsi_abort("`channels` data frames must include a list column `vector`/`od` or RGB columns.")
    }
    colour <- channels$colour %||% channels$color %||% wsi_stain_palette(nrow(channels))
    strength <- channels$strength %||% channels$gain %||% 1
    visible <- channels$visible %||% TRUE
    opacity <- channels$opacity %||% 1
    contrast_min <- channels$contrast_min %||% 0
    contrast_max <- channels$contrast_max %||% 1
    return(wsi_stain_channels(
      name = name,
      vector = vector,
      colour = colour,
      strength = strength,
      visible = visible,
      opacity = opacity,
      contrast_min = contrast_min,
      contrast_max = contrast_max
    ))
  }
  if (is.list(channels) &&
      all(vapply(channels, function(x) is.numeric(x) && length(x) == 3L, logical(1)))) {
    name <- names(channels)
    if (is.null(name) || any(!nzchar(name))) {
      name <- paste0("Channel ", seq_along(channels))
    }
    return(wsi_stain_channels(name = name, vector = channels))
  }
  if (is.list(channels) &&
      all(vapply(channels, function(x) is.list(x) && !is.null(x$vector), logical(1)))) {
    n <- length(channels)
    channel_names <- names(channels)
    name <- vapply(seq_len(n), function(i) {
      channels[[i]]$name %||%
        if (!is.null(channel_names) && nzchar(channel_names[[i]])) channel_names[[i]] else paste0("Channel ", i)
    }, character(1))
    vector <- lapply(channels, `[[`, "vector")
    colour <- vapply(seq_len(n), function(i) {
      channels[[i]]$colour %||% channels[[i]]$color %||% wsi_stain_palette(n)[[i]]
    }, character(1))
    strength <- vapply(seq_len(n), function(i) {
      channels[[i]]$strength %||% channels[[i]]$gain %||% 1
    }, numeric(1))
    visible <- vapply(seq_len(n), function(i) {
      channels[[i]]$visible %||% TRUE
    }, logical(1))
    opacity <- vapply(seq_len(n), function(i) {
      channels[[i]]$opacity %||% 1
    }, numeric(1))
    contrast_min <- vapply(seq_len(n), function(i) {
      channels[[i]]$contrast_min %||% 0
    }, numeric(1))
    contrast_max <- vapply(seq_len(n), function(i) {
      channels[[i]]$contrast_max %||% 1
    }, numeric(1))
    return(wsi_stain_channels(
      name = name,
      vector = vector,
      colour = colour,
      strength = strength,
      visible = visible,
      opacity = opacity,
      contrast_min = contrast_min,
      contrast_max = contrast_max
    ))
  }
  wsi_abort("`channels` must be created by `wsi_stain_channels()` or supplied as a compatible list/data.frame.")
}

wsi_cross_product <- function(a, b) {
  c(
    a[[2L]] * b[[3L]] - a[[3L]] * b[[2L]],
    a[[3L]] * b[[1L]] - a[[1L]] * b[[3L]],
    a[[1L]] * b[[2L]] - a[[2L]] * b[[1L]]
  )
}

wsi_residual_stain_vector <- function(primary, secondary, name = "residual") {
  primary <- wsi_normalize_stain_vector(primary, "primary")
  secondary <- wsi_normalize_stain_vector(secondary, "secondary")
  wsi_normalize_stain_vector(wsi_cross_product(primary, secondary), name)
}

wsi_complete_stain_basis <- function(channels) {
  vectors <- lapply(channels, `[[`, "vector")
  n <- length(vectors)
  if (n > 3L) {
    wsi_abort("RGB brightfield color deconvolution supports at most three stain channels at a time.")
  }
  if (n == 1L) {
    seed <- if (abs(vectors[[1L]][[1L]]) < 0.9) c(1, 0, 0) else c(0, 1, 0)
    second <- wsi_normalize_stain_vector(wsi_cross_product(vectors[[1L]], seed), "residual_1")
    third <- wsi_normalize_stain_vector(wsi_cross_product(vectors[[1L]], second), "residual_2")
    vectors <- c(vectors, list(second, third))
  } else if (n == 2L) {
    residual <- wsi_normalize_stain_vector(wsi_cross_product(vectors[[1L]], vectors[[2L]]), "residual")
    vectors <- c(vectors, list(residual))
  }

  matrix <- do.call(cbind, vectors)
  if (abs(det(matrix)) < 1e-8) {
    wsi_abort("The supplied stain vectors are not linearly independent.")
  }
  list(
    matrix = matrix,
    basis = lapply(seq_len(3L), function(i) unname(matrix[, i])),
    channel_count = n
  )
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

wsi_format_stain_image <- function(image, format = c("array", "raster", "magick")) {
  format <- match.arg(format)
  if (identical(format, "array")) {
    return(image)
  }
  raster <- wsi_array_to_raster(image)
  if (identical(format, "raster")) {
    return(raster)
  }
  wsi_require_magick("return a stain-normalized image as a magick object")
  magick::image_read(raster)
}

wsi_validate_stain_matrix <- function(matrix, name = "stain_matrix",
                                      min_channels = 2L) {
  if (!is.matrix(matrix) && !is.data.frame(matrix)) {
    wsi_abort(sprintf("`%s` must be a numeric matrix with three RGB rows.", name))
  }
  matrix <- as.matrix(matrix)
  storage.mode(matrix) <- "double"
  if (nrow(matrix) != 3L || ncol(matrix) < min_channels ||
      anyNA(matrix) || any(!is.finite(matrix))) {
    wsi_abort(sprintf(
      "`%s` must be a finite numeric matrix with 3 rows and at least %s columns.",
      name,
      min_channels
    ))
  }
  for (i in seq_len(ncol(matrix))) {
    matrix[, i] <- wsi_normalize_stain_vector(matrix[, i], sprintf("%s[, %s]", name, i))
  }
  matrix
}

#' Define standard H&E stain channels
#'
#' Creates hematoxylin and eosin stain-channel definitions for H&E
#' deconvolution, Macenko-style normalisation, and reconstruction. The default
#' optical-density vectors are common H&E starting values; use slide- or
#' laboratory-specific vectors when quantitative reproducibility matters.
#'
#' @param hematoxylin,eosin RGB optical-density vectors.
#' @param include_residual Include the residual optical-density channel in the
#'   returned channel definitions.
#' @param hematoxylin_colour,eosin_colour Display colours for recoloured
#'   channel visualisation.
#' @param residual_colour Display colour for the residual channel.
#' @param hematoxylin_strength,eosin_strength,residual_strength Initial display
#'   gains for the channels.
#' @param residual_visible Whether the residual channel is visible by default in
#'   the interactive viewer and recoloured image outputs.
#'
#' @return A `wsi_stain_channels` object with hematoxylin, eosin, and optionally
#'   residual channels.
#' @export
wsi_he_stain_channels <- function(hematoxylin = c(0.644, 0.717, 0.267),
                                  eosin = c(0.093, 0.954, 0.283),
                                  include_residual = TRUE,
                                  hematoxylin_colour = "#4b3f99",
                                  eosin_colour = "#e85b90",
                                  residual_colour = "#6b7280",
                                  hematoxylin_strength = 1,
                                  eosin_strength = 1,
                                  residual_strength = 1,
                                  residual_visible = FALSE) {
  hematoxylin <- wsi_normalize_stain_vector(hematoxylin, "hematoxylin")
  eosin <- wsi_normalize_stain_vector(eosin, "eosin")
  names <- c("Hematoxylin", "Eosin")
  vectors <- list(hematoxylin, eosin)
  colours <- c(hematoxylin_colour, eosin_colour)
  strengths <- c(hematoxylin_strength, eosin_strength)
  visible <- c(TRUE, TRUE)
  if (isTRUE(include_residual)) {
    names <- c(names, "Residual")
    vectors <- c(vectors, list(wsi_residual_stain_vector(hematoxylin, eosin, "residual")))
    colours <- c(colours, residual_colour)
    strengths <- c(strengths, residual_strength)
    visible <- c(visible, isTRUE(residual_visible))
  }
  wsi_stain_channels(
    name = names,
    vector = vectors,
    colour = colours,
    strength = strengths,
    visible = visible
  )
}

#' Estimate H&E stain channel definitions from a slide or thumbnail
#'
#' Estimates slide-specific hematoxylin/eosin optical-density vectors from a
#' low-resolution thumbnail or already-small H&E image, then returns channel
#' definitions suitable for [wsi_deconvolve_he()], [wsi_viewer_he()], and live
#' tiled stain-channel viewing. When `x` is a `wsi_slide`, only a thumbnail is
#' read; the level-0 whole-slide image is never loaded into R memory.
#'
#' @param x A `wsi_slide`, slide path, RGB/RGBA array, raster object, or magick
#'   image. Slide inputs are sampled through [wsi_thumbnail()].
#' @param method Stain-vector estimation method. `"macenko"` is usually a good
#'   first choice for H&E. `"vahadane"` is an experimental dependency-free NMF
#'   estimate. `"fixed"` uses the standard H&E vectors without reading a
#'   thumbnail.
#' @param thumbnail_width Width of the low-resolution thumbnail used when `x` is
#'   a slide or path.
#' @param include_residual Include a residual optical-density channel.
#' @param ... Additional arguments passed to [wsi_estimate_stain_matrix()], such
#'   as `luminosity_threshold`, `od_threshold`, or `max_pixels`.
#' @inheritParams wsi_he_stain_channels
#'
#' @return A `wsi_stain_channels` object with slide-specific hematoxylin, eosin,
#'   and optionally residual channel definitions.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("he_sample.svs")
#' channels <- wsi_estimate_he_stain_channels(slide, method = "macenko")
#' html <- wsi_viewer_he(slide, channels = channels, open = FALSE)
#' wsi_close(slide)
#' }
wsi_estimate_he_stain_channels <- function(x,
                                           method = c("macenko", "vahadane", "fixed"),
                                           thumbnail_width = 2048,
                                           include_residual = TRUE,
                                           hematoxylin_colour = "#4b3f99",
                                           eosin_colour = "#e85b90",
                                           residual_colour = "#6b7280",
                                           hematoxylin_strength = 1,
                                           eosin_strength = 1,
                                           residual_strength = 1,
                                           residual_visible = FALSE,
                                           ...) {
  method <- match.arg(method)
  include_residual <- isTRUE(include_residual)
  if (identical(method, "fixed")) {
    return(wsi_he_stain_channels(
      include_residual = include_residual,
      hematoxylin_colour = hematoxylin_colour,
      eosin_colour = eosin_colour,
      residual_colour = residual_colour,
      hematoxylin_strength = hematoxylin_strength,
      eosin_strength = eosin_strength,
      residual_strength = residual_strength,
      residual_visible = residual_visible
    ))
  }

  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  image <- x
  if (is.character(x) && length(x) == 1L) {
    opened <- wsi_open(x, backend = "auto")
    image <- opened
  }
  if (inherits(image, "wsi_slide")) {
    thumbnail_width <- as.integer(wsi_check_scalar_number(
      thumbnail_width,
      "thumbnail_width",
      allow_zero = FALSE
    ))
    image <- wsi_thumbnail(image, width = thumbnail_width, format = "array")
  }

  matrix <- wsi_estimate_stain_matrix(image, method = method, ...)
  wsi_he_stain_channels(
    hematoxylin = matrix[, "hematoxylin"],
    eosin = matrix[, "eosin"],
    include_residual = include_residual,
    hematoxylin_colour = hematoxylin_colour,
    eosin_colour = eosin_colour,
    residual_colour = residual_colour,
    hematoxylin_strength = hematoxylin_strength,
    eosin_strength = eosin_strength,
    residual_strength = residual_strength,
    residual_visible = residual_visible
  )
}

#' @rdname wsi_he_stain_channels
#' @return `wsi_he_stain_matrix()` returns a 3 x 3 matrix with hematoxylin,
#'   eosin, and residual optical-density vectors.
#' @export
wsi_he_stain_matrix <- function(hematoxylin = c(0.644, 0.717, 0.267),
                                eosin = c(0.093, 0.954, 0.283)) {
  channels <- wsi_he_stain_channels(
    hematoxylin = hematoxylin,
    eosin = eosin,
    include_residual = FALSE
  )
  matrix <- wsi_complete_stain_basis(channels)$matrix
  colnames(matrix) <- c("hematoxylin", "eosin", "residual")
  matrix
}

wsi_channels_from_matrix <- function(matrix,
                                     names = c("Hematoxylin", "Eosin"),
                                     colours = c("#4b3f99", "#e85b90"),
                                     strengths = 1,
                                     visible = TRUE) {
  matrix <- wsi_validate_stain_matrix(matrix, "matrix", min_channels = length(names))
  colours <- as.character(wsi_recycle_to(colours, length(names), "colours"))
  strengths <- as.numeric(wsi_recycle_to(strengths, length(names), "strengths"))
  visible <- wsi_recycle_to(visible, length(names), "visible")
  wsi_stain_channels(
    name = names,
    vector = lapply(seq_along(names), function(i) matrix[, i]),
    colour = colours,
    strength = strengths,
    visible = visible
  )
}

wsi_stain_od_pixels <- function(image, epsilon = 1 / 255,
                                luminosity_threshold = 0.8,
                                od_threshold = 0.15,
                                max_pixels = 10000) {
  arr <- wsi_image_to_array(image)
  rgb <- arr[, , seq_len(3L), drop = FALSE]
  rgb_mat <- cbind(as.vector(rgb[, , 1L]), as.vector(rgb[, , 2L]), as.vector(rgb[, , 3L]))
  od <- -log(pmax(rgb_mat, epsilon))
  brightness <- rowMeans(rgb_mat)
  keep <- brightness < luminosity_threshold & rowSums(od) > od_threshold
  od <- od[keep, , drop = FALSE]
  if (!nrow(od)) {
    return(od)
  }
  max_pixels <- as.integer(wsi_check_scalar_number(max_pixels, "max_pixels", allow_zero = FALSE))
  if (nrow(od) > max_pixels) {
    index <- unique(as.integer(round(seq(1, nrow(od), length.out = max_pixels))))
    od <- od[index, , drop = FALSE]
  }
  od
}

wsi_order_he_vectors <- function(vectors, hematoxylin_reference = c(0.644, 0.717, 0.267),
                                 eosin_reference = c(0.093, 0.954, 0.283)) {
  vectors <- wsi_validate_stain_matrix(vectors, "vectors", min_channels = 2L)[, seq_len(2L), drop = FALSE]
  hematoxylin_reference <- wsi_normalize_stain_vector(hematoxylin_reference, "hematoxylin_reference")
  eosin_reference <- wsi_normalize_stain_vector(eosin_reference, "eosin_reference")
  score_direct <- sum(vectors[, 1L] * hematoxylin_reference) + sum(vectors[, 2L] * eosin_reference)
  score_swap <- sum(vectors[, 2L] * hematoxylin_reference) + sum(vectors[, 1L] * eosin_reference)
  if (score_swap > score_direct) {
    vectors <- vectors[, c(2L, 1L), drop = FALSE]
  }
  colnames(vectors) <- c("hematoxylin", "eosin")
  vectors
}

wsi_estimate_macenko_matrix <- function(image, luminosity_threshold = 0.8,
                                        od_threshold = 0.15,
                                        angular_percentile = 1,
                                        max_pixels = 10000,
                                        epsilon = 1 / 255) {
  od <- wsi_stain_od_pixels(
    image,
    epsilon = epsilon,
    luminosity_threshold = luminosity_threshold,
    od_threshold = od_threshold,
    max_pixels = max_pixels
  )
  if (nrow(od) < 3L) {
    wsi_warn("Too few tissue-like pixels for Macenko stain estimation; using standard H&E vectors.")
    return(wsi_he_stain_matrix())
  }

  angular_percentile <- wsi_check_scalar_number(angular_percentile, "angular_percentile")
  if (angular_percentile <= 0 || angular_percentile >= 50) {
    wsi_abort("`angular_percentile` must be greater than 0 and less than 50.")
  }

  sv <- svd(od)
  plane <- sv$v[, seq_len(2L), drop = FALSE]
  projected <- od %*% plane
  angles <- atan2(projected[, 2L], projected[, 1L])
  angle_min <- unname(stats::quantile(angles, probs = angular_percentile / 100, na.rm = TRUE, names = FALSE))
  angle_max <- unname(stats::quantile(angles, probs = 1 - angular_percentile / 100, na.rm = TRUE, names = FALSE))
  vectors <- cbind(
    plane[, 1L] * cos(angle_min) + plane[, 2L] * sin(angle_min),
    plane[, 1L] * cos(angle_max) + plane[, 2L] * sin(angle_max)
  )
  vectors <- abs(vectors)
  vectors <- wsi_order_he_vectors(vectors)
  wsi_he_stain_matrix(hematoxylin = vectors[, 1L], eosin = vectors[, 2L])
}

wsi_estimate_vahadane_matrix <- function(image, luminosity_threshold = 0.8,
                                         od_threshold = 0.15,
                                         max_pixels = 5000,
                                         nmf_iterations = 80,
                                         sparsity = 0.05,
                                         epsilon = 1 / 255) {
  od <- wsi_stain_od_pixels(
    image,
    epsilon = epsilon,
    luminosity_threshold = luminosity_threshold,
    od_threshold = od_threshold,
    max_pixels = max_pixels
  )
  if (nrow(od) < 3L) {
    wsi_warn("Too few tissue-like pixels for Vahadane-style stain estimation; using standard H&E vectors.")
    return(wsi_he_stain_matrix())
  }

  nmf_iterations <- as.integer(wsi_check_scalar_number(nmf_iterations, "nmf_iterations", allow_zero = FALSE))
  sparsity <- wsi_check_scalar_number(sparsity, "sparsity")
  init <- tryCatch(
    t(wsi_estimate_macenko_matrix(
      image,
      luminosity_threshold = luminosity_threshold,
      od_threshold = od_threshold,
      max_pixels = max_pixels,
      epsilon = epsilon
    )[, seq_len(2L), drop = FALSE]),
    error = function(err) t(wsi_he_stain_matrix()[, seq_len(2L), drop = FALSE])
  )
  h <- pmax(init, epsilon)
  w <- pmax(od %*% t(h), epsilon)
  for (i in seq_len(nmf_iterations)) {
    w <- w * ((od %*% t(h)) / pmax(w %*% h %*% t(h) + sparsity, epsilon))
    h <- h * ((t(w) %*% od) / pmax(t(w) %*% w %*% h, epsilon))
    row_norm <- sqrt(rowSums(h^2))
    row_norm[row_norm <= 0 | !is.finite(row_norm)] <- 1
    h <- h / row_norm
    w <- sweep(w, 2L, row_norm, `*`)
  }
  vectors <- wsi_order_he_vectors(t(h))
  wsi_he_stain_matrix(hematoxylin = vectors[, 1L], eosin = vectors[, 2L])
}

#' Estimate H&E stain vectors from an image patch
#'
#' Estimates a 3 x 3 optical-density stain matrix for an already-small H&E image
#' patch, thumbnail, or tile. `method = "macenko"` implements a lightweight
#' Macenko-style PCA estimate. `method = "vahadane"` uses a small non-negative
#' matrix-factorisation approximation inspired by Vahadane-style stain
#' separation. Both methods are dependency-free and intended for preprocessing
#' support, not as a replacement for carefully validated laboratory-specific
#' calibration.
#'
#' @param image RGB/RGBA array, raster object, or magick image.
#' @param method `"macenko"`, `"vahadane"`, or `"fixed"` standard H&E vectors.
#' @param luminosity_threshold Pixels brighter than this RGB mean are treated as
#'   background during estimation.
#' @param od_threshold Minimum optical-density sum for tissue-like pixels.
#' @param angular_percentile Macenko angular percentile.
#' @param max_pixels Maximum number of tissue-like pixels used for estimation.
#' @param nmf_iterations,sparsity Controls for the experimental
#'   Vahadane-style NMF estimator.
#' @param epsilon Lower bound used before taking optical-density logarithms.
#'
#' @return A 3 x 3 stain matrix with hematoxylin, eosin, and residual columns.
#' @export
wsi_estimate_stain_matrix <- function(image,
                                      method = c("macenko", "vahadane", "fixed"),
                                      luminosity_threshold = 0.8,
                                      od_threshold = 0.15,
                                      angular_percentile = 1,
                                      max_pixels = 10000,
                                      nmf_iterations = 80,
                                      sparsity = 0.05,
                                      epsilon = 1 / 255) {
  method <- match.arg(method)
  epsilon <- wsi_check_scalar_number(epsilon, "epsilon", allow_zero = FALSE)
  if (epsilon >= 1) {
    wsi_abort("`epsilon` must be less than 1.")
  }
  if (identical(method, "fixed")) {
    return(wsi_he_stain_matrix())
  }
  if (identical(method, "macenko")) {
    return(wsi_estimate_macenko_matrix(
      image,
      luminosity_threshold = luminosity_threshold,
      od_threshold = od_threshold,
      angular_percentile = angular_percentile,
      max_pixels = max_pixels,
      epsilon = epsilon
    ))
  }
  wsi_estimate_vahadane_matrix(
    image,
    luminosity_threshold = luminosity_threshold,
    od_threshold = od_threshold,
    max_pixels = max_pixels,
    nmf_iterations = nmf_iterations,
    sparsity = sparsity,
    epsilon = epsilon
  )
}

wsi_channel_ids_from_output <- function(channels) {
  metadata <- channels$channel_metadata
  if (!is.null(metadata)) {
    return(vapply(metadata, `[[`, character(1), "id"))
  }
  setdiff(names(channels), c("alpha", "stain_matrix", "channel_metadata"))
}

wsi_recolour_stain_channels <- function(channels, colours = NULL, strengths = NULL,
                                        visible = NULL) {
  ids <- wsi_channel_ids_from_output(channels)
  if (!length(ids)) {
    wsi_abort("No stain channels are available to recolour.")
  }
  metadata <- channels$channel_metadata
  if (is.null(metadata)) {
    metadata <- lapply(ids, function(id) list(
      id = id,
      name = id,
      colour = wsi_stain_palette(length(ids))[[match(id, ids)]],
      strength = 1,
      visible = TRUE
    ))
  }

  if (is.null(colours)) {
    colours <- vapply(metadata, `[[`, character(1), "colour")
  }
  if (is.null(strengths)) {
    strengths <- vapply(metadata, `[[`, numeric(1), "strength")
  }
  if (is.null(visible)) {
    visible <- vapply(metadata, `[[`, logical(1), "visible")
  }

  colours <- as.character(wsi_recycle_to(colours, length(ids), "colours"))
  strengths <- as.numeric(wsi_recycle_to(strengths, length(ids), "strengths"))
  visible <- wsi_recycle_to(visible, length(ids), "visible")
  if (!is.logical(visible) || anyNA(visible)) {
    wsi_abort("`visible` must be TRUE or FALSE for each stain channel.")
  }

  first <- channels[[ids[[1L]]]]
  height <- nrow(first)
  width <- ncol(first)
  out <- array(1, dim = c(height, width, 4L))

  for (i in seq_along(ids)) {
    if (!isTRUE(visible[[i]])) {
      next
    }
    colour <- grDevices::col2rgb(wsi_colour_to_hex(colours[[i]], sprintf("colours[[%s]]", i)))[, 1L] / 255
    strength <- wsi_check_scalar_number(strengths[[i]], sprintf("strengths[[%s]]", i))
    tint <- pmin(1, pmax(0, 1 - exp(-channels[[ids[[i]]]] * strength)))
    for (channel in seq_len(3L)) {
      out[, , channel] <- out[, , channel] * (1 - tint) + colour[[channel]] * tint
    }
  }
  if (!is.null(channels$alpha)) {
    out[, , 4L] <- channels$alpha
  }
  out
}

wsi_recolour_ihc <- function(channels, hematoxylin_colour = "#4b3f99",
                             hrp_colour = "#8b5a2b",
                             hematoxylin_strength = 1,
                             hrp_strength = 1) {
  wsi_recolour_stain_channels(
    channels,
    colours = c(hematoxylin_colour, hrp_colour),
    strengths = c(hematoxylin_strength, hrp_strength)
  )
}

wsi_format_ihc_output <- function(channels, format, colours = NULL,
                                  strengths = NULL, visible = NULL) {
  if (identical(format, "channels")) {
    return(channels)
  }

  image <- wsi_recolour_stain_channels(
    channels,
    colours = colours,
    strengths = strengths,
    visible = visible
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

wsi_deconvolve_array <- function(image, channels, epsilon) {
  arr <- wsi_image_to_array(image)
  dims <- dim(arr)
  rgb <- pmax(arr[, , seq_len(3L), drop = FALSE], epsilon)
  od <- -log(rgb)
  od_mat <- cbind(as.vector(od[, , 1L]), as.vector(od[, , 2L]), as.vector(od[, , 3L]))
  basis <- wsi_complete_stain_basis(channels)
  inverse <- tryCatch(
    solve(basis$matrix),
    error = function(err) {
      wsi_abort("The supplied stain vectors are not linearly independent.")
    }
  )
  concentration <- od_mat %*% t(inverse)
  ids <- vapply(channels, `[[`, character(1), "id")

  channel_values <- lapply(seq_along(ids), function(i) {
    matrix(pmax(0, concentration[, i]), nrow = dims[[1L]], ncol = dims[[2L]])
  })
  names(channel_values) <- ids
  if (length(ids) < 3L) {
    residual_ids <- if (length(ids) == 2L) "residual" else c("residual_1", "residual_2")
    residual_values <- lapply(seq_along(residual_ids), function(i) {
      basis_index <- length(ids) + i
      matrix(pmax(0, concentration[, basis_index]), nrow = dims[[1L]], ncol = dims[[2L]])
    })
    names(residual_values) <- residual_ids
    channel_values <- c(channel_values, residual_values)
  }

  structure(
    c(
      channel_values,
      list(
        alpha = if (dims[[3L]] >= 4L) arr[, , 4L] else NULL,
        stain_matrix = basis$matrix,
        channel_metadata = unclass(channels)
      )
    ),
    class = "wsi_ihc_channels"
  )
}

wsi_deconvolve_two_stain_array <- function(image, channels, epsilon,
                                           include_residual = TRUE) {
  arr <- wsi_image_to_array(image)
  dims <- dim(arr)
  rgb <- pmax(arr[, , seq_len(3L), drop = FALSE], epsilon)
  od <- -log(rgb)
  od_mat <- cbind(as.vector(od[, , 1L]), as.vector(od[, , 2L]), as.vector(od[, , 3L]))
  if (length(channels) < 2L) {
    wsi_abort("Two-stain deconvolution requires at least two stain vectors.")
  }
  vectors <- do.call(cbind, lapply(channels[seq_len(2L)], `[[`, "vector"))
  gram <- crossprod(vectors)
  inverse <- tryCatch(
    solve(gram),
    error = function(err) {
      wsi_abort("The supplied H&E stain vectors are not linearly independent.")
    }
  )
  concentration <- od_mat %*% vectors %*% inverse
  concentration[concentration < 0] <- 0
  ids <- vapply(channels[seq_len(2L)], `[[`, character(1), "id")

  channel_values <- lapply(seq_along(ids), function(i) {
    matrix(concentration[, i], nrow = dims[[1L]], ncol = dims[[2L]])
  })
  names(channel_values) <- ids

  metadata <- unclass(channels)[seq_len(2L)]
  if (isTRUE(include_residual)) {
    reconstructed <- concentration %*% t(vectors)
    residual <- sqrt(rowSums((od_mat - reconstructed)^2))
    residual_id <- if (length(channels) >= 3L) channels[[3L]]$id else "residual"
    channel_values[[residual_id]] <- matrix(pmax(0, residual), nrow = dims[[1L]], ncol = dims[[2L]])
    if (length(channels) >= 3L) {
      metadata <- c(metadata, unclass(channels)[3L])
    } else {
      metadata <- c(metadata, list(list(
        id = residual_id,
        name = "Residual",
        vector = wsi_residual_stain_vector(vectors[, 1L], vectors[, 2L]),
        colour = "#6b7280",
        strength = 1,
        visible = FALSE,
        opacity = 1,
        contrast_min = 0,
        contrast_max = 1
      )))
    }
  }

  structure(
    c(
      channel_values,
      list(
        alpha = if (dims[[3L]] >= 4L) arr[, , 4L] else NULL,
        stain_matrix = wsi_complete_stain_basis(channels[seq_len(2L)])$matrix,
        channel_metadata = metadata
      )
    ),
    class = "wsi_ihc_channels"
  )
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

  channel_definitions <- wsi_stain_channels(
    name = c("Hematoxylin", "HRP/DAB"),
    vector = list(hematoxylin, hrp),
    colour = c(hematoxylin_colour, hrp_colour),
    strength = c(hematoxylin_strength, hrp_strength),
    visible = c(TRUE, TRUE)
  )
  channels <- wsi_deconvolve_array(image, channel_definitions, epsilon = epsilon)

  wsi_format_ihc_output(
    channels,
    format = format,
    colours = c(hematoxylin_colour, hrp_colour),
    strengths = c(hematoxylin_strength, hrp_strength)
  )
}

#' Deconvolve hematoxylin, eosin, and residual staining from an H&E image
#'
#' Separates an already-small H&E image patch, tile, or thumbnail into
#' hematoxylin, eosin, and residual optical-density concentration channels. This
#' is the H&E counterpart to [wsi_deconvolve_ihc()] and can use fixed stain
#' vectors or vectors estimated separately with [wsi_estimate_stain_matrix()].
#'
#' @param image RGB/RGBA array, raster object, or magick image.
#' @param format Output format. `"channels"` returns numeric concentration
#'   matrices for `hematoxylin`, `eosin`, and, when `include_residual = TRUE`,
#'   `residual`; image formats return a recoloured visualisation.
#' @param stain_matrix Optional 3 x 3 stain matrix. When supplied, the first two
#'   columns are used as hematoxylin and eosin. The residual channel reports the
#'   optical-density norm left after fitting hematoxylin/eosin rather than
#'   repeatedly deconvolving through an arbitrary third stain.
#' @param hematoxylin,eosin RGB optical-density vectors used when
#'   `stain_matrix` is not supplied.
#' @param include_residual Include residual staining as an explicit output
#'   channel.
#' @param hematoxylin_colour,eosin_colour,residual_colour Display colours for
#'   recoloured output.
#' @param hematoxylin_strength,eosin_strength,residual_strength Display gains
#'   for recoloured output.
#' @param residual_visible Whether residual is visible by default in recoloured
#'   outputs. The numeric `residual` matrix is still returned when
#'   `include_residual = TRUE`.
#' @param epsilon Lower bound used before taking optical-density logarithms.
#'
#' @return A `wsi_ihc_channels` object, array, raster, or magick image.
#' @export
#' @examples
#' patch <- array(0.8, dim = c(32, 32, 3))
#' channels <- wsi_deconvolve_he(patch)
wsi_deconvolve_he <- function(image,
                              format = c("channels", "array", "raster", "magick"),
                              stain_matrix = NULL,
                              hematoxylin = c(0.644, 0.717, 0.267),
                              eosin = c(0.093, 0.954, 0.283),
                              include_residual = TRUE,
                              hematoxylin_colour = "#4b3f99",
                              eosin_colour = "#e85b90",
                              residual_colour = "#6b7280",
                              hematoxylin_strength = 1,
                              eosin_strength = 1,
                              residual_strength = 1,
                              residual_visible = FALSE,
                              epsilon = 1 / 255) {
  format <- match.arg(format)
  epsilon <- wsi_check_scalar_number(epsilon, "epsilon", allow_zero = FALSE)
  if (epsilon >= 1) {
    wsi_abort("`epsilon` must be less than 1.")
  }
  include_residual <- isTRUE(include_residual)
  names <- c("Hematoxylin", "Eosin")
  colours <- c(hematoxylin_colour, eosin_colour)
  strengths <- c(hematoxylin_strength, eosin_strength)
  visible <- c(TRUE, TRUE)
  if (include_residual) {
    names <- c(names, "Residual")
    colours <- c(colours, residual_colour)
    strengths <- c(strengths, residual_strength)
    visible <- c(visible, isTRUE(residual_visible))
  }
  if (is.null(stain_matrix)) {
    channel_definitions <- wsi_he_stain_channels(
      hematoxylin = hematoxylin,
      eosin = eosin,
      include_residual = include_residual,
      hematoxylin_colour = hematoxylin_colour,
      eosin_colour = eosin_colour,
      residual_colour = residual_colour,
      hematoxylin_strength = hematoxylin_strength,
      eosin_strength = eosin_strength,
      residual_strength = residual_strength,
      residual_visible = residual_visible
    )
  } else {
    channel_definitions <- wsi_channels_from_matrix(
      stain_matrix,
      names = names,
      colours = colours,
      strengths = strengths,
      visible = visible
    )
  }
  channels <- wsi_deconvolve_two_stain_array(
    image,
    channel_definitions,
    epsilon = epsilon,
    include_residual = include_residual
  )
  wsi_format_ihc_output(
    channels,
    format = format,
    colours = colours,
    strengths = strengths,
    visible = visible
  )
}

#' Reconstruct an RGB image from stain concentration channels
#'
#' Reconstructs a brightfield RGB image from optical-density concentration
#' channels and a stain matrix. This is useful after H&E separation or stain
#' normalisation, where concentrations are recombined with a target stain basis.
#'
#' @param channels A `wsi_ihc_channels` object returned by
#'   [wsi_deconvolve_he()], [wsi_deconvolve_ihc()], or
#'   [wsi_deconvolve_multi_ihc()].
#' @param stain_matrix Optional 3-row stain matrix. When omitted, the matrix
#'   stored in `channels` is used.
#' @param format Output format.
#'
#' @return An RGB/RGBA array, raster, or magick image.
#' @export
wsi_reconstruct_stains <- function(channels, stain_matrix = NULL,
                                   format = c("array", "raster", "magick")) {
  format <- match.arg(format)
  if (!inherits(channels, "wsi_ihc_channels")) {
    wsi_abort("`channels` must be a `wsi_ihc_channels` object.")
  }
  ids <- wsi_channel_ids_from_output(channels)
  if (!length(ids)) {
    wsi_abort("No stain concentration channels are available.")
  }
  first <- channels[[ids[[1L]]]]
  height <- nrow(first)
  width <- ncol(first)
  concentration <- do.call(cbind, lapply(ids, function(id) {
    value <- channels[[id]]
    if (is.null(value) || !identical(dim(value), dim(first))) {
      wsi_abort("All stain concentration channels must have identical dimensions.")
    }
    as.vector(pmax(0, value))
  }))
  if (is.null(stain_matrix)) {
    stain_matrix <- channels$stain_matrix
  }
  stain_matrix <- wsi_validate_stain_matrix(stain_matrix, "stain_matrix", min_channels = length(ids))
  stain_matrix <- stain_matrix[, seq_along(ids), drop = FALSE]
  od <- concentration %*% t(stain_matrix)
  rgb <- exp(-od)
  image <- array(rgb, dim = c(height, width, 3L))
  image <- pmin(pmax(image, 0), 1)
  if (!is.null(channels$alpha)) {
    image <- array(c(image, channels$alpha), dim = c(height, width, 4L))
  }
  wsi_format_stain_image(image, format = format)
}

wsi_stain_channel_quantiles <- function(channels, ids = c("hematoxylin", "eosin"),
                                        concentration_percentile = 99) {
  concentration_percentile <- wsi_check_scalar_number(concentration_percentile, "concentration_percentile")
  if (concentration_percentile <= 0 || concentration_percentile > 100) {
    wsi_abort("`concentration_percentile` must be greater than 0 and less than or equal to 100.")
  }
  out <- vapply(ids, function(id) {
    values <- channels[[id]]
    if (is.null(values)) {
      wsi_abort(sprintf("Stain channel `%s` is not available.", id))
    }
    values <- as.vector(values)
    values <- values[is.finite(values)]
    if (!length(values)) {
      return(1)
    }
    q <- unname(stats::quantile(values, probs = concentration_percentile / 100, na.rm = TRUE, names = FALSE))
    if (!is.finite(q) || q <= 0) 1 else q
  }, numeric(1))
  unname(out)
}

wsi_scale_he_channels <- function(channels, source_concentrations,
                                  target_concentrations) {
  ids <- c("hematoxylin", "eosin")
  source_concentrations <- as.numeric(source_concentrations)
  target_concentrations <- as.numeric(target_concentrations)
  if (length(source_concentrations) != 2L || length(target_concentrations) != 2L ||
      anyNA(source_concentrations) || anyNA(target_concentrations) ||
      any(!is.finite(source_concentrations)) || any(!is.finite(target_concentrations))) {
    wsi_abort("Source and target concentration summaries must be numeric vectors of length 2.")
  }
  out <- channels
  ratio <- target_concentrations / pmax(source_concentrations, .Machine$double.eps)
  for (i in seq_along(ids)) {
    value <- pmax(0, out[[ids[[i]]]] * ratio[[i]])
    dim(value) <- dim(out[[ids[[i]]]])
    out[[ids[[i]]]] <- value
  }
  out
}

wsi_stain_normalization_result <- function(image, channels, source_matrix,
                                           target_matrix, source_concentrations,
                                           target_concentrations, method) {
  structure(
    list(
      image = image,
      channels = channels,
      source_matrix = source_matrix,
      target_matrix = target_matrix,
      source_concentrations = source_concentrations,
      target_concentrations = target_concentrations,
      method = method
    ),
    class = "wsi_stain_normalization"
  )
}

#' Normalize H&E stain appearance for a patch or tile
#'
#' Performs dependency-free H&E stain normalisation on an already-small image
#' patch, tile, or thumbnail. The default `method = "macenko"` estimates source
#' H&E stain vectors from the supplied image and reconstructs the patch with
#' target H&E vectors. `method = "vahadane"` provides a lightweight
#' Vahadane-style non-negative matrix-factorisation estimate. `method = "fixed"`
#' uses the standard H&E vectors without estimation.
#'
#' This function is deliberately patch-oriented. For whole-slide images, call
#' [wsi_stain_normalize_region()] or apply it inside a tiled workflow; do not
#' load an entire WSI into R memory.
#'
#' @param image RGB/RGBA array, raster object, or magick image.
#' @param method Stain-estimation method: `"macenko"`, `"vahadane"`, or
#'   `"fixed"`.
#' @param target Optional reference image used to estimate target vectors and
#'   target concentration percentiles.
#' @param source_matrix Optional source H&E stain matrix. If omitted, it is
#'   estimated from `image` unless `method = "fixed"`.
#' @param target_matrix Optional target H&E stain matrix. If omitted and
#'   `target` is not supplied, standard H&E vectors are used.
#' @param source_concentrations,target_concentrations Optional hematoxylin/eosin
#'   concentration percentile values. When omitted, source values are measured
#'   from `image`; target values come from `target` when supplied, otherwise
#'   source values are reused so only stain-vector normalisation is applied.
#' @param concentration_percentile Percentile used for concentration scaling.
#' @param format `"array"`, `"raster"`, `"magick"`, or `"result"` for a detailed
#'   object containing the normalized image, matrices, and concentration
#'   summaries.
#' @inheritParams wsi_estimate_stain_matrix
#'
#' @return A normalized image or a `wsi_stain_normalization` object.
#' @export
#' @examples
#' patch <- array(0.8, dim = c(32, 32, 3))
#' normalized <- wsi_normalize_stains(patch, method = "fixed")
wsi_normalize_stains <- function(image,
                                 method = c("macenko", "vahadane", "fixed"),
                                 target = NULL,
                                 source_matrix = NULL,
                                 target_matrix = NULL,
                                 source_concentrations = NULL,
                                 target_concentrations = NULL,
                                 concentration_percentile = 99,
                                 luminosity_threshold = 0.8,
                                 od_threshold = 0.15,
                                 angular_percentile = 1,
                                 max_pixels = 10000,
                                 nmf_iterations = 80,
                                 sparsity = 0.05,
                                 epsilon = 1 / 255,
                                 format = c("array", "raster", "magick", "result")) {
  method <- match.arg(method)
  format <- match.arg(format)
  arr <- wsi_image_to_array(image)
  if (is.null(source_matrix)) {
    source_matrix <- wsi_estimate_stain_matrix(
      arr,
      method = method,
      luminosity_threshold = luminosity_threshold,
      od_threshold = od_threshold,
      angular_percentile = angular_percentile,
      max_pixels = max_pixels,
      nmf_iterations = nmf_iterations,
      sparsity = sparsity,
      epsilon = epsilon
    )
  } else {
    source_matrix <- wsi_validate_stain_matrix(source_matrix, "source_matrix", min_channels = 2L)
  }

  target_arr <- NULL
  if (!is.null(target)) {
    target_arr <- wsi_image_to_array(target)
  }
  if (is.null(target_matrix)) {
    target_matrix <- if (is.null(target_arr)) {
      wsi_he_stain_matrix()
    } else {
      wsi_estimate_stain_matrix(
        target_arr,
        method = method,
        luminosity_threshold = luminosity_threshold,
        od_threshold = od_threshold,
        angular_percentile = angular_percentile,
        max_pixels = max_pixels,
        nmf_iterations = nmf_iterations,
        sparsity = sparsity,
        epsilon = epsilon
      )
    }
  } else {
    target_matrix <- wsi_validate_stain_matrix(target_matrix, "target_matrix", min_channels = 2L)
  }

  channels <- wsi_deconvolve_he(arr, stain_matrix = source_matrix, format = "channels", epsilon = epsilon)
  if (is.null(source_concentrations)) {
    source_concentrations <- wsi_stain_channel_quantiles(channels, concentration_percentile = concentration_percentile)
  }
  if (is.null(target_concentrations)) {
    target_concentrations <- if (is.null(target_arr)) {
      source_concentrations
    } else {
      target_channels <- wsi_deconvolve_he(target_arr, stain_matrix = target_matrix, format = "channels", epsilon = epsilon)
      wsi_stain_channel_quantiles(target_channels, concentration_percentile = concentration_percentile)
    }
  }
  channels <- wsi_scale_he_channels(channels, source_concentrations, target_concentrations)
  normalized <- wsi_reconstruct_stains(channels, stain_matrix = target_matrix, format = "array")
  result <- wsi_stain_normalization_result(
    image = normalized,
    channels = channels,
    source_matrix = source_matrix,
    target_matrix = target_matrix,
    source_concentrations = source_concentrations,
    target_concentrations = target_concentrations,
    method = method
  )
  if (identical(format, "result")) {
    return(result)
  }
  wsi_format_stain_image(normalized, format = format)
}

#' @rdname wsi_normalize_stains
#' @export
wsi_normalise_stains <- wsi_normalize_stains

#' Normalize stains in a slide region
#'
#' Reads only the requested region from a slide and applies
#' [wsi_normalize_stains()]. This keeps stain normalisation compatible with WSI
#' workflows by avoiding full-slide reads.
#'
#' @param slide A `wsi_slide` object or path readable by [wsi_open()].
#' @param x,y,width,height,level Region coordinates; see [wsi_read_region()].
#' @param ... Arguments passed to [wsi_normalize_stains()].
#'
#' @return See [wsi_normalize_stains()].
#' @export
wsi_stain_normalize_region <- function(slide, x, y, width, height, level = 0, ...) {
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  if (is.character(slide) && length(slide) == 1L) {
    opened <- wsi_open(slide, backend = "auto")
    slide <- opened
  } else {
    wsi_check_slide(slide)
  }

  patch <- wsi_read_region(
    slide,
    x = x,
    y = y,
    width = width,
    height = height,
    level = level,
    format = "array"
  )
  wsi_normalize_stains(patch, ...)
}

#' @rdname wsi_stain_normalize_region
#' @export
wsi_stain_normalise_region <- wsi_stain_normalize_region

#' Deconvolve multiple brightfield IHC stain channels from an image
#'
#' Performs RGB optical-density color deconvolution for one to three supplied
#' stain channels. This is useful for brightfield multiplex IHC or chromogenic
#' assays where marker channels are represented by different stain vectors.
#'
#' This function works on an already-small image object such as a patch,
#' thumbnail, or region read. It does not load a whole-slide image into memory.
#'
#' @param image RGB/RGBA array, raster object, or magick image.
#' @param channels Channel definitions from [wsi_stain_channels()]. Compatible
#'   lists and data frames are also accepted.
#' @param format Output format. `"channels"` returns concentration matrices;
#'   image formats return a recolored multi-channel visualization.
#' @param epsilon Lower bound used before taking optical-density logarithms.
#'
#' @return A `wsi_ihc_channels` object, array, raster, or magick image.
#' @export
#' @examples
#' patch <- array(0.8, dim = c(32, 32, 3))
#' channels <- wsi_stain_channels(
#'   name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
#'   vector = list(c(0.650, 0.704, 0.286), c(0.268, 0.570, 0.776),
#'                 c(0.213, 0.851, 0.477))
#' )
#' result <- wsi_deconvolve_multi_ihc(patch, channels = channels)
wsi_deconvolve_multi_ihc <- function(image, channels,
                                     format = c("channels", "array", "raster", "magick"),
                                     epsilon = 1 / 255) {
  format <- match.arg(format)
  epsilon <- wsi_check_scalar_number(epsilon, "epsilon", allow_zero = FALSE)
  if (epsilon >= 1) {
    wsi_abort("`epsilon` must be less than 1.")
  }
  channels <- wsi_as_stain_channels(channels)
  result <- wsi_deconvolve_array(image, channels, epsilon = epsilon)
  wsi_format_ihc_output(result, format = format)
}

#' Deconvolve hematoxylin and HRP/DAB stains from a slide region
#'
#' Reads only the requested region, then applies [wsi_deconvolve_ihc()]. This is
#' the preferred R-side workflow for WSI patches because the full slide is never
#' loaded into memory.
#'
#' @param slide A `wsi_slide` object or a path that can be opened with
#'   [wsi_open()].
#' @param x,y,width,height,level Region coordinates; see [wsi_read_region()].
#' @param ... Arguments passed to [wsi_deconvolve_ihc()].
#'
#' @return See [wsi_deconvolve_ihc()].
#' @export
wsi_deconvolve_region <- function(slide, x, y, width, height, level = 0, ...) {
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  if (is.character(slide) && length(slide) == 1L) {
    opened <- wsi_open(slide, backend = "auto")
    slide <- opened
  } else {
    wsi_check_slide(slide)
  }

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

#' Deconvolve hematoxylin, eosin, and residual staining from a slide region
#'
#' Reads only the requested region, then applies [wsi_deconvolve_he()]. This is
#' the preferred R-side H&E workflow for WSI patches because the full slide is
#' never loaded into memory.
#'
#' @inheritParams wsi_deconvolve_region
#' @param ... Arguments passed to [wsi_deconvolve_he()], including
#'   `include_residual`, stain vectors, colours, and display gains.
#'
#' @return See [wsi_deconvolve_he()].
#' @export
wsi_deconvolve_he_region <- function(slide, x, y, width, height, level = 0, ...) {
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  if (is.character(slide) && length(slide) == 1L) {
    opened <- wsi_open(slide, backend = "auto")
    slide <- opened
  } else {
    wsi_check_slide(slide)
  }

  patch <- wsi_read_region(
    slide,
    x = x,
    y = y,
    width = width,
    height = height,
    level = level,
    format = "array"
  )
  wsi_deconvolve_he(patch, ...)
}

wsi_ihc_stain_config <- function(stain = c("none", "ihc", "he"),
                                 channels = NULL,
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
  if (is.null(channels)) {
    channels <- if (identical(stain, "he")) {
      wsi_he_stain_channels()
    } else {
      wsi_stain_channels(
        name = c("Hematoxylin", "HRP/DAB"),
        vector = list(hematoxylin, hrp),
        colour = c(hematoxylin_colour, hrp_colour),
        strength = c(hematoxylin_strength, hrp_strength),
        visible = c(TRUE, TRUE)
      )
    }
  } else {
    channels <- wsi_as_stain_channels(channels)
  }
  basis <- wsi_complete_stain_basis(channels)
  channel_count <- length(channels)
  ids <- vapply(channels, `[[`, character(1), "id")
  is_he <- all(c("hematoxylin", "eosin") %in% ids)
  label <- if (is_he) {
    if ("residual" %in% ids) "H&E H/E/residual" else "H&E"
  } else if (channel_count > 2L) {
    "IHC channels"
  } else {
    "IHC H-DAB"
  }
  list(
    enabled = TRUE,
    type = if (is_he) "H&E" else if (channel_count > 2L) "multi-IHC" else "H-DAB",
    label = label,
    button_label = if (is_he) "H&E" else if (channel_count > 2L) "mIHC" else "IHC",
    channels = unclass(channels),
    basis = basis$basis,
    channel_count = channel_count
  )
}

#' View an IHC slide with interactive stain-channel deconvolution
#'
#' Writes an HTML viewer with browser-side color deconvolution controls. By
#' default this uses H-DAB hematoxylin plus HRP/DAB channels. Supply `channels`
#' to inspect one to three brightfield immunohistochemistry channels with simple
#' `Original`, `All stains`, and show-only display buttons. Show-only views are
#' auto-contrasted from visible pixels. In tiled mode, the browser recolors only
#' the visible Deep Zoom tiles, so the full WSI is not loaded into R memory.
#'
#' @param slide A `wsi_slide` object.
#' @param mode Viewer mode passed to [wsi_viewer()]. `"tiles"` is recommended
#'   for full-resolution inspection.
#' @param channels Optional stain channels created by [wsi_stain_channels()].
#'   If supplied, these replace the default hematoxylin and HRP/DAB channels.
#' @param hematoxylin,hrp RGB optical-density vectors used when `channels` is
#'   not supplied.
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
                           channels = NULL,
                           hematoxylin = c(0.650, 0.704, 0.286),
                           hrp = c(0.268, 0.570, 0.776),
                           hematoxylin_colour = "#4b3f99",
                           hrp_colour = "#8b5a2b",
                           hematoxylin_strength = 1,
                           hrp_strength = 1,
                           ...) {
  mode <- match.arg(mode)
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  if (is.character(slide) && length(slide) == 1L) {
    opened <- wsi_open(slide, backend = "auto")
    slide <- opened
  } else {
    wsi_check_slide(slide)
  }

  wsi_viewer(
    slide,
    mode = mode,
    stain = "ihc",
    channels = channels,
    hematoxylin = hematoxylin,
    hrp = hrp,
    hematoxylin_colour = hematoxylin_colour,
    hrp_colour = hrp_colour,
    hematoxylin_strength = hematoxylin_strength,
    hrp_strength = hrp_strength,
    ...
  )
}

#' View an H&E slide with interactive hematoxylin/eosin/residual deconvolution
#'
#' Convenience wrapper around [wsi_viewer()] for H&E brightfield images. The
#' `Stains` menu exposes `Original`, `All stains`, and show-only buttons for
#' hematoxylin, eosin, and residual channels. Show-only views are
#' auto-contrasted from visible pixels. In tiled mode, the browser recolours
#' only visible tiles, so the full WSI is not loaded into R memory.
#'
#' @inheritParams wsi_viewer_ihc
#' @param method Stain-vector method used when `channels = NULL`.
#'   `"fixed"` uses standard H&E vectors. `"macenko"` or `"vahadane"` estimate
#'   vectors from a low-resolution thumbnail, which is often better for a
#'   specific laboratory or scanner.
#' @param thumbnail_width Width of the thumbnail used for stain-vector
#'   estimation when `method` is not `"fixed"`.
#' @param hematoxylin,eosin RGB optical-density vectors used for
#'   `method = "fixed"`.
#' @param hematoxylin_colour,eosin_colour,residual_colour Initial display
#'   colours.
#' @param hematoxylin_strength,eosin_strength,residual_strength Initial display
#'   gains.
#' @param residual_visible Whether residual staining is visible by default.
#'
#' @return The HTML viewer path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("he_sample.svs")
#' html <- wsi_viewer_he(slide, output = "he_viewer.html", open = FALSE)
#' wsi_close(slide)
#' }
wsi_viewer_he <- function(slide, mode = c("tiles", "thumbnail"),
                          channels = NULL,
                          method = c("fixed", "macenko", "vahadane"),
                          thumbnail_width = 2048,
                          hematoxylin = c(0.644, 0.717, 0.267),
                          eosin = c(0.093, 0.954, 0.283),
                          hematoxylin_colour = "#4b3f99",
                          eosin_colour = "#e85b90",
                          residual_colour = "#6b7280",
                          hematoxylin_strength = 1,
                          eosin_strength = 1,
                          residual_strength = 1,
                          residual_visible = FALSE,
                          ...) {
  mode <- match.arg(mode)
  method <- match.arg(method)
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  if (is.character(slide) && length(slide) == 1L) {
    opened <- wsi_open(slide, backend = "auto")
    slide <- opened
  } else {
    wsi_check_slide(slide)
  }
  if (is.null(channels)) {
    channels <- if (identical(method, "fixed")) {
      wsi_he_stain_channels(
        hematoxylin = hematoxylin,
        eosin = eosin,
        include_residual = TRUE,
        hematoxylin_colour = hematoxylin_colour,
        eosin_colour = eosin_colour,
        residual_colour = residual_colour,
        hematoxylin_strength = hematoxylin_strength,
        eosin_strength = eosin_strength,
        residual_strength = residual_strength,
        residual_visible = residual_visible
      )
    } else {
      wsi_estimate_he_stain_channels(
        slide,
        method = method,
        thumbnail_width = thumbnail_width,
        include_residual = TRUE,
        hematoxylin_colour = hematoxylin_colour,
        eosin_colour = eosin_colour,
        residual_colour = residual_colour,
        hematoxylin_strength = hematoxylin_strength,
        eosin_strength = eosin_strength,
        residual_strength = residual_strength,
        residual_visible = residual_visible
      )
    }
  }

  wsi_viewer(
    slide,
    mode = mode,
    stain = "he",
    channels = channels,
    ...
  )
}

#' View a multi-IHC slide with selectable stain channels
#'
#' Convenience wrapper around [wsi_viewer()] for brightfield multiplex IHC.
#' The `Stains` menu includes a master `mIHC` toggle plus `Original`,
#' `All stains`, and show-only buttons. The default channels are starting values
#' for hematoxylin, HRP/DAB, and a red chromogen; pass assay-specific vectors
#' with [wsi_stain_channels()] for real analysis.
#'
#' @inheritParams wsi_viewer_ihc
#' @param channels Stain channels created by [wsi_stain_channels()]. Up to
#'   three channels can be deconvolved from RGB brightfield data at a time.
#'
#' @return The HTML viewer path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("multiplex_ihc.svs")
#' channels <- wsi_stain_channels(
#'   name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
#'   vector = list(c(0.650, 0.704, 0.286), c(0.268, 0.570, 0.776),
#'                 c(0.213, 0.851, 0.477)),
#'   colour = c("#4b3f99", "#8b5a2b", "#d73027")
#' )
#' html <- wsi_viewer_multi_ihc(slide, channels = channels, open = FALSE)
#' wsi_close(slide)
#' }
wsi_viewer_multi_ihc <- function(slide, mode = c("tiles", "thumbnail"),
                                 channels = wsi_default_multi_ihc_channels(),
                                 ...) {
  mode <- match.arg(mode)
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  if (is.character(slide) && length(slide) == 1L) {
    opened <- wsi_open(slide, backend = "auto")
    slide <- opened
  } else {
    wsi_check_slide(slide)
  }

  wsi_viewer(
    slide,
    mode = mode,
    stain = "ihc",
    channels = channels,
    ...
  )
}

#' @export
print.wsi_ihc_channels <- function(x, ...) {
  ids <- wsi_channel_ids_from_output(x)
  dims <- dim(x[[ids[[1L]]]])
  cat("<wsi_ihc_channels>\n")
  cat("  size: ", dims[[2L]], " x ", dims[[1L]], " px\n", sep = "")
  cat("  channels: ", paste(ids, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.wsi_stain_normalization <- function(x, ...) {
  dims <- dim(x$image)
  cat("<wsi_stain_normalization>\n")
  cat("  method: ", x$method %||% "unknown", "\n", sep = "")
  cat("  size: ", dims[[2L]], " x ", dims[[1L]], " px\n", sep = "")
  ids <- tryCatch(wsi_channel_ids_from_output(x$channels), error = function(err) c("hematoxylin", "eosin"))
  cat("  channels: ", paste(ids, collapse = ", "), "\n", sep = "")
  invisible(x)
}
