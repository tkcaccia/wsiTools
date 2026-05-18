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
                               visible = TRUE) {
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
      visible = isTRUE(visible[[i]])
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
    return(wsi_stain_channels(
      name = name,
      vector = vector,
      colour = colour,
      strength = strength,
      visible = visible
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
    return(wsi_stain_channels(
      name = name,
      vector = vector,
      colour = colour,
      strength = strength,
      visible = visible
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

wsi_ihc_stain_config <- function(stain = c("none", "ihc"),
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
    channels <- wsi_stain_channels(
      name = c("Hematoxylin", "HRP/DAB"),
      vector = list(hematoxylin, hrp),
      colour = c(hematoxylin_colour, hrp_colour),
      strength = c(hematoxylin_strength, hrp_strength),
      visible = c(TRUE, TRUE)
    )
  } else {
    channels <- wsi_as_stain_channels(channels)
  }
  basis <- wsi_complete_stain_basis(channels)
  channel_count <- length(channels)
  label <- if (channel_count > 2L) "IHC channels" else "IHC H-DAB"
  list(
    enabled = TRUE,
    type = if (channel_count > 2L) "multi-IHC" else "H-DAB",
    label = label,
    button_label = if (channel_count > 2L) "mIHC" else "IHC",
    channels = unclass(channels),
    basis = basis$basis,
    channel_count = channel_count
  )
}

#' View an IHC slide with interactive stain-channel deconvolution
#'
#' Writes an HTML viewer with browser-side color deconvolution controls. By
#' default this uses H-DAB hematoxylin plus HRP/DAB channels. Supply `channels`
#' to inspect one to three brightfield immunohistochemistry channels, each with
#' its own visibility checkbox, display colour, and gain slider. In tiled mode,
#' the browser recolors only the visible Deep Zoom tiles, so the full WSI is not
#' loaded into R memory.
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

#' View a multi-IHC slide with selectable stain channels
#'
#' Convenience wrapper around [wsi_viewer()] for brightfield multiplex IHC.
#' The `Stains` menu includes a master `mIHC` toggle plus per-channel
#' checkboxes, colours, and gain sliders. The default channels are starting
#' values for hematoxylin, HRP/DAB, and a red chromogen; pass assay-specific
#' vectors with [wsi_stain_channels()] for real analysis.
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
