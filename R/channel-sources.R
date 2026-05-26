wsi_channel_type <- function(type = "stain") {
  match.arg(type, c("stain", "deepzoom", "dynamic"))
}

wsi_channel_opacity <- function(opacity = 1) {
  opacity <- wsi_check_scalar_number(opacity, "opacity")
  max(0, min(1, opacity))
}

wsi_channel_gain <- function(gain = 1) {
  wsi_check_scalar_number(gain, "gain")
}

wsi_channel_contrast <- function(contrast_min = 0, contrast_max = 1) {
  contrast_min <- wsi_check_scalar_number(contrast_min, "contrast_min")
  contrast_max <- wsi_check_scalar_number(contrast_max, "contrast_max")
  if (contrast_max <= contrast_min) {
    wsi_abort("`contrast_max` must be greater than `contrast_min`.")
  }
  c(min = contrast_min, max = contrast_max)
}

wsi_channel_source_id <- function(id = NULL, name = NULL) {
  id <- id %||% name %||% "channel"
  id <- wsi_safe_id(id, "channel")
  if (!nzchar(id)) {
    id <- "channel"
  }
  id
}

#' Define an OpenSeadragon channel tile source
#'
#' Channel sources describe optional tiled layers that can be shown over a base
#' OpenSeadragon image. A source may point to a precomputed Deep Zoom pyramid, a
#' live dynamic tile endpoint, or a virtual brightfield stain channel controlled
#' by the browser-side deconvolution display. This keeps channel display
#' tile-based and avoids loading a whole-slide image into R memory.
#'
#' @param name Human-readable channel name.
#' @param id Stable channel ID.
#' @param type Channel source type: `"stain"`, `"deepzoom"`, or `"dynamic"`.
#' @param tile_url_base,tile_url_template Tile URL information for tiled
#'   channel sources. Templates may contain `{level}`, `{x}`, `{y}`, and
#'   `{format}` placeholders.
#' @param tile_dir Optional directory containing a `slide_files` Deep Zoom tile
#'   pyramid.
#' @param width,height Full-resolution image dimensions.
#' @param tile_size,tile_format,max_level,tile_overlap OpenSeadragon tile
#'   metadata.
#' @param vector Optional RGB optical-density vector for virtual stain channels.
#' @param visible,opacity,colour,gain,contrast_min,contrast_max Initial display
#'   settings.
#' @param metadata Optional list of extra source metadata.
#'
#' @return A `wsi_channel_source` object.
#' @export
wsi_channel_source <- function(name = NULL, id = NULL,
                               type = c("stain", "deepzoom", "dynamic"),
                               tile_url_base = NULL, tile_url_template = NULL,
                               tile_dir = NULL, width = NULL, height = NULL,
                               tile_size = 512, tile_format = c("png", "jpg", "jpeg"),
                               max_level = NULL, tile_overlap = 0,
                               vector = NULL, visible = TRUE, opacity = 1,
                               colour = "#ffffff", gain = 1,
                               contrast_min = 0, contrast_max = 1,
                               metadata = list()) {
  type <- wsi_channel_type(type)
  tile_format <- wsi_dynamic_tile_format(tile_format)
  id <- wsi_channel_source_id(id, name)
  name <- as.character(name %||% id)
  if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
    wsi_abort("`visible` must be `TRUE` or `FALSE`.")
  }
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  opacity <- wsi_channel_opacity(opacity)
  gain <- wsi_channel_gain(gain)
  contrast <- wsi_channel_contrast(contrast_min, contrast_max)
  colour <- wsi_colour_to_hex(colour, "colour")

  if (!is.null(tile_dir)) {
    tile_files <- file.path(tile_dir, "slide_files")
    if (!dir.exists(tile_files) && dir.exists(tile_dir)) {
      tile_files <- tile_dir
    }
    tile_url_base <- tile_url_base %||% wsi_file_url(tile_files)
  }
  if (type %in% c("deepzoom", "dynamic")) {
    if ((is.null(tile_url_base) || !nzchar(tile_url_base)) &&
        (is.null(tile_url_template) || !nzchar(tile_url_template))) {
      wsi_abort("Tiled channel sources require `tile_url_base`, `tile_url_template`, or `tile_dir`.")
    }
    if (is.null(width) || is.null(height) || is.null(max_level)) {
      wsi_abort("Tiled channel sources require `width`, `height`, and `max_level`.")
    }
    width <- as.numeric(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
    height <- as.numeric(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
    max_level <- as.integer(wsi_check_scalar_number(max_level, "max_level"))
  } else {
    width <- if (is.null(width)) NA_real_ else as.numeric(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
    height <- if (is.null(height)) NA_real_ else as.numeric(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
    max_level <- if (is.null(max_level)) NA_integer_ else as.integer(wsi_check_scalar_number(max_level, "max_level"))
  }
  if (!is.null(vector)) {
    vector <- wsi_normalize_stain_vector(vector, "vector")
  }

  source <- list(
    id = id,
    name = name,
    type = type,
    tile_url_base = tile_url_base,
    tile_url_template = tile_url_template,
    tile_url_style = if (!is.null(tile_url_template) || identical(type, "dynamic")) "slash" else "deepzoom",
    width = width,
    height = height,
    tile_size = tile_size,
    tile_format = tile_format,
    max_level = max_level,
    tile_overlap = tile_overlap,
    vector = vector,
    visible = isTRUE(visible),
    opacity = opacity,
    colour = colour,
    gain = gain,
    contrast_min = unname(contrast[["min"]]),
    contrast_max = unname(contrast[["max"]]),
    metadata = metadata %||% list()
  )
  class(source) <- c("wsi_channel_source", "list")
  source
}

wsi_channel_source_payload <- function(source) {
  if (inherits(source, "wsi_dynamic_image_tile_source")) {
    source <- wsi_channel_source_from_dynamic(source)
  }
  if (inherits(source, "wsi_channel_source")) {
    source <- unclass(source)
  }
  if (!is.list(source)) {
    wsi_abort("Channel source entries must be lists or `wsi_channel_source` objects.")
  }
  source$id <- wsi_channel_source_id(source$id %||% NULL, source$name %||% NULL)
  source$name <- as.character(source$name %||% source$id)
  source$type <- wsi_channel_type(source$type %||% "stain")
  source$visible <- isTRUE(source$visible %||% TRUE)
  source$opacity <- wsi_channel_opacity(source$opacity %||% 1)
  source$colour <- wsi_colour_to_hex(source$colour %||% source$color %||% "#ffffff", "colour")
  source$gain <- wsi_channel_gain(source$gain %||% source$strength %||% 1)
  contrast <- wsi_channel_contrast(source$contrast_min %||% 0, source$contrast_max %||% 1)
  source$contrast_min <- unname(contrast[["min"]])
  source$contrast_max <- unname(contrast[["max"]])
  source
}

wsi_channel_sources_payload <- function(channel_sources = NULL) {
  if (is.null(channel_sources)) {
    return(list())
  }
  if (inherits(channel_sources, "wsi_channel_source")) {
    channel_sources <- list(channel_sources)
  }
  if (!is.list(channel_sources)) {
    wsi_abort("`channel_sources` must be a list of channel source objects.")
  }
  lapply(channel_sources, wsi_channel_source_payload)
}

wsi_channel_source_from_dynamic <- function(source, base_url = NULL) {
  if (!inherits(source, "wsi_dynamic_tile_source")) {
    wsi_abort("`source` must be a dynamic tile source.")
  }
  metadata <- wsi_dynamic_tile_metadata(source, base_url = base_url)
  wsi_channel_source(
    name = metadata$name %||% metadata$id,
    id = metadata$id,
    type = "dynamic",
    tile_url_base = metadata$tile_url_base,
    tile_url_template = metadata$tile_url_template,
    width = metadata$width,
    height = metadata$height,
    tile_size = metadata$tile_size,
    tile_format = metadata$tile_format,
    max_level = metadata$max_level,
    tile_overlap = metadata$tile_overlap,
    visible = metadata$visible %||% TRUE,
    opacity = metadata$opacity %||% 1,
    colour = metadata$colour %||% "#ffffff",
    gain = metadata$gain %||% 1,
    contrast_min = metadata$contrast_min %||% 0,
    contrast_max = metadata$contrast_max %||% 1,
    metadata = utils::modifyList(
      metadata$metadata %||% list(),
      list(
        kind = metadata$kind %||% "image",
        page = metadata$page %||% NULL,
        extent = metadata$extent %||% NULL,
        cache_key = metadata$cache_key %||% NULL,
        server_colourized = inherits(source, "wsi_dynamic_image_tile_source")
      ),
      keep.null = TRUE
    )
  )
}

wsi_dynamic_channel_sources <- function(channel_sources = NULL) {
  if (is.null(channel_sources)) {
    return(list())
  }
  if (inherits(channel_sources, "wsi_mihc_channel_sources")) {
    return(channel_sources$dynamic_sources %||% list())
  }
  if (inherits(channel_sources, "wsi_dynamic_tile_source")) {
    return(list(channel_sources))
  }
  if (!is.list(channel_sources)) {
    return(list())
  }
  out <- list()
  for (source in channel_sources) {
    if (inherits(source, "wsi_mihc_channel_sources")) {
      out <- c(out, source$dynamic_sources %||% list())
    } else if (inherits(source, "wsi_dynamic_tile_source")) {
      out[[length(out) + 1L]] <- source
    }
  }
  out
}

wsi_static_channel_sources <- function(channel_sources = NULL) {
  if (is.null(channel_sources) || inherits(channel_sources, "wsi_mihc_channel_sources") ||
      inherits(channel_sources, "wsi_dynamic_tile_source")) {
    return(list())
  }
  if (inherits(channel_sources, "wsi_channel_source")) {
    return(list(channel_sources))
  }
  if (!is.list(channel_sources)) {
    return(list())
  }
  out <- list()
  for (source in channel_sources) {
    if (inherits(source, "wsi_channel_source") ||
        (is.list(source) && !inherits(source, "wsi_dynamic_tile_source") &&
         !inherits(source, "wsi_mihc_channel_sources"))) {
      out[[length(out) + 1L]] <- source
    }
  }
  out
}

wsi_live_channel_sources <- function(channel_sources = NULL, base_url = NULL) {
  c(
    wsi_static_channel_sources(channel_sources),
    lapply(wsi_dynamic_channel_sources(channel_sources), wsi_channel_source_from_dynamic, base_url = base_url)
  )
}

wsi_empty_channel_settings <- function() {
  data.frame(
    id = character(),
    name = character(),
    type = character(),
    visible = logical(),
    opacity = numeric(),
    colour = character(),
    gain = numeric(),
    contrast_min = numeric(),
    contrast_max = numeric(),
    stringsAsFactors = FALSE
  )
}

wsi_channel_settings_from_sources <- function(sources) {
  sources <- wsi_channel_sources_payload(sources)
  if (!length(sources)) {
    return(wsi_empty_channel_settings())
  }
  out <- data.frame(
    id = vapply(sources, function(x) as.character(x$id %||% ""), character(1)),
    name = vapply(sources, function(x) as.character(x$name %||% x$id %||% ""), character(1)),
    type = vapply(sources, function(x) as.character(x$type %||% "stain"), character(1)),
    visible = vapply(sources, function(x) isTRUE(x$visible), logical(1)),
    opacity = vapply(sources, function(x) as.numeric(x$opacity %||% 1), numeric(1)),
    colour = vapply(sources, function(x) as.character(x$colour %||% x$color %||% "#ffffff"), character(1)),
    gain = vapply(sources, function(x) as.numeric(x$gain %||% x$strength %||% 1), numeric(1)),
    contrast_min = vapply(sources, function(x) as.numeric(x$contrast_min %||% 0), numeric(1)),
    contrast_max = vapply(sources, function(x) as.numeric(x$contrast_max %||% 1), numeric(1)),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_channel_settings", class(out))
  out
}

wsi_ome_channel_names <- function(path, pages = NULL) {
  description <- tryCatch(
    wsi_vips_field(path, "image-description"),
    error = function(err) NA_character_
  )
  names <- character()
  if (is.character(description) && length(description) == 1L && !is.na(description) && nzchar(description)) {
    matches <- gregexpr("<Channel[^>]*\\bName=\"([^\"]+)\"", description, perl = TRUE)
    raw <- regmatches(description, matches)[[1L]]
    if (length(raw) && !identical(raw, character(0))) {
      names <- sub(".*\\bName=\"([^\"]+)\".*", "\\1", raw, perl = TRUE)
    }
  }
  if (is.null(pages)) {
    pages <- seq_along(names) - 1L
  }
  n <- length(pages)
  if (length(names) < n) {
    names <- c(names, sprintf("Channel %d", seq_len(n)))[seq_len(n)]
  } else {
    names <- names[seq_len(n)]
  }
  names
}

wsi_channel_palette <- function(n) {
  palette <- c(
    "#ff3366", "#33ccff", "#ffd400", "#66ff66", "#cc66ff",
    "#ff8c33", "#00e5a8", "#ffffff"
  )
  rep(palette, length.out = n)
}

wsi_registration_extent <- function(registration = NULL) {
  if (is.null(registration)) {
    return(NULL)
  }
  data <- registration
  if (is.character(registration) && length(registration) == 1L && !is.na(registration) && nzchar(registration)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      wsi_abort("Reading registration JSON requires the `jsonlite` package.", class = "wsi_missing_dependency")
    }
    data <- jsonlite::fromJSON(wsi_validate_input_path(registration), simplifyVector = FALSE)
  }
  if (!is.list(data)) {
    wsi_abort("`registration` must be a registration JSON path, list, or `NULL`.")
  }
  bbox <- data$crop_bbox_xyxy %||% data$bbox %||% data$extent %||% NULL
  if (is.list(bbox) && all(c("x0", "y0", "x1", "y1") %in% names(bbox))) {
    x0 <- as.numeric(bbox$x0)
    y0 <- as.numeric(bbox$y0)
    x1 <- as.numeric(bbox$x1)
    y1 <- as.numeric(bbox$y1)
    extent <- c(x = x0, y = y0, width = x1 - x0, height = y1 - y0)
    if (all(is.finite(extent)) && all(extent[c("width", "height")] > 0)) {
      return(extent)
    }
  }
  if (is.list(bbox) && all(c("x", "y", "width", "height") %in% names(bbox))) {
    extent <- c(
      x = as.numeric(bbox$x),
      y = as.numeric(bbox$y),
      width = as.numeric(bbox$width),
      height = as.numeric(bbox$height)
    )
    if (all(is.finite(extent)) && all(extent[c("width", "height")] > 0)) {
      return(extent)
    }
  }
  offset <- data$offset_crop_to_original %||% NULL
  crop <- data$crop_size %||% NULL
  if (is.list(offset) && is.list(crop) &&
      all(c("dx", "dy") %in% names(offset)) &&
      all(c("width", "height") %in% names(crop))) {
    extent <- c(
      x = as.numeric(offset$dx),
      y = as.numeric(offset$dy),
      width = as.numeric(crop$width),
      height = as.numeric(crop$height)
    )
    if (all(is.finite(extent)) && all(extent[c("width", "height")] > 0)) {
      return(extent)
    }
  }
  wsi_abort("Could not derive `x`, `y`, `width`, and `height` channel extent from `registration`.")
}

#' Create tiled mIHC channel sources from an image or OME-TIFF
#'
#' Creates one dynamic tiled channel source per image page. This is intended for
#' overlaying mIHC channels or probability maps over an H&E base image in
#' [wsi_viewer_live()] without reading the full mIHC image into R memory.
#'
#' @param path mIHC image path readable by libvips, commonly a pyramidal
#'   OME-TIFF.
#' @param pages Zero-based page/channel indices. Defaults to all pages reported
#'   by libvips.
#' @param channel_names Optional display names.
#' @param colours Initial channel colours.
#' @param opacity Initial channel opacity.
#' @param visible Initial visibility.
#' @param tile_size Tile size for dynamic channel tiles.
#' @param format Tile format.
#' @param cache_dir Optional cache directory shared by generated channel tiles.
#' @param route HTTP route prefix used by the live viewer.
#' @param registration Optional registration JSON path or list. A CellPhenotyper
#'   crop/shift file with `crop_bbox_xyxy`, `offset_crop_to_original`, and
#'   `crop_size` is converted to the channel placement extent.
#' @param extent Optional placement extent in H&E/base-slide level-0
#'   coordinates, `c(x, y, width, height)`. Leave `NULL` to fit the channel
#'   layer to the base image width.
#'
#' @return A `wsi_mihc_channel_sources` object for `channel_sources`.
#' @export
wsi_mihc_channel_sources <- function(path, pages = NULL, channel_names = NULL,
                                     colours = NULL, opacity = 0.55,
                                     visible = TRUE, tile_size = 512,
                                     format = c("png", "jpg", "jpeg"),
                                     cache_dir = NULL, route = "/tiles",
                                     registration = NULL, extent = NULL) {
  if (!wsi_has_vips()) {
    wsi_abort("mIHC channel overlays require libvips (`vips` and `vipsheader`) on PATH.", class = "wsi_backend_unavailable")
  }
  path <- wsi_validate_input_path(path)
  format <- wsi_dynamic_tile_format(format)
  extent <- extent %||% wsi_registration_extent(registration)
  page_count <- wsi_vips_tiff_page_count(path)
  if (is.null(pages)) {
    pages <- seq_len(page_count) - 1L
  }
  pages <- as.integer(pages)
  if (!length(pages) || anyNA(pages) || any(pages < 0L) || any(pages >= page_count)) {
    wsi_abort(sprintf("`pages` must be zero-based indices between 0 and %d.", page_count - 1L))
  }
  n <- length(pages)
  channel_names <- channel_names %||% wsi_ome_channel_names(path, pages = pages)
  channel_names <- rep(as.character(channel_names), length.out = n)
  colours <- rep(as.character(colours %||% wsi_channel_palette(n)), length.out = n)
  opacity <- rep(as.numeric(opacity), length.out = n)
  visible <- rep(as.logical(visible), length.out = n)
  shared_cache <- wsi_dynamic_tile_cache_dir(cache_dir)
  dynamic_sources <- vector("list", n)
  for (i in seq_len(n)) {
    source_id <- sprintf(
      "%s_page_%d_%s",
      wsi_safe_id(tools::file_path_sans_ext(basename(path)), "mihc"),
      pages[[i]],
      wsi_safe_id(channel_names[[i]], paste0("channel_", i))
    )
    dynamic_sources[[i]] <- wsi_dynamic_image_tile_source(
      path = path,
      source_id = source_id,
      name = channel_names[[i]],
      page = pages[[i]],
      tile_size = tile_size,
      format = format,
      cache_dir = shared_cache,
      route = route,
      colour = colours[[i]],
      opacity = opacity[[i]],
      visible = isTRUE(visible[[i]]),
      extent = extent,
      metadata = list(source_path = path, source_page = pages[[i]], source_type = "mihc")
    )
  }
  out <- list(
    path = path,
    pages = pages,
    dynamic_sources = dynamic_sources,
    cache_dir = shared_cache
  )
  class(out) <- c("wsi_mihc_channel_sources", "list")
  out
}

#' Open an H&E slide with tiled mIHC channel overlays
#'
#' Convenience wrapper for the common workflow where an H&E WSI is the base
#' OpenSeadragon image and an mIHC OME-TIFF/probability image supplies one or
#' more dynamic tiled overlay channels. By default the H&E base image also gets
#' interactive hematoxylin/eosin/residual deconvolution controls in the
#' `Stains` menu. Pass `stain = "none"` through `...` to disable H&E
#' deconvolution, or pass custom H&E `channels` through `...`. The full images
#' are not loaded into R.
#'
#' @param he H&E slide path or an existing `wsi_slide`.
#' @param mihc mIHC image path readable by libvips.
#' @param pages,channel_names,colours,opacity,visible,extent Passed to
#'   [wsi_mihc_channel_sources()].
#' @param registration Optional registration JSON path or list used to place
#'   the mIHC overlay in H&E coordinates.
#' @param ... Additional arguments passed to [wsi_viewer_live()].
#' @param dynamic_tiles Whether to use the live dynamic tile server for the H&E
#'   base image. When `TRUE`, JPEG tiles are used by default for a faster
#'   brightfield base layer unless `dynamic_tile_format` is supplied in `...`.
#' @param mode Viewer mode; defaults to `"tiles"`.
#'
#' @return A `wsi_viewer_session`.
#' @export
wsi_viewer_he_mihc <- function(he, mihc, pages = NULL, channel_names = NULL,
                               colours = NULL, opacity = 0.55, visible = TRUE,
                               extent = NULL, registration = NULL, ...,
                               dynamic_tiles = TRUE,
                               mode = "tiles") {
  slide <- if (inherits(he, "wsi_slide")) {
    he
  } else {
    wsi_open(he)
  }
  channels <- wsi_mihc_channel_sources(
    mihc,
    pages = pages,
    channel_names = channel_names,
    colours = colours,
    opacity = opacity,
    visible = visible,
    registration = registration,
    extent = extent
  )
  dots <- list(...)
  if (!is.null(dots$channel_sources)) {
    dots$channel_sources <- if (is.list(dots$channel_sources) &&
                                !inherits(dots$channel_sources, c("wsi_channel_source", "wsi_dynamic_tile_source", "wsi_mihc_channel_sources"))) {
      c(dots$channel_sources, list(channels))
    } else {
      list(dots$channel_sources, channels)
    }
  } else {
    dots$channel_sources <- channels
  }
  dots$project_images <- dots$project_images %||% list(
    list(path = slide$path %||% as.character(he), name = "H&E", role = "base"),
    list(path = mihc, name = "mIHC", role = "overlay")
  )
  dots$base_layer_name <- dots$base_layer_name %||% "H&E"
  dots$stain <- dots$stain %||% "he"
  if (isTRUE(dynamic_tiles) && is.null(dots$dynamic_tile_format)) {
    dots$dynamic_tile_format <- "jpg"
  }
  do.call(
    wsi_viewer_live,
    c(
      list(
        slide = slide,
        mode = mode,
        dynamic_tiles = dynamic_tiles
      ),
      dots
    )
  )
}

wsi_channel_settings_from_payload <- function(payload) {
  if (is.null(payload) || !length(payload)) {
    return(wsi_empty_channel_settings())
  }
  rows <- if (is.data.frame(payload)) {
    split(payload, seq_len(nrow(payload)))
  } else {
    payload
  }
  sources <- lapply(rows, function(row) {
    if (is.data.frame(row)) {
      row <- as.list(row[1L, , drop = FALSE])
      row <- lapply(row, function(x) if (length(x)) x[[1L]] else NULL)
    }
    wsi_channel_source_payload(row)
  })
  wsi_channel_settings_from_sources(sources)
}

wsi_channel_update_one <- function(settings, id, patch = list()) {
  if (!is.data.frame(settings) || !nrow(settings)) {
    settings <- wsi_empty_channel_settings()
  }
  id <- wsi_channel_source_id(id)
  idx <- match(id, settings$id)
  if (is.na(idx)) {
    row <- wsi_channel_settings_from_sources(list(utils::modifyList(list(id = id, name = id), patch, keep.null = TRUE)))
    settings <- rbind(as.data.frame(settings), as.data.frame(row))
    class(settings) <- c("wsi_channel_settings", setdiff(class(settings), "wsi_channel_settings"))
    return(settings)
  }
  if (!is.null(patch$visible)) settings$visible[[idx]] <- isTRUE(patch$visible)
  if (!is.null(patch$opacity)) settings$opacity[[idx]] <- wsi_channel_opacity(patch$opacity)
  if (!is.null(patch$colour) || !is.null(patch$color)) settings$colour[[idx]] <- wsi_colour_to_hex(patch$colour %||% patch$color, "colour")
  if (!is.null(patch$gain) || !is.null(patch$strength)) settings$gain[[idx]] <- wsi_channel_gain(patch$gain %||% patch$strength)
  if (!is.null(patch$contrast_min) || !is.null(patch$contrast_max)) {
    contrast <- wsi_channel_contrast(
      patch$contrast_min %||% settings$contrast_min[[idx]],
      patch$contrast_max %||% settings$contrast_max[[idx]]
    )
    settings$contrast_min[[idx]] <- unname(contrast[["min"]])
    settings$contrast_max[[idx]] <- unname(contrast[["max"]])
  }
  settings
}

#' Manage channel tile sources in a live viewer
#'
#' These helpers update channel sources and display settings in a
#' [wsi_viewer_live()] session. They are no-ops for the static HTML viewer
#' because static viewers cannot send changes back to R.
#'
#' @param viewer A `wsi_viewer_session`.
#' @param source A channel source from [wsi_channel_source()].
#' @param id Channel source ID.
#' @param visible,opacity,colour,gain,contrast_min,contrast_max Display setting
#'   values.
#' @param service Whether to service pending live-viewer events after queuing
#'   the command.
#'
#' @return The viewer session, invisibly, except
#'   `wsi_channel_source()` which returns a source object.
#' @name wsi_channel_sources
NULL

#' @rdname wsi_channel_sources
#' @export
wsi_add_channel_source <- function(viewer, source, service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  viewer$add_channel_source(source, service = service)
}

#' @rdname wsi_channel_sources
#' @export
wsi_remove_channel_source <- function(viewer, id, service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  viewer$remove_channel_source(id, service = service)
}

#' @rdname wsi_channel_sources
#' @export
wsi_set_channel_visible <- function(viewer, id, visible = TRUE, service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  viewer$set_channel_visible(id, visible = visible, service = service)
}

#' @rdname wsi_channel_sources
#' @export
wsi_set_channel_opacity <- function(viewer, id, opacity = 1, service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  viewer$set_channel_opacity(id, opacity = opacity, service = service)
}

#' @rdname wsi_channel_sources
#' @export
wsi_set_channel_colour <- function(viewer, id, colour, service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  viewer$set_channel_colour(id, colour = colour, service = service)
}

#' @rdname wsi_channel_sources
#' @export
wsi_set_channel_contrast <- function(viewer, id, contrast_min = 0,
                                     contrast_max = 1, gain = NULL,
                                     service = TRUE) {
  if (!inherits(viewer, "wsi_viewer_session")) {
    wsi_abort("`viewer` must be a `wsi_viewer_session` object.")
  }
  viewer$set_channel_contrast(
    id,
    contrast_min = contrast_min,
    contrast_max = contrast_max,
    gain = gain,
    service = service
  )
}
