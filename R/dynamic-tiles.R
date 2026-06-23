wsi_dynamic_tile_format <- function(format = "png") {
  if (length(format) > 1L) {
    format <- format[[1L]]
  }
  format <- match.arg(format, c("png", "jpg", "jpeg"))
  if (identical(format, "jpeg")) "jpg" else format
}

wsi_dynamic_tile_content_type <- function(format) {
  switch(
    wsi_dynamic_tile_format(format),
    jpg = "image/jpeg",
    png = "image/png"
  )
}

wsi_dynamic_tile_numeric <- function(x, fallback = NULL, min = -Inf, max = Inf) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value)) {
    return(fallback)
  }
  value <- base::min(base::max(value, min), max)
  value
}

wsi_dynamic_tile_query_settings <- function(query = NULL) {
  if (is.null(query) || !nzchar(query)) {
    return(list())
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1L]]
  values <- list()
  for (part in parts) {
    if (!nzchar(part)) {
      next
    }
    kv <- strsplit(part, "=", fixed = TRUE)[[1L]]
    key <- utils::URLdecode(kv[[1L]] %||% "")
    value <- utils::URLdecode(kv[[2L]] %||% "")
    if (nzchar(key)) {
      values[[key]] <- value
    }
  }
  out <- list()
  if (!is.null(values$colour) || !is.null(values$color)) {
    out$colour <- wsi_colour_to_hex(values$colour %||% values$color, "colour")
  }
  gain <- wsi_dynamic_tile_numeric(values$gain, fallback = NULL, min = 0, max = 100)
  if (!is.null(gain)) {
    out$gain <- gain
  }
  contrast_min <- wsi_dynamic_tile_numeric(values$contrast_min, fallback = NULL, min = 0, max = 1)
  contrast_max <- wsi_dynamic_tile_numeric(values$contrast_max, fallback = NULL, min = 0, max = 1)
  if (!is.null(contrast_min)) {
    out$contrast_min <- contrast_min
  }
  if (!is.null(contrast_max)) {
    out$contrast_max <- contrast_max
  }
  out
}

wsi_dynamic_tile_settings_key <- function(settings = list()) {
  if (is.null(settings) || !length(settings)) {
    return("")
  }
  colour <- settings$colour %||% settings$color %||% NULL
  if (!is.null(colour)) {
    colour <- gsub("[^A-Fa-f0-9]", "", wsi_colour_to_hex(colour, "colour"))
  }
  gain <- settings$gain %||% NULL
  cmin <- settings$contrast_min %||% NULL
  cmax <- settings$contrast_max %||% NULL
  key <- paste(
    c(
      if (!is.null(colour)) paste0("c", colour),
      if (!is.null(gain)) paste0("g", format(round(as.numeric(gain), 4), scientific = FALSE, trim = TRUE)),
      if (!is.null(cmin)) paste0("l", format(round(as.numeric(cmin), 4), scientific = FALSE, trim = TRUE)),
      if (!is.null(cmax)) paste0("h", format(round(as.numeric(cmax), 4), scientific = FALSE, trim = TRUE))
    ),
    collapse = "_"
  )
  gsub("[^A-Za-z0-9_]+", "_", key)
}

wsi_dynamic_tile_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- cache_dir %||% tempfile("wsi_dynamic_tiles_")
  if (!dir.exists(cache_dir) &&
      !dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create dynamic tile cache directory: %s", cache_dir))
  }
  normalizePath(cache_dir, winslash = "/", mustWork = TRUE)
}

wsi_dynamic_tile_route <- function(route = "/tiles") {
  if (!is.character(route) || length(route) != 1L || is.na(route) || !nzchar(route)) {
    wsi_abort("`tile_path` must be a single non-empty route.")
  }
  if (!startsWith(route, "/")) {
    route <- paste0("/", route)
  }
  sub("/+$", "", route)
}

#' Create metadata for live on-demand viewer tiles
#'
#' `wsi_dynamic_tile_source()` describes an RGB OpenSeadragon tile endpoint used
#' by [wsi_viewer_live()]. Tiles are generated one region at a time and cached
#' in a temporary directory; the full whole-slide image is never loaded into R.
#'
#' @param slide A `wsi_slide` object.
#' @param slide_id Stable ID used in tile URLs.
#' @param tile_size Tile size in pixels.
#' @param tile_overlap Tile overlap in pixels. A small overlap avoids visible
#'   seams between browser tiles.
#' @param format Tile image format.
#' @param cache_dir Directory for generated tile cache files.
#' @param route HTTP route prefix.
#'
#' @return A `wsi_dynamic_tile_source` object.
#' @export
wsi_dynamic_tile_source <- function(slide, slide_id = NULL, tile_size = 512,
                                    tile_overlap = 1,
                                    format = c("png", "jpg", "jpeg"),
                                    cache_dir = NULL, route = "/tiles") {
  wsi_check_slide(slide)
  format <- wsi_dynamic_tile_format(format)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  if (tile_overlap >= tile_size) {
    wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
  }
  route <- wsi_dynamic_tile_route(route)
  id <- wsi_safe_id(slide_id %||% wsi_slide_id(slide), "slide")
  cache_dir <- wsi_dynamic_tile_cache_dir(cache_dir)
  max_level <- wsi_dz_max_level(slide$dimensions[["width"]], slide$dimensions[["height"]])

  source <- list(
    id = id,
    slide = slide,
    width = unname(as.numeric(slide$dimensions[["width"]])),
    height = unname(as.numeric(slide$dimensions[["height"]])),
    tile_size = tile_size,
    tile_format = format,
    max_level = as.integer(max_level),
    min_level = 0L,
    tile_overlap = tile_overlap,
    route = route,
    cache_dir = cache_dir,
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
  )
  class(source) <- c("wsi_dynamic_tile_source", "list")
  source
}

wsi_vips_image_input <- function(path, page = NULL, subifd = NULL) {
  options <- character()
  if (!is.null(page)) {
    options <- c(options, sprintf("page=%d", as.integer(page)))
  }
  if (!is.null(subifd) && is.finite(subifd) && as.integer(subifd) >= 0L) {
    options <- c(options, sprintf("subifd=%d", as.integer(subifd)))
  }
  if (!length(options)) {
    return(path)
  }
  sprintf("%s[%s]", path, paste(options, collapse = ","))
}

wsi_vips_image_dimensions <- function(path, page = NULL, subifd = NULL) {
  input <- wsi_vips_image_input(path, page = page, subifd = subifd)
  width <- suppressWarnings(as.numeric(wsi_vips_field(input, "width")))
  height <- suppressWarnings(as.numeric(wsi_vips_field(input, "height")))
  if (!is.finite(width) || !is.finite(height) || width <= 0 || height <= 0) {
    return(NULL)
  }
  c(width = width, height = height)
}

wsi_vips_tiff_page_count <- function(path) {
  props <- tryCatch(wsi_vips_properties(path), error = function(err) list())
  pages <- suppressWarnings(as.integer(props[["n-pages"]] %||% 1L))
  if (!is.finite(pages) || pages < 1L) 1L else pages
}

wsi_vips_tiff_subifd_count <- function(path) {
  props <- tryCatch(wsi_vips_properties(path), error = function(err) list())
  subifds <- suppressWarnings(as.integer(props[["n-subifds"]] %||% 0L))
  if (!is.finite(subifds) || subifds < 0L) 0L else subifds
}

wsi_dynamic_image_levels <- function(path, page = 0L) {
  if (!wsi_has_vips()) {
    wsi_abort("Dynamic image channel tiles require libvips (`vips` and `vipsheader`) on PATH.", class = "wsi_backend_unavailable")
  }
  page <- as.integer(wsi_check_scalar_number(page, "page", allow_zero = TRUE))
  base <- wsi_vips_image_dimensions(path, page = page)
  page_option <- TRUE
  if (is.null(base) && page == 0L) {
    base <- wsi_vips_image_dimensions(path)
    page_option <- FALSE
  }
  if (is.null(base)) {
    wsi_abort(sprintf("libvips could not read page %d from `%s`.", page, path))
  }
  levels <- data.frame(
    level = 0L,
    width = unname(base[["width"]]),
    height = unname(base[["height"]]),
    downsample = 1,
    subifd = NA_integer_,
    stringsAsFactors = FALSE
  )
  subifds <- wsi_vips_tiff_subifd_count(path)
  if (subifds > 0L) {
    for (subifd in seq_len(subifds) - 1L) {
      dims <- wsi_vips_image_dimensions(
        path,
        page = if (isTRUE(page_option)) page else NULL,
        subifd = subifd
      )
      if (is.null(dims)) {
        next
      }
      levels <- rbind(
        levels,
        data.frame(
          level = nrow(levels),
          width = unname(dims[["width"]]),
          height = unname(dims[["height"]]),
          downsample = unname(base[["width"]]) / unname(dims[["width"]]),
          subifd = as.integer(subifd),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  attr(levels, "page_option") <- page_option
  levels
}

#' Create an on-demand tiled image channel source
#'
#' `wsi_dynamic_image_tile_source()` creates a live tile source for a large
#' ordinary image, OME-TIFF page, or pyramidal TIFF page. It is designed for
#' overlay channels such as mIHC probability maps. Tiles are cropped from the
#' requested page/subIFD with libvips and cached one tile at a time.
#'
#' @param path Image path readable by libvips.
#' @param source_id Stable source ID used in tile URLs.
#' @param name Human-readable channel name.
#' @param page Zero-based image/OME-TIFF page index.
#' @param tile_size Tile size in pixels.
#' @param tile_overlap Tile overlap in pixels. A small overlap avoids visible
#'   seams between browser tiles.
#' @param format Tile image format.
#' @param cache_dir Directory for generated tile cache files.
#' @param route HTTP route prefix.
#' @param colour,gain,contrast_min,contrast_max Initial server-side colour map.
#' @param visible,opacity Initial browser display settings.
#' @param extent Optional placement extent in base-slide level-0 coordinates,
#'   with `x`, `y`, `width`, and `height`. When `NULL`, the viewer fits the
#'   channel to the base image width.
#' @param metadata Optional source metadata.
#'
#' @return A `wsi_dynamic_image_tile_source` object.
#' @export
wsi_dynamic_image_tile_source <- function(path, source_id = NULL, name = NULL,
                                          page = 0L, tile_size = 512,
                                          tile_overlap = 1,
                                          format = c("png", "jpg", "jpeg"),
                                          cache_dir = NULL, route = "/tiles",
                                          colour = "#ff00ff", gain = 1,
                                          contrast_min = 0, contrast_max = 1,
                                          visible = TRUE, opacity = 0.65,
                                          extent = NULL, metadata = list()) {
  if (!wsi_has_vips()) {
    wsi_abort("Dynamic image channel tiles require libvips (`vips` and `vipsheader`) on PATH.", class = "wsi_backend_unavailable")
  }
  path <- wsi_validate_input_path(path)
  page <- as.integer(wsi_check_scalar_number(page, "page", allow_zero = TRUE))
  format <- wsi_dynamic_tile_format(format)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  if (tile_overlap >= tile_size) {
    wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
  }
  route <- wsi_dynamic_tile_route(route)
  id <- wsi_safe_id(source_id %||% name %||% sprintf("%s_page_%d", tools::file_path_sans_ext(basename(path)), page + 1L), "channel")
  name <- as.character(name %||% id)
  cache_dir <- wsi_dynamic_tile_cache_dir(cache_dir)
  levels <- wsi_dynamic_image_levels(path, page = page)
  page_option <- isTRUE(attr(levels, "page_option", exact = TRUE) %||% TRUE)
  max_level <- wsi_dz_max_level(levels$width[[1L]], levels$height[[1L]])
  colour <- wsi_colour_to_hex(colour, "colour")
  contrast <- wsi_channel_contrast(contrast_min, contrast_max)
  gain <- wsi_channel_gain(gain)
  opacity <- wsi_channel_opacity(opacity)
  if (!is.logical(visible) || length(visible) != 1L || is.na(visible)) {
    wsi_abort("`visible` must be `TRUE` or `FALSE`.")
  }
  if (!is.null(extent)) {
    if (!is.numeric(extent) || length(extent) != 4L || anyNA(extent) || !all(is.finite(extent))) {
      wsi_abort("`extent` must be numeric `c(x, y, width, height)` or `NULL`.")
    }
    names(extent) <- c("x", "y", "width", "height")
    extent <- as.list(as.numeric(extent))
    names(extent) <- c("x", "y", "width", "height")
  }

  source <- list(
    id = id,
    kind = "image",
    path = path,
    page = page,
    page_option = page_option,
    name = name,
    width = unname(as.numeric(levels$width[[1L]])),
    height = unname(as.numeric(levels$height[[1L]])),
    levels = levels,
    tile_size = tile_size,
    tile_format = format,
    max_level = as.integer(max_level),
    min_level = 0L,
    tile_overlap = tile_overlap,
    route = route,
    cache_dir = cache_dir,
    colour = colour,
    gain = gain,
    contrast_min = unname(contrast[["min"]]),
    contrast_max = unname(contrast[["max"]]),
    visible = isTRUE(visible),
    opacity = opacity,
    extent = extent,
    metadata = metadata %||% list(),
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
  )
  class(source) <- c("wsi_dynamic_image_tile_source", "wsi_dynamic_tile_source", "list")
  source
}

#' Create an on-demand tiled CZI section source
#'
#' `wsi_dynamic_czi_section_tile_source()` describes an OpenSeadragon-compatible
#' tile endpoint for one CZI scene/section. Tiles are generated from the native
#' CZI region reader and cached one at a time; the complete CZI scene is not
#' loaded into R memory.
#'
#' @param path CZI file path.
#' @param section A scene row/list with `scene`, `x`, `y`, `width`, and
#'   `height` fields, typically from native CZI metadata.
#' @param source_id Stable tile source ID.
#' @param name Human-readable section name.
#' @param tile_size,tile_overlap Tile size and overlap in pixels.
#' @param format Tile image format.
#' @param cache_dir Tile cache directory.
#' @param route HTTP route prefix.
#' @param channel Zero-based CZI channel index.
#' @param pyramid_factors Optional native pyramid downsample factors.
#' @param metadata Optional source metadata.
#'
#' @return A `wsi_dynamic_czi_section_tile_source` object.
#' @export
wsi_dynamic_czi_section_tile_source <- function(path, section, source_id = NULL,
                                                name = NULL, tile_size = 512,
                                                tile_overlap = 1,
                                                format = c("png", "jpg", "jpeg"),
                                                cache_dir = NULL,
                                                route = "/tiles",
                                                channel = 0,
                                                pyramid_factors = NULL,
                                                metadata = list(),
                                                persistent_reader = TRUE) {
  if (!wsi_has_native_czi()) {
    wsi_abort("Full-resolution CZI section tiles require the optional native CZI backend. Run `wsi_install_native_czi()` first.", class = "wsi_backend_unavailable")
  }
  path <- wsi_validate_input_path(path)
  if (!is.list(section) && !is.data.frame(section)) {
    wsi_abort("`section` must be a CZI scene row or list with `scene`, `x`, `y`, `width`, and `height`.")
  }
  if (is.data.frame(section)) {
    section <- as.list(section[1L, , drop = FALSE])
    section <- lapply(section, function(x) if (length(x)) x[[1L]] else NULL)
  }
  needed <- c("scene", "x", "y", "width", "height")
  missing <- setdiff(needed, names(section))
  if (length(missing)) {
    wsi_abort(sprintf("`section` is missing required field%s: %s", if (length(missing) == 1L) "" else "s", paste(missing, collapse = ", ")))
  }
  format <- wsi_dynamic_tile_format(format)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  if (tile_overlap >= tile_size) {
    wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
  }
  route <- wsi_dynamic_tile_route(route)
  width <- as.numeric(wsi_check_scalar_number(section$width, "section$width", allow_zero = FALSE))
  height <- as.numeric(wsi_check_scalar_number(section$height, "section$height", allow_zero = FALSE))
  scene <- as.integer(wsi_check_scalar_number(section$scene, "section$scene", allow_zero = TRUE))
  id <- wsi_safe_id(source_id %||% sprintf("%s_scene_%d", tools::file_path_sans_ext(basename(path)), scene), "czi")
  name <- as.character(name %||% sprintf("Scene %d", scene))
  cache_dir <- wsi_dynamic_tile_cache_dir(cache_dir)
  pyramid_factors <- suppressWarnings(as.numeric(pyramid_factors %||% numeric()))
  pyramid_factors <- sort(unique(pyramid_factors[is.finite(pyramid_factors) & pyramid_factors > 1]))
  levels <- data.frame(
    level = seq_len(length(pyramid_factors) + 1L) - 1L,
    width = ceiling(width / c(1, pyramid_factors)),
    height = ceiling(height / c(1, pyramid_factors)),
    downsample = c(1, pyramid_factors),
    subifd = NA_integer_,
    stringsAsFactors = FALSE
  )
  source <- list(
    id = id,
    kind = "czi_section",
    path = path,
    scene = scene,
    channel = as.integer(channel),
    name = name,
    x = as.numeric(section$x),
    y = as.numeric(section$y),
    width = unname(width),
    height = unname(height),
    levels = levels,
    tile_size = tile_size,
    tile_format = format,
    max_level = as.integer(wsi_dz_max_level(width, height)),
    min_level = 0L,
    tile_overlap = tile_overlap,
    route = route,
    cache_dir = cache_dir,
    czi_handle = NULL,
    metadata = metadata %||% list(),
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
  )
  persistent_reader <- isTRUE(persistent_reader) &&
    !tolower(Sys.getenv("WSITOOLS_CZI_PERSISTENT_TILE_READER", unset = "true")) %in% c("0", "false", "no", "off")
  if (isTRUE(persistent_reader) && wsi_native_available("wsi_native_czi_open_handle")) {
    source$czi_handle <- tryCatch(
      wsi_native_czi_open_handle(path),
      error = function(err) {
        cli::cli_warn(c(
          "Could not open persistent native CZI tile reader; falling back to per-tile reader setup.",
          "x" = conditionMessage(err)
        ))
        NULL
      }
    )
  }
  class(source) <- c("wsi_dynamic_czi_section_tile_source", "wsi_dynamic_tile_source", "list")
  source
}

#' Create tiled stain-deconvolution channel sources
#'
#' `wsi_stain_channel_sources()` exposes brightfield deconvolution channels as
#' live tiled OpenSeadragon layers. Each tile is read as a small region,
#' deconvolved, colourised, cached, and served back to the viewer. The full
#' whole-slide image is never loaded into R memory.
#'
#' @param slide A `wsi_slide` object.
#' @param stain Stain model, `"he"` for hematoxylin/eosin/residual or `"ihc"`
#'   for hematoxylin plus HRP/DAB-style channels.
#' @param channels Optional custom stain channels created by
#'   [wsi_stain_channels()] or [wsi_he_stain_channels()].
#' @param source_prefix Prefix used for generated tile-source IDs.
#' @param tile_size,tile_overlap Tile size and overlap in pixels.
#' @param format Tile image format. PNG is recommended because stain overlays
#'   use alpha.
#' @param cache_dir Tile cache directory.
#' @param route HTTP route prefix.
#' @param opacity Initial overlay opacity.
#' @param metadata Optional metadata added to each channel source.
#'
#' @return A `wsi_stain_channel_sources` object for `channel_sources`.
#' @export
wsi_stain_channel_sources <- function(slide, stain = c("he", "ihc"),
                                      channels = NULL,
                                      source_prefix = NULL,
                                      tile_size = 512,
                                      tile_overlap = 1,
                                      format = c("png", "jpg", "jpeg"),
                                      cache_dir = NULL,
                                      route = "/tiles",
                                      opacity = 0.9,
                                      metadata = list()) {
  wsi_check_slide(slide)
  stain <- match.arg(stain)
  format <- wsi_dynamic_tile_format(format)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  if (tile_overlap >= tile_size) {
    wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
  }
  route <- wsi_dynamic_tile_route(route)
  opacity <- wsi_channel_opacity(opacity)
  channel_definitions <- if (is.null(channels)) {
    if (identical(stain, "he")) {
      wsi_he_stain_channels()
    } else {
      wsi_stain_channels()
    }
  } else {
    wsi_as_stain_channels(channels)
  }
  if (!length(channel_definitions)) {
    wsi_abort("At least one stain channel is required.")
  }
  cache_dir <- wsi_dynamic_tile_cache_dir(cache_dir)
  slide_id <- wsi_safe_id(source_prefix %||% wsi_slide_id(slide), "slide")
  max_level <- wsi_dz_max_level(slide$dimensions[["width"]], slide$dimensions[["height"]])
  width <- unname(as.numeric(slide$dimensions[["width"]]))
  height <- unname(as.numeric(slide$dimensions[["height"]]))

  dynamic_sources <- lapply(seq_along(channel_definitions), function(i) {
    channel <- channel_definitions[[i]]
    channel_id <- wsi_channel_source_id(channel$id %||% NULL, channel$name %||% NULL)
    id <- wsi_safe_id(paste(slide_id, "stain", channel_id, sep = "_"), "stain")
    source <- list(
      id = id,
      kind = "stain_channel",
      slide = slide,
      stain_type = stain,
      channels = channel_definitions,
      channel_index = as.integer(i),
      channel_id = channel_id,
      name = channel$name %||% channel_id,
      width = width,
      height = height,
      tile_size = tile_size,
      tile_format = format,
      max_level = as.integer(max_level),
      min_level = 0L,
      tile_overlap = tile_overlap,
      route = route,
      cache_dir = cache_dir,
      colour = channel$colour %||% "#ffffff",
      gain = channel$strength %||% channel$gain %||% 1,
      contrast_min = channel$contrast_min %||% 0,
      contrast_max = channel$contrast_max %||% 1,
      visible = FALSE,
      opacity = opacity,
      metadata = utils::modifyList(
        metadata %||% list(),
        list(
          source_path = slide$path %||% NULL,
          target_path = slide$path %||% NULL,
          slide_path = slide$path %||% NULL,
          source_type = "stain_deconvolution",
          kind = "stain_channel",
          stain_type = stain,
          stain_channel_id = channel_id,
          stain_channel_index = as.integer(i),
          server_colourized = TRUE
        ),
        keep.null = TRUE
      ),
      created = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z")
    )
    class(source) <- c("wsi_dynamic_stain_channel_tile_source", "wsi_dynamic_tile_source", "list")
    source
  })

  out <- list(
    stain = stain,
    channels = channel_definitions,
    dynamic_sources = dynamic_sources,
    cache_dir = cache_dir
  )
  class(out) <- c("wsi_stain_channel_sources", "list")
  out
}

wsi_dynamic_tile_metadata <- function(source, base_url = NULL) {
  if (!inherits(source, "wsi_dynamic_tile_source")) {
    wsi_abort("`source` must be a `wsi_dynamic_tile_source` object.")
  }
  tile_path <- paste0(source$route, "/", source$id)
  template <- paste0(tile_path, "/{level}/{x}/{y}.{format}")
  if (!is.null(base_url)) {
    base <- sub("/+$", "", base_url)
    template <- paste0(base, template)
    tile_path <- paste0(base, tile_path)
  }
  list(
    id = source$id,
    type = "dynamic",
    width = source$width,
    height = source$height,
    tile_size = source$tile_size,
    tile_format = source$tile_format,
    tile_url_base = tile_path,
    tile_url_template = template,
    tile_url_style = "slash",
    tile_overlap = source$tile_overlap,
    min_level = source$min_level,
    max_level = source$max_level,
    cache_dir = source$cache_dir,
    kind = source$kind %||% "slide",
    name = source$name %||% source$id,
    page = source$page %||% NULL,
    colour = source$colour %||% NULL,
    gain = source$gain %||% NULL,
    contrast_min = source$contrast_min %||% NULL,
    contrast_max = source$contrast_max %||% NULL,
    visible = source$visible %||% TRUE,
    opacity = source$opacity %||% 1,
    extent = source$extent %||% NULL,
    cache_key = wsi_safe_id(source$created %||% as.character(Sys.time()), "tile_cache"),
    metadata = source$metadata %||% list()
  )
}

wsi_dynamic_tile_parse <- function(path, route = "/tiles") {
  route <- wsi_dynamic_tile_route(route)
  pattern <- paste0(
    "^",
    gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", route),
    "/([^/]+)/([0-9]+)/([0-9]+)/([0-9]+)\\.([A-Za-z0-9]+)$"
  )
  match <- regexec(pattern, path)
  parts <- regmatches(path, match)[[1L]]
  if (length(parts) != 6L) {
    return(NULL)
  }
  list(
    slide_id = parts[[2L]],
    level = as.integer(parts[[3L]]),
    x = as.integer(parts[[4L]]),
    y = as.integer(parts[[5L]]),
    format = wsi_dynamic_tile_format(tolower(parts[[6L]]))
  )
}

wsi_dynamic_native_level <- function(slide, downsample) {
  levels <- wsi_levels(slide)
  if (!nrow(levels) || !all(c("level", "downsample") %in% names(levels))) {
    return(list(level = 0L, downsample = 1))
  }
  ds <- suppressWarnings(as.numeric(levels$downsample))
  ds[!is.finite(ds) | ds <= 0] <- 1
  idx <- which.min(abs(log(ds / max(downsample, 1e-9))))
  list(level = as.integer(levels$level[[idx]]), downsample = ds[[idx]])
}

wsi_dynamic_source_native_level <- function(source, downsample) {
  if (identical(source$kind %||% "slide", "czi_section")) {
    levels <- source$levels
    if (!is.data.frame(levels) || !nrow(levels)) {
      return(list(level = 0L, downsample = 1, subifd = NA_integer_))
    }
    ds <- suppressWarnings(as.numeric(levels$downsample))
    ds[!is.finite(ds) | ds <= 0] <- 1
    idx <- which.min(abs(log(ds / max(downsample, 1e-9))))
    return(list(
      level = as.integer(levels$level[[idx]]),
      downsample = ds[[idx]],
      subifd = NA_integer_
    ))
  }
  if (identical(source$kind %||% "slide", "image")) {
    levels <- source$levels
    if (!is.data.frame(levels) || !nrow(levels)) {
      return(list(level = 0L, downsample = 1, subifd = NA_integer_))
    }
    ds <- suppressWarnings(as.numeric(levels$downsample))
    ds[!is.finite(ds) | ds <= 0] <- 1
    idx <- which.min(abs(log(ds / max(downsample, 1e-9))))
    return(list(
      level = as.integer(levels$level[[idx]]),
      downsample = ds[[idx]],
      subifd = suppressWarnings(as.integer(levels$subifd[[idx]] %||% NA_integer_))
    ))
  }
  native <- wsi_dynamic_native_level(source$slide, downsample)
  native$subifd <- NA_integer_
  native
}

wsi_dynamic_tile_region <- function(source, level, col, row) {
  if (!inherits(source, "wsi_dynamic_tile_source")) {
    wsi_abort("`source` must be a `wsi_dynamic_tile_source` object.")
  }
  level <- as.integer(level)
  col <- as.integer(col)
  row <- as.integer(row)
  if (anyNA(c(level, col, row)) || level < source$min_level || level > source$max_level ||
      col < 0L || row < 0L) {
    wsi_abort("Invalid dynamic tile coordinates.")
  }
  downsample <- 2^(source$max_level - level)
  level_width <- ceiling(source$width / downsample)
  level_height <- ceiling(source$height / downsample)
  level_x <- col * source$tile_size
  level_y <- row * source$tile_size
  if (level_x >= level_width || level_y >= level_height) {
    wsi_abort("Requested dynamic tile is outside the slide bounds.", class = "wsi_region_out_of_bounds")
  }

  nominal_width <- as.integer(min(source$tile_size, level_width - level_x))
  nominal_height <- as.integer(min(source$tile_size, level_height - level_y))
  overlap <- as.integer(source$tile_overlap %||% 0L)
  level_left <- max(0, level_x - overlap)
  level_top <- max(0, level_y - overlap)
  level_right <- min(level_width, level_x + nominal_width + overlap)
  level_bottom <- min(level_height, level_y + nominal_height + overlap)
  desired_width <- as.integer(level_right - level_left)
  desired_height <- as.integer(level_bottom - level_top)
  x <- level_left * downsample
  y <- level_top * downsample
  coverage_width <- min(desired_width * downsample, source$width - x)
  coverage_height <- min(desired_height * downsample, source$height - y)
  native <- wsi_dynamic_source_native_level(source, downsample)
  native_width <- max(1L, as.integer(ceiling(coverage_width / native$downsample)))
  native_height <- max(1L, as.integer(ceiling(coverage_height / native$downsample)))

  list(
    x = as.integer(round(x)),
    y = as.integer(round(y)),
    width = native_width,
    height = native_height,
    level = native$level,
    subifd = native$subifd %||% NA_integer_,
    downsample = native$downsample,
    desired_width = desired_width,
    desired_height = desired_height,
    deepzoom_downsample = downsample
  )
}

wsi_dynamic_tile_cache_file <- function(source, level, col, row, format = NULL,
                                        settings = list()) {
  format <- wsi_dynamic_tile_format(format %||% source$tile_format)
  dir <- file.path(source$cache_dir, source$id, as.character(level), as.character(col))
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create dynamic tile cache subdirectory: %s", dir))
  }
  kind <- source$kind %||% "slide"
  key <- if (kind %in% c("image", "stain_channel")) {
    wsi_dynamic_tile_settings_key(settings)
  } else {
    ""
  }
  if (kind %in% c("image", "czi_section", "stain_channel")) {
    prefix <- if (identical(kind, "stain_channel")) "stain_v1" else "r2"
    key <- paste(c(prefix, key[nzchar(key)]), collapse = "_")
  }
  suffix <- if (nzchar(key)) paste0("_", key) else ""
  file.path(dir, sprintf("%s%s.%s", row, suffix, format))
}

wsi_dynamic_tile_region_is_native <- function(region) {
  isTRUE(
    is.list(region) &&
      is.finite(region$downsample) &&
      is.finite(region$deepzoom_downsample) &&
      abs(region$downsample - region$deepzoom_downsample) <=
        max(1e-6, region$deepzoom_downsample * 1e-6) &&
      as.integer(region$width) == as.integer(region$desired_width) &&
      as.integer(region$height) == as.integer(region$desired_height)
  )
}

wsi_dynamic_write_mock_tile <- function(file, width, height, format = "png") {
  format <- wsi_dynamic_tile_format(format)
  device <- if (identical(format, "jpg")) grDevices::jpeg else grDevices::png
  device(file, width = width, height = height, units = "px", bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, width), ylim = c(0, height), asp = 1)
  graphics::rect(0, 0, width, height, col = "#f2f2f2", border = NA)
  graphics::segments(0, 0, width, height, col = "#d9d9d9", lwd = 2)
  graphics::segments(width, 0, 0, height, col = "#d9d9d9", lwd = 2)
  invisible(file)
}

wsi_dynamic_resize_tile <- function(input, output, width, height, format) {
  if (wsi_has_vips()) {
    in_width <- suppressWarnings(as.numeric(wsi_vips_field(input, "width")))
    in_height <- suppressWarnings(as.numeric(wsi_vips_field(input, "height")))
    if (is.finite(in_width) && is.finite(in_height) && in_width > 0 && in_height > 0) {
      scale <- width / in_width
      vscale <- height / in_height
      wsi_run_command(
        "vips",
        args = c("resize", input, output, as.character(scale), "--vscale", as.character(vscale)),
        error_message = "libvips failed to resize a dynamic viewer tile."
      )
      return(invisible(output))
    }
  }
  wsi_require_magick("resize a generated viewer tile")
  image <- magick::image_read(input)
  image <- magick::image_resize(image, sprintf("%dx%d!", as.integer(width), as.integer(height)))
  magick::image_write(image, path = output, format = format)
  invisible(output)
}

wsi_dynamic_ensure_rgb_tile <- function(file, format) {
  if (!wsi_has_vips()) {
    return(invisible(file))
  }
  bands <- suppressWarnings(as.integer(wsi_vips_field(file, "bands")))
  if (!is.finite(bands) || bands <= 3L) {
    return(invisible(file))
  }
  format <- wsi_dynamic_tile_format(format)
  tmp_rgb <- tempfile(fileext = paste0(".", format), tmpdir = dirname(file))
  on.exit(unlink(tmp_rgb), add = TRUE)
  wsi_run_command(
    "vips",
    args = c("extract_band", file, tmp_rgb, "0", "--n", "3"),
    error_message = "libvips failed to convert a dynamic viewer tile to RGB."
  )
  if (!file.copy(tmp_rgb, file, overwrite = TRUE)) {
    wsi_abort(sprintf("Could not update dynamic tile cache file: %s", file))
  }
  invisible(file)
}

wsi_dynamic_level_cache_max_pixels <- function() {
  value <- suppressWarnings(as.numeric(Sys.getenv(
    "WSITOOLS_DYNAMIC_TILE_LEVEL_CACHE_MAX_PIXELS",
    unset = "6000000"
  )))
  if (!is.finite(value) || value <= 0) {
    value <- 6000000
  }
  value
}

wsi_dynamic_can_cache_vips_level <- function(source, level, region) {
  identical(source$kind %||% "slide", "slide") &&
    isTRUE(wsi_has_vips()) &&
    identical(source$slide$backend %||% "", "vips") &&
    is.character(source$slide$path) &&
    length(source$slide$path) == 1L &&
    nzchar(source$slide$path) &&
    file.exists(source$slide$path) &&
    is.finite(region$deepzoom_downsample) &&
    region$deepzoom_downsample >= 4 &&
    is.finite(source$width) &&
    is.finite(source$height) &&
    ceiling(source$width / region$deepzoom_downsample) *
      ceiling(source$height / region$deepzoom_downsample) <=
      wsi_dynamic_level_cache_max_pixels()
}

wsi_dynamic_vips_level_cache_file <- function(source, level, region) {
  level_dir <- file.path(source$cache_dir, source$id, "_levels")
  if (!dir.exists(level_dir) &&
      !dir.create(level_dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create dynamic tile level-cache directory: %s", level_dir))
  }
  level_file <- file.path(level_dir, sprintf("level_%d.tif", as.integer(level)))
  if (file.exists(level_file) && file.info(level_file)$size > 0) {
    return(level_file)
  }

  level_width <- ceiling(source$width / region$deepzoom_downsample)
  level_height <- ceiling(source$height / region$deepzoom_downsample)
  scale <- level_width / source$width
  vscale <- level_height / source$height
  tmp <- tempfile(fileext = ".tif", tmpdir = level_dir)
  on.exit(unlink(tmp), add = TRUE)
  wsi_run_command(
    "vips",
    args = c(
      "resize",
      source$slide$path,
      tmp,
      format(scale, scientific = FALSE, trim = TRUE),
      "--vscale",
      format(vscale, scientific = FALSE, trim = TRUE)
    ),
    error_message = "libvips failed to create a cached low-resolution viewer tile level."
  )

  if (!file.exists(level_file)) {
    if (!file.rename(tmp, level_file)) {
      if (!file.copy(tmp, level_file, overwrite = FALSE)) {
        wsi_abort(sprintf("Could not create dynamic tile level-cache file: %s", level_file))
      }
    }
  }
  level_file
}

wsi_dynamic_vips_cached_level_tile_to_file <- function(source, level, region, output, format) {
  if (!wsi_dynamic_can_cache_vips_level(source, level, region)) {
    return(FALSE)
  }
  level_file <- wsi_dynamic_vips_level_cache_file(source, level, region)
  crop_x <- as.integer(floor(region$x / region$deepzoom_downsample))
  crop_y <- as.integer(floor(region$y / region$deepzoom_downsample))
  wsi_run_command(
    "vips",
    args = c(
      "crop",
      level_file,
      output,
      as.character(crop_x),
      as.character(crop_y),
      as.character(region$desired_width),
      as.character(region$desired_height)
    ),
    error_message = "libvips failed to crop a cached low-resolution viewer tile."
  )
  wsi_dynamic_ensure_rgb_tile(output, format)
  TRUE
}

wsi_dynamic_image_tile_settings <- function(source, settings = list()) {
  settings <- settings %||% list()
  contrast <- wsi_channel_contrast(
    settings$contrast_min %||% source$contrast_min %||% 0,
    settings$contrast_max %||% source$contrast_max %||% 1
  )
  list(
    colour = wsi_colour_to_hex(settings$colour %||% settings$color %||% source$colour %||% "#ffffff", "colour"),
    gain = wsi_channel_gain(settings$gain %||% source$gain %||% 1),
    contrast_min = unname(contrast[["min"]]),
    contrast_max = unname(contrast[["max"]])
  )
}

wsi_dynamic_colourize_tile <- function(input, output, source, settings = list()) {
  settings <- wsi_dynamic_image_tile_settings(source, settings)
  rgb <- grDevices::col2rgb(settings$colour)[, 1L]
  lo <- settings$contrast_min
  hi <- settings$contrast_max
  scale <- settings$gain / max(hi - lo, 1e-6)
  offset <- -255 * lo * scale
  current <- input
  bands <- suppressWarnings(as.integer(wsi_vips_field(input, "bands")))
  if (is.finite(bands) && bands > 1L) {
    tmp_gray <- tempfile(fileext = ".png", tmpdir = dirname(output))
    on.exit(unlink(tmp_gray), add = TRUE)
    wsi_run_command(
      "vips",
      args = c("extract_band", input, tmp_gray, "0", "--n", "1"),
      error_message = "libvips failed to extract a scalar dynamic channel tile."
    )
    current <- tmp_gray
  }

  tmp_alpha_float <- tempfile(fileext = ".tif", tmpdir = dirname(output))
  tmp_alpha <- tempfile(fileext = ".tif", tmpdir = dirname(output))
  tmp_bands <- tempfile(pattern = paste0("channel_band_", seq_along(rgb), "_"), fileext = ".tif", tmpdir = dirname(output))
  tmp_joined <- tempfile(fileext = ".tif", tmpdir = dirname(output))
  on.exit(unlink(c(tmp_alpha_float, tmp_alpha, tmp_bands, tmp_joined)), add = TRUE)

  wsi_run_command(
    "vips",
    args = c(
      "linear",
      current,
      tmp_alpha_float,
      format(scale, scientific = FALSE, trim = TRUE),
      "--",
      format(offset, scientific = FALSE, trim = TRUE)
    ),
    error_message = "libvips failed to scale a dynamic channel tile."
  )
  wsi_run_command(
    "vips",
    args = c("cast", tmp_alpha_float, tmp_alpha, "uchar"),
    error_message = "libvips failed to cast a dynamic channel tile."
  )

  width <- suppressWarnings(as.integer(wsi_vips_field(tmp_alpha, "width")))
  height <- suppressWarnings(as.integer(wsi_vips_field(tmp_alpha, "height")))
  if (!is.finite(width) || !is.finite(height) || width <= 0L || height <= 0L) {
    wsi_abort("libvips did not report dimensions for a dynamic channel tile.")
  }

  for (i in seq_along(rgb)) {
    wsi_run_command(
      "vips",
      args = c(
        "linear",
        tmp_alpha,
        tmp_bands[[i]],
        format(rgb[[i]] / 255, scientific = FALSE, trim = TRUE),
        "0"
      ),
      error_message = "libvips failed to colourise a dynamic channel tile."
    )
  }

  if (identical(wsi_dynamic_tile_format(tools::file_ext(output) %||% source$tile_format), "png")) {
    wsi_run_command(
      "vips",
      args = c("bandjoin", paste(c(tmp_bands, tmp_alpha), collapse = " "), tmp_joined),
      error_message = "libvips failed to add alpha to a dynamic channel tile."
    )
    wsi_run_command(
      "vips",
      args = c("copy", tmp_joined, output, "--interpretation", "srgb"),
      error_message = "libvips failed to write a dynamic channel tile."
    )
    return(invisible(output))
  }

  wsi_run_command(
    "vips",
    args = c("bandjoin", paste(tmp_bands, collapse = " "), tmp_joined),
    error_message = "libvips failed to write a dynamic channel tile."
  )
  wsi_run_command(
    "vips",
    args = c("copy", tmp_joined, output, "--interpretation", "srgb"),
    error_message = "libvips failed to write a dynamic channel tile."
  )
  invisible(output)
}

wsi_dynamic_image_region_to_file <- function(source, region, output, settings = list()) {
  if (!wsi_has_vips()) {
    wsi_abort("Dynamic image channel tiles require libvips (`vips` and `vipsheader`) on PATH.", class = "wsi_backend_unavailable")
  }
  subifd <- region$subifd
  if (!is.null(subifd) && (!is.finite(subifd) || subifd < 0L)) {
    subifd <- NULL
  }
  input <- wsi_vips_image_input(
    source$path,
    page = if (isTRUE(source$page_option %||% TRUE)) source$page %||% 0L else NULL,
    subifd = subifd
  )
  crop_x <- as.integer(floor(region$x / region$downsample))
  crop_y <- as.integer(floor(region$y / region$downsample))
  tmp_crop <- tempfile(fileext = ".png", tmpdir = dirname(output))
  tmp_resized <- tempfile(fileext = ".png", tmpdir = dirname(output))
  on.exit(unlink(c(tmp_crop, tmp_resized)), add = TRUE)
  wsi_run_command(
    "vips",
    args = c(
      "crop",
      input,
      tmp_crop,
      as.character(crop_x),
      as.character(crop_y),
      as.character(region$width),
      as.character(region$height)
    ),
    error_message = "libvips failed to crop a dynamic channel tile."
  )
  current <- tmp_crop
  current_width <- suppressWarnings(as.numeric(wsi_vips_field(current, "width")))
  current_height <- suppressWarnings(as.numeric(wsi_vips_field(current, "height")))
  needs_resize <- !is.finite(current_width) || !is.finite(current_height) ||
    as.integer(round(current_width)) != region$desired_width ||
    as.integer(round(current_height)) != region$desired_height
  if (isTRUE(needs_resize)) {
    wsi_dynamic_resize_tile(current, tmp_resized, region$desired_width, region$desired_height, "png")
    current <- tmp_resized
  }
  wsi_dynamic_colourize_tile(current, output, source, settings = settings)
  invisible(output)
}

wsi_dynamic_array_to_file <- function(array, output, format = "png") {
  format <- wsi_dynamic_tile_format(format)
  dims <- dim(array)
  if (length(dims) != 3L || dims[[1L]] <= 0L || dims[[2L]] <= 0L || dims[[3L]] < 3L) {
    wsi_abort("Dynamic CZI tile decoding did not return an RGB array.")
  }
  if (identical(format, "jpg")) {
    grDevices::jpeg(output, width = dims[[2L]], height = dims[[1L]], units = "px", bg = "white", quality = 92)
  } else {
    grDevices::png(output, width = dims[[2L]], height = dims[[1L]], units = "px", bg = "white")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, dims[[2L]]), ylim = c(0, dims[[1L]]), asp = NA)
  graphics::rasterImage(wsi_array_to_raster(array), 0, 0, dims[[2L]], dims[[1L]], interpolate = FALSE)
  invisible(output)
}

wsi_dynamic_rgba_to_file <- function(array, output, format = "png") {
  format <- wsi_dynamic_tile_format(format)
  dims <- dim(array)
  if (length(dims) != 3L || dims[[1L]] <= 0L || dims[[2L]] <= 0L || dims[[3L]] < 4L) {
    wsi_abort("Dynamic stain channel tiles require an RGBA array.")
  }
  if (identical(format, "jpg")) {
    grDevices::jpeg(output, width = dims[[2L]], height = dims[[1L]], units = "px", bg = "white", quality = 92)
  } else {
    grDevices::png(output, width = dims[[2L]], height = dims[[1L]], units = "px", bg = "transparent")
  }
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, dims[[2L]]), ylim = c(0, dims[[1L]]), asp = NA)
  graphics::rasterImage(wsi_array_to_raster(array), 0, 0, dims[[2L]], dims[[1L]], interpolate = FALSE)
  invisible(output)
}

wsi_dynamic_czi_section_region_to_file <- function(source, region, output, format = "png") {
  full_width <- max(1L, as.integer(ceiling(region$width * region$downsample)))
  full_height <- max(1L, as.integer(ceiling(region$height * region$downsample)))
  reader <- source$czi_handle %||% NULL
  array <- if (!is.null(reader)) {
    wsi_native_czi_handle_read_region(
      reader,
      x = as.integer(round(source$x + region$x)),
      y = as.integer(round(source$y + region$y)),
      width = full_width,
      height = full_height,
      zoom = 1 / max(region$downsample, 1e-9),
      channel = source$channel %||% 0L,
      scene = NA_integer_
    )
  } else {
    wsi_native_czi_read_region(
      source$path,
      x = as.integer(round(source$x + region$x)),
      y = as.integer(round(source$y + region$y)),
      width = full_width,
      height = full_height,
      zoom = 1 / max(region$downsample, 1e-9),
      channel = source$channel %||% 0L,
      scene = NA_integer_
    )
  }
  tmp <- tempfile(fileext = paste0(".", format), tmpdir = dirname(output))
  on.exit(unlink(tmp), add = TRUE)
  wsi_dynamic_array_to_file(array, tmp, format = format)
  current_width <- suppressWarnings(as.numeric(wsi_vips_field(tmp, "width")))
  current_height <- suppressWarnings(as.numeric(wsi_vips_field(tmp, "height")))
  needs_resize <- !is.finite(current_width) || !is.finite(current_height) ||
    as.integer(round(current_width)) != region$desired_width ||
    as.integer(round(current_height)) != region$desired_height
  if (isTRUE(needs_resize)) {
    wsi_dynamic_resize_tile(tmp, output, region$desired_width, region$desired_height, format)
  } else if (!file.copy(tmp, output, overwrite = TRUE)) {
    wsi_abort(sprintf("Could not write dynamic CZI tile cache file: %s", output))
  }
  invisible(output)
}

wsi_dynamic_stain_channel_matrix <- function(source, image) {
  channels <- source$channels
  if (!inherits(channels, "wsi_stain_channels")) {
    class(channels) <- "wsi_stain_channels"
  }
  values <- if (identical(source$stain_type %||% "he", "he")) {
    wsi_deconvolve_two_stain_array(image, channels, epsilon = 1 / 255, include_residual = TRUE)
  } else {
    wsi_deconvolve_array(image, channels, epsilon = 1 / 255)
  }
  channel_id <- source$channel_id %||% NULL
  channel <- values[[channel_id]]
  if (is.null(channel)) {
    ids <- wsi_channel_ids_from_output(values)
    idx <- as.integer(source$channel_index %||% 1L)
    channel <- values[[ids[[idx]]]]
  }
  if (is.null(channel)) {
    wsi_abort("Requested stain channel was not produced by the deconvolution.")
  }
  channel
}

wsi_dynamic_stain_channel_rgba <- function(channel, source, settings = list()) {
  settings <- wsi_dynamic_image_tile_settings(source, settings)
  rgb <- grDevices::col2rgb(settings$colour)[, 1L] / 255
  lo <- settings$contrast_min
  hi <- settings$contrast_max
  values <- pmax(0, channel)
  values[!is.finite(values)] <- 0
  scaled <- pmin(1, pmax(0, (values - lo) / max(hi - lo, 1e-6)))
  intensity <- pmin(1, pmax(0, 1 - exp(-scaled * max(settings$gain, 0) * 2.4)))
  dims <- dim(channel)
  out <- array(0, dim = c(dims[[1L]], dims[[2L]], 4L))
  for (i in seq_len(3L)) {
    out[, , i] <- rgb[[i]]
  }
  out[, , 4L] <- intensity
  out
}

wsi_dynamic_stain_channel_region_to_file <- function(source, region, output,
                                                     format = "png",
                                                     settings = list()) {
  image <- wsi_read_region(
    source$slide,
    x = region$x,
    y = region$y,
    width = region$width,
    height = region$height,
    level = region$level,
    format = "array"
  )
  channel <- wsi_dynamic_stain_channel_matrix(source, image)
  rgba <- wsi_dynamic_stain_channel_rgba(channel, source, settings = settings)
  tmp <- tempfile(fileext = paste0(".", format), tmpdir = dirname(output))
  on.exit(unlink(tmp), add = TRUE)
  wsi_dynamic_rgba_to_file(rgba, tmp, format = format)
  current_width <- dim(rgba)[[2L]]
  current_height <- dim(rgba)[[1L]]
  needs_resize <- !is.finite(current_width) || !is.finite(current_height) ||
    as.integer(round(current_width)) != region$desired_width ||
    as.integer(round(current_height)) != region$desired_height
  if (isTRUE(needs_resize)) {
    wsi_dynamic_resize_tile(tmp, output, region$desired_width, region$desired_height, format)
  } else if (!file.copy(tmp, output, overwrite = TRUE)) {
    wsi_abort(sprintf("Could not write dynamic stain channel tile cache file: %s", output))
  }
  invisible(output)
}

wsi_dynamic_tile_file <- function(source, level, col, row, format = NULL,
                                  settings = list()) {
  format <- wsi_dynamic_tile_format(format %||% source$tile_format)
  output <- wsi_dynamic_tile_cache_file(source, level, col, row, format, settings = settings)
  if (file.exists(output) && file.info(output)$size > 0) {
    return(output)
  }
  region <- wsi_dynamic_tile_region(source, level, col, row)
  tmp <- tempfile(fileext = ".png", tmpdir = dirname(output))
  on.exit(unlink(tmp), add = TRUE)

  if (identical(source$kind %||% "slide", "image")) {
    wsi_dynamic_image_region_to_file(source, region, output, settings = settings)
    return(output)
  }
  if (identical(source$kind %||% "slide", "czi_section")) {
    wsi_dynamic_czi_section_region_to_file(source, region, output, format = format)
    return(output)
  }
  if (identical(source$kind %||% "slide", "stain_channel")) {
    wsi_dynamic_stain_channel_region_to_file(source, region, output, format = format, settings = settings)
    return(output)
  }

  if (identical(source$slide$backend, "mock")) {
    wsi_dynamic_write_mock_tile(output, region$desired_width, region$desired_height, format = format)
    return(output)
  }

  if (wsi_dynamic_vips_cached_level_tile_to_file(source, level, region, output, format)) {
    return(output)
  }

  read_region <- list(
    x = region$x,
    y = region$y,
    width = region$width,
    height = region$height,
    level = region$level,
    downsample = region$downsample
  )
  if (wsi_has_vips() && wsi_dynamic_tile_region_is_native(region)) {
    wsi_region_to_file(source$slide, read_region, output, backend = "vips")
    wsi_dynamic_ensure_rgb_tile(output, format)
    return(output)
  }
  wsi_region_to_file(source$slide, read_region, tmp, backend = "auto")

  current_width <- suppressWarnings(as.numeric(wsi_vips_field(tmp, "width")))
  current_height <- suppressWarnings(as.numeric(wsi_vips_field(tmp, "height")))
  needs_resize <- !is.finite(current_width) || !is.finite(current_height) ||
    as.integer(round(current_width)) != region$desired_width ||
    as.integer(round(current_height)) != region$desired_height

  if (isTRUE(needs_resize) || !identical(format, "png")) {
    wsi_dynamic_resize_tile(tmp, output, region$desired_width, region$desired_height, format)
  } else if (!file.copy(tmp, output, overwrite = TRUE)) {
    wsi_abort(sprintf("Could not write dynamic tile cache file: %s", output))
  }
  wsi_dynamic_ensure_rgb_tile(output, format)
  output
}

wsi_http_file_response <- function(file, content_type, status = 200L) {
  size <- file.info(file)$size
  if (is.na(size) || size <= 0) {
    wsi_abort(sprintf("Could not read generated tile file: %s", file))
  }
  body <- readBin(file, what = "raw", n = size)
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = content_type,
      "Cache-Control" = "no-store, max-age=0",
      "Pragma" = "no-cache",
      "Access-Control-Allow-Origin" = "*",
      "Access-Control-Allow-Methods" = "GET, OPTIONS",
      "Access-Control-Allow-Headers" = "Content-Type"
    ),
    body = body
  )
}

wsi_dynamic_tile_response <- function(source, level, col, row, format = NULL,
                                      settings = list()) {
  format <- wsi_dynamic_tile_format(format %||% source$tile_format)
  file <- wsi_dynamic_tile_file(source, level, col, row, format = format, settings = settings)
  wsi_http_file_response(file, wsi_dynamic_tile_content_type(format))
}

wsi_dynamic_tile_cleanup <- function(source) {
  if (inherits(source, "wsi_dynamic_czi_section_tile_source") && !is.null(source$czi_handle)) {
    try(wsi_native_czi_close_handle(source$czi_handle), silent = TRUE)
  }
  if (inherits(source, "wsi_dynamic_tile_source") &&
      is.character(source$cache_dir) && length(source$cache_dir) == 1L &&
      nzchar(source$cache_dir) && dir.exists(source$cache_dir)) {
    unlink(source$cache_dir, recursive = TRUE, force = TRUE)
  }
  invisible(TRUE)
}
