#' Check for native CZI support
#'
#' `wsi_has_native_czi()` reports whether the optional native CZI reader is
#' available. The native backend calls ZEISS libCZI/libCZIAPI directly from R
#' through the package's compiled code; it does not use the Python wrappers.
#' Set `WSITOOLS_LIBCZIAPI` to the full path of the libCZIAPI shared library
#' when it is not already on the operating-system dynamic library path.
#'
#' @return `TRUE` when native CZI symbols are available, otherwise `FALSE`.
#' @export
#' @examples
#' wsi_has_native_czi()
wsi_has_native_czi <- function() {
  wsi_native_czi_autoload()
  if (!wsi_native_available("wsi_native_czi_available")) {
    return(FALSE)
  }
  isTRUE(tryCatch(.Call("wsi_native_czi_available", PACKAGE = "wsiTools"), error = function(err) FALSE))
}

wsi_native_czi_autoload <- function() {
  if (nzchar(Sys.getenv("WSITOOLS_LIBCZIAPI")) || nzchar(Sys.getenv("WSITOOLS_LIBCZI"))) {
    return(invisible(TRUE))
  }
  finder <- get0("wsi_native_czi_find_library", mode = "function", inherits = TRUE)
  if (!is.function(finder)) {
    return(invisible(FALSE))
  }
  library_path <- tryCatch(finder(), error = function(err) NA_character_)
  if (!is.na(library_path) && nzchar(library_path)) {
    Sys.setenv(WSITOOLS_LIBCZIAPI = library_path)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

wsi_native_czi_version <- function() {
  wsi_native_czi_autoload()
  if (!wsi_native_available("wsi_native_czi_version")) {
    return(NA_character_)
  }
  as.character(tryCatch(.Call("wsi_native_czi_version", PACKAGE = "wsiTools"), error = function(err) NA_character_))
}

wsi_native_czi_project_preview <- function(path, width = 768, height = NULL, sections = TRUE) {
  if (!wsi_has_native_czi()) {
    wsi_abort(
      paste(
        "Native CZI preview is not available in this build.",
        "Install ZEISS libCZI/libCZIAPI and set `WSITOOLS_LIBCZIAPI` to the shared library path.",
        "wsiTools does not use Python CZI wrappers unless `WSITOOLS_CZI_ALLOW_PYTHON=true` is set explicitly.",
        sep = "\n"
      ),
      class = "wsi_backend_unavailable"
    )
  }
  info <- wsi_native_czi_info(path)
  if (isTRUE(sections)) {
    scene_sections <- wsi_native_czi_scene_previews(path, info, width = width, height = height)
    if (length(scene_sections)) {
      return(list(
        path = path,
        backend = "native_czi",
        info = info,
        sections = scene_sections
      ))
    }
  }
  scene_width <- as.integer(info$width %||% width)
  scene_height <- as.integer(info$height %||% height %||% width)
  x <- as.integer(info$x %||% 0L)
  y <- as.integer(info$y %||% 0L)
  preview_plan <- wsi_native_czi_preview_plan(info, width = width)
  preview <- wsi_native_czi_read_region(
    path,
    x = x,
    y = y,
    width = scene_width,
    height = scene_height,
    zoom = preview_plan$zoom,
    channel = 0,
    scene = NA_integer_
  )
  preview_uri <- wsi_array_png_data_uri(preview)
  content_bbox <- wsi_array_content_bbox(preview, scene_width, scene_height)
  mpp <- wsi_native_czi_mpp(info$metadata_xml %||% NA_character_)
  list(
    path = path,
    backend = "native_czi",
    info = info,
    mpp = if (all(is.finite(mpp)) && all(mpp > 0)) as.list(mpp) else NULL,
    sections = list(list(
      id = "scene_0",
      label = sprintf("CZI native preview: %s x %s px", scene_width, scene_height),
      scene = 0L,
      width = scene_width,
      height = scene_height,
      preview_width = dim(preview)[[2L]],
      preview_height = dim(preview)[[1L]],
      preview_downsample = preview_plan$downsample,
      preview_source = preview_plan$source,
      x = x,
      y = y,
      mpp = if (all(is.finite(mpp)) && all(mpp > 0)) as.list(mpp) else NULL,
      content_bbox = content_bbox,
      status = "low-resolution preview",
      message = sprintf(
        "Low-resolution CZI scene preview generated through direct libCZIAPI calls from R (%.3gx zoom, %s).",
        preview_plan$zoom,
        preview_plan$source
      ),
      image_data_uri = preview_uri,
      navigator_image_data_uri = preview_uri
    ))
  )
}

wsi_native_czi_scene_previews <- function(path, info, width = 768, height = NULL) {
  scenes <- info$scenes
  if (!is.data.frame(scenes) || !nrow(scenes)) {
    return(list())
  }
  scenes <- scenes[order(scenes$scene), , drop = FALSE]
  scene_meta <- wsi_native_czi_scene_metadata(info$metadata_xml %||% NA_character_)
  mpp <- wsi_native_czi_mpp(info$metadata_xml %||% NA_character_)
  mpp_payload <- if (all(is.finite(mpp)) && all(mpp > 0)) as.list(mpp) else NULL
  out <- vector("list", nrow(scenes))
  for (i in seq_len(nrow(scenes))) {
    scene <- scenes[i, , drop = FALSE]
    scene_index <- as.integer(scene$scene[[1L]])
    scene_width <- as.integer(scene$width[[1L]])
    scene_height <- as.integer(scene$height[[1L]])
    if (is.na(scene_width) || is.na(scene_height) || scene_width <= 0L || scene_height <= 0L) {
      next
    }
    scene_info <- info
    scene_info$width <- scene_width
    scene_info$height <- scene_height
    preview_plan <- wsi_native_czi_preview_plan(scene_info, width = width)
    preview <- tryCatch(
      wsi_native_czi_read_region(
        path,
        x = as.integer(scene$x[[1L]]),
        y = as.integer(scene$y[[1L]]),
        width = scene_width,
        height = scene_height,
        zoom = preview_plan$zoom,
        channel = 0,
        scene = scene_index
      ),
      error = function(err) NULL
    )
    if (is.null(preview)) {
      next
    }
    preview_uri <- wsi_array_png_data_uri(preview)
    content_bbox <- wsi_array_content_bbox(preview, scene_width, scene_height)
    name <- scene_meta$name[match(scene_index, scene_meta$scene)] %||% NA_character_
    if (is.na(name) || !nzchar(name)) {
      name <- sprintf("Scene %d", scene_index)
    }
    out[[i]] <- list(
      id = sprintf("scene_%d", scene_index),
      label = sprintf("%s: %s x %s px", name, scene_width, scene_height),
      scene = scene_index,
      width = scene_width,
      height = scene_height,
      preview_width = dim(preview)[[2L]],
      preview_height = dim(preview)[[1L]],
      preview_downsample = preview_plan$downsample,
      preview_source = preview_plan$source,
      x = as.integer(scene$x[[1L]]),
      y = as.integer(scene$y[[1L]]),
      mpp = mpp_payload,
      content_bbox = content_bbox,
      status = "low-resolution scene preview",
      message = sprintf(
        "Low-resolution CZI scene preview generated through direct libCZIAPI calls from R (%.3gx zoom, %s).",
        preview_plan$zoom,
        preview_plan$source
      ),
      image_data_uri = preview_uri,
      navigator_image_data_uri = preview_uri
    )
  }
  Filter(Negate(is.null), out)
}

wsi_array_content_bbox <- function(array, width, height, brightness_threshold = 0.96) {
  dims <- dim(array)
  if (length(dims) != 3L || dims[[1L]] < 2L || dims[[2L]] < 2L || dims[[3L]] < 3L) {
    return(NULL)
  }
  rgb <- array[, , seq_len(3L), drop = FALSE]
  brightness <- (rgb[, , 1L] + rgb[, , 2L] + rgb[, , 3L]) / 3
  mask <- is.finite(brightness) & brightness < brightness_threshold
  if (sum(mask) < max(16L, length(mask) * 0.001)) {
    return(NULL)
  }
  rows <- which(rowSums(mask) > 0)
  cols <- which(colSums(mask) > 0)
  if (!length(rows) || !length(cols)) {
    return(NULL)
  }
  scale_x <- as.numeric(width) / dims[[2L]]
  scale_y <- as.numeric(height) / dims[[1L]]
  list(
    xmin = max(0, (min(cols) - 1) * scale_x),
    ymin = max(0, (min(rows) - 1) * scale_y),
    xmax = min(as.numeric(width), max(cols) * scale_x),
    ymax = min(as.numeric(height), max(rows) * scale_y)
  )
}

wsi_native_czi_scene_metadata <- function(metadata_xml) {
  empty <- data.frame(scene = integer(), name = character(), stringsAsFactors = FALSE)
  metadata_xml <- metadata_xml[[1L]] %||% NA_character_
  if (is.na(metadata_xml) || !nzchar(metadata_xml)) {
    return(empty)
  }
  matches <- gregexpr("(?s)<Scene\\b[^>]*>.*?</Scene>", metadata_xml, perl = TRUE)[[1L]]
  if (identical(matches[[1L]], -1L)) {
    return(empty)
  }
  blocks <- regmatches(metadata_xml, list(matches))[[1L]]
  rows <- lapply(blocks, function(block) {
    open_tag <- regmatches(block, regexpr("<Scene\\b[^>]*>", block, perl = TRUE))
    index <- suppressWarnings(as.integer(sub('.*\\bIndex="([^"]+)".*', "\\1", open_tag, perl = TRUE)))
    name <- sub('.*\\bName="([^"]*)".*', "\\1", open_tag, perl = TRUE)
    if (identical(name, open_tag)) {
      name <- NA_character_
    }
    data.frame(scene = index, name = name, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out[is.finite(out$scene), , drop = FALSE]
}

wsi_native_czi_mpp <- function(metadata_xml) {
  metadata_xml <- metadata_xml[[1L]] %||% NA_character_
  if (is.na(metadata_xml) || !nzchar(metadata_xml)) {
    return(c(x = NA_real_, y = NA_real_))
  }
  metadata_xml <- iconv(metadata_xml, from = "", to = "UTF-8", sub = "")
  c(
    x = wsi_native_czi_distance_um(metadata_xml, "X"),
    y = wsi_native_czi_distance_um(metadata_xml, "Y")
  )
}

wsi_native_czi_distance_um <- function(metadata_xml, axis) {
  axis <- toupper(axis[[1L]])
  block_pattern <- sprintf(
    "(?is)<Distance\\b[^>]*\\bId=[\"']%s[\"'][^>]*>.*?</Distance>",
    axis
  )
  block_match <- regexpr(block_pattern, metadata_xml, perl = TRUE)
  if (identical(block_match[[1L]], -1L)) {
    return(NA_real_)
  }
  block <- regmatches(metadata_xml, block_match)
  value_match <- regexec("(?is)<Value\\b[^>]*>\\s*([^<]+?)\\s*</Value>", block, perl = TRUE)
  parts <- regmatches(block, value_match)[[1L]]
  if (length(parts) < 2L) {
    return(NA_real_)
  }
  value <- suppressWarnings(as.numeric(parts[[2L]]))
  if (!is.finite(value) || value <= 0) {
    return(NA_real_)
  }
  # Zeiss CZI scaling values are normally stored in metres. If a backend has
  # already returned microns, keep plausible micron-scale values unchanged.
  if (value < 0.01) value * 1e6 else value
}

wsi_czi_project_preview <- function(path, width = 768, height = NULL, sections = TRUE) {
  if (wsi_has_native_czi()) {
    preview <- tryCatch(
      wsi_native_czi_project_preview(path, width = width, height = height, sections = sections),
      error = function(err) err
    )
    if (!inherits(preview, "error") && !is.null(preview)) {
      preview$backend <- "native_czi"
      return(preview)
    }
  }

  if (wsi_has_bioformats_java()) {
    preview <- tryCatch(
      wsi_bioformats_java_project_preview(path, width = width, height = height),
      error = function(err) err
    )
    if (!inherits(preview, "error") && !is.null(preview)) {
      preview$backend <- "bioformats_java"
      return(preview)
    }
  }

  if (!wsi_allow_czi_python()) {
    return(NULL)
  }

  preview <- tryCatch(
    wsi_czi_python_project_preview(path, width = width),
    error = function(err) err
  )
  if (!inherits(preview, "error") && !is.null(preview)) {
    preview$backend <- "aicspylibczi"
    return(preview)
  }

  NULL
}

wsi_czi_fast_slide <- function(path) {
  errors <- character()
  candidates <- c(
    if (wsi_has_openslide()) "openslide",
    if (wsi_has_vips()) "vips"
  )
  for (candidate in candidates) {
    slide <- tryCatch(
      switch(
        candidate,
        openslide = wsi_openslide_open(path),
        vips = wsi_vips_open(path)
      ),
      error = function(err) {
        errors[[candidate]] <<- conditionMessage(err)
        NULL
      }
    )
    if (inherits(slide, "wsi_slide")) {
      return(slide)
    }
  }
  NULL
}

wsi_czi_initial_preview_width <- function(width) {
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  cap <- suppressWarnings(as.integer(Sys.getenv("WSITOOLS_CZI_INITIAL_PREVIEW_WIDTH", unset = "1024")))
  if (is.na(cap) || cap <= 0L) {
    cap <- 1024L
  }
  min(width, cap)
}

wsi_czi_minimum_preview_width <- function(target) {
  target <- as.integer(wsi_check_scalar_number(target, "target", allow_zero = FALSE))
  minimum <- suppressWarnings(as.integer(Sys.getenv("WSITOOLS_CZI_MIN_PREVIEW_WIDTH", unset = "768")))
  if (is.na(minimum) || minimum <= 0L) {
    minimum <- 768L
  }
  min(target, minimum)
}

wsi_czi_pyramid_factors <- function(pyramid_json) {
  if (is.null(pyramid_json) || length(pyramid_json) == 0L) {
    return(numeric())
  }
  pyramid_json <- pyramid_json[[1L]]
  if (is.na(pyramid_json) || !nzchar(pyramid_json)) {
    return(numeric())
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(pyramid_json, simplifyVector = FALSE),
    error = function(err) NULL
  )
  if (is.null(parsed)) {
    return(numeric())
  }
  raw_factors <- numeric()
  layer_numbers <- numeric()
  collect <- function(node) {
    if (!is.list(node)) {
      return(invisible(NULL))
    }
    if (!is.null(node$minificationFactor)) {
      base_factor <- suppressWarnings(as.numeric(node$minificationFactor))
      layer_no <- suppressWarnings(as.numeric(node$pyramidLayerNo %||% NA_real_))
      raw_factors <<- c(raw_factors, base_factor)
      layer_numbers <<- c(layer_numbers, layer_no)
    }
    for (item in node) {
      collect(item)
    }
    invisible(NULL)
  }
  collect(parsed)
  valid <- is.finite(raw_factors) & raw_factors > 1
  raw_factors <- raw_factors[valid]
  layer_numbers <- layer_numbers[valid]
  if (!length(raw_factors)) {
    return(numeric())
  }
  finite_layers <- layer_numbers[is.finite(layer_numbers)]
  repeated_base <- length(unique(raw_factors)) == 1L && length(unique(finite_layers)) > 1L
  factors <- if (repeated_base && length(finite_layers)) {
    raw_factors^ifelse(is.finite(layer_numbers) & layer_numbers > 0, layer_numbers, 1)
  } else {
    raw_factors
  }
  sort(unique(factors[is.finite(factors) & factors > 1]))
}

wsi_native_czi_preview_plan <- function(info, width = 768) {
  scene_width <- suppressWarnings(as.numeric(info$width %||% NA_real_))
  scene_height <- suppressWarnings(as.numeric(info$height %||% NA_real_))
  dimensions <- c(scene_width, scene_height)
  max_dim <- if (all(is.na(dimensions))) NA_real_ else max(dimensions, na.rm = TRUE)
  if (!is.finite(max_dim) || max_dim <= 0) {
    max_dim <- as.numeric(width)
  }
  target <- wsi_czi_initial_preview_width(width)
  minimum <- wsi_czi_minimum_preview_width(target)
  factors <- wsi_czi_pyramid_factors(info$pyramid_json %||% NA_character_)

  if (length(factors)) {
    largest_usable <- max_dim / max(minimum, 1)
    usable <- factors[factors <= largest_usable]
    downsample <- if (length(usable)) max(usable) else min(factors)
    source <- "native pyramid"
  } else {
    downsample <- max(1, max_dim / max(target, 1))
    source <- "scaled native accessor"
  }

  list(
    target_width = target,
    minimum_width = minimum,
    downsample = downsample,
    zoom = min(1, 1 / downsample),
    source = source,
    pyramid_factors = factors
  )
}

wsi_allow_czi_python <- function() {
  value <- tolower(Sys.getenv("WSITOOLS_CZI_ALLOW_PYTHON", unset = "false"))
  value %in% c("1", "true", "yes", "on")
}

wsi_native_czi_info <- function(path) {
  path <- wsi_validate_input_path(path)
  if (!wsi_native_available("wsi_native_czi_info")) {
    wsi_abort(
      "This wsiTools build does not include the native CZI bridge.",
      class = "wsi_backend_unavailable"
    )
  }
  .Call("wsi_native_czi_info", path, PACKAGE = "wsiTools")
}

wsi_native_czi_read_region <- function(path, x, y, width, height, zoom = 1,
                                       channel = 0, scene = NA_integer_) {
  path <- wsi_validate_input_path(path)
  if (!wsi_native_available("wsi_native_czi_read_region")) {
    wsi_abort(
      "This wsiTools build does not include native CZI region reading.",
      class = "wsi_backend_unavailable"
    )
  }
  .Call(
    "wsi_native_czi_read_region",
    path,
    as.integer(x),
    as.integer(y),
    as.integer(width),
    as.integer(height),
    as.numeric(zoom),
    as.integer(channel),
    as.integer(scene),
    PACKAGE = "wsiTools"
  )
}

wsi_native_czi_open_handle <- function(path) {
  path <- wsi_validate_input_path(path)
  if (!wsi_native_available("wsi_native_czi_open_handle")) {
    wsi_abort(
      "This wsiTools build does not include persistent native CZI tile handles.",
      class = "wsi_backend_unavailable"
    )
  }
  .Call("wsi_native_czi_open_handle", path, PACKAGE = "wsiTools")
}

wsi_native_czi_close_handle <- function(handle) {
  if (is.null(handle) || !wsi_native_available("wsi_native_czi_close_handle")) {
    return(invisible(FALSE))
  }
  .Call("wsi_native_czi_close_handle", handle, PACKAGE = "wsiTools")
  invisible(TRUE)
}

wsi_native_czi_handle_read_region <- function(handle, x, y, width, height,
                                              zoom = 1, channel = 0,
                                              scene = NA_integer_) {
  if (is.null(handle)) {
    wsi_abort("`handle` must be an open native CZI handle.")
  }
  if (!wsi_native_available("wsi_native_czi_handle_read_region")) {
    wsi_abort(
      "This wsiTools build does not include persistent native CZI tile handles.",
      class = "wsi_backend_unavailable"
    )
  }
  .Call(
    "wsi_native_czi_handle_read_region",
    handle,
    as.integer(x),
    as.integer(y),
    as.integer(width),
    as.integer(height),
    as.numeric(zoom),
    as.integer(channel),
    as.integer(scene),
    PACKAGE = "wsiTools"
  )
}

wsi_native_czi_open <- function(path) {
  if (!wsi_has_native_czi()) {
    wsi_abort(
      "Native CZI backend is not available. Install ZEISS libCZI/libCZIAPI and set `WSITOOLS_LIBCZIAPI`, then restart R.",
      class = "wsi_backend_unavailable"
    )
  }
  info <- wsi_native_czi_info(path)
  width <- as.numeric(info$width %||% NA_real_)
  height <- as.numeric(info$height %||% NA_real_)
  if (is.na(width) || is.na(height)) {
    wsi_abort("Native CZI backend did not report image dimensions for this file.")
  }
  levels <- data.frame(
    level = 0L,
    width = width,
    height = height,
    downsample = 1,
    stringsAsFactors = FALSE
  )
  metadata <- list(
    vendor = "Zeiss",
    backend_version = info$version %||% wsi_native_czi_version(),
    native_czi = info
  )
  mpp <- wsi_native_czi_mpp(info$metadata_xml %||% NA_character_)
  props <- list(
    "czi.backend" = "native_czi",
    "czi.library" = info$library %||% NA_character_,
    "czi.version" = info$version %||% NA_character_,
    "czi.sub_block_count" = as.character(info$sub_block_count %||% NA_integer_),
    "czi.attachment_count" = as.character(info$attachment_count %||% NA_integer_),
    "czi.pyramid_json" = info$pyramid_json %||% NA_character_,
    "czi.metadata_xml" = info$metadata_xml %||% NA_character_
  )
  if (all(is.finite(mpp)) && all(mpp > 0)) {
    props[["mpp-x"]] <- as.character(unname(mpp[["x"]]))
    props[["mpp-y"]] <- as.character(unname(mpp[["y"]]))
    props[["czi.mpp-x"]] <- as.character(unname(mpp[["x"]]))
    props[["czi.mpp-y"]] <- as.character(unname(mpp[["y"]]))
  }
  dims <- info$dimensions
  if (is.list(dims) && length(dims$dimension)) {
    for (i in seq_along(dims$dimension)) {
      key <- sprintf("czi.dimension.%s", dims$dimension[[i]])
      props[[paste0(key, ".start")]] <- as.character(dims$start[[i]])
      props[[paste0(key, ".size")]] <- as.character(dims$size[[i]])
    }
  }
  wsi_make_slide(
    path = path,
    backend = "native_czi",
    dimensions = c(width = width, height = height),
    levels = levels,
    properties = props,
    metadata = metadata,
    associated_images = character()
  )
}

wsi_array_png_data_uri <- function(array) {
  raster <- wsi_array_to_raster(array)
  height <- dim(array)[[1L]]
  width <- dim(array)[[2L]]
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = width, height = height, bg = "white")
  closed <- FALSE
  old <- graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  on.exit(if (!closed) {
    graphics::par(old)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)
  graphics::rasterImage(raster, 0, 0, 1, 1, interpolate = FALSE)
  graphics::par(old)
  grDevices::dev.off()
  closed <- TRUE
  wsi_image_data_uri(tmp, mime = "image/png")
}
