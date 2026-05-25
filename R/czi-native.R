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
  if (!wsi_native_available("wsi_native_czi_available")) {
    return(FALSE)
  }
  isTRUE(tryCatch(.Call("wsi_native_czi_available", PACKAGE = "wsiTools"), error = function(err) FALSE))
}

wsi_native_czi_version <- function() {
  if (!wsi_native_available("wsi_native_czi_version")) {
    return(NA_character_)
  }
  as.character(tryCatch(.Call("wsi_native_czi_version", PACKAGE = "wsiTools"), error = function(err) NA_character_))
}

wsi_native_czi_project_preview <- function(path, width = 768, height = NULL) {
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
  scene_width <- as.integer(info$width %||% width)
  scene_height <- as.integer(info$height %||% height %||% width)
  x <- as.integer(info$x %||% 0L)
  y <- as.integer(info$y %||% 0L)
  target <- as.integer(width)
  scale <- min(1, target / max(scene_width, scene_height, 1))
  preview <- wsi_native_czi_read_region(
    path,
    x = x,
    y = y,
    width = scene_width,
    height = scene_height,
    zoom = scale,
    channel = 0,
    scene = NA_integer_
  )
  preview_uri <- wsi_array_png_data_uri(preview)
  list(
    path = path,
    backend = "native_czi",
    info = info,
    sections = list(list(
      id = "scene_0",
      label = sprintf("CZI native preview: %s x %s px", scene_width, scene_height),
      scene = 0L,
      width = scene_width,
      height = scene_height,
      preview_width = dim(preview)[[2L]],
      preview_height = dim(preview)[[1L]],
      x = x,
      y = y,
      status = "preview",
      message = "Preview generated through direct libCZIAPI calls from R",
      image_data_uri = preview_uri,
      navigator_image_data_uri = preview_uri
    ))
  )
}

wsi_czi_project_preview <- function(path, width = 768, height = NULL) {
  if (wsi_has_native_czi()) {
    preview <- tryCatch(
      wsi_native_czi_project_preview(path, width = width, height = height),
      error = function(err) err
    )
    if (!inherits(preview, "error") && !is.null(preview)) {
      preview$backend <- "native_czi"
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
  props <- list(
    "czi.backend" = "native_czi",
    "czi.library" = info$library %||% NA_character_,
    "czi.version" = info$version %||% NA_character_,
    "czi.sub_block_count" = as.character(info$sub_block_count %||% NA_integer_),
    "czi.attachment_count" = as.character(info$attachment_count %||% NA_integer_),
    "czi.pyramid_json" = info$pyramid_json %||% NA_character_,
    "czi.metadata_xml" = info$metadata_xml %||% NA_character_
  )
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
