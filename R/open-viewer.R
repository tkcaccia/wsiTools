#' Open an image in the viewer with one command
#'
#' `wsi_open_viewer()` is the friendliest entry point for first-time use:
#' supply an image path and let wsiTools choose the backend, viewer type,
#' tiled/full-resolution mode, and browser launch behavior. It does not load a
#' complete whole-slide image into R memory.
#'
#' The automatic choices are conservative:
#' \itemize{
#'   \item live mode is used in interactive R sessions when `httpuv` is
#'     installed; otherwise a static HTML viewer is written;
#'   \item tiled mode is used for real file-backed slides when a tiled route is
#'     available; mock slides and unsupported cases use thumbnail mode;
#'   \item prebuilt Deep Zoom tiles are preferred for libvips-readable slides;
#'     live dynamic tiles are used for very large files, when prebuilt libvips
#'     Deep Zoom tiles are not the better default, or when explicitly requested;
#'   \item CZI paths are opened through the existing section-aware project
#'     viewer helpers.
#' }
#'
#' @param input Image path, character vector of image paths, or an existing
#'   `wsi_slide` object.
#' @param ... Additional arguments passed to [wsi_viewer()],
#'   [wsi_viewer_live()], [wsi_viewer_project()], or
#'   [wsi_viewer_czi_project_live()], depending on the selected route.
#' @param backend Backend passed to [wsi_open()] for non-CZI single-image paths.
#' @param live `"auto"`, `"yes"`, or `"no"`. `"auto"` uses live mode only in
#'   interactive sessions when the optional `httpuv` package is available.
#' @param tiled `"auto"`, `"yes"`, or `"no"`. `"auto"` uses tiled mode when
#'   feasible for the selected viewer route.
#' @param dynamic_tiles `"auto"`, `"yes"`, or `"no"`. Dynamic tiles require live
#'   mode. `"auto"` uses them when prebuilt libvips Deep Zoom tiles are not the
#'   better default.
#' @param renderer Base-image renderer passed to every selected viewer route.
#'   `"auto"` uses GPU-accelerated OpenSeadragon when available with a Canvas
#'   fallback; `"gpu"` requires WebGL and `"cpu"` forces Canvas. Tauri and direct
#'   R launches use the same option.
#' @param open Whether to open the viewer in a browser.
#' @param wait For live viewers, whether to service the local `httpuv` bridge
#'   until interrupted. The default uses `TRUE` only for interactive live runs.
#' @param output Optional HTML output path.
#' @param overwrite Whether to overwrite `output`.
#' @param close_slide For static viewers opened from a path, close the slide
#'   after writing the HTML. Live viewers keep the slide in the returned session.
#' @param czi_sections For CZI paths, show detected scenes/sections separately
#'   in the Project panel.
#' @param quiet If `FALSE`, print the route selected by wsiTools.
#'
#' @return A `wsi_viewer_session` for live viewers, otherwise the HTML path.
#' @export
#'
#' @examples
#' \dontrun{
#' viewer <- wsi_open_viewer("sample.svs")
#'
#' # Force a static HTML viewer:
#' html <- wsi_open_viewer("sample.svs", live = "no", open = FALSE)
#' }
wsi_open_viewer <- function(input, ...,
                            backend = c("auto", "openslide", "vips", "native_czi", "bioformats", "omezarr", "imagemagick"),
                            live = c("auto", "yes", "no"),
                            tiled = c("auto", "yes", "no"),
                            dynamic_tiles = c("auto", "yes", "no"),
                            renderer = c("auto", "gpu", "cpu"),
                            open = interactive(),
                            wait = NULL,
                            output = NULL,
                            overwrite = FALSE,
                            close_slide = TRUE,
                            czi_sections = TRUE,
                            quiet = FALSE) {
  backend <- match.arg(backend)
  live <- wsi_open_viewer_flag(live, "live", yes_alias = "live", no_alias = "static")
  tiled <- wsi_open_viewer_flag(tiled, "tiled", yes_alias = "tiles", no_alias = "thumbnail")
  dynamic_tiles <- wsi_open_viewer_flag(dynamic_tiles, "dynamic_tiles", yes_alias = "dynamic")
  renderer <- wsi_viewer_renderer(if (missing(renderer)) NULL else renderer)
  if (!is.logical(open) || length(open) != 1L || is.na(open)) {
    wsi_abort("`open` must be `TRUE` or `FALSE`.")
  }
  if (!is.null(wait) && (!is.logical(wait) || length(wait) != 1L || is.na(wait))) {
    wsi_abort("`wait` must be `NULL`, `TRUE`, or `FALSE`.")
  }
  if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite)) {
    wsi_abort("`overwrite` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(close_slide) || length(close_slide) != 1L || is.na(close_slide)) {
    wsi_abort("`close_slide` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(czi_sections) || length(czi_sections) != 1L || is.na(czi_sections)) {
    wsi_abort("`czi_sections` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    wsi_abort("`quiet` must be `TRUE` or `FALSE`.")
  }

  dots <- list(...)
  dots$renderer <- dots$renderer %||% renderer
  if (!is.null(dots$open)) {
    open <- dots$open
    dots$open <- NULL
  }
  if (!is.null(dots$wait)) {
    wait <- dots$wait
    dots$wait <- NULL
  }
  if (!is.null(dots$output) && is.null(output)) {
    output <- dots$output
    dots$output <- NULL
  }
  if (!is.null(dots$overwrite)) {
    overwrite <- isTRUE(dots$overwrite)
    dots$overwrite <- NULL
  }
  if (!is.null(dots$mode)) {
    mode <- match.arg(dots$mode, c("thumbnail", "tiles"))
    tiled <- if (identical(mode, "tiles")) "yes" else "no"
    dots$mode <- NULL
  }

  httpuv_ready <- requireNamespace("httpuv", quietly = TRUE)
  use_live <- wsi_open_viewer_use_live(live, httpuv_ready = httpuv_ready)
  if (is.null(wait)) {
    wait <- isTRUE(use_live) && interactive()
  }
  if (identical(dynamic_tiles, "yes") && !isTRUE(use_live)) {
    wsi_abort("`dynamic_tiles = \"yes\"` requires `live = \"yes\"` or live auto-detection with `httpuv` available.")
  }

  if (wsi_open_viewer_is_path_vector(input)) {
    return(wsi_open_viewer_project_route(
      input = input,
      dots = dots,
      live = live,
      use_live = use_live,
      open = open,
      wait = wait,
      output = output,
      overwrite = overwrite,
      czi_sections = czi_sections,
      quiet = quiet
    ))
  }

  slide <- input
  opened_slide <- FALSE
  if (!inherits(slide, "wsi_slide")) {
    if (!is.character(input) || length(input) != 1L || is.na(input) || !nzchar(input)) {
      wsi_abort("`input` must be an image path, path vector, or `wsi_slide` object.")
    }
    if (wsi_is_czi_path(input)) {
      return(wsi_open_viewer_project_route(
        input = input,
        dots = dots,
        live = live,
        use_live = use_live,
        open = open,
        wait = wait,
        output = output,
        overwrite = overwrite,
        czi_sections = czi_sections,
        quiet = quiet
      ))
    }
    slide <- wsi_open(input, backend = backend)
    opened_slide <- TRUE
  } else {
    wsi_check_slide(slide)
  }

  use_tiles <- wsi_open_viewer_use_tiles(slide, tiled = tiled, use_live = use_live)
  mode <- if (isTRUE(use_tiles)) "tiles" else "thumbnail"
  use_dynamic_tiles <- wsi_open_viewer_use_dynamic_tiles(
    slide,
    dynamic_tiles = dynamic_tiles,
    use_live = use_live,
    use_tiles = use_tiles
  )
  if (isTRUE(use_dynamic_tiles)) {
    dots$tile_prefetch_margin <- dots$tile_prefetch_margin %||% 1L
    dots$tile_cache_count <- dots$tile_cache_count %||% 768L
    dots$tile_prefetch_cache_count <- dots$tile_prefetch_cache_count %||% 512L
    dots$progressive_preview <- dots$progressive_preview %||% TRUE
    dots$dynamic_tile_format <- dots$dynamic_tile_format %||% "jpg"
  }
  if (isTRUE(use_live)) {
    if (isTRUE(opened_slide) && isTRUE(close_slide) && !isTRUE(quiet)) {
      message("wsi_open_viewer(): keeping the opened slide alive inside the returned live viewer session.")
    }
    if (!isTRUE(quiet)) {
      message(sprintf(
        "wsi_open_viewer(): opening live %s viewer%s.",
        mode,
        if (isTRUE(use_dynamic_tiles)) " with progressive dynamic tiles" else ""
      ))
    }
    args <- c(
      list(
        slide = slide,
        mode = mode,
        dynamic_tiles = use_dynamic_tiles,
        open = open,
        wait = wait,
        output = output,
        overwrite = overwrite
      ),
      dots
    )
    return(do.call(wsi_viewer_live, args))
  }

  if (isTRUE(opened_slide) && isTRUE(close_slide)) {
    on.exit(wsi_close(slide), add = TRUE)
  }
  if (!isTRUE(quiet)) {
    message(sprintf("wsi_open_viewer(): opening static %s viewer.", mode))
  }
  args <- c(
    list(
      slide = slide,
      mode = mode,
      open = open,
      output = output,
      overwrite = overwrite
    ),
    dots
  )
  html <- do.call(wsi_viewer, args)
  normalizePath(html, winslash = "/", mustWork = FALSE)
}

#' Launch the native Rust/WGPU viewer from R
#'
#' `wsi_viewer_native()` starts the same live R bridge used by
#' [wsi_viewer_live()] and opens its tile and state endpoints in the native
#' Rust/WGPU desktop renderer. R remains the owner of the slide, annotations,
#' spatial object, and analyses; the native application never loads the whole
#' slide into memory.
#'
#' The function requires a wsiTools Desktop build with the `native-wgpu`
#' feature. The desktop launcher can be supplied explicitly through
#' `app_path`, or discovered from `WSITOOLS_DESKTOP_APP`, the macOS
#' Applications folder, or `PATH`.
#'
#' @param input An image path, a vector of image paths, or an existing
#'   `wsi_slide` object. Multiple inputs become independently navigable native
#'   project sources.
#' @param ... Additional arguments passed to [wsi_viewer_live()].
#' @param backend Backend passed to [wsi_open()] when `input` is a path.
#' @param app_path Path to the `wsitools-desktop` executable. `NULL` discovers
#'   the installed desktop application.
#' @param wait Whether to keep servicing the live R bridge until interrupted.
#'   This defaults to `TRUE` in interactive R and is normally what users want.
#' @param native_args Extra command-line arguments for the desktop executable.
#'
#' @return Invisibly, a `wsi_viewer_session`.
#' @export
#'
#' @examples
#' \dontrun{
#' wsi_viewer_native("sample.svs", dynamic_tiles = TRUE)
#' }
wsi_viewer_native <- function(input, ...,
                              backend = c("auto", "openslide", "vips", "native_czi", "bioformats", "omezarr", "imagemagick"),
                              app_path = NULL,
                              wait = interactive(),
                              native_args = character()) {
  backend <- match.arg(backend)
  if (!is.logical(wait) || length(wait) != 1L || is.na(wait)) {
    wsi_abort("`wait` must be `TRUE` or `FALSE`.")
  }
  if (!is.character(native_args) || anyNA(native_args)) {
    wsi_abort("`native_args` must be a character vector.")
  }
  dots <- list(...)
  input_items <- if (inherits(input, "wsi_slide")) list(input) else as.list(input)
  if (!length(input_items) || any(!vapply(input_items, function(item) {
    inherits(item, "wsi_slide") ||
      (is.character(item) && length(item) == 1L && !is.na(item) && nzchar(item))
  }, logical(1)))) {
    wsi_abort("`input` must be an image path, a vector of image paths, or a `wsi_slide` object.")
  }
  open_item <- function(item) {
    if (inherits(item, "wsi_slide")) item else wsi_open(item, backend = backend)
  }
  slide <- open_item(input_items[[1L]])
  source_identity <- function(item) {
    value <- if (inherits(item, "wsi_slide")) item$path %||% "" else item
    value <- as.character(value %||% "")
    if (!length(value) || is.na(value[[1L]]) || !nzchar(value[[1L]])) return("")
    normalizePath(value[[1L]], winslash = "/", mustWork = FALSE)
  }
  base_identity <- source_identity(input_items[[1L]])
  project_items <- c(input_items[-1L], dots$project_images %||% list())
  dots$project_images <- NULL
  project_sources <- list()
  project_records <- list()
  known_identities <- base_identity
  for (index in seq_along(project_items)) {
    item <- project_items[[index]]
    spec <- if (is.list(item) && !inherits(item, "wsi_slide")) item else list(path = item)
    image_input <- spec$slide %||% spec$path %||% NULL
    if (is.null(image_input) || !(inherits(image_input, "wsi_slide") ||
        (is.character(image_input) && length(image_input) == 1L && !is.na(image_input) && nzchar(image_input)))) {
      wsi_abort("Each native project image must provide a slide or a single image path.")
    }
    identity <- source_identity(image_input)
    if (nzchar(identity) && identity %in% known_identities) next
    project_slide <- open_item(image_input)
    path <- as.character(spec$path %||% project_slide$path %||% "")
    label <- as.character(spec$label %||% spec$name %||% basename(path %||% ""))
    if (!length(label) || is.na(label[[1L]]) || !nzchar(label[[1L]])) {
      label <- sprintf("Image %d", index + 1L)
    }
    source_id <- wsi_safe_id(spec$id %||% path %||% label, sprintf("native_project_%d", index + 1L))
    source <- wsi_dynamic_tile_source(project_slide, slide_id = source_id)
    source$name <- label
    source$metadata <- utils::modifyList(source$metadata %||% list(), list(
      project_item_id = source_id,
      id = source_id,
      label = label,
      path = path,
      backend = project_slide$backend %||% "dynamic",
      type = spec$type %||% "native_project_image",
      status = "live dynamic tiles",
      message = "Full-resolution image tiles are served on demand by the live R session.",
      active = FALSE,
      mpp = project_slide$mpp %||% NULL,
      objective_power = project_slide$objective_power %||% NULL
    ), keep.null = TRUE)
    project_sources[[length(project_sources) + 1L]] <- source
    project_records[[length(project_records) + 1L]] <- list(
      id = source_id, label = label, path = path, backend = project_slide$backend %||% "dynamic",
      type = source$metadata$type, status = "live dynamic tiles"
    )
    if (nzchar(identity)) known_identities <- c(known_identities, identity)
  }
  if (length(project_sources)) {
    dots$project_tile_sources <- c(dots$project_tile_sources %||% list(), project_sources)
    dots$project_images <- project_records
  }
  requested_name <- as.character(dots$name %||% "")
  if (!length(requested_name) || is.na(requested_name[[1L]]) || !nzchar(requested_name[[1L]])) {
    base_path <- as.character(slide$path %||% "")
    dots$name <- if (nzchar(base_path)) basename(base_path) else "wsi_native_viewer"
  }
  desktop <- wsi_native_desktop_path(app_path)
  # Native WGPU always consumes the live dynamic tile endpoint. Ignore browser
  # launch flags supplied through `...` rather than passing duplicates to R.
  dots$dynamic_tiles <- TRUE
  dots$open <- FALSE
  dots$wait <- FALSE
  session <- do.call(wsi_viewer_live, c(list(slide = slide), dots))
  status <- tryCatch(
    system2(desktop, args = c("--native-viewer-url", session$url, native_args), wait = FALSE),
    error = function(error) error
  )
  if (inherits(status, "error") || (!is.null(status) && !identical(status, 0L))) {
    try(session$stop(), silent = TRUE)
    wsi_abort(paste0(
      "Could not launch the native Rust/WGPU viewer. ",
      if (inherits(status, "error")) conditionMessage(status) else "The desktop process returned a non-zero status.",
      "\nDesktop executable: ", desktop
    ))
  }
  message("wsiTools native WGPU viewer launched at ", session$url)
  if (isTRUE(wait)) {
    message("Press Ctrl+C or Esc to stop the native live session and return to R.")
    on.exit(try(session$stop(), silent = TRUE), add = TRUE)
    tryCatch(
      repeat session$service(100L),
      interrupt = function(error) NULL
    )
  }
  invisible(session)
}

wsi_native_desktop_path <- function(app_path = NULL) {
  if (!is.null(app_path)) {
    if (!is.character(app_path) || length(app_path) != 1L || is.na(app_path) || !nzchar(app_path)) {
      wsi_abort("`app_path` must be `NULL` or a single non-empty executable path.")
    }
    if (!file.exists(app_path)) {
      wsi_abort(paste0("The specified native desktop executable does not exist: ", app_path))
    }
    return(normalizePath(app_path, winslash = "/", mustWork = TRUE))
  }
  candidates <- c(
    Sys.getenv("WSITOOLS_DESKTOP_APP", unset = ""),
    if (identical(Sys.info()[["sysname"]], "Darwin")) {
      "/Applications/wsiTools Desktop.app/Contents/MacOS/wsitools-desktop"
    } else {
      ""
    },
    Sys.which("wsitools-desktop")
  )
  candidates <- unique(as.character(candidates))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    wsi_abort(paste(
      "The wsiTools Desktop native renderer was not found.",
      "Install wsiTools Desktop or set `WSITOOLS_DESKTOP_APP` to its executable path.",
      sep = "\n"
    ))
  }
  normalizePath(existing[[1L]], winslash = "/", mustWork = TRUE)
}

#' Open a large image with optimized progressive tiled viewing
#'
#' `wsi_open_fast_viewer()` is a convenience wrapper for large whole-slide
#' images. It opens a live, full-resolution tiled viewer, serves tiles on demand
#' through the local R session, keeps a low-resolution preview visible while
#' sharper tiles stream in, and increases browser-side tile caching. The full
#' slide is not loaded into R memory.
#'
#' @param input Image path or `wsi_slide` object.
#' @param ... Additional arguments passed to [wsi_open_viewer()].
#' @param live,tiled,dynamic_tiles Viewer routing flags passed to
#'   [wsi_open_viewer()].
#' @param renderer Base-image renderer passed to [wsi_open_viewer()].
#' @param tile_prefetch_margin Number of tile rows/columns beyond the visible
#'   viewport to prefetch.
#' @param tile_cache_count Maximum number of OpenSeadragon decoded tiles kept
#'   in the browser cache.
#' @param tile_prefetch_cache_count Maximum number of speculative prefetch tile
#'   images kept by wsiTools.
#' @param progressive_preview Whether to keep the low-resolution preview behind
#'   the tiled canvas while high-resolution tiles load.
#' @param dynamic_tile_format Tile image format for the live dynamic tile
#'   server.
#' @param open,wait Browser launch and live bridge servicing options.
#'
#' @return A `wsi_viewer_session` for live viewers.
#' @export
#'
#' @examples
#' \dontrun{
#' viewer <- wsi_open_fast_viewer("very_large_slide.svs")
#' }
wsi_open_fast_viewer <- function(input, ...,
                                 live = "yes",
                                 tiled = "yes",
                                 dynamic_tiles = "yes",
                                 renderer = c("auto", "gpu", "cpu"),
                                 tile_prefetch_margin = 1L,
                                 tile_cache_count = 1024L,
                                 tile_prefetch_cache_count = 768L,
                                 progressive_preview = TRUE,
                                 dynamic_tile_format = "jpg",
                                 open = interactive(),
                                 wait = interactive()) {
  renderer <- wsi_viewer_renderer(if (missing(renderer)) NULL else renderer)
  wsi_open_viewer(
    input,
    ...,
    live = live,
    tiled = tiled,
    dynamic_tiles = dynamic_tiles,
    renderer = renderer,
    tile_prefetch_margin = tile_prefetch_margin,
    tile_cache_count = tile_cache_count,
    tile_prefetch_cache_count = tile_prefetch_cache_count,
    progressive_preview = progressive_preview,
    dynamic_tile_format = dynamic_tile_format,
    open = open,
    wait = wait
  )
}

wsi_open_viewer_flag <- function(x, name, yes_alias = NULL, no_alias = NULL) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(if (isTRUE(x)) "yes" else "no")
  }
  if (!is.character(x) || length(x) < 1L || is.na(x[[1L]]) || !nzchar(x[[1L]])) {
    wsi_abort(sprintf("`%s` must be one of \"auto\", \"yes\", or \"no\".", name))
  }
  value <- tolower(x[[1L]])
  yes_values <- c("yes", "true", "t", "1", yes_alias)
  no_values <- c("no", "false", "f", "0", no_alias)
  if (value %in% yes_values) {
    return("yes")
  }
  if (value %in% no_values) {
    return("no")
  }
  match.arg(value, c("auto", "yes", "no"))
}

wsi_open_viewer_use_live <- function(live, httpuv_ready = requireNamespace("httpuv", quietly = TRUE)) {
  if (identical(live, "yes")) {
    if (!isTRUE(httpuv_ready)) {
      wsi_abort(
        paste(
          "Live viewer mode requires the optional R package `httpuv`.",
          "Install it with: install.packages(\"httpuv\")",
          "Or use: wsi_open_viewer(..., live = \"no\")",
          sep = "\n"
        ),
        class = "wsi_missing_dependency"
      )
    }
    return(TRUE)
  }
  if (identical(live, "no")) {
    return(FALSE)
  }
  isTRUE(httpuv_ready) && interactive()
}

wsi_open_viewer_is_path_vector <- function(input) {
  is.character(input) && length(input) > 1L && !anyNA(input) && all(nzchar(input))
}

wsi_open_viewer_use_tiles <- function(slide, tiled, use_live) {
  if (identical(tiled, "yes")) {
    return(TRUE)
  }
  if (identical(tiled, "no")) {
    return(FALSE)
  }
  if (identical(slide$backend, "mock") || identical(slide$backend, "omezarr")) {
    return(FALSE)
  }
  if (wsi_has_vips()) {
    return(TRUE)
  }
  isTRUE(use_live) && wsi_open_viewer_region_backend_available(slide)
}

wsi_open_viewer_region_backend_available <- function(slide) {
  if (identical(slide$backend, "openslide")) {
    return(wsi_command_exists("openslide-write-png"))
  }
  if (identical(slide$backend, "vips")) {
    return(wsi_has_vips())
  }
  if (identical(slide$backend, "bioformats")) {
    return(wsi_has_bioformats())
  }
  if (identical(slide$backend, "imagemagick")) {
    return(wsi_has_imagemagick())
  }
  FALSE
}

wsi_open_viewer_file_size <- function(slide) {
  path <- slide$path %||% NA_character_
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(NA_real_)
  }
  size <- suppressWarnings(file.info(path)$size)
  if (!is.finite(size)) {
    return(NA_real_)
  }
  as.numeric(size)
}

wsi_open_viewer_large_file_threshold <- function() {
  value <- suppressWarnings(as.numeric(Sys.getenv(
    "WSITOOLS_LARGE_IMAGE_BYTES",
    unset = as.character(5 * 1024^3)
  )))
  if (!is.finite(value) || value <= 0) {
    return(5 * 1024^3)
  }
  value
}

wsi_open_viewer_is_large_file <- function(slide, threshold = wsi_open_viewer_large_file_threshold()) {
  size <- wsi_open_viewer_file_size(slide)
  is.finite(size) && is.finite(threshold) && size >= threshold
}

wsi_open_viewer_use_dynamic_tiles <- function(slide, dynamic_tiles, use_live, use_tiles) {
  if (!isTRUE(use_live) || !isTRUE(use_tiles)) {
    return(FALSE)
  }
  if (identical(dynamic_tiles, "yes")) {
    return(TRUE)
  }
  if (identical(dynamic_tiles, "no")) {
    return(FALSE)
  }
  !wsi_has_vips() || identical(slide$backend, "bioformats") || wsi_open_viewer_is_large_file(slide)
}

wsi_open_viewer_project_route <- function(input, dots, live, use_live,
                                          open, wait, output, overwrite,
                                          czi_sections, quiet) {
  paths <- vapply(input, wsi_validate_input_path, character(1))
  czi <- vapply(paths, wsi_is_czi_path, logical(1))
  if (all(czi) && isTRUE(use_live) && wsi_has_native_czi()) {
    if (!isTRUE(quiet)) {
      message("wsi_open_viewer(): opening live full-resolution CZI project viewer.")
    }
    args <- c(
      list(
        images = paths,
        output = output,
        open = open,
        wait = wait,
        overwrite = overwrite,
        sections = czi_sections
      ),
      dots
    )
    return(do.call(wsi_viewer_czi_project_live, args))
  }

  if (identical(live, "yes") && all(czi) && !wsi_has_native_czi() && !isTRUE(quiet)) {
    message("wsi_open_viewer(): native CZI live tiles are unavailable; using the static section-aware project viewer.")
  } else if (!isTRUE(quiet)) {
    message("wsi_open_viewer(): opening static project viewer.")
  }
  args <- c(
    list(
      images = paths,
      output = output,
      open = open,
      overwrite = overwrite,
      czi_sections = czi_sections
    ),
    dots
  )
  html <- do.call(wsi_viewer_project, args)
  normalizePath(html, winslash = "/", mustWork = FALSE)
}
