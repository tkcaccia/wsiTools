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
#'     live dynamic tiles are used only when needed or explicitly requested;
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
  if (isTRUE(use_live)) {
    if (isTRUE(opened_slide) && isTRUE(close_slide) && !isTRUE(quiet)) {
      message("wsi_open_viewer(): keeping the opened slide alive inside the returned live viewer session.")
    }
    if (!isTRUE(quiet)) {
      message(sprintf(
        "wsi_open_viewer(): opening live %s viewer%s.",
        mode,
        if (isTRUE(use_dynamic_tiles)) " with dynamic tiles" else ""
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
  !wsi_has_vips() || identical(slide$backend, "bioformats")
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
