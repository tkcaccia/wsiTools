#' Check available WSI backends
#'
#' `wsi_backends()` reports whether optional command-line backends are available.
#' The package can be installed with no WSI backend present; backend-dependent
#' functions fail with informative errors when a required tool is missing.
#'
#' @return A data frame with backend name, installation status, version,
#'   capabilities, and notes.
#' @export
#' @examples
#' wsi_backends()
wsi_backends <- function() {
  data.frame(
    backend = c("openslide", "libvips", "bioformats", "imagemagick"),
    installed = c(
      wsi_has_openslide(),
      wsi_has_vips(),
      wsi_has_bioformats(),
      wsi_command_exists("magick") || requireNamespace("magick", quietly = TRUE)
    ),
    version = c(
      wsi_command_version("openslide-show-properties"),
      wsi_command_version("vips"),
      wsi_bioformats_version(),
      wsi_command_version("magick")
    ),
    capabilities = c(
      "metadata, pyramid levels, region reads via openslide-write-png when available",
      "large-image conversion, thumbnails, cropping, pyramidal TIFF export",
      "future microscopy format bridge through Bio-Formats command-line tools",
      "optional image preview and R array/raster conversion"
    ),
    notes = c(
      "Requires OpenSlide command-line tools for this milestone; native C bindings are planned.",
      "Requires vips and vipsheader on PATH.",
      "Optional future backend; not required for OpenSlide/libvips workflows.",
      "Optional; useful for returning image data to R."
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname wsi_backends
#' @return `TRUE` or `FALSE` for `wsi_has_*()` helpers.
#' @export
wsi_has_openslide <- function() {
  wsi_command_exists("openslide-show-properties")
}

#' @rdname wsi_backends
#' @export
wsi_has_vips <- function() {
  wsi_command_exists("vips") && wsi_command_exists("vipsheader")
}

#' @rdname wsi_backends
#' @export
wsi_has_bioformats <- function() {
  wsi_command_exists("bfconvert") || wsi_command_exists("showinf")
}

wsi_bioformats_version <- function() {
  if (wsi_command_exists("bfconvert")) {
    return(wsi_command_version("bfconvert", "-version"))
  }
  if (wsi_command_exists("showinf")) {
    return(wsi_command_version("showinf", "-version"))
  }
  NA_character_
}

wsi_choose_open_backend <- function() {
  if (wsi_has_openslide()) {
    return("openslide")
  }
  if (wsi_has_vips()) {
    return("vips")
  }
  wsi_abort(
    "No WSI backend is available. Install OpenSlide command-line tools or libvips, then retry.",
    class = "wsi_backend_unavailable"
  )
}

wsi_choose_region_backend <- function(slide, backend = c("auto", "vips", "openslide")) {
  backend <- match.arg(backend)
  if (backend != "auto") {
    return(backend)
  }
  if (wsi_has_vips()) {
    return("vips")
  }
  if (identical(slide$backend, "openslide") && wsi_command_exists("openslide-write-png")) {
    return("openslide")
  }
  slide$backend
}
