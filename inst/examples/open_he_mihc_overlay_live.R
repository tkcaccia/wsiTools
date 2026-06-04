# Open an H&E image with a registered mIHC/OME-TIFF channel overlay.
#
# From R:
#   Sys.setenv(
#     WSITOOLS_HE_IMAGE = "/path/to/he_slide.svs",
#     WSITOOLS_MIHC_IMAGE = "/path/to/gigatime_probs.ome.tif",
#     WSITOOLS_MIHC_REGISTRATION = "/path/to/shift.json"
#   )
#   source(system.file("examples/open_he_mihc_overlay_live.R", package = "wsiTools"))
#
# Optional environment variables:
#   WSITOOLS_HE_IMAGE            H&E image path.
#   WSITOOLS_MIHC_IMAGE          mIHC/GigaTIME OME-TIFF path.
#   WSITOOLS_MIHC_REGISTRATION   Optional shift/registration JSON.
#   WSITOOLS_CHANNEL_NAMES       Optional comma-separated channel names.
#   WSITOOLS_CHANNEL_COLOURS     Optional comma-separated channel hex colours.
#   WSITOOLS_CHANNEL_OPACITY     Numeric opacity. Default: 0.55.
#   WSITOOLS_OUTPUT              Output HTML path. Default: open_he_mihc_overlay_live.html.
#   WSITOOLS_OPEN                true/false. Default: true.
#   WSITOOLS_WAIT                true/false. Default: true for Rscript, false for interactive R.

library(wsiTools)

example_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("true", "1", "yes", "y", "on")
}

example_csv <- function(name) {
  value <- Sys.getenv(name, "")
  if (!nzchar(value)) {
    return(NULL)
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

example_path <- function(name, label, choose = interactive()) {
  path <- Sys.getenv(name, "")
  if (!nzchar(path) && isTRUE(choose)) {
    message("Choose ", label, "...")
    path <- file.choose()
  }
  if (!nzchar(path)) {
    stop("Set ", name, " to ", label, ".", call. = FALSE)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

he <- example_path("WSITOOLS_HE_IMAGE", "an H&E image path")
mihc <- example_path("WSITOOLS_MIHC_IMAGE", "an mIHC/GigaTIME OME-TIFF image path")
registration <- Sys.getenv("WSITOOLS_MIHC_REGISTRATION", "")
registration <- if (nzchar(registration)) normalizePath(registration, winslash = "/", mustWork = FALSE) else NULL

for (path in c(he, mihc, registration)) {
  if (!is.null(path) && !file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
}

channel_names <- example_csv("WSITOOLS_CHANNEL_NAMES")
channel_colours <- example_csv("WSITOOLS_CHANNEL_COLOURS")
opacity <- suppressWarnings(as.numeric(Sys.getenv("WSITOOLS_CHANNEL_OPACITY", "0.55")))
if (is.na(opacity) || opacity < 0 || opacity > 1) {
  opacity <- 0.55
}
output <- Sys.getenv("WSITOOLS_OUTPUT", file.path(getwd(), "open_he_mihc_overlay_live.html"))
open_browser <- example_bool("WSITOOLS_OPEN", TRUE)
wait <- example_bool("WSITOOLS_WAIT", !interactive())

message("Backend status:")
print(wsi_backends())

viewer <- wsi_viewer_he_mihc(
  he = he,
  mihc = mihc,
  registration = registration,
  channel_names = channel_names,
  colours = channel_colours,
  opacity = opacity,
  visible = TRUE,
  dynamic_tiles = TRUE,
  dynamic_tile_format = "jpg",
  mode = "tiles",
  stain = "he",
  base_layer_name = "H&E",
  output = output,
  overwrite = TRUE,
  open = open_browser,
  wait = wait,
  transport = "auto",
  name = "he_mihc_overlay_live_viewer_state"
)

assign("he_mihc_overlay_live_viewer", viewer, envir = .GlobalEnv)
message("Viewer object saved as `he_mihc_overlay_live_viewer`.")
message("Use the top Stains menu to show/hide the H&E base and mIHC channels.")
message("Inspect channel settings with: he_mihc_overlay_live_viewer$get_channel_settings()")
