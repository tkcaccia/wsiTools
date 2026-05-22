# wsiTools live viewer example
#
# This script opens a whole-slide image in the interactive viewer and keeps
# browser-side work synchronized back into the R session:
#   - drawn/painted/edited annotations
#   - imported GeoJSON annotations
#   - distance measurements
#   - StarDist/cell segmentation overlays
#
# Run from R or RStudio on the remote PC:
#
#   source("/media/user/Lion/Lion/wsitools/example_live_viewer_sync.R")
#
# Or from a shell:
#
#   Rscript /media/user/Lion/Lion/wsitools/example_live_viewer_sync.R
#
# Optional environment variables:
#   WSITOOLS_LIVE_SLIDE       Slide path. Defaults to SAPC 0052.svs in this folder.
#   WSITOOLS_LIVE_MODE        "tiles" or "thumbnail". Defaults to "tiles".
#   WSITOOLS_LIVE_OUTPUT      Output HTML path.
#   WSITOOLS_ENABLE_STARDIST  "auto", "true", or "false". Defaults to "auto".
#   WSITOOLS_STARDIST_COMMAND StarDist command, e.g. python or stardist-predict2d.
#   WSITOOLS_STARDIST_SCRIPT  Optional Python/R script used with command.
#   WSITOOLS_STARDIST_MODEL   StarDist model name. Defaults to 2D_versatile_he.

library(wsiTools)

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (is.null(script_file) || !nzchar(script_file)) {
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  script_file <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else NULL
}
script_dir <- if (is.null(script_file) || !nzchar(script_file)) {
  getwd()
} else {
  dirname(normalizePath(script_file, mustWork = FALSE))
}
slide_path <- Sys.getenv(
  "WSITOOLS_LIVE_SLIDE",
  file.path(script_dir, "SAPC 0052.svs")
)
slide_path <- normalizePath(slide_path, mustWork = FALSE)

if (!file.exists(slide_path)) {
  stop(
    "Slide file not found: ", slide_path, "\n",
    "Set WSITOOLS_LIVE_SLIDE to the slide you want to view."
  )
}

mode <- Sys.getenv("WSITOOLS_LIVE_MODE", "tiles")
if (!mode %in% c("tiles", "thumbnail")) {
  stop("WSITOOLS_LIVE_MODE must be 'tiles' or 'thumbnail'.")
}
if (identical(mode, "tiles") && !wsi_has_vips()) {
  warning("libvips is not available; falling back to WSITOOLS_LIVE_MODE='thumbnail'.")
  mode <- "thumbnail"
}

output_html <- Sys.getenv(
  "WSITOOLS_LIVE_OUTPUT",
  file.path(script_dir, "SAPC_0052_live_viewer.html")
)
tile_dir <- file.path(script_dir, "SAPC_0052_live_viewer_tiles")

message("Opening slide metadata only: ", slide_path)
slide <- wsi_open(slide_path)
on.exit(wsi_close(slide), add = TRUE)

message("Backend status:")
print(wsi_backends())

stardist_command <- Sys.getenv("WSITOOLS_STARDIST_COMMAND", "")
stardist_script <- Sys.getenv("WSITOOLS_STARDIST_SCRIPT", "")
stardist_model <- Sys.getenv("WSITOOLS_STARDIST_MODEL", "2D_versatile_he")
enable_stardist <- tolower(Sys.getenv("WSITOOLS_ENABLE_STARDIST", "auto"))
if (!enable_stardist %in% c("auto", "true", "false")) {
  stop("WSITOOLS_ENABLE_STARDIST must be 'auto', 'true', or 'false'.")
}
stardist_args <- if (nzchar(stardist_script)) {
  c(stardist_script, "{input}", "{output}", "{model}")
} else {
  NULL
}
use_stardist <- switch(
  enable_stardist,
  "true" = TRUE,
  "false" = FALSE,
  "auto" = nzchar(stardist_command) || wsi_has_stardist()
)

if (use_stardist) {
  message("StarDist selected-ROI button will be enabled in the viewer.")
} else {
  message("StarDist command not found; viewer will still support ROI export/import.")
  message("To enable the button, install stardist-predict2d or set WSITOOLS_STARDIST_COMMAND.")
}

message("")
message("Starting live viewer.")
message("In the browser:")
message("  1. Draw, brush, edit, or import GeoJSON ROIs.")
message("  2. Use Measure -> Distance to measure between two points.")
message("  3. If StarDist is enabled, select an ROI and use Segmentation -> Run segmentation.")
message("  4. Return to R with Esc or Ctrl+C. Synced objects will be saved below.")
message("")

session <- wsi_viewer_live(
  slide,
  mode = mode,
  output = output_html,
  tile_dir = if (identical(mode, "tiles")) tile_dir else NULL,
  open = TRUE,
  wait = FALSE,
  name = "viewer_state",
  stardist = use_stardist,
  stardist_output_dir = file.path(script_dir, "stardist_selected_roi"),
  stardist_command = if (nzchar(stardist_command)) stardist_command else NULL,
  stardist_args = stardist_args,
  stardist_model = stardist_model
)

session$on("roi_created", function(roi) {
  message("Callback roi_created: ", nrow(roi), " ROI available.")
})
session$on("roi_selected", function(roi) {
  if (inherits(roi, "wsi_roi") && nrow(roi)) {
    message("Callback roi_selected: ", roi$name[[1]])
  }
})
session$on("segmentation_finished", function(cells) {
  message("Callback segmentation_finished: ", nrow(cells), " cell/segmentation overlay(s).")
})

preview_grid <- head(wsi_tile_grid(slide, tile_size = 2048, level = 0, include_partial = FALSE), 200)
if (nrow(preview_grid)) {
  session$add_layer(
    "Example tile grid",
    preview_grid,
    opacity = 0.45,
    colour = "#facc15",
    service = FALSE
  )
  message("Queued an R-controlled example tile-grid layer.")
}

message("Live callbacks are registered. Press Ctrl+C or Esc in R to stop servicing the viewer.")
tryCatch(
  repeat session$service(100),
  interrupt = function(e) NULL
)

state <- session$get_state(service = FALSE)

message("")
message("Live viewer returned to R.")
message("ROIs synced: ", nrow(state$rois))
message("Measurements synced: ", nrow(state$measurements))
message("Segmentation overlays synced: ", nrow(state$segmentation))

roi_file <- file.path(script_dir, "viewer_state_rois.geojson")
measurement_file <- file.path(script_dir, "viewer_state_measurements.csv")
segmentation_file <- file.path(script_dir, "viewer_state_segmentation.geojson")

if (nrow(state$rois)) {
  write_geojson(state$rois, roi_file, overwrite = TRUE)
  message("Saved synced ROI GeoJSON: ", roi_file)
}

if (nrow(state$measurements)) {
  utils::write.csv(state$measurements, measurement_file, row.names = FALSE)
  message("Saved synced measurements CSV: ", measurement_file)
}

if (nrow(state$segmentation)) {
  write_geojson(state$segmentation, segmentation_file, overwrite = TRUE)
  message("Saved synced segmentation GeoJSON: ", segmentation_file)
}

message("")
message("The live objects are also available in this R session as:")
message("  viewer_state")
message("  viewer_state_rois")
message("  viewer_state_measurements")
message("  viewer_state_segmentation")
message("")
message("The returned session object also exposes:")
message("  session$get_rois()")
message("  session$get_selected_roi()")
message("  session$get_measurements()")
message("  session$get_segmentation()")
message("  session$add_rois(rois)")
message("  session$add_segmentation(cells)")
message("  session$save_project('case_001.wsiproject')")
