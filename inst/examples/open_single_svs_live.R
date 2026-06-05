# Open one SVS/WSI image in a live wsiTools viewer.
#
# From R:
#   Sys.setenv(WSITOOLS_SVS = "/path/to/sample.svs")
#   source(system.file("examples/open_single_svs_live.R", package = "wsiTools"))
#
# From a shell:
#   WSITOOLS_SVS="/path/to/sample.svs" Rscript inst/examples/open_single_svs_live.R
#
# Optional environment variables:
#   WSITOOLS_SVS                 Slide path. Interactive R falls back to file.choose().
#   WSITOOLS_OUTPUT              Output HTML path. Default: open_single_svs_live.html.
#   WSITOOLS_TILE_DIR            Optional Deep Zoom tile directory to reuse.
#   WSITOOLS_TILE_FORMAT         jpg/png. Default: jpg.
#   WSITOOLS_TILE_QUALITY        JPEG quality for prebuilt tiles. Default: 90.
#   WSITOOLS_REBUILD_TILES       true/false. Default: false.
#   WSITOOLS_DYNAMIC_TILES       true/false. Default: false.
#   WSITOOLS_DYNAMIC_TILE_FORMAT jpg/png/jpeg. Default: jpg.
#   WSITOOLS_OPEN                true/false. Default: true.
#   WSITOOLS_WAIT                true/false. Default: true for Rscript, false for interactive R.

library(wsiTools)

example_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("true", "1", "yes", "y", "on")
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

slide_path <- example_path("WSITOOLS_SVS", "an SVS or WSI image path")
if (!file.exists(slide_path)) {
  stop("Slide file not found: ", slide_path, call. = FALSE)
}
if (!identical(tolower(tools::file_ext(slide_path)), "svs")) {
  message("Note: file extension is not .svs; wsi_open() will still try available backends.")
}

output <- Sys.getenv("WSITOOLS_OUTPUT", file.path(getwd(), "open_single_svs_live.html"))
tile_dir <- Sys.getenv("WSITOOLS_TILE_DIR", "")
tile_dir <- if (nzchar(tile_dir)) normalizePath(tile_dir, winslash = "/", mustWork = FALSE) else NULL
tile_format <- Sys.getenv("WSITOOLS_TILE_FORMAT", "jpg")
tile_quality <- as.integer(Sys.getenv("WSITOOLS_TILE_QUALITY", "90"))
if (!is.finite(tile_quality)) {
  tile_quality <- 90L
}
rebuild_tiles <- example_bool("WSITOOLS_REBUILD_TILES", FALSE)
dynamic_tiles <- example_bool("WSITOOLS_DYNAMIC_TILES", FALSE)
dynamic_format <- Sys.getenv("WSITOOLS_DYNAMIC_TILE_FORMAT", "jpg")
open_browser <- example_bool("WSITOOLS_OPEN", TRUE)
wait <- example_bool("WSITOOLS_WAIT", !interactive())

message("Backend status:")
print(wsi_backends())

message("Opening slide metadata only: ", slide_path)
slide <- wsi_open(slide_path)

if (isTRUE(dynamic_tiles)) {
  message("Using live dynamic tiles. This avoids prebuilding tiles but can be slower for very large WSI files.")
} else {
  message("Using prebuilt Deep Zoom tiles. First run may take a while; later runs reuse the tile pyramid.")
}

viewer_args <- list(
  slide = slide,
  mode = "tiles",
  dynamic_tiles = dynamic_tiles,
  output = output,
  overwrite = TRUE,
  open = open_browser,
  wait = wait,
  transport = "auto",
  name = "single_svs_live_viewer_state"
)
if (isTRUE(dynamic_tiles)) {
  viewer_args$dynamic_tile_format <- dynamic_format
} else {
  viewer_args$tile_dir <- tile_dir
  viewer_args$tile_format <- tile_format
  viewer_args$quality <- tile_quality
  viewer_args$rebuild <- rebuild_tiles
}

viewer <- do.call(wsi_viewer_live, viewer_args)

assign("single_svs_live_viewer", viewer, envir = .GlobalEnv)
message("Viewer object saved as `single_svs_live_viewer`.")
message("Retrieve browser state with:")
message("  single_svs_live_viewer$get_rois()")
message("  single_svs_live_viewer$get_selected_roi()")
message("  single_svs_live_viewer$get_measurements()")
message("Stop the live viewer with:")
message("  single_svs_live_viewer$stop()")
