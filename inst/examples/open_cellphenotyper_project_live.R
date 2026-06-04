# Open a CellPhenotyper project in a live wsiTools viewer.
#
# From R:
#   Sys.setenv(WSITOOLS_CELLPHENOTYPER_PROJECT = "/path/to/CellPhenotyper_outputs")
#   source(system.file("examples/open_cellphenotyper_project_live.R", package = "wsiTools"))
#
# The project directory should contain 00_execution/project_outputs.tsv.
#
# Optional environment variables:
#   WSITOOLS_CELLPHENOTYPER_PROJECT CellPhenotyper output directory.
#   WSITOOLS_OUTPUT                  Output HTML path. Default: open_cellphenotyper_project_live.html.
#   WSITOOLS_DYNAMIC_TILES           true/false. Default: true.
#   WSITOOLS_GIGATIME_OVERLAY        true/false. Default: true.
#   WSITOOLS_OPEN                    true/false. Default: true.
#   WSITOOLS_WAIT                    true/false. Default: true for Rscript, false for interactive R.

library(wsiTools)

example_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("true", "1", "yes", "y", "on")
}

project_dir <- Sys.getenv("WSITOOLS_CELLPHENOTYPER_PROJECT", "")
if (!nzchar(project_dir)) {
  stop("Set WSITOOLS_CELLPHENOTYPER_PROJECT to a CellPhenotyper output directory.", call. = FALSE)
}
project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
if (!dir.exists(project_dir)) {
  stop("CellPhenotyper project directory not found: ", project_dir, call. = FALSE)
}

output <- Sys.getenv("WSITOOLS_OUTPUT", file.path(getwd(), "open_cellphenotyper_project_live.html"))
dynamic_tiles <- example_bool("WSITOOLS_DYNAMIC_TILES", TRUE)
gigatime_overlay <- example_bool("WSITOOLS_GIGATIME_OVERLAY", TRUE)
open_browser <- example_bool("WSITOOLS_OPEN", TRUE)
wait <- example_bool("WSITOOLS_WAIT", !interactive())

message("Backend status:")
print(wsi_backends())

project <- wsi_read_cellphenotyper_project(project_dir)
print(project)
message("Input image: ", project$input_image)
gigatime_probs <- if (!is.null(project$files$gigatime_probs)) project$files$gigatime_probs else NA_character_
if (!is.na(gigatime_probs) && nzchar(gigatime_probs)) {
  message("GigaTIME probability OME-TIFF: ", project$files$gigatime_probs)
}

viewer <- wsi_viewer_cellphenotyper(
  project,
  output = output,
  mode = "tiles",
  live = TRUE,
  dynamic_tiles = dynamic_tiles,
  gigatime_overlay = gigatime_overlay,
  open = open_browser,
  overwrite = TRUE,
  wait = wait,
  transport = "auto"
)

assign("cellphenotyper_project_live_viewer", viewer, envir = .GlobalEnv)
message("Viewer object saved as `cellphenotyper_project_live_viewer`.")
message("Useful commands:")
message("  cellphenotyper_project_live_viewer$get_rois()")
message("  cellphenotyper_project_live_viewer$get_segmentation()")
message("  cellphenotyper_project_live_viewer$get_channel_settings()")
message("  cellphenotyper_project_live_viewer$save_project('case_01.wsiproject')")
