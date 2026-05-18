# Example: segment cells inside one selected ROI with StarDist
#
# This script is intentionally usable in two modes:
#   1. R mode: source this file or run it with Rscript.
#   2. CLI mode: use the printed `wsitools stardist-roi` command in a shell.
#
# It does not make StarDist mandatory. By default it exports the selected ROI
# crop and prints the command plan. Set WSITOOLS_STARDIST_RUN=TRUE only after
# you have a StarDist command/script available.
#
# Minimal usage from the package source tree:
#
#   WSITOOLS_IMAGE="/path/to/slide.svs" \
#   WSITOOLS_ROI_GEOJSON="/path/to/selected_roi.geojson" \
#   Rscript inst/examples/stardist-selected-roi-cli.R
#
# Optional StarDist run:
#
#   WSITOOLS_STARDIST_RUN=TRUE \
#   WSITOOLS_STARDIST_COMMAND="python" \
#   WSITOOLS_STARDIST_ARGS="run_stardist.py;{input};{output};{model}" \
#   WSITOOLS_IMAGE="/path/to/slide.svs" \
#   WSITOOLS_ROI_GEOJSON="/path/to/selected_roi.geojson" \
#   Rscript inst/examples/stardist-selected-roi-cli.R
#
# The ROI GeoJSON can be exported from the wsiTools viewer Segmentation menu
# after selecting or drawing an ROI, or from QuPath.

library(wsiTools)

`%||%` <- function(x, y) if (is.null(x)) y else x

read_bool_env <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    return(isTRUE(default))
  }
  value <- tolower(trimws(value))
  if (value %in% c("1", "true", "t", "yes", "y", "on")) {
    return(TRUE)
  }
  if (value %in% c("0", "false", "f", "no", "n", "off")) {
    return(FALSE)
  }
  stop("Environment variable ", name, " must be TRUE or FALSE.", call. = FALSE)
}

split_semicolon_args <- function(value) {
  if (!nzchar(value)) {
    return(NULL)
  }
  tokens <- strsplit(value, ";", fixed = TRUE)[[1L]]
  tokens <- trimws(tokens)
  tokens[nzchar(tokens)]
}

required_path_env <- function(name, description) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) {
    stop(
      "Set ", name, " to ", description, ".\n",
      "Example: Sys.setenv(", name, " = \"/path/to/file\")",
      call. = FALSE
    )
  }
  value <- normalizePath(value, winslash = "/", mustWork = TRUE)
  value
}

slide_path <- required_path_env("WSITOOLS_IMAGE", "the WSI or large-image path")
roi_path <- required_path_env("WSITOOLS_ROI_GEOJSON", "the selected ROI GeoJSON path")

output_dir <- Sys.getenv("WSITOOLS_STARDIST_OUTPUT_DIR", unset = "stardist_selected_roi")
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

roi_id <- Sys.getenv("WSITOOLS_ROI_ID", unset = "")
roi_id <- if (nzchar(roi_id)) roi_id else NULL

level <- suppressWarnings(as.integer(Sys.getenv("WSITOOLS_LEVEL", unset = "0")))
if (is.na(level) || level < 0L) {
  stop("WSITOOLS_LEVEL must be a non-negative integer.", call. = FALSE)
}

crop_format <- Sys.getenv("WSITOOLS_CROP_FORMAT", unset = "png")
model <- Sys.getenv("WSITOOLS_STARDIST_MODEL", unset = "2D_versatile_he")
command <- Sys.getenv("WSITOOLS_STARDIST_COMMAND", unset = "")
command <- if (nzchar(command)) command else NULL
args <- split_semicolon_args(Sys.getenv("WSITOOLS_STARDIST_ARGS", unset = ""))
run_stardist <- read_bool_env("WSITOOLS_STARDIST_RUN", default = FALSE)
overwrite <- read_bool_env("WSITOOLS_OVERWRITE", default = FALSE)

message("Opening slide metadata: ", slide_path)
slide <- wsi_open(slide_path)
on.exit(wsi_close(slide), add = TRUE)

message("Reading selected ROI GeoJSON: ", roi_path)
rois <- read_geojson(roi_path)
if (!nrow(rois)) {
  stop("The ROI GeoJSON did not contain any polygon regions.", call. = FALSE)
}
selected_roi <- roi_id %||% rois$roi_id[[1L]]

viewer_file <- file.path(output_dir, "selected_roi_viewer.html")
message("Writing viewer with ROI overlay: ", viewer_file)
viewer_add_rois(
  slide,
  rois,
  mode = "tiles",
  output = viewer_file,
  tile_dir = file.path(output_dir, "selected_roi_tiles")
)

message("Exporting selected ROI crop", if (run_stardist) " and running StarDist." else " and printing StarDist plan.")
result <- stardist_segment_roi(
  image = slide,
  roi = rois,
  output_dir = output_dir,
  roi_id = selected_roi,
  level = level,
  crop_format = crop_format,
  model = model,
  command = command,
  args = args,
  overwrite = overwrite,
  run = run_stardist
)
print(result)

if (run_stardist && inherits(result$segmentation, "wsi_segmentation")) {
  overlay_file <- file.path(output_dir, "stardist_overlay_viewer.html")
  message("Writing viewer with StarDist overlay: ", overlay_file)
  viewer_add_segmentation(
    slide,
    result$segmentation,
    mode = "tiles",
    output = overlay_file,
    tile_dir = file.path(output_dir, "stardist_overlay_tiles")
  )
}

wsitools_bin <- system.file("exec", "wsitools", package = "wsiTools")
if (!nzchar(wsitools_bin)) {
  wsitools_bin <- "./exec/wsitools"
}

cli_args <- c(
  "stardist-roi",
  "--image", slide_path,
  "--roi", roi_path,
  "--output-dir", output_dir,
  "--roi-id", selected_roi,
  "--level", as.character(level),
  "--crop-format", crop_format,
  "--model", model
)
if (!is.null(command)) {
  cli_args <- c(cli_args, "--command", command)
}
for (token in args %||% character()) {
  cli_args <- c(cli_args, "--arg", token)
}
if (overwrite) {
  cli_args <- c(cli_args, "--overwrite")
}
if (!run_stardist) {
  cli_args <- c(cli_args, "--plan")
}

message("")
message("Equivalent command-line invocation:")
message(paste(shQuote(c(wsitools_bin, cli_args)), collapse = " "))
message("")
message("Outputs:")
message("  ROI viewer: ", normalizePath(viewer_file, winslash = "/", mustWork = FALSE))
message("  ROI crop:   ", normalizePath(result$crop %||% "", winslash = "/", mustWork = FALSE))
if (!is.null(result$output)) {
  message("  StarDist:   ", normalizePath(result$output, winslash = "/", mustWork = FALSE))
}
if (!is.null(result$slide_output)) {
  message("  Slide GeoJSON: ", normalizePath(result$slide_output, winslash = "/", mustWork = FALSE))
}
