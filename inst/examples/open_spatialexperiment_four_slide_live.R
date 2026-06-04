# Open a four-slide SpatialExperiment project in wsiTools.
#
# The multi-slide project viewer keeps the four tissue sections together in one
# Project panel. For live browser-to-R feedback on one section, set
# WSITOOLS_SPE_LIVE_SECTION to the sample ID you want to open as a live session.
#
# From R:
#   Sys.setenv(
#     WSITOOLS_SPE_OBJECT = "/path/to/spatialexperiment.rds",
#     WSITOOLS_SPE_IMAGES = paste(c("/path/s1.tif", "/path/s2.tif", "/path/s3.tif", "/path/s4.tif"), collapse = .Platform$path.sep),
#     WSITOOLS_SPE_SAMPLE_IDS = "sample1,sample2,sample3,sample4"
#   )
#   source(system.file("examples/open_spatialexperiment_four_slide_live.R", package = "wsiTools"))
#
# Optional environment variables:
#   WSITOOLS_SPE_OBJECT          .rds, .rda, or .RData file containing a SpatialExperiment.
#   WSITOOLS_SPE_IMAGES          Four image paths separated by .Platform$path.sep.
#   WSITOOLS_SPE_SAMPLE_IDS      Comma-separated sample IDs matching colData().
#   WSITOOLS_SPE_LABELS          Optional comma-separated labels for the Project panel.
#   WSITOOLS_SPE_REDUCTION       Reduction name. Default: PCA.
#   WSITOOLS_SPE_LIVE_SECTION    Optional sample ID to also open as a live single-section viewer.
#   WSITOOLS_OUTPUT              Project HTML path. Default: open_spatialexperiment_four_slide.html.
#   WSITOOLS_OPEN                true/false. Default: true.
#   WSITOOLS_WAIT                true/false for optional live section. Default: true for Rscript, false for interactive R.

library(wsiTools)

example_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("true", "1", "yes", "y", "on")
}

example_csv <- function(name, default = character()) {
  value <- Sys.getenv(name, "")
  if (!nzchar(value)) {
    return(default)
  }
  trimws(strsplit(value, ",", fixed = TRUE)[[1]])
}

example_paths <- function(name) {
  value <- Sys.getenv(name, "")
  if (!nzchar(value)) {
    stop("Set ", name, " to image paths separated by .Platform$path.sep.", call. = FALSE)
  }
  paths <- strsplit(value, .Platform$path.sep, fixed = TRUE)[[1]]
  paths <- normalizePath(paths[nzchar(paths)], winslash = "/", mustWork = FALSE)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing image file(s):\n", paste(missing, collapse = "\n"), call. = FALSE)
  }
  paths
}

load_spatialexperiment_object <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!file.exists(path)) {
    stop("SpatialExperiment object file not found: ", path, call. = FALSE)
  }
  ext <- tolower(tools::file_ext(path))
  if (identical(ext, "rds")) {
    return(readRDS(path))
  }
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  objects <- mget(loaded, envir = env, inherits = FALSE)
  spe_like <- Filter(function(x) inherits(x, "SpatialExperiment"), objects)
  if (length(spe_like)) {
    return(spe_like[[1L]])
  }
  if (length(objects) == 1L) {
    return(objects[[1L]])
  }
  stop("Could not identify a SpatialExperiment object in: ", path, call. = FALSE)
}

spe_path <- Sys.getenv("WSITOOLS_SPE_OBJECT", "")
if (!nzchar(spe_path)) {
  stop("Set WSITOOLS_SPE_OBJECT to a .rds, .rda, or .RData SpatialExperiment file.", call. = FALSE)
}
spe <- load_spatialexperiment_object(spe_path)
images <- example_paths("WSITOOLS_SPE_IMAGES")
sample_ids <- example_csv("WSITOOLS_SPE_SAMPLE_IDS")
if (!length(sample_ids)) {
  sample_ids <- tools::file_path_sans_ext(basename(images))
}
if (length(sample_ids) != length(images)) {
  stop("WSITOOLS_SPE_SAMPLE_IDS must have the same length as WSITOOLS_SPE_IMAGES.", call. = FALSE)
}
names(images) <- sample_ids
labels <- example_csv("WSITOOLS_SPE_LABELS", default = sample_ids)
reduction <- Sys.getenv("WSITOOLS_SPE_REDUCTION", "PCA")
output <- Sys.getenv("WSITOOLS_OUTPUT", file.path(getwd(), "open_spatialexperiment_four_slide.html"))
open_browser <- example_bool("WSITOOLS_OPEN", TRUE)
wait <- example_bool("WSITOOLS_WAIT", !interactive())
mode <- if (wsi_has_vips()) "tiles" else "thumbnail"

message("Backend status:")
print(wsi_backends())
message("Opening SpatialExperiment project with ", length(images), " section(s).")

html <- wsi_viewer_spatialexperiment_project(
  spe,
  images = images,
  sample_ids = sample_ids,
  labels = labels,
  reduction = reduction,
  mode = mode,
  output = output,
  open = open_browser,
  overwrite = TRUE
)

assign("spatialexperiment_four_slide_viewer_html", html, envir = .GlobalEnv)
message("Project viewer HTML saved as `spatialexperiment_four_slide_viewer_html`: ", normalizePath(html, winslash = "/", mustWork = FALSE))

live_section <- Sys.getenv("WSITOOLS_SPE_LIVE_SECTION", "")
if (nzchar(live_section)) {
  if (!live_section %in% sample_ids) {
    stop("WSITOOLS_SPE_LIVE_SECTION must be one of: ", paste(sample_ids, collapse = ", "), call. = FALSE)
  }
  message("Opening live R-synced SpatialExperiment section: ", live_section)
  live_viewer <- wsi_viewer_spatialexperiment(
    spe,
    image = images[[live_section]],
    sample_id = live_section,
    image_name = live_section,
    reduction = reduction,
    live = TRUE,
    dynamic_tiles = TRUE,
    output = file.path(dirname(output), paste0("open_spatialexperiment_", live_section, "_live.html")),
    open = open_browser,
    overwrite = TRUE,
    wait = wait,
    name = "spatialexperiment_live_viewer_state"
  )
  assign("spatialexperiment_live_viewer", live_viewer, envir = .GlobalEnv)
  message("Live section viewer saved as `spatialexperiment_live_viewer`.")
  message("Selected spots: spatialexperiment_live_viewer$get_selected_spots()")
}
