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
#   WSITOOLS_SPE_DIR             Folder containing the SpatialExperiment object and images.
#   WSITOOLS_SPE_OBJECT          .rds, .rda, or .RData file containing a SpatialExperiment.
#                                If omitted, the script searches WSITOOLS_SPE_DIR.
#   WSITOOLS_SPE_IMAGES          Four image paths separated by .Platform$path.sep.
#                                If omitted, the script searches WSITOOLS_SPE_DIR.
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

example_spatial_dir <- function() {
  path <- Sys.getenv("WSITOOLS_SPE_DIR", "")
  if (!nzchar(path)) {
    object_path <- Sys.getenv("WSITOOLS_SPE_OBJECT", "")
    if (nzchar(object_path)) {
      path <- dirname(object_path)
    }
  }
  if (!nzchar(path)) {
    return("")
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

example_find_spatialexperiment_object <- function(dir) {
  if (!nzchar(dir) || !dir.exists(dir)) {
    return("")
  }
  candidates <- list.files(
    dir,
    pattern = "\\.(rds|rda|rdata)$",
    recursive = FALSE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(candidates)) {
    return("")
  }
  score <- grepl("spatial|spe|object|obj|obejct", basename(candidates), ignore.case = TRUE)
  candidates <- c(candidates[score], candidates[!score])
  normalizePath(candidates[[1L]], winslash = "/", mustWork = FALSE)
}

example_find_images <- function(dir) {
  if (!nzchar(dir) || !dir.exists(dir)) {
    return(character())
  }
  candidates <- list.files(
    dir,
    pattern = "\\.(tif|tiff|ome\\.tif|ome\\.tiff)$",
    recursive = FALSE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (!length(candidates)) {
    return(character())
  }
  preferred <- grepl("full.*image|image", basename(candidates), ignore.case = TRUE)
  candidates <- c(candidates[preferred], candidates[!preferred])
  candidates <- candidates[order(basename(candidates))]
  normalizePath(candidates, winslash = "/", mustWork = FALSE)
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

spe_dir <- example_spatial_dir()
spe_path <- Sys.getenv("WSITOOLS_SPE_OBJECT", "")
if (!nzchar(spe_path) || !file.exists(spe_path)) {
  detected <- example_find_spatialexperiment_object(spe_dir)
  if (nzchar(detected)) {
    spe_path <- detected
    message("Detected SpatialExperiment object: ", spe_path)
  }
}
if (!nzchar(spe_path)) {
  stop("Set WSITOOLS_SPE_OBJECT to a .rds, .rda, or .RData SpatialExperiment file, or set WSITOOLS_SPE_DIR to a folder containing one.", call. = FALSE)
}
spe <- load_spatialexperiment_object(spe_path)
images <- character()
if (nzchar(Sys.getenv("WSITOOLS_SPE_IMAGES", ""))) {
  images <- tryCatch(
    example_paths("WSITOOLS_SPE_IMAGES"),
    error = function(e) {
      detected_images <- example_find_images(spe_dir)
      if (length(detected_images)) {
        message("Ignoring WSITOOLS_SPE_IMAGES because one or more files were not found.")
        message("Detected image files:\n", paste(detected_images, collapse = "\n"))
        return(detected_images)
      }
      stop(conditionMessage(e), call. = FALSE)
    }
  )
}
if (!length(images)) {
  images <- example_find_images(spe_dir)
  if (!length(images)) {
    stop("Set WSITOOLS_SPE_IMAGES to image paths separated by .Platform$path.sep, or set WSITOOLS_SPE_DIR to a folder containing TIFF images.", call. = FALSE)
  }
  message("Detected image files:\n", paste(images, collapse = "\n"))
}
sample_ids <- example_csv("WSITOOLS_SPE_SAMPLE_IDS")
if (!length(sample_ids)) {
  sample_ids <- sub("_full_image$", "", tools::file_path_sans_ext(basename(images)), ignore.case = TRUE)
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

project_viewer <- wsi_viewer_spatialexperiment_project(
  spe,
  images = images,
  sample_ids = sample_ids,
  labels = labels,
  reduction = reduction,
  mode = mode,
  output = output,
  open = open_browser,
  wait = FALSE,
  overwrite = TRUE
)

project_html <- if (inherits(project_viewer, "wsi_viewer_session")) project_viewer$html else project_viewer
assign("spatialexperiment_four_slide_viewer", project_viewer, envir = .GlobalEnv)
assign("spatialexperiment_four_slide_viewer_html", project_html, envir = .GlobalEnv)
message("Project viewer saved as `spatialexperiment_four_slide_viewer`.")
message("Project viewer HTML saved as `spatialexperiment_four_slide_viewer_html`: ", normalizePath(project_html, winslash = "/", mustWork = FALSE))
if (inherits(project_viewer, "wsi_viewer_session")) {
  message("Live project sync is active. Use: spatialexperiment_four_slide_viewer$get_rois()")
}

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
    dynamic_tiles = TRUE,
    output = file.path(dirname(output), paste0("open_spatialexperiment_", live_section, "_live.html")),
    open = open_browser,
    overwrite = TRUE,
    wait = FALSE,
    name = "spatialexperiment_live_viewer_state"
  )
  assign("spatialexperiment_live_viewer", live_viewer, envir = .GlobalEnv)
  message("Live section viewer saved as `spatialexperiment_live_viewer`.")
  message("Selected spots: spatialexperiment_live_viewer$get_selected_spots()")
}

if (isTRUE(wait) && inherits(project_viewer, "wsi_viewer_session")) {
  message("Press Ctrl+C or Esc to stop the live sync loop and return to R.")
  tryCatch(
    repeat {
      project_viewer$service(100)
      if (exists("live_viewer", inherits = FALSE) && inherits(live_viewer, "wsi_viewer_session")) {
        live_viewer$service(0)
      }
    },
    interrupt = function(e) NULL
  )
}
