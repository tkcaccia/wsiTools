# Open an updated live wsiTools viewer and test trajectory undo.
#
# From this source checkout:
# source("/Users/stefano/Documents/wsitools/inst/examples/open-trajectory-undo-viewer.R")
#
# Local convenience copy:
# source("/Users/stefano/Documents/viewer/open_trajectory_undo_viewer.R")

load_wsitools <- function(repo = Sys.getenv("WSITOOLS_REPO", "/Users/stefano/Documents/wsitools")) {
  if (dir.exists(repo) && requireNamespace("pkgload", quietly = TRUE)) {
    ok <- tryCatch({
      pkgload::load_all(repo, quiet = TRUE)
      TRUE
    }, error = function(err) {
      message("pkgload::load_all() failed: ", conditionMessage(err))
      FALSE
    })
    if (isTRUE(ok)) {
      return(invisible(TRUE))
    }
  }
  library(wsiTools)
  invisible(TRUE)
}

first_existing <- function(paths) {
  paths <- paths[nzchar(paths)]
  hits <- paths[file.exists(paths)]
  if (length(hits)) hits[[1]] else NA_character_
}

stop_old_viewer <- function(name) {
  if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    old <- get(name, envir = .GlobalEnv)
    try(wsi_viewer_stop(old), silent = TRUE)
    rm(list = name, envir = .GlobalEnv)
  }
}

load_wsitools()

if (!requireNamespace("httpuv", quietly = TRUE)) {
  stop("Install optional package `httpuv` to run the live viewer bridge.", call. = FALSE)
}

stop_old_viewer("trajectory_undo_viewer")

viewer_dir <- Sys.getenv(
  "WSITOOLS_TRAJECTORY_VIEWER_DIR",
  "/Users/stefano/Documents/viewer/trajectory_undo_viewer"
)
dir.create(viewer_dir, recursive = TRUE, showWarnings = FALSE)

slide_path <- first_existing(c(
  Sys.getenv("WSITOOLS_VIEWER_IMAGE", unset = ""),
  "/Users/stefano/Downloads/AP-GY-26-04_HE.svs",
  "/Users/stefano/Documents/viewer/Visium_FFPE_Human_Prostate_Cancer_image.tif"
))

if (is.na(slide_path)) {
  stop(
    "No test image found. Set WSITOOLS_VIEWER_IMAGE to a local SVS/TIFF/BTF/OME-TIFF path.",
    call. = FALSE
  )
}

project_images <- c(
  "/Users/stefano/Downloads/2025_10_24__1328.czi",
  "/Users/stefano/Downloads/2025_10_24__1329.czi",
  "/Users/stefano/Documents/CellPhenotyper/remote_previews/apgy2604_ometiff_jpeg_pyramid/gigatime_probs.ome.tif"
)
project_images <- project_images[file.exists(project_images)]

message("Opening: ", slide_path)
slide <- wsi_open(slide_path)

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  dynamic_tiles = TRUE,
  dynamic_tile_format = "jpg",
  dynamic_tile_cache_dir = file.path(viewer_dir, "tile_cache"),
  output = file.path(viewer_dir, "trajectory_undo_viewer.html"),
  overwrite = TRUE,
  project_images = project_images,
  title = "wsiTools trajectory undo test",
  name = "trajectory_undo_viewer",
  autosave_path = file.path(viewer_dir, "trajectory_undo_autosave.wsiproject"),
  autosave_interval = 5,
  transport = "auto",
  open = interactive(),
  wait = FALSE
)

assign("trajectory_undo_slide", slide, envir = .GlobalEnv)
assign("trajectory_undo_viewer", viewer, envir = .GlobalEnv)

cat("\nViewer ready:\n", viewer$url, "\n", sep = "")
cat("HTML file:\n", viewer$html, "\n", sep = "")
cat("\nTrajectory undo test:\n")
cat("1. Press T, or use Trajectories > Draw.\n")
cat("2. Click a few trajectory points.\n")
cat("3. While still drawing, press Ctrl+Z / Command+Z to remove the last point.\n")
cat("4. Double-click or press Enter to save the trajectory.\n")
cat("5. Press Ctrl+Z / Command+Z again to remove the saved trajectory.\n")
cat("6. Press Ctrl+Shift+Z or Ctrl+Y to redo it.\n\n")
cat("Objects in R:\n")
cat("  trajectory_undo_viewer$get_state()\n")
cat("  trajectory_undo_viewer$get_history()\n")
cat("  wsi_viewer_stop(trajectory_undo_viewer)\n\n")

if (interactive()) {
  cat("Keep this R session open while using the viewer. Press Esc/Ctrl+C to pause the service loop.\n")
  tryCatch(
    repeat {
      trajectory_undo_viewer$service(100)
      Sys.sleep(0.02)
    },
    interrupt = function(e) {
      cat("\nLive service loop paused. Run trajectory_undo_viewer$service(100) if needed.\n")
    }
  )
}
