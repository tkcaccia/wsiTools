# Example: save and reopen a reproducible wsiTools project/session
#
# This script is written for the remote PC project folder:
#   /media/user/Lion/Lion/wsitools
#
# It demonstrates:
# - loading the current local wsiTools source, avoiding stale installed code
# - opening a WSI/BTF image without loading all pixels
# - importing GeoJSON ROIs
# - creating a tile manifest without reading tile pixels
# - calculating measurements and optional H-DAB stain summaries from one region
# - saving a reopenable .wsiproject directory
# - reopening the project and inspecting the restored objects

base_dir <- Sys.getenv("WSITOOLS_BASE_DIR", "/media/user/Lion/Lion/wsitools")
output_dir <- Sys.getenv(
  "WSITOOLS_PROJECT_OUTPUT_DIR",
  file.path(base_dir, "example_project_session_output")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

load_wsitools <- function(base_dir) {
  source_description <- file.path(base_dir, "DESCRIPTION")
  if (file.exists(source_description) && requireNamespace("devtools", quietly = TRUE)) {
    message("Loading wsiTools from local source: ", base_dir)
    devtools::load_all(base_dir, quiet = TRUE)
  } else if (file.exists(source_description) && requireNamespace("pkgload", quietly = TRUE)) {
    message("Loading wsiTools from local source with pkgload: ", base_dir)
    pkgload::load_all(base_dir, quiet = TRUE)
  } else {
    message("Loading installed wsiTools package")
    library(wsiTools)
  }

  required <- c("wsi_project", "wsi_read_project", "measurement_report")
  missing <- required[!vapply(required, exists, logical(1), mode = "function")]
  if (length(missing)) {
    stop(
      "Could not load the required wsiTools project functions: ",
      paste(missing, collapse = ", "),
      "\nCheck that WSITOOLS_BASE_DIR points to the current wsiTools source directory.",
      call. = FALSE
    )
  }
}

first_existing <- function(paths) {
  paths <- paths[file.exists(paths)]
  if (length(paths)) paths[[1L]] else NA_character_
}

open_viewer_file <- function(file) {
  file <- normalizePath(file, winslash = "/", mustWork = TRUE)
  file_url <- paste0("file://", file)
  message("Opening viewer: ", file_url)

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::viewer(file_url)
    return(invisible(file_url))
  }

  has_display <- .Platform$OS.type == "windows" ||
    identical(Sys.info()[["sysname"]], "Darwin") ||
    nzchar(Sys.getenv("DISPLAY")) ||
    nzchar(Sys.getenv("WAYLAND_DISPLAY"))

  if (!has_display) {
    message(
      "No graphical display was detected for this R session. ",
      "Open the Viewer HTML path in the remote PC browser."
    )
    return(invisible(file_url))
  }

  tryCatch(
    utils::browseURL(file_url),
    error = function(err) {
      message("Could not open the viewer automatically: ", conditionMessage(err))
      message("Open this file manually: ", file)
    }
  )
  invisible(file_url)
}

make_example_roi <- function(slide, file) {
  width <- min(4000, slide$dimensions[["width"]] / 4)
  height <- min(4000, slide$dimensions[["height"]] / 4)
  x0 <- max(0, slide$dimensions[["width"]] * 0.10)
  y0 <- max(0, slide$dimensions[["height"]] * 0.10)
  geojson <- list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature",
      id = "example_roi_1",
      properties = list(
        name = "Example ROI",
        classification = list(name = "tumour"),
        class = "tumour"
      ),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          list(x0, y0),
          list(x0 + width, y0),
          list(x0 + width, y0 + height),
          list(x0, y0 + height),
          list(x0, y0)
        ))
      )
    ))
  )
  jsonlite::write_json(geojson, file, auto_unbox = TRUE, pretty = TRUE)
  file
}

load_wsitools(base_dir)

slide_path <- Sys.getenv("WSITOOLS_SLIDE", unset = NA_character_)
if (is.na(slide_path) || !nzchar(slide_path)) {
  slide_path <- first_existing(file.path(
    base_dir,
    c(
      "SAPC 0052.svs",
      "Visium_HD_6p5mm_Human_Heart_tissue_image.btf"
    )
  ))
}
if (is.na(slide_path) || !file.exists(slide_path)) {
  stop("No slide found. Set WSITOOLS_SLIDE=/path/to/file.svs or .btf", call. = FALSE)
}

roi_path <- Sys.getenv("WSITOOLS_ROI_GEOJSON", unset = NA_character_)
if (is.na(roi_path) || !nzchar(roi_path)) {
  roi_path <- first_existing(file.path(
    base_dir,
    c(
      "example-qupath-annotations.geojson",
      "example_annotations.geojson",
      "Visium_HD.geojson"
    )
  ))
}

message("Opening slide: ", slide_path)
slide <- wsi_open(slide_path)
on.exit(wsi_close(slide), add = TRUE)

if (is.na(roi_path) || !file.exists(roi_path)) {
  roi_path <- make_example_roi(slide, file.path(output_dir, "example_roi.geojson"))
  message("No ROI GeoJSON found; created: ", roi_path)
}
message("Reading ROIs: ", roi_path)
rois <- read_geojson(roi_path)

create_viewer <- !tolower(Sys.getenv("WSITOOLS_CREATE_VIEWER", "true")) %in% c("false", "0", "no")
open_viewer <- !tolower(Sys.getenv("WSITOOLS_OPEN_VIEWER", "true")) %in% c("false", "0", "no")
viewer_mode <- Sys.getenv("WSITOOLS_VIEWER_MODE", "tiles")
if (!viewer_mode %in% c("tiles", "thumbnail")) {
  viewer_mode <- "tiles"
}
viewer_html <- NA_character_
if (create_viewer) {
  viewer_html <- tryCatch(
    wsi_viewer(
      slide,
      roi = rois,
      mode = viewer_mode,
      output = file.path(output_dir, "project_session_viewer.html"),
      tile_dir = file.path(output_dir, "viewer_tiles"),
      overwrite = TRUE,
      open = FALSE
    ),
    error = function(err) {
      message("Viewer creation skipped: ", conditionMessage(err))
      NA_character_
    }
  )
  if (!is.na(viewer_html)) {
    message("Viewer HTML: ", viewer_html)
    if (open_viewer) {
      open_viewer_file(viewer_html)
    } else {
      message("Viewer was written but not opened because WSITOOLS_OPEN_VIEWER=FALSE.")
    }
  }
} else {
  message("Viewer creation skipped because WSITOOLS_CREATE_VIEWER=FALSE.")
}

message("Creating coordinate-only tile manifest")
tile_manifest <- tryCatch(
  extract_tiles(
    slide,
    roi = rois,
    tile_size = 512,
    stride = 512,
    save_images = FALSE,
    max_tiles = 50,
    seed = 2026,
    manifest_file = file.path(output_dir, "tile_manifest.csv"),
    overwrite = TRUE
  ),
  error = function(err) {
    message("ROI tile extraction skipped; falling back to whole-slide grid: ", conditionMessage(err))
    grid <- wsi_tile_grid(slide, tile_size = 512, level = 0)
    grid <- utils::head(grid, 50)
    class(grid) <- c("wsi_tile_manifest", class(grid))
    utils::write.csv(as.data.frame(grid), file.path(output_dir, "tile_manifest.csv"), row.names = FALSE)
    grid
  }
)

roi_center_x <- rowMeans(cbind(rois$xmin, rois$xmax))
roi_center_y <- rowMeans(cbind(rois$ymin, rois$ymax))
cells <- data.frame(
  cell_id = paste0("example_cell_", seq_along(roi_center_x)),
  x = roi_center_x,
  y = roi_center_y,
  stringsAsFactors = FALSE
)

deconv_x <- max(0, floor(rois$xmin[[1L]]))
deconv_y <- max(0, floor(rois$ymin[[1L]]))
stain_channels <- tryCatch(
  wsi_deconvolve_region(
    slide,
    x = deconv_x,
    y = deconv_y,
    width = 512,
    height = 512,
    level = 0
  ),
  error = function(err) {
    message("Stain deconvolution skipped: ", conditionMessage(err))
    NULL
  }
)

message("Creating measurement report")
report <- measurement_report(
  rois,
  cells = cells,
  stains = stain_channels,
  image_origin = c(x = deconv_x, y = deconv_y),
  pixel_size = wsi_mpp(slide),
  output_dir = file.path(output_dir, "measurement_report"),
  prefix = "example_case",
  overwrite = TRUE
)

viewer_state <- list(
  view = list(
    zoom = 1,
    center = list(
      x = slide$dimensions[["width"]] / 2,
      y = slide$dimensions[["height"]] / 2
    )
  ),
  stain = if (!is.null(stain_channels)) {
    list(type = "H-DAB", enabled = TRUE)
  } else {
    list(enabled = FALSE)
  },
  rois = rois,
  measurements = data.frame(
    id = character(),
    start_x = numeric(),
    start_y = numeric(),
    end_x = numeric(),
    end_y = numeric(),
    distance_px = numeric(),
    distance_um = numeric(),
    stringsAsFactors = FALSE
  ),
  segmentation = data.frame(),
  events = list(list(event = "project_example_created")),
  last_event = "project_example_created"
)

project_dir <- file.path(output_dir, "example_case.wsiproject")
message("Saving project: ", project_dir)
project <- wsi_project(
  project_dir,
  slide = slide,
  viewer_state = viewer_state,
  rois = rois,
  measurements = report,
  segmentation = cells,
  stains = stain_channels,
  tile_manifest = tile_manifest,
  metadata = list(
    case_id = "example_case",
    source_slide = basename(slide_path),
    roi_source = basename(roi_path),
    viewer_html = viewer_html
  ),
  overwrite = TRUE
)

message("Reopening project")
reopened <- wsi_read_project(project_dir)

print(project)
print(reopened)

message("\nCreated files:")
message("  Viewer HTML: ", viewer_html)
message("  Project:     ", project_dir)
message("  Manifest:    ", file.path(project_dir, "project.json"))
message("  ROIs:        ", file.path(project_dir, "rois.geojson"))
message("  Measurements:", file.path(project_dir, "measurements"))
message("  Tiles CSV:   ", file.path(project_dir, "tile_manifests", "tile_manifest.csv"))
