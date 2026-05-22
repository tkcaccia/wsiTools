#' Create a first-run guided example project
#'
#' Builds a small self-contained example project that can be run immediately
#' after installing wsiTools from GitHub. The example uses a mock whole-slide
#' image, synthetic ROI annotations, a labelled mask converted to ROIs,
#' centroid-based segmentation overlays, a coordinate-only tile grid, a saved
#' project directory, and an HTML viewer.
#'
#' @param path Directory where the example project should be written. Defaults
#'   to a temporary directory.
#' @param open Whether to open the generated HTML viewer with
#'   [utils::browseURL()].
#' @param overwrite Whether to overwrite files previously created at `path`.
#'
#' @return A `wsi_first_run_example` list containing the mock slide, ROIs, mask,
#'   segmentation, tile grid, viewer path, project object, and written file
#'   paths.
#' @export
#'
#' @examples
#' \dontrun{
#' example <- wsi_first_run_example(open = TRUE)
#' example$path
#' head(example$tile_grid)
#' }
wsi_first_run_example <- function(path = tempfile("wsiTools-first-run-"),
                                  open = interactive(),
                                  overwrite = FALSE) {
  path <- wsi_first_run_prepare_dir(path)

  slide <- wsi_mock_slide(width = 4096, height = 3072, levels = c(1, 4, 16))
  manual_rois <- wsi_first_run_manual_rois()
  mask <- wsi_first_run_mask()
  mask_rois <- wsi_mask_to_rois(
    mask,
    background = 0,
    class_map = c("1" = "tumour mask", "2" = "stroma mask"),
    color_map = c("1" = "#F97316", "2" = "#22C55E"),
    origin = c(x = 0, y = 0),
    scale = c(x = 32, y = 32),
    min_area = 10,
    prefix = "first_run_mask"
  )
  annotations <- wsi_viewer_bind_rois(manual_rois, mask_rois)

  centroids <- wsi_first_run_centroids()
  segmentation <- wsi_segmentation_to_rois(centroids, radius = 28, label = "nucleus")
  segmentation$name <- paste("Example cell", seq_len(nrow(segmentation)))
  segmentation$object_type <- "detection"
  segmentation$color <- "#38BDF8"
  segmentation$classification_color <- "#38BDF8"

  tile_grid <- wsi_tile_grid(
    slide,
    tile_size = 512,
    overlap = 64,
    level = 0,
    region = c(x = 512, y = 512, width = 2048, height = 1536),
    include_partial = TRUE
  )

  viewer_rois <- wsi_viewer_bind_rois(annotations, segmentation)
  viewer_file <- file.path(path, "first-run-viewer.html")
  wsi_viewer(
    slide,
    output = viewer_file,
    open = FALSE,
    overwrite = overwrite,
    title = "wsiTools first-run mock pathology viewer",
    mode = "thumbnail",
    width = 1400,
    roi = viewer_rois
  )

  files <- wsi_first_run_write_files(
    path = path,
    mask = mask,
    manual_rois = manual_rois,
    mask_rois = mask_rois,
    annotations = annotations,
    centroids = centroids,
    segmentation = segmentation,
    tile_grid = tile_grid,
    viewer_file = viewer_file,
    overwrite = overwrite
  )

  project <- wsi_project(
    path,
    slide = slide,
    viewer_state = list(
      view = list(center = list(x = 2048, y = 1536), zoom = 1),
      rois = annotations,
      segmentation = segmentation,
      last_event = "first_run_example"
    ),
    rois = annotations,
    segmentation = segmentation,
    tile_manifest = tile_grid,
    metadata = list(
      title = "wsiTools first-run guided example",
      description = "Mock slide with synthetic ROIs, mask-derived annotations, segmentation, and a tile grid.",
      viewer_file = basename(viewer_file)
    ),
    processing_provenance = list(
      example = "wsi_first_run_example",
      pixel_data_loaded = FALSE,
      note = "Synthetic mock data for onboarding; no WSI backend required."
    ),
    overwrite = overwrite
  )

  files$project <- file.path(path, "project.json")
  files$rois <- file.path(path, "rois.geojson")
  files$segmentation <- file.path(path, "segmentation.geojson")
  files$tile_manifest <- file.path(path, "tile_manifests", "tile_manifest.csv")

  out <- list(
    path = path,
    slide = slide,
    manual_rois = manual_rois,
    mask = mask,
    mask_rois = mask_rois,
    rois = annotations,
    centroids = centroids,
    segmentation = segmentation,
    tile_grid = tile_grid,
    viewer = viewer_file,
    files = files,
    project = project
  )
  class(out) <- "wsi_first_run_example"

  if (isTRUE(open)) {
    utils::browseURL(viewer_file)
  }
  out
}

wsi_first_run_prepare_dir <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    wsi_abort("`path` must be a single non-empty directory path.")
  }
  if (file.exists(path) && !dir.exists(path)) {
    wsi_abort(sprintf("`path` exists but is not a directory: %s", path))
  }
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create example directory: %s", path))
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

wsi_first_run_ring <- function(points) {
  lapply(seq_len(nrow(points)), function(i) unname(c(points[i, 1L], points[i, 2L])))
}

wsi_first_run_feature <- function(id, name, class, color, points) {
  list(
    type = "Feature",
    id = id,
    properties = list(
      objectType = "annotation",
      name = name,
      label = name,
      class = class,
      classification = list(name = class, color = color),
      source = "wsiTools first-run example"
    ),
    geometry = list(
      type = "Polygon",
      coordinates = list(wsi_first_run_ring(points))
    )
  )
}

wsi_first_run_manual_rois <- function() {
  geojson <- list(
    type = "FeatureCollection",
    name = "wsiTools first-run annotations",
    features = list(
      wsi_first_run_feature(
        id = "manual_tumour_1",
        name = "Example tumour ROI",
        class = "tumour",
        color = "#E11D48",
        points = matrix(
          c(560, 480, 1760, 520, 1900, 1360, 1260, 1680, 600, 1320, 560, 480),
          ncol = 2,
          byrow = TRUE
        )
      ),
      wsi_first_run_feature(
        id = "manual_stroma_1",
        name = "Example stroma ROI",
        class = "stroma",
        color = "#2563EB",
        points = matrix(
          c(2140, 720, 3220, 820, 3520, 1580, 2980, 2240, 2260, 1940, 2140, 720),
          ncol = 2,
          byrow = TRUE
        )
      ),
      wsi_first_run_feature(
        id = "manual_exclusion_1",
        name = "Example exclusion area",
        class = "exclusion",
        color = "#6B7280",
        points = matrix(
          c(1420, 980, 1690, 1020, 1640, 1230, 1390, 1180, 1420, 980),
          ncol = 2,
          byrow = TRUE
        )
      )
    )
  )
  wsi_roi_from_geojson(geojson)
}

wsi_first_run_mask <- function() {
  mask <- matrix(0L, nrow = 96L, ncol = 128L)
  mask[16:54, 18:72] <- 1L
  mask[61:83, 84:116] <- 2L
  mask[33:41, 38:48] <- 0L
  mask
}

wsi_first_run_centroids <- function() {
  cells <- data.frame(
    cell_id = sprintf("cell_%02d", seq_len(10L)),
    x = c(780, 980, 1180, 1450, 1660, 2350, 2600, 2860, 3180, 3360),
    y = c(720, 1060, 1350, 1180, 880, 1120, 1500, 1850, 1380, 1740),
    confidence = c(0.96, 0.91, 0.88, 0.94, 0.9, 0.89, 0.93, 0.86, 0.9, 0.87),
    stringsAsFactors = FALSE
  )
  class(cells) <- c("wsi_segmentation_centroids", "wsi_segmentation", class(cells))
  cells
}

wsi_first_run_write_files <- function(path, mask, manual_rois, mask_rois,
                                      annotations, centroids, segmentation,
                                      tile_grid, viewer_file, overwrite = FALSE) {
  files <- list(
    viewer = viewer_file,
    manual_rois = file.path(path, "manual_rois.geojson"),
    mask = file.path(path, "mask.csv"),
    mask_rois = file.path(path, "mask_rois.geojson"),
    annotations = file.path(path, "annotations_combined.geojson"),
    segmentation_centroids = file.path(path, "segmentation_centroids.csv"),
    segmentation_rois = file.path(path, "segmentation_rois.geojson"),
    tile_grid = file.path(path, "tile_grid.csv"),
    readme = file.path(path, "README.md")
  )

  write_geojson(manual_rois, files$manual_rois, overwrite = overwrite)
  write_geojson(mask_rois, files$mask_rois, overwrite = overwrite)
  write_geojson(annotations, files$annotations, overwrite = overwrite)
  write_geojson(segmentation, files$segmentation_rois, overwrite = overwrite)
  utils::write.csv(mask, wsi_validate_output_path(files$mask, overwrite = overwrite), row.names = FALSE)
  utils::write.csv(as.data.frame(centroids), wsi_validate_output_path(files$segmentation_centroids, overwrite = overwrite), row.names = FALSE)
  utils::write.csv(tile_grid, wsi_validate_output_path(files$tile_grid, overwrite = overwrite), row.names = FALSE)
  wsi_first_run_write_readme(files$readme, overwrite = overwrite)
  files
}

wsi_first_run_write_readme <- function(file, overwrite = FALSE) {
  file <- wsi_validate_output_path(file, overwrite = overwrite)
  writeLines(
    c(
      "# wsiTools First-Run Guided Example",
      "",
      "This directory was created by `wsi_first_run_example()`.",
      "",
      "Contents:",
      "",
      "- `project.json`: reproducible wsiTools project manifest.",
      "- `first-run-viewer.html`: interactive mock slide viewer with overlays.",
      "- `rois.geojson`: manual and mask-derived annotations saved by the project.",
      "- `segmentation.geojson`: synthetic nucleus segmentation overlay.",
      "- `tile_manifests/tile_manifest.csv`: coordinate-only tile grid.",
      "- `mask.csv`: small labelled mask used to create mask-derived ROIs.",
      "- `segmentation_centroids.csv`: synthetic cell centroid table.",
      "",
      "Reopen from R:",
      "",
      "```r",
      "library(wsiTools)",
      "project <- wsi_read_project(\".\")",
      "browseURL(\"first-run-viewer.html\")",
      "head(project$tile_manifest)",
      "```",
      "",
      "No whole-slide image backend is required. The slide is a mock object used",
      "to demonstrate coordinates, annotations, segmentation overlays, and tile",
      "metadata without loading WSI pixel data."
    ),
    con = file,
    useBytes = TRUE
  )
  invisible(file)
}

#' @export
print.wsi_first_run_example <- function(x, ...) {
  cat("<wsi_first_run_example>\n")
  cat("  path:          ", x$path, "\n", sep = "")
  cat("  viewer:        ", x$viewer, "\n", sep = "")
  cat("  annotations:   ", nrow(x$rois), "\n", sep = "")
  cat("  mask ROIs:     ", nrow(x$mask_rois), "\n", sep = "")
  cat("  cells:         ", nrow(x$centroids), "\n", sep = "")
  cat("  tile rows:     ", nrow(x$tile_grid), "\n", sep = "")
  invisible(x)
}
