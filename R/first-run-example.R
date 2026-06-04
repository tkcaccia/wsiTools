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

#' Create a lightweight built-in demo project
#'
#' `wsi_demo_project()` creates a tiny, self-contained demo directory that can
#' be used immediately after installing wsiTools. It contains a mock slide,
#' synthetic ROI GeoJSON, a tiny PNG/TIFF image pair, fake cell centroids, fake
#' tile coordinates, fake spatial spots/expression, and an HTML viewer. The
#' demo is generated on demand and does not ship large image data with the
#' package.
#'
#' `wsi_demo_viewer()` is the shortest onboarding command: it creates the demo
#' project, opens the viewer when requested, and returns the demo object.
#'
#' @param path Directory where the demo project should be written. Defaults to
#'   a temporary directory.
#' @param open Whether to open the generated HTML viewer.
#' @param overwrite Whether to overwrite files previously created at `path`.
#'
#' @return A `wsi_demo_project` list with paths, mock slide, ROIs, segmentation,
#'   tile grid, spatial spots, and viewer path.
#' @export
#'
#' @examples
#' \dontrun{
#' demo <- wsi_demo_project(open = FALSE)
#' demo$viewer
#' }
wsi_demo_project <- function(path = tempfile("wsiTools-demo-"),
                             open = FALSE,
                             overwrite = FALSE) {
  example <- wsi_first_run_example(path = path, open = FALSE, overwrite = overwrite)
  path <- example$path

  tiny <- wsi_demo_write_tiny_images(path, overwrite = overwrite)
  spatial <- wsi_demo_spatial_data(example$slide)
  spatial_files <- wsi_demo_write_spatial_files(path, spatial, overwrite = overwrite)

  cells_csv <- file.path(path, "cells.csv")
  fake_rois <- file.path(path, "fake_rois.geojson")
  fake_tile_grid <- file.path(path, "fake_tile_grid.csv")
  utils::write.csv(as.data.frame(example$centroids), wsi_validate_output_path(cells_csv, overwrite = overwrite), row.names = FALSE)
  write_geojson(example$rois, fake_rois, overwrite = overwrite)
  utils::write.csv(example$tile_grid, wsi_validate_output_path(fake_tile_grid, overwrite = overwrite), row.names = FALSE)

  viewer_rois <- wsi_viewer_bind_rois(example$rois, example$segmentation)
  demo_viewer <- file.path(path, "demo-viewer.html")
  wsi_viewer(
    example$slide,
    output = demo_viewer,
    open = FALSE,
    overwrite = overwrite,
    title = "wsiTools demo project",
    mode = "thumbnail",
    width = 1400,
    roi = viewer_rois,
    layers = list(spatial$layer),
    seurat = spatial$linked
  )

  files <- example$files
  files$viewer <- demo_viewer
  files$first_run_viewer <- example$viewer
  files$tiny_png <- tiny$png
  files$tiny_tiff <- tiny$tiff
  files$cells_csv <- cells_csv
  files$fake_rois <- fake_rois
  files$fake_tile_grid <- fake_tile_grid
  files$spatial_spots <- spatial_files$spots
  files$spatial_expression <- spatial_files$expression
  files$spatial_reduction <- spatial_files$reduction
  files$demo_readme <- wsi_demo_write_readme(path, files, overwrite = overwrite)

  project <- wsi_project(
    path,
    slide = example$slide,
    viewer_state = list(
      view = list(center = list(x = 2048, y = 1536), zoom = 1),
      rois = example$rois,
      segmentation = example$segmentation,
      layers = list(spatial$layer),
      last_event = "demo_project"
    ),
    rois = example$rois,
    segmentation = example$segmentation,
    tile_manifest = example$tile_grid,
    metadata = list(
      title = "wsiTools lightweight demo project",
      description = "Synthetic mock slide with tiny images, ROIs, cells, tile grid, and fake spatial spots.",
      viewer_file = basename(demo_viewer),
      tiny_png = basename(files$tiny_png),
      tiny_tiff = basename(files$tiny_tiff),
      cells_csv = basename(files$cells_csv),
      spatial_spots = basename(files$spatial_spots)
    ),
    processing_provenance = list(
      example = "wsi_demo_project",
      pixel_data_loaded = FALSE,
      note = "Generated synthetic data for onboarding; no WSI backend or large image download required."
    ),
    overwrite = TRUE
  )

  out <- example
  out$viewer <- demo_viewer
  out$files <- files
  out$project <- project
  out$tiny_images <- tiny
  out$spatial <- spatial$linked
  out$spatial_spots <- spatial$spots
  out$spatial_expression <- spatial$expression
  out$spatial_reduction <- spatial$reduction
  out$demo_note <- "Synthetic lightweight demo; no SVS/CZI/OME-TIFF sample data required."
  class(out) <- c("wsi_demo_project", setdiff(class(out), "wsi_demo_project"))

  if (isTRUE(open)) {
    utils::browseURL(demo_viewer)
  }
  out
}

#' @rdname wsi_demo_project
#' @export
wsi_demo_viewer <- function(path = tempfile("wsiTools-demo-"),
                            open = interactive(),
                            overwrite = FALSE) {
  wsi_demo_project(path = path, open = open, overwrite = overwrite)
}

wsi_demo_rgb_array <- function(width = 240L, height = 160L) {
  width <- as.integer(width)
  height <- as.integer(height)
  x <- rep(seq(0, 1, length.out = width), each = height)
  y <- rep(seq(0, 1, length.out = height), times = width)
  r <- 248 - 34 * x + 16 * y
  g <- 212 - 42 * y + 18 * x
  b <- 218 - 28 * x - 18 * y
  tumour <- ((x - 0.35) / 0.24)^2 + ((y - 0.47) / 0.31)^2 < 1
  stroma <- ((x - 0.68) / 0.22)^2 + ((y - 0.56) / 0.25)^2 < 1
  vessel <- ((x - 0.55) / 0.06)^2 + ((y - 0.25) / 0.11)^2 < 1
  r[tumour] <- 190
  g[tumour] <- 112
  b[tumour] <- 178
  r[stroma] <- 232
  g[stroma] <- 153
  b[stroma] <- 170
  r[vessel] <- 132
  g[vessel] <- 45
  b[vessel] <- 77
  rgb <- array(0L, dim = c(height, width, 3L))
  rgb[, , 1L] <- matrix(pmax(0, pmin(255, round(r))), nrow = height, ncol = width)
  rgb[, , 2L] <- matrix(pmax(0, pmin(255, round(g))), nrow = height, ncol = width)
  rgb[, , 3L] <- matrix(pmax(0, pmin(255, round(b))), nrow = height, ncol = width)
  rgb
}

wsi_demo_write_tiny_images <- function(path, overwrite = FALSE) {
  png_file <- wsi_validate_output_path(file.path(path, "tiny_tissue.png"), overwrite = overwrite)
  tiff_file <- wsi_validate_output_path(file.path(path, "tiny_tissue.tif"), overwrite = overwrite)
  rgb <- wsi_demo_rgb_array()
  if (!isTRUE(capabilities("png"))) {
    wsi_abort("Creating the demo PNG requires PNG graphics-device support in this R build.")
  }
  grDevices::png(png_file, width = dim(rgb)[2L], height = dim(rgb)[1L], bg = "white")
  old <- graphics::par(no.readonly = TRUE)
  on.exit({
    graphics::par(old)
    grDevices::dev.off()
  }, add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
  graphics::rasterImage(grDevices::as.raster(rgb / 255), 0, 0, 1, 1, interpolate = FALSE)
  grDevices::dev.off()
  on.exit(NULL, add = FALSE)

  wsi_demo_write_tiff(tiff_file, rgb)
  list(png = png_file, tiff = tiff_file)
}

wsi_demo_write_tiff <- function(file, rgb) {
  height <- dim(rgb)[1L]
  width <- dim(rgb)[2L]
  n_entries <- 10L
  ifd_offset <- 8L
  ifd_size <- 2L + n_entries * 12L + 4L
  bits_offset <- ifd_offset + ifd_size
  strip_offset <- bits_offset + 6L
  pixel_bytes <- as.raw(as.vector(aperm(rgb, c(3L, 2L, 1L))))
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  writeChar("II", con, eos = NULL, useBytes = TRUE)
  writeBin(as.integer(42L), con, size = 2L, endian = "little")
  writeBin(as.integer(ifd_offset), con, size = 4L, endian = "little")
  writeBin(as.integer(n_entries), con, size = 2L, endian = "little")
  write_tiff_entry <- function(tag, type, count, value, short = FALSE) {
    writeBin(as.integer(tag), con, size = 2L, endian = "little")
    writeBin(as.integer(type), con, size = 2L, endian = "little")
    writeBin(as.integer(count), con, size = 4L, endian = "little")
    if (isTRUE(short) && count == 1L) {
      writeBin(as.integer(value), con, size = 2L, endian = "little")
      writeBin(as.integer(0L), con, size = 2L, endian = "little")
    } else {
      writeBin(as.integer(value), con, size = 4L, endian = "little")
    }
  }
  write_tiff_entry(256L, 4L, 1L, width)
  write_tiff_entry(257L, 4L, 1L, height)
  write_tiff_entry(258L, 3L, 3L, bits_offset)
  write_tiff_entry(259L, 3L, 1L, 1L, short = TRUE)
  write_tiff_entry(262L, 3L, 1L, 2L, short = TRUE)
  write_tiff_entry(273L, 4L, 1L, strip_offset)
  write_tiff_entry(277L, 3L, 1L, 3L, short = TRUE)
  write_tiff_entry(278L, 4L, 1L, height)
  write_tiff_entry(279L, 4L, 1L, length(pixel_bytes))
  write_tiff_entry(284L, 3L, 1L, 1L, short = TRUE)
  writeBin(as.integer(0L), con, size = 4L, endian = "little")
  writeBin(as.integer(c(8L, 8L, 8L)), con, size = 2L, endian = "little")
  writeBin(pixel_bytes, con)
  invisible(file)
}

wsi_demo_spatial_data <- function(slide) {
  spot_ids <- sprintf("demo_spot_%02d", seq_len(16L))
  coords <- data.frame(
    barcode = spot_ids,
    imagecol = rep(c(760, 1120, 1480, 1840), 4L),
    imagerow = rep(c(720, 1080, 1440, 1800), each = 4L),
    stringsAsFactors = FALSE
  )
  embeddings <- cbind(
    PC_1 = seq(-2, 2, length.out = length(spot_ids)),
    PC_2 = sin(seq(0, 2 * pi, length.out = length(spot_ids)))
  )
  rownames(embeddings) <- spot_ids
  expression <- rbind(
    DemoGeneA = wsi_demo_scale01(coords$imagecol),
    DemoGeneB = wsi_demo_scale01(coords$imagerow),
    DemoGeneC = wsi_demo_scale01(coords$imagecol + coords$imagerow)
  )
  colnames(expression) <- spot_ids
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    assays = list(Spatial = list(data = expression)),
    images = list(
      demo = list(
        coordinates = coords,
        scale.factors = list(spot = 55)
      )
    )
  )
  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    image_name = "demo",
    coordinate_scale = "none",
    spot_genes = rownames(expression),
    default_gene = "DemoGeneA",
    spot_radius = 55,
    colour_by = "gene"
  )
  list(
    linked = linked,
    layer = wsi_seurat_spots_layer(linked),
    spots = linked$spots,
    expression = expression,
    reduction = linked$pca$points
  )
}

wsi_demo_scale01 <- function(x) {
  rng <- range(x, finite = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) {
    return(rep(0, length(x)))
  }
  (x - rng[[1L]]) / diff(rng)
}

wsi_demo_write_spatial_files <- function(path, spatial, overwrite = FALSE) {
  files <- list(
    spots = file.path(path, "spatial_spots.csv"),
    expression = file.path(path, "spatial_expression.csv"),
    reduction = file.path(path, "spatial_pca.csv")
  )
  utils::write.csv(spatial$spots, wsi_validate_output_path(files$spots, overwrite = overwrite), row.names = FALSE)
  expression <- as.data.frame(t(spatial$expression), stringsAsFactors = FALSE)
  expression$spot_id <- rownames(expression)
  expression <- expression[, c("spot_id", setdiff(names(expression), "spot_id")), drop = FALSE]
  utils::write.csv(expression, wsi_validate_output_path(files$expression, overwrite = overwrite), row.names = FALSE)
  utils::write.csv(spatial$reduction, wsi_validate_output_path(files$reduction, overwrite = overwrite), row.names = FALSE)
  files
}

wsi_demo_write_readme <- function(path, files, overwrite = FALSE) {
  file <- wsi_validate_output_path(file.path(path, "DEMO_README.md"), overwrite = overwrite)
  writeLines(
    c(
      "# wsiTools Demo Project",
      "",
      "This directory was created by `wsi_demo_project()`.",
      "",
      "It is intentionally tiny and synthetic. It lets new users test the viewer",
      "without downloading SVS, CZI, OME-TIFF, StarDist models, or CellPhenotyper outputs.",
      "",
      "Important files:",
      "",
      "- `demo-viewer.html`: demo viewer with ROIs, cells, fake spatial spots, and reduction plot controls.",
      "- `tiny_tissue.png` and `tiny_tissue.tif`: tiny ordinary image assets.",
      "- `fake_rois.geojson`: synthetic annotation polygons.",
      "- `cells.csv`: synthetic cell centroid table.",
      "- `fake_tile_grid.csv`: coordinate-only tile grid.",
      "- `spatial_spots.csv`, `spatial_expression.csv`, `spatial_pca.csv`: fake spatial transcriptomics data.",
      "",
      "Recreate from R:",
      "",
      "```r",
      "library(wsiTools)",
      "demo <- wsi_demo_viewer(open = TRUE)",
      "demo$path",
      "demo$files",
      "```"
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

#' @export
print.wsi_demo_project <- function(x, ...) {
  cat("<wsi_demo_project>\n")
  cat("  path:          ", x$path, "\n", sep = "")
  cat("  viewer:        ", x$viewer, "\n", sep = "")
  cat("  tiny PNG:      ", x$files$tiny_png, "\n", sep = "")
  cat("  tiny TIFF:     ", x$files$tiny_tiff, "\n", sep = "")
  cat("  annotations:   ", nrow(x$rois), "\n", sep = "")
  cat("  cells:         ", nrow(x$centroids), "\n", sep = "")
  cat("  spatial spots: ", nrow(x$spatial_spots), "\n", sep = "")
  cat("  tile rows:     ", nrow(x$tile_grid), "\n", sep = "")
  invisible(x)
}
