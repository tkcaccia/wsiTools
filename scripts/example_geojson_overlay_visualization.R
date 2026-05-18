#!/usr/bin/env Rscript

# Example: import QuPath-style GeoJSON annotations and visualize them over a WSI.
#
# The viewer uses full-resolution Deep Zoom tiles. If the tile pyramid already
# exists, it is reused; otherwise libvips creates it without loading the whole
# slide into R memory.

library(wsiTools)

image_path <- Sys.getenv(
  "WSITOOLS_INPUT",
  "/media/user/Lion/Lion/wsitools/Visium_HD_6p5mm_Human_Heart_tissue_image.btf"
)

geojson_path <- Sys.getenv(
  "WSITOOLS_GEOJSON",
  "/media/user/Lion/Lion/wsitools/example_annotations.geojson"
)

output_dir <- Sys.getenv(
  "WSITOOLS_ROI_VIEWER_DIR",
  "/media/user/Lion/Lion/wsitools/wsiTools_fullres_viewer"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

viewer_html <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_roi_viewer.html")
tile_dir <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_deepzoom_tiles")

cat("Reading GeoJSON annotations:\n", geojson_path, "\n\n", sep = "")
roi <- wsi_read_geojson(geojson_path)
print(roi[, c("roi_id", "name", "class", "geometry_type", "xmin", "ymin", "xmax", "ymax")])
cat("\n")

slide <- wsi_open(image_path, backend = "vips")
on.exit(wsi_close(slide), add = TRUE)

cat("Slide:\n")
print(slide)
cat("\n")

viewer <- wsi_viewer_roi(
  slide,
  roi,
  mode = "tiles",
  output = viewer_html,
  tile_dir = tile_dir,
  tile_size = 1024,
  tile_format = "jpg",
  quality = 85,
  open = interactive(),
  overwrite = TRUE,
  rebuild = FALSE,
  roi_fill_alpha = 0.18,
  title = "Visium HD Human Heart with GeoJSON ROI overlays"
)

cat("Interactive ROI viewer written to:\n", viewer, "\n\n", sep = "")
cat("Open the HTML file in a browser. Use ROI to show/hide overlays and GeoJSON to list all geometries.\n")
