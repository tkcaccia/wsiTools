#!/usr/bin/env Rscript

# Example: interactive visualization of a large BTF/WSI with wsiTools.
#
# This script writes a small thumbnail viewer and a full-resolution tiled viewer.
# The tiled viewer uses libvips Deep Zoom tiles, so zooming requests
# higher-resolution tiles instead of magnifying one thumbnail.

library(wsiTools)

image_path <- Sys.getenv(
  "WSITOOLS_INPUT",
  "/media/user/Lion/Lion/wsitools/Visium_HD_6p5mm_Human_Heart_tissue_image.btf"
)

output_dir <- Sys.getenv(
  "WSITOOLS_OUTPUT_DIR",
  "/media/user/Lion/Lion/wsitools/wsiTools_examples"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

thumbnail_viewer_html <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_thumbnail_viewer.html")
region_preview_png <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_region_preview.png")
full_res_dir <- Sys.getenv(
  "WSITOOLS_FULLRES_OUTPUT_DIR",
  "/media/user/Lion/Lion/wsitools/wsiTools_fullres_viewer"
)
dir.create(full_res_dir, recursive = TRUE, showWarnings = FALSE)
full_res_viewer_html <- file.path(full_res_dir, "Visium_HD_6p5mm_Human_Heart_full_resolution_viewer.html")
full_res_tile_dir <- file.path(full_res_dir, "Visium_HD_6p5mm_Human_Heart_deepzoom_tiles")

cat("Backend availability:\n")
print(wsi_backends())
cat("\n")

slide <- wsi_open(image_path, backend = "vips")
on.exit(wsi_close(slide), add = TRUE)

cat("Slide summary:\n")
print(slide)
cat("\n")

cat("Slide metadata:\n")
print(wsi_info(slide))
cat("\n")

# Optional static region preview file.
wsi_crop(
  slide,
  x = 0,
  y = 0,
  width = 2048,
  height = 2048,
  level = 0,
  output = region_preview_png,
  format = "png",
  overwrite = TRUE
)

# Lightweight thumbnail HTML viewer.
wsi_viewer(
  slide,
  width = 1600,
  output = thumbnail_viewer_html,
  open = FALSE,
  overwrite = TRUE,
  title = "Visium HD Human Heart thumbnail viewer"
)

# Full-resolution tiled HTML viewer.
# The first run creates Deep Zoom tiles; later runs reuse them unless
# `rebuild = TRUE`.
wsi_viewer(
  slide,
  mode = "tiles",
  output = full_res_viewer_html,
  tile_dir = full_res_tile_dir,
  tile_size = 1024,
  tile_format = "jpg",
  quality = 85,
  open = interactive(),
  overwrite = TRUE,
  rebuild = FALSE,
  title = "Visium HD Human Heart full-resolution viewer"
)

cat("Static region preview written to:\n", region_preview_png, "\n\n", sep = "")
cat("Thumbnail viewer written to:\n", thumbnail_viewer_html, "\n\n", sep = "")
cat("Full-resolution tiled viewer written to:\n", full_res_viewer_html, "\n\n", sep = "")
cat("Open the full-resolution HTML file in a browser to pan, zoom, and inspect level-0 coordinates.\n")
