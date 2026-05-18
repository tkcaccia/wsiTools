#!/usr/bin/env Rscript

# Example: full-resolution interactive visualization with Deep Zoom tiles.
#
# This is different from the lightweight thumbnail viewer. The first run asks
# libvips to create a tiled Deep Zoom pyramid on disk, so browser zoom uses
# higher-resolution tiles instead of magnifying a thumbnail. This can take time
# and use substantial disk space for a large WSI, but it still avoids loading
# the full slide into R memory.

library(wsiTools)

image_path <- Sys.getenv(
  "WSITOOLS_INPUT",
  "/media/user/Lion/Lion/wsitools/Visium_HD_6p5mm_Human_Heart_tissue_image.btf"
)

output_dir <- Sys.getenv(
  "WSITOOLS_OUTPUT_DIR",
  "/media/user/Lion/Lion/wsitools/wsiTools_fullres_viewer"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

viewer_html <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_full_resolution_viewer.html")
tile_dir <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_deepzoom_tiles")

cat("Input image:\n", image_path, "\n\n", sep = "")
cat("Viewer HTML:\n", viewer_html, "\n\n", sep = "")
cat("Deep Zoom tile directory:\n", tile_dir, "\n\n", sep = "")

slide <- wsi_open(image_path, backend = "vips")
on.exit(wsi_close(slide), add = TRUE)

cat("Slide:\n")
print(slide)
cat("\n")

cat("Creating or reusing full-resolution Deep Zoom tiles...\n")
viewer <- wsi_viewer(
  slide,
  mode = "tiles",
  output = viewer_html,
  tile_dir = tile_dir,
  tile_size = 1024,
  tile_format = "jpg",
  quality = 85,
  open = interactive(),
  overwrite = TRUE,
  rebuild = FALSE,
  title = "Visium HD Human Heart full-resolution viewer"
)

cat("Interactive full-resolution viewer written to:\n", viewer, "\n\n", sep = "")
cat("Open this HTML file in a browser. Zooming will request Deep Zoom tiles from:\n",
    file.path(tile_dir, "slide_files"), "\n\n", sep = "")

if (nzchar(Sys.which("du"))) {
  cat("Output size:\n")
  print(system2("du", c("-sh", output_dir), stdout = TRUE, stderr = TRUE))
}
