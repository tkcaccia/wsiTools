#!/usr/bin/env Rscript

# Example: interactive H-DAB/HRP immunohistochemistry deconvolution viewer.
#
# The full-resolution viewer uses libvips Deep Zoom tiles. Color deconvolution
# is applied in the browser to the visible canvas pixels, so the full slide is
# not loaded into R memory.

library(wsiTools)

image_path <- Sys.getenv(
  "WSITOOLS_INPUT",
  "/media/user/Lion/Lion/wsitools/Visium_HD_6p5mm_Human_Heart_tissue_image.btf"
)

output_dir <- Sys.getenv(
  "WSITOOLS_IHC_VIEWER_DIR",
  "/media/user/Lion/Lion/wsitools/wsiTools_fullres_viewer"
)

geojson_path <- Sys.getenv("WSITOOLS_GEOJSON", "")

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

viewer_html <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_ihc_deconvolution_viewer.html")
tile_dir <- file.path(output_dir, "Visium_HD_6p5mm_Human_Heart_deepzoom_tiles")

cat("Input image:\n", image_path, "\n\n", sep = "")
cat("IHC viewer HTML:\n", viewer_html, "\n\n", sep = "")
cat("Deep Zoom tile directory:\n", tile_dir, "\n\n", sep = "")

slide <- wsi_open(image_path, backend = "vips")
on.exit(wsi_close(slide), add = TRUE)

cat("Slide:\n")
print(slide)
cat("\n")

roi <- NULL
if (nzchar(geojson_path) && file.exists(geojson_path)) {
  cat("Overlay GeoJSON:\n", geojson_path, "\n\n", sep = "")
  roi <- wsi_read_geojson(geojson_path)
}

viewer <- wsi_viewer_ihc(
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
  roi = roi,
  roi_fill_alpha = 0.18,
  title = "Visium HD Human Heart IHC H-DAB deconvolution viewer"
)

cat("Interactive IHC viewer written to:\n", viewer, "\n\n", sep = "")
cat("Use the IHC controls to toggle deconvolution, set hematoxylin/HRP colours, and adjust channel gains.\n")
