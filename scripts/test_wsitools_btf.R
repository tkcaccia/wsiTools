#!/usr/bin/env Rscript

# Smoke test for the Visium HD Human Heart BTF with wsiTools.
# This script uses region-based reads and libvips-backed conversion; it does
# not load the full whole-slide image into R memory.

library(wsiTools)

input <- Sys.getenv(
  "WSITOOLS_INPUT",
  "/media/user/Lion/Lion/wsitools/Visium_HD_6p5mm_Human_Heart_tissue_image.btf"
)
out_dir <- Sys.getenv(
  "WSITOOLS_OUTPUT_DIR",
  "/media/user/Lion/Lion/wsitools/wsiTools_test_output"
)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

region_png <- file.path(out_dir, "region_x0_y0_512.png")
converted <- file.path(out_dir, "Visium_HD_6p5mm_Human_Heart_tissue_image.lzw-pyramid.ome.tiff")
viewer_html <- file.path(out_dir, "Visium_HD_6p5mm_Human_Heart.viewer.html")
force_convert <- identical(tolower(Sys.getenv("WSITOOLS_FORCE_CONVERT", "false")), "true")

cat("Input:", input, "\n")
cat("Input size bytes:", file.info(input)$size, "\n\n")

cat("Backends:\n")
print(wsi_backends())
cat("\n")

slide <- wsi_open(input, backend = "vips")
on.exit(wsi_close(slide), add = TRUE)

cat("Slide object:\n")
print(slide)
cat("\n")

cat("Slide info:\n")
print(wsi_info(slide))
cat("\n")

cat("Pyramid levels:\n")
print(wsi_levels(slide))
cat("\n")

thumb <- wsi_thumbnail(slide, width = 512, format = "array")
cat("Thumbnail dimensions:", paste(dim(thumb), collapse = "x"), "\n")

grid <- wsi_tile_grid(slide, tile_size = 512, level = 0, include_partial = FALSE)
cat("Tile grid rows:", nrow(grid), "\n")
print(utils::head(grid, 3))
cat("\n")

if (file.exists(region_png)) {
  unlink(region_png)
}
wsi_export_region(
  slide,
  x = 0,
  y = 0,
  width = 512,
  height = 512,
  level = 0,
  output = region_png,
  format = "png"
)
cat("Region PNG:", region_png, "\n")
cat("Region PNG size bytes:", file.info(region_png)$size, "\n\n")

if (force_convert && file.exists(converted)) {
  unlink(converted)
}

if (!file.exists(converted)) {
  wsi_convert(
    input = input,
    output = converted,
    format = "ome-tiff",
    backend = "vips",
    tile_size = 512,
    compression = "lzw",
    pyramid = TRUE,
    bigtiff = TRUE
  )
} else {
  cat("Converted OME-TIFF already exists; set WSITOOLS_FORCE_CONVERT=true to rebuild it.\n")
}
cat("Converted OME-TIFF:", converted, "\n")
cat("Converted OME-TIFF size bytes:", file.info(converted)$size, "\n")
cat("Converted OME-TIFF header:\n")
print(system2("vipsheader", converted, stdout = TRUE, stderr = TRUE))
cat("\n")

if (file.exists(viewer_html)) {
  unlink(viewer_html)
}
wsi_viewer(
  slide,
  width = 1600,
  output = viewer_html,
  open = FALSE
)
cat("Interactive viewer HTML:", viewer_html, "\n")
cat("Interactive viewer size bytes:", file.info(viewer_html)$size, "\n")
