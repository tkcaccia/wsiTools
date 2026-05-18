# Example: visualize an H&E image saved as BigTIFF/BTF and convert it.
#
# This example is written for a local .btf file, such as a Visium/10x tissue
# image or another H&E brightfield image stored as BigTIFF. It opens only
# metadata first, creates a tiled interactive viewer, and converts the input to
# a pyramidal OME-TIFF with compression.
#
# Run from R with:
#   Sys.setenv(WSITOOLS_HE_BTF = "/path/to/he_image.btf")
#   source(system.file("examples/he-btf-viewer-convert.R", package = "wsiTools"))
#
# Or from the package source tree:
#   WSITOOLS_HE_BTF="/path/to/he_image.btf" Rscript inst/examples/he-btf-viewer-convert.R
#
# Optional public example download, if you do not already have a .btf file:
#   curl -L -o Visium_HD_6p5mm_Human_Heart_tissue_image.btf \
#     https://cf.10xgenomics.com/samples/spatial-exp/4.1.0/Visium_HD_6p5mm_Human_Heart/Visium_HD_6p5mm_Human_Heart_tissue_image.btf
#   Sys.setenv(WSITOOLS_HE_BTF = "Visium_HD_6p5mm_Human_Heart_tissue_image.btf")

library(wsiTools)

btf_candidates <- c(
  Sys.getenv("WSITOOLS_HE_BTF", unset = ""),
  "Visium_HD_6p5mm_Human_Heart_tissue_image.btf",
  "he_image.btf",
  "sample_he.btf"
)
btf_candidates <- btf_candidates[nzchar(btf_candidates)]
existing_btf <- btf_candidates[file.exists(btf_candidates)]
btf_path <- existing_btf[1L]

if (!length(existing_btf) || is.na(btf_path) || !file.exists(btf_path)) {
  stop(
    paste0(
      "Could not find an H&E .btf image. Set WSITOOLS_HE_BTF to the full path, ",
      "for example Sys.setenv(WSITOOLS_HE_BTF = \"/path/to/he_image.btf\")."
    ),
    call. = FALSE
  )
}

if (!wsi_has_vips()) {
  stop(
    "This example requires libvips command-line tools (`vips` and `vipsheader`) ",
    "for tiled viewing and conversion.",
    call. = FALSE
  )
}

output_dir <- Sys.getenv("WSITOOLS_HE_OUTPUT_DIR", unset = "he_btf_wsitools_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

overwrite <- isTRUE(as.logical(Sys.getenv("WSITOOLS_OVERWRITE", unset = "FALSE")))

slide <- wsi_open(btf_path, backend = "auto")
on.exit(wsi_close(slide), add = TRUE)

info <- wsi_info(slide)
print(info)

# Create an interactive tiled viewer. This writes a Deep Zoom tile pyramid next
# to the HTML file, so zooming requests tiles instead of loading the full image
# into R memory.
viewer_html <- wsi_viewer(
  slide,
  mode = "tiles",
  output = file.path(output_dir, "he_btf_interactive_viewer.html"),
  tile_dir = file.path(output_dir, "he_btf_deepzoom_tiles"),
  tile_size = 512,
  tile_format = "jpg",
  quality = 90,
  open = interactive(),
  overwrite = TRUE,
  rebuild = overwrite
)

# Convert the BTF image to a tiled, pyramidal OME-TIFF. The output is not
# overwritten unless WSITOOLS_OVERWRITE=TRUE is set.
ome_tiff <- file.path(output_dir, "he_btf_pyramidal.ome.tiff")
wsi_convert(
  input = btf_path,
  output = ome_tiff,
  format = "ome-tiff",
  tile_size = 512,
  compression = "lzw",
  pyramid = TRUE,
  bigtiff = TRUE,
  overwrite = overwrite
)

message("Input BTF:   ", normalizePath(btf_path, mustWork = FALSE))
message("Viewer HTML: ", normalizePath(viewer_html, mustWork = FALSE))
message("OME-TIFF:    ", normalizePath(ome_tiff, mustWork = FALSE))
