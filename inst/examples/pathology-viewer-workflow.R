# Practical pathology viewer workflow
# Run from an installed wsiTools package with OpenSlide/libvips available.

library(wsiTools)

slide <- wsi_open("sample.svs")
processed <- wsi_open("sample_processed.ome.tiff")

# Compare original and processed images with synchronized zoom and pan.
viewer_compare(
  slide,
  processed,
  sync = TRUE,
  output = "compare_original_processed.html"
)

# Open OME-Zarr metadata without decoding full image chunks.
zarr <- open_omezarr("sample.ome.zarr")
omezarr_metadata("sample.ome.zarr")

# Import QuPath annotations, relabel one class, and write GeoJSON back out.
rois <- read_geojson("annotations.geojson")
rois <- wsi_set_roi_class(rois, "tumour", roi_id = rois$roi_id[1])
write_geojson(rois, "annotations_relabelled.geojson", overwrite = TRUE)

# Overlay ROIs in the viewer.
viewer_add_rois(
  slide,
  rois,
  mode = "tiles",
  output = "slide_with_rois.html",
  tile_dir = "slide_with_rois_tiles"
)

# Export the first ROI bounding box for optional external analysis with tools
# such as StarDist or Cellpose, then import a model output.
export_roi_crop(slide, rois, "roi_crop.png", roi_id = rois$roi_id[1])
segmentation <- import_segmentation("model_output.geojson")
viewer_add_segmentation(slide, segmentation, output = "segmentation_overlay.html")

# Optional StarDist integration: pass the command/script that is valid for your
# Python environment. The placeholders are replaced with the exported ROI crop,
# expected output path, and model name.
stardist_result <- stardist_segment_roi(
  slide,
  rois,
  output_dir = "stardist_roi",
  roi_id = rois$roi_id[1],
  command = "python",
  args = c("run_stardist.py", "{input}", "{output}", "{model}"),
  run = FALSE
)
print(stardist_result)

# Command-line equivalent from a source checkout:
# ./exec/wsitools stardist-roi \
#   --image sample.svs \
#   --roi annotations.geojson \
#   --output-dir stardist_roi \
#   --command python \
#   --arg run_stardist.py \
#   --arg '{input}' \
#   --arg '{output}' \
#   --arg '{model}'

# Generate tile coordinates only, or save tile images when output_dir is given.
tile_grid <- extract_tiles(slide, roi = rois, tile_size = 512, stride = 512, save_images = FALSE)
tiles <- extract_tiles(slide, roi = rois, tile_size = 512, stride = 512, output_dir = "roi_tiles")

# Measure cells and summarise tissue classes.
cells <- data.frame(x = c(1000, 1200, 3000), y = c(800, 900, 1800))
measure_cell_density(cells, rois, pixel_size = wsi_mpp(slide))
summary <- summarise_rois(rois, cells = cells, pixel_size = wsi_mpp(slide), file = "roi_summary.csv")

# Register serial sections from manual landmarks and transfer ROIs.
landmarks1 <- data.frame(x = c(100, 500, 100), y = c(100, 100, 500))
landmarks2 <- data.frame(x = c(130, 540, 125), y = c(90, 120, 490))
transform <- estimate_transform(landmarks1, landmarks2)
transformed_rois <- transform_rois(rois, transform)

wsi_close(slide)
wsi_close(processed)
wsi_close(zarr)
