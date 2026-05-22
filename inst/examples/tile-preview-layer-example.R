# Preview a tile grid in the live viewer, then export the confirmed tiles.
#
# Replace the slide path with your local SVS/BTF/NDPI/etc. file. The preview
# step creates coordinates only; no tile pixels are read or written until
# `extract_tile_preview()` is called.

library(wsiTools)

slide <- wsi_open("sample.svs")

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  wait = FALSE
)

# Optional: draw/select ROIs in the viewer first. If ROIs are selected, the
# preview uses them; otherwise it previews a whole-slide grid.
preview <- viewer$preview_tiles(
  tile_size = 512,
  stride = 512,
  layer_name = "Tile preview"
)

# Inspect the locked tile-preview layer in the viewer. If it looks right,
# export exactly this grid.
tiles <- viewer$extract_tile_preview(
  output_dir = "confirmed_tiles",
  format = "png",
  manifest_file = "confirmed_tiles_manifest.csv"
)

viewer$get_tile_preview()
head(tiles)

wsi_viewer_stop(viewer)
wsi_close(slide)
