# Detect tissue and extract tiles

**Outcome:** estimate tissue from a low-resolution thumbnail, generate tile
coordinates without reading tile pixels, and export a reproducible manifest of
selected image patches.

## 1. Open the slide

```r
library(wsiTools)

slide <- wsi_open("/path/to/sample.svs")
```

## 2. Estimate a tissue mask

```r
tissue <- wsi_tissue_mask(
  slide,
  thumbnail_width = 2048,
  method = "otsu",
  min_area = 1000
)

tissue$tissue_percentage
tissue$tissue_bounding_box
```

The mask is estimated from a thumbnail. The complete level-0 slide is not loaded
into R memory.

For slides where Otsu thresholding is not suitable, try the simple threshold
method and adjust saturation or brightness explicitly:

```r
tissue <- wsi_tissue_mask(
  slide,
  method = "simple",
  saturation_threshold = 0.05,
  brightness_threshold = 0.80,
  min_area = 1000
)
```

## 3. Generate coordinates only

```r
grid <- wsi_tile_grid(
  slide,
  tile_size = 512,
  overlap = 0,
  level = 0,
  tissue_mask = tissue,
  include_partial = FALSE
)

nrow(grid)
head(grid)
```

`wsi_tile_grid()` creates metadata and coordinates; it does not read image pixels.
Review the grid before starting an export that may create many files.

## 4. Export selected tiles

```r
manifest <- wsi_export_tiles(
  slide,
  grid = grid,
  output_dir = "tiles",
  format = "png",
  overwrite = FALSE
)

write.csv(
  manifest,
  file = "tiles/manifest.csv",
  row.names = FALSE
)
```

Keep the manifest with downstream model outputs. It records the slide-coordinate
origin and dimensions needed to map patch-level results back to the WSI.

## 5. Close the slide

```r
wsi_close(slide)
```

## Checkpoint

The workflow is complete when `tiles/manifest.csv` has one row per exported tile
and the number of image files matches the successful manifest rows.

## Next

Use selected regions rather than the full slide in
[Segment a selected ROI](segment-selected-roi.md). For additional whitespace,
artifact, focus, and stain-quality filtering, use the generated R function
reference and the package examples.
