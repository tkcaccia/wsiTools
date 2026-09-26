# Segment a selected ROI

**Outcome:** run or import cell segmentation for one selected region, return cells
to the live viewer, and avoid applying a model to the complete WSI by default.

!!! warning "Models are optional external runtimes"
    wsiTools does not bundle StarDist, Mesmer, TensorFlow, DeepCell, or model
    weights. Configure an external command or import an existing segmentation.

## 1. Check available engines

```r
library(wsiTools)

wsi_has_stardist()
wsi_has_mesmer()
wsi_backends()
```

When an executable is not on `PATH`, point wsiTools to a wrapper command:

```r
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist-wrapper")
Sys.setenv(WSITOOLS_MESMER_COMMAND = "/path/to/mesmer-wrapper")
```

## 2. Open a segmentation-enabled viewer

```r
slide <- wsi_open("/path/to/sample.svs")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_engines = c("stardist_he", "stardist_ihc", "mesmer_dapi"),
  segmentation_default_engine = "stardist_he",
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32,
  open = TRUE,
  wait = FALSE
)

viewer$open()
```

## 3. Select one ROI

Draw or select an annotation in the browser. Confirm that R can retrieve it:

```r
roi <- viewer$get_selected_roi()
roi
```

Do not continue until a single intended region is selected.

## 4. Run the model from R

```r
result <- wsi_cell_segment_roi(
  slide,
  roi,
  output_dir = "roi_cells",
  engine = "stardist_he",
  tiles_x = 32,
  tiles_y = 32
)

viewer$add_segmentation(result$segmentation)
cells <- viewer$get_segmentation()
```

The intended execution pattern is: export the ROI crop, split it into model
tiles, run the external command, merge outputs, and map cells back into slide
coordinates.

## 5. Or import an existing segmentation

```r
cells <- import_segmentation("/path/to/cells.geojson")
viewer$add_segmentation(cells)
```

Centroid tables and masks are also supported by the detailed
[cell segmentation guide](../cell-segmentation.md).

## 6. Finish

```r
viewer$stop()
wsi_close(slide)
```

## Checkpoint

The workflow is complete when the cells appear over the selected region and
`viewer$get_segmentation()` returns the current segmentation state.

## Next

Persist the segmentation with the other case outputs in
[Export and save results](export-results.md).
