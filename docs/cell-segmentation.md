# Cell Segmentation

wsiTools can help launch or import cell segmentation results, but the model
stacks are optional runtime tools. The package does not bundle StarDist,
DeepCell, Mesmer, TensorFlow, or model weights automatically. This keeps the R
package lightweight while still allowing a live viewer to work with external
segmentation pipelines.

The recommended production workflow is:

1. Open the slide in a live tiled viewer.
2. Draw or select one ROI.
3. Segment only that selected ROI, not the whole WSI.
4. Use tiled model execution for large ROI crops.
5. Import cells back into the viewer and R session.
6. Export cells as GeoJSON, CSV, or a project artifact.

This avoids loading a full whole-slide image into R memory.

## Supported Engines

| Engine preset | Intended input | Typical use |
| --- | --- | --- |
| `stardist_he` | H&E brightfield ROI crop | nuclei/cell detection in H&E tissue |
| `stardist_ihc` | IHC brightfield ROI crop | nuclei/cell detection after IHC staining |
| `mesmer_dapi` | DAPI or mIHC nuclear channel ROI crop | cell/nuclear segmentation in multiplex images |

These presets describe how wsiTools prepares and labels the job. The actual
model command is external and must be installed/configured by the user or by a
separate analysis environment such as CellPhenotyper.

Check availability:

```r
library(wsiTools)

wsi_has_stardist()
wsi_has_mesmer()
wsi_backends()
```

If the executable is not on `PATH`, point wsiTools to it:

```r
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist-wrapper")
Sys.setenv(WSITOOLS_MESMER_COMMAND = "/path/to/mesmer-wrapper")
```

## Live Viewer Example

Open a tiled live viewer with selected-ROI segmentation enabled:

```r
library(wsiTools)

slide <- wsi_open("/path/to/slide.svs")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_engines = c("stardist_he", "stardist_ihc", "mesmer_dapi"),
  segmentation_default_engine = "stardist_he",
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32
)

viewer$open()
```

Short form:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32
)

viewer$open()
```

In the browser:

1. Draw or select an ROI.
2. Open the `Cells` menu.
3. Choose `StarDist H&E`, `StarDist IHC`, or `Mesmer DAPI`.
4. Run segmentation on the selected ROI.

In R, retrieve the result:

```r
cells <- viewer$get_segmentation()
cell_summary <- viewer$get_cell_summary()
last_run <- viewer$get_state()$last_segmentation
```

## Selected ROI Only

The segmentation endpoint is designed around a selected ROI. This is important
for memory use and speed: wsiTools exports a crop for the selected region, runs
the external command on that crop, then maps the cells back into slide
coordinates.

Run the same workflow directly from R:

```r
roi <- viewer$get_selected_roi()

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

For IHC:

```r
result <- wsi_cell_segment_roi(
  slide,
  roi,
  output_dir = "ihc_cells",
  engine = "stardist_ihc",
  tiles_x = 32,
  tiles_y = 32
)
```

For DAPI/mIHC via Mesmer-style external command:

```r
result <- wsi_cell_segment_roi(
  slide,
  roi,
  output_dir = "mesmer_cells",
  engine = "mesmer_dapi",
  tiles_x = 32,
  tiles_y = 32
)
```

## Low-RAM Tiled Execution

For large ROIs, use tiling arguments whenever the external engine supports
them. The exact meaning depends on the wrapper, but the intended pattern is:

- export one selected ROI crop;
- split that crop into model tiles;
- run the model tile by tile;
- merge the outputs;
- return GeoJSON, centroid CSV/TSV, or a mask.

Example StarDist wrapper arguments:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  stardist_args = c(
    "--input", "{input}",
    "--output", "{output}",
    "--model", "{model}",
    "--tiles", "{tiles_y}", "{tiles_x}",
    "--min-area", "{min_area}"
  ),
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32,
  segmentation_min_area = 120
)
```

The placeholders are filled by wsiTools before calling the external command.
The command itself should process the crop without loading the full slide.

## Loading Masks From File

If segmentation was done outside wsiTools, import it as an overlay.

GeoJSON polygons:

```r
cells <- import_segmentation("/path/to/cells.geojson")
viewer$add_segmentation(cells)
```

Centroid table:

```r
cells <- import_segmentation("/path/to/cell_centroids.csv")
viewer$add_segmentation(cells, radius = 8)
```

Mask image:

```r
mask <- import_segmentation(
  "/path/to/cell_mask.png",
  type = "mask"
)

viewer$add_segmentation(mask)
```

Convert connected mask components into editable cell ROI polygons:

```r
cell_rois <- import_segmentation(
  "/path/to/cell_mask.png",
  type = "mask",
  mask_as_rois = TRUE,
  threshold = 0.5,
  min_area = 20,
  prefix = "cell"
)

viewer$add_segmentation(cell_rois)
```

For TIFF masks, import from R with `import_segmentation()` rather than relying
on browser-side decoding.

## Returning Cells To R

The live viewer keeps cell overlays synchronized with the R session:

```r
cells <- viewer$get_segmentation()
cell_summary <- viewer$get_cell_summary()
roi_summary <- viewer$get_roi_summary()
measurements <- viewer$get_measurements()
```

For project-style workflows, save the state:

```r
viewer$save_project("case_01.wsiproject")
```

CellPhenotyper projects can be opened directly and their cell overlays are
available through the same `get_segmentation()` and `get_cell_summary()`
methods.

## Exporting Cells

Export cell polygons or centroid-derived cell ROIs to GeoJSON:

```r
cells <- viewer$get_segmentation()
write_geojson(cells, "cells.geojson", overwrite = TRUE)
```

Export a simple table:

```r
cells <- viewer$get_segmentation()
cell_summary <- viewer$get_cell_summary()

utils::write.csv(
  cell_summary,
  "cell_summary.csv",
  row.names = FALSE
)
```

For downstream feature extraction:

```r
counts <- wsi_cell_counts(
  slide = slide,
  segmentation = cells,
  rois = viewer$get_rois()
)
```

## Troubleshooting

Problem: the viewer says no live cell-segmentation endpoint is configured.

Meaning: the viewer was not opened with selected-ROI segmentation enabled, or
the live endpoint was not started.

Fix:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE, stardist = TRUE)
viewer$open()
```

Problem: StarDist or Mesmer is not configured.

Meaning: wsiTools can orchestrate the external command, but it cannot find one.

Check:

```r
wsi_has_stardist()
wsi_has_mesmer()
Sys.getenv("WSITOOLS_STARDIST_COMMAND")
Sys.getenv("WSITOOLS_MESMER_COMMAND")
```

Fix: install/configure the external model environment, or load an existing
GeoJSON, CSV/TSV, or mask file instead.

Problem: memory use is too high.

Fix: reduce the ROI size, increase model tiling, and avoid full-slide model
execution. Use `segmentation_tiles_x` and `segmentation_tiles_y`, and run
segmentation on a selected ROI crop only.

Problem: cell coordinates are shifted.

Fix: confirm whether the external model wrote crop-local or slide-level
coordinates. wsiTools maps selected-ROI runs back to slide coordinates; imported
external files may need an offset depending on how they were created.
