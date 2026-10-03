# Add spatial omics

**Outcome:** overlay spatial coordinates and analysis fields on a tissue image
while retaining the source object and on-demand feature access in R.

## 1. Prepare an object and image

The image must correspond to the coordinate system in the object. Confirm image
orientation, scale, and any registration transform before interpreting overlays.

For a Seurat workflow:

```r
library(wsiTools)
library(Seurat)

seurat_obj <- readRDS("/path/to/seurat_object.rds")
```

## 2. Open a tiled live viewer

```r
viewer <- wsi_viewer_seurat(
  seurat = seurat_obj,
  image = "/path/to/high_resolution_tissue_image.tif",
  dynamic_tiles = TRUE,
  mode = "tiles",
  open = TRUE,
  wait = FALSE
)
```

In live mode, selected genes or features can be requested from R when needed
rather than embedding an entire expression matrix in browser HTML.

## 3. Link annotations to spots

Draw one or more annotations in the browser, then retrieve both geometry and
annotation-to-spot associations:

```r
rois <- viewer$get_rois()
annotation_spots <- viewer$get_annotation_spots()

head(annotation_spots)
```

## 4. Use another supported container

Giotto and SpatialExperiment follow the same pattern:

=== "Giotto"

    ```r
    viewer <- wsi_viewer_giotto(
      giotto = giotto_obj,
      image = "/path/to/high_resolution_tissue_image.tif",
      dynamic_tiles = TRUE,
      mode = "tiles",
      open = TRUE,
      wait = FALSE
    )
    ```

=== "SpatialExperiment"

    ```r
    viewer <- wsi_viewer_spatialexperiment(
      spe = spe,
      image = "/path/to/high_resolution_tissue_image.tif",
      dynamic_tiles = TRUE,
      mode = "tiles",
      open = TRUE,
      wait = FALSE
    )
    ```

See the [spatial omics guide](../spatial-omics.md) for multi-section projects,
CellPhenotyper overlays, proximity analysis, and additional object-specific
requirements.

## Checkpoint

The workflow is complete when spots or cells align with tissue landmarks and an
annotation produces a non-empty, plausible association table.

## Next

Write the association table, annotations, measurements, and project state in
[Export and save results](export-results.md).
