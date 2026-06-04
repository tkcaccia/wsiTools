# Screenshots and GIFs

This gallery shows lightweight screenshot-style previews of the wsiTools viewer.
The images are generated from synthetic demo data, so no patient data or large
whole-slide images are stored in the repository.

To regenerate the gallery locally:

```r
source("docs/media/generate-gallery-assets.R")
```

## Full-Resolution Viewer

OpenSeadragon tiled viewing keeps the browser at full resolution by requesting
tiles instead of loading the whole slide into R memory.

![Full-resolution tiled viewer](media/full-resolution-viewer.png)

## Annotation Panel

The left-side annotation manager shows project images, ROI classes, selected
annotations, visibility, editing state, and brush settings.

![Annotation panel](media/annotation-panel.png)

## GeoJSON Overlay

QuPath-compatible GeoJSON annotations can be imported, visualized, edited, and
exported again for downstream pathology workflows.

![GeoJSON overlay](media/geojson-overlay.png)

## H&E and mIHC Overlay

Registered H&E and multiplex immunohistochemistry channels can be overlaid, with
channel visibility controlled from the viewer.

![mIHC channel overlay](media/mihc-channel-overlay.png)

## Spatial Transcriptomics Spots

Seurat, Giotto, and SpatialExperiment projects can show image-aligned spots and
dimensionality-reduction selections.

![Seurat and SpatialExperiment spots](media/spatial-spots.png)

## CellPhenotyper Cells

CellPhenotyper outputs can be loaded as a project with cells, masks, channels,
and analysis overlays.

![CellPhenotyper cells](media/cellphenotyper-cells.png)

## Tile Grid Preview

Tile grids can be previewed before extraction so users can confirm the exact
regions that will be exported.

![Tile grid preview](media/tile-grid.png)

## Live R Synchronization

The live viewer uses an `httpuv` bridge so annotations, selections, measurements,
and segmentation or cell overlays can be retrieved from the active R session.

![Live R synchronization](media/live-r-synchronization.png)

## ROI Round Trip

This short GIF illustrates the intended workflow: draw an ROI in the viewer,
retrieve it in R, and export it as GeoJSON.

![Drawing an ROI and retrieving it in R](media/roi-roundtrip.gif)
