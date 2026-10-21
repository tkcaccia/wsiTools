# Tutorials

These tutorials teach wsiTools through complete pathology workflows rather than
through an alphabetical function list. Each page starts with an outcome, uses a
small set of commands, includes a checkpoint, and points to the next useful task.

The progression is motivated by the task-oriented structure of the
[QuPath tutorials](https://qupath.readthedocs.io/en/stable/docs/tutorials/index.html):
projects and images first, then tissue, annotations, cells, analysis layers, and
exports. The commands and memory model here are specific to wsiTools.

## Before you begin

Install wsiTools, attach the package, and inspect available backends:

```r
library(wsiTools)
wsi_backends()
wsi_setup_report()
```

The core package installs without large image or model backends. A tutorial that
needs an optional backend says so before it performs the relevant step.

## Learning path

1. **[Open your first slide](first-slide.md)** — open a real image or the built-in
   demonstration, inspect metadata, and close resources cleanly.
2. **[Annotate and round-trip](annotations-roundtrip.md)** — draw an ROI in the
   browser, retrieve it in R, write GeoJSON, and add it back to the viewer.
3. **[Detect tissue and extract tiles](tissue-and-tiles.md)** — create a
   low-resolution tissue mask, a coordinate-only grid, and a reproducible tile
   manifest.
4. **[Segment a selected ROI](segment-selected-roi.md)** — keep model execution
   bounded to one selected region and return cells to the live session.
5. **[Add spatial omics](spatial-omics.md)** — connect Seurat, Giotto, or
   SpatialExperiment data to an image without embedding a full expression matrix
   in browser HTML.
6. **[Export and save results](export-results.md)** — persist annotations,
   measurements, annotation-to-spot associations, and project state.

## Choose by task

| I need to… | Tutorial |
| --- | --- |
| Confirm the package and viewer work | [Open your first slide](first-slide.md) |
| Exchange annotations with another tool | [Annotate and round-trip](annotations-roundtrip.md) |
| Prepare model-ready image patches | [Detect tissue and extract tiles](tissue-and-tiles.md) |
| Run StarDist or Mesmer without processing a full WSI | [Segment a selected ROI](segment-selected-roi.md) |
| View spots, clusters, or expression over tissue | [Add spatial omics](spatial-omics.md) |
| Make the analysis reproducible | [Export and save results](export-results.md) |

!!! tip "Use the detailed guides when you need options"
    Tutorials intentionally show one recommended path. The [examples gallery](../examples.md),
    [live viewer guide](../live-viewer.md), [cell segmentation guide](../cell-segmentation.md),
    and [spatial omics guide](../spatial-omics.md) cover additional formats,
    integrations, and failure modes.
