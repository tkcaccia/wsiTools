# wsiTools

<div class="wsitools-hero" markdown>

**Memory-efficient whole-slide image workflows in R.**

Open, inspect, tile, annotate, convert, and connect pathology images to spatial
omics without loading a complete level-0 slide into R memory by default.

[Install wsiTools](installation.md){ .md-button .md-button--primary }
[Follow the tutorials](tutorials/index.md){ .md-button }

</div>

!!! info "Designed for images that are larger than memory"
    wsiTools works from backend metadata, pyramid levels, explicit region reads,
    tile manifests, and streamed conversion. Optional tools such as OpenSlide,
    libvips, Bio-Formats, native CZI support, StarDist, and Mesmer are discovered
    at runtime rather than bundled into the core R package.

<div class="grid cards" markdown>

-   **Open and inspect**

    Use a friendly one-image entry point, or keep explicit control of slide
    metadata, pyramid levels, pixel size, associated images, and backend choice.

    [Open one image](open-one-image.md)

-   **View and annotate**

    Run a static viewer for inspection or a live R-connected viewer that returns
    annotations, measurements, selections, segmentation, and project state.

    [Use the live viewer](live-viewer.md)

-   **Tile and preprocess**

    Estimate tissue at low resolution, generate coordinate-only tile grids, and
    export only the image regions needed by downstream analysis.

    [Detect tissue and extract tiles](tutorials/tissue-and-tiles.md)

-   **Connect analysis layers**

    Overlay Seurat, Giotto, SpatialExperiment, CellPhenotyper, segmentation, and
    multiplexed imaging results while keeping data access in R.

    [Add spatial omics](tutorials/spatial-omics.md)

</div>

## Start in five minutes

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE
)

library(wsiTools)
wsi_backends()

viewer <- wsi_open_viewer("/path/to/sample.svs")
```

No slide is available yet? Open the synthetic, lightweight demonstration:

```r
demo <- wsi_demo_viewer(open = TRUE)
demo$path
```

## Choose a path

| Goal | Start here |
| --- | --- |
| Install the R package and diagnose the machine | [Installation](installation.md) |
| Open one SVS, CZI, OME-TIFF, TIFF, or another supported image | [Open one image](open-one-image.md) |
| Keep the browser synchronized with R | [Live viewer](live-viewer.md) |
| Draw annotations and recover them as GeoJSON in R | [Annotation round-trip](tutorials/annotations-roundtrip.md) |
| Generate reproducible tissue-aware tiles | [Tissue and tiles](tutorials/tissue-and-tiles.md) |
| Segment cells inside one selected ROI | [Selected-ROI segmentation](tutorials/segment-selected-roi.md) |
| Add Seurat, Giotto, or SpatialExperiment coordinates | [Spatial omics](tutorials/spatial-omics.md) |
| Diagnose missing tiles, backends, or live synchronization | [Troubleshooting](troubleshooting.md) |

## How the documentation is organized

**Tutorials** lead through complete outcomes in a deliberate order. **Guides**
answer focused questions about installation, backends, viewers, projects, and
integrations. The generated **R function reference** remains on the pkgdown site,
so narrative documentation does not duplicate roxygen documentation.

## Familiar tasks, translated to an R workflow

The tutorial sequence follows familiar digital-pathology tasks—opening projects,
finding tissue, drawing annotations, detecting cells, adding analysis layers,
and exporting results. This task-first progression is motivated by the
[QuPath tutorials](https://qupath.readthedocs.io/en/stable/docs/tutorials/index.html),
but the implementation is specific to wsiTools: R-first, backend-aware,
reproducible, and designed around bounded memory use.

wsiTools is not a replacement for all QuPath functionality. GeoJSON and exported
measurements provide practical interoperability where workflows overlap.
