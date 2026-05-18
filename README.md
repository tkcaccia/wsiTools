# wsiTools

wsiTools is an R toolkit for memory-efficient WSI access and preprocessing
through region-based reading, tiling, pyramidal image handling, conversion and
export.

Whole-slide images can be extremely large, so wsiTools is designed around
backend metadata, pyramid levels, explicit region reads, tile manifests, and
streaming conversion. It does not load complete level-0 slides into R memory by
default.

## Installation

```r
# install.packages("remotes")
remotes::install_github("stefano/wsiTools")
```

From a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

## System dependencies

wsiTools can be installed without WSI system libraries, but backend-dependent
functions need the corresponding tools at runtime:

- OpenSlide command-line tools for OpenSlide-backed metadata and region reads.
- libvips command-line tools (`vips`, `vipsheader`) for conversion, pyramids,
  thumbnails, cropping, and export.
- Bio-Formats command-line tools are planned for future microscopy format
  support.

Check your local capabilities with:

```r
library(wsiTools)
wsi_backends()
```

Supported formats depend on the installed backend and the specific file. The
package reports backend availability and errors explicitly rather than claiming
that every WSI variant is guaranteed to work.

## Basic example

```r
library(wsiTools)

slide <- wsi_open("sample.svs")
wsi_info(slide)

thumb <- wsi_thumbnail(slide, width = 1000)

viewer <- wsi_viewer(slide, width = 1600)

full_res_viewer <- wsi_viewer(
  slide,
  mode = "tiles",
  output = "sample_viewer.html",
  tile_dir = "sample_viewer_tiles"
)

patch <- wsi_read_region(
  slide,
  x = 10000,
  y = 20000,
  width = 512,
  height = 512,
  level = 0
)

tiles <- wsi_tile(
  slide,
  output_dir = "tiles",
  tile_size = 512,
  level = 0,
  tissue_mask = TRUE
)

wsi_close(slide)
```

## Tile grids without reading pixels

```r
grid <- wsi_tile_grid(
  slide,
  tile_size = 512,
  overlap = 64,
  level = 0,
  include_partial = FALSE
)
head(grid)
```

`wsi_tile_grid()` only creates coordinates. Tile pixels are read later by
`wsi_tile()` or `wsi_export_tiles()`.

## Interactive preview

```r
viewer <- wsi_viewer(slide, width = 1600)
```

`wsi_viewer()` creates a self-contained HTML viewer from a backend-generated
thumbnail. It supports pan, zoom, and level-0 coordinate readout without loading
the full slide into R memory.

The interactive toolbar includes pan/select modes, fit and 1:1 zoom, ROI and
label toggles, ROI opacity, previous/next ROI navigation, a side window listing
all GeoJSON geometries, crosshair display, coordinate copying, polygon drawing,
and GeoJSON export. Use `GeoJSON` to open the geometry list; each row shows the
geometry type, bounds, point count, source, and id. Use `Draw ROI`, click
polygon vertices, double-click or press `Finish`, then use `Save GeoJSON`. In a
static browser viewer this opens the browser's normal save/download flow rather
than silently writing to a server path.

For full-resolution zooming, build a Deep Zoom tile pyramid with libvips:

```r
viewer <- wsi_viewer(
  slide,
  mode = "tiles",
  output = "sample_viewer.html",
  tile_dir = "sample_viewer_tiles",
  tile_size = 512
)
```

This writes browser-readable tiles next to the HTML file. Zooming then requests
higher-resolution tiles instead of magnifying a thumbnail.

## IHC stain deconvolution

For hematoxylin plus HRP/DAB immunohistochemistry, deconvolve a region without
loading the full slide:

```r
channels <- wsi_deconvolve_region(
  slide,
  x = 10000,
  y = 20000,
  width = 1024,
  height = 1024
)

names(channels)
```

`channels$hematoxylin` and `channels$hrp` are separate optical-density
concentration matrices. To inspect a slide interactively, use the IHC viewer:

```r
ihc_viewer <- wsi_viewer_ihc(
  slide,
  mode = "tiles",
  output = "sample_ihc_viewer.html",
  tile_dir = "sample_viewer_tiles"
)
```

The viewer adds an `IHC` toggle, separate hematoxylin and HRP/DAB visibility
controls, color pickers, and gain sliders. In tiled mode the browser recolors
only the visible Deep Zoom tiles, so the full WSI is not loaded into R memory.

## Conversion example

```r
wsi_convert(
  input = "sample.svs",
  output = "sample.ome.tiff",
  format = "ome-tiff",
  pyramid = TRUE,
  compression = "lzw"
)
```

## ROI example

```r
roi <- wsi_read_geojson("annotations.geojson")
tiles <- wsi_tile_roi(slide, roi, output_dir = "roi_tiles")

roi_viewer <- wsi_viewer_roi(
  slide,
  roi,
  mode = "tiles",
  output = "sample_roi_viewer.html",
  tile_dir = "sample_viewer_tiles"
)
```

The first milestone reads QuPath-style GeoJSON and records polygon coordinates
and bounding boxes. Polygon-aware cropping and tiling are intentionally routed
through optional `sf` support.

GeoJSON annotations can also be overlaid directly in the interactive viewer:

```r
roi <- wsi_read_geojson("annotations.geojson")

viewer <- wsi_viewer_roi(
  slide,
  roi,
  mode = "tiles",
  output = "sample_roi_viewer.html",
  tile_dir = "sample_viewer_tiles"
)
```

ROI polygons are drawn in level-0 slide coordinates, which matches common
QuPath GeoJSON exports.

The same viewer can create new polygon annotations interactively and export
them as a GeoJSON `FeatureCollection`.

## First milestone status

Implemented:

- package skeleton with roxygen2 documentation
- backend checks for OpenSlide, libvips, Bio-Formats, and ImageMagick
- lightweight `wsi_slide` abstraction
- OpenSlide/libvips command-line metadata paths when installed
- mock slide support for tests
- slide info, levels, properties, MPP, objective power
- coordinate validation and region-read abstraction
- libvips command wrapper, conversion, and pyramid helpers
- thumbnail and crop scaffolding
- tile grid generation and tile manifest class
- simple thumbnail-based tissue mask
- basic GeoJSON ROI parser
- S3 print, summary, and plot methods
- testthat suite

Planned:

- native OpenSlide C/Rcpp bindings with external-pointer finalizers
- associated image reads
- richer ROI geometry filtering and polygon masking
- parallel tile extraction
- advanced tissue segmentation
