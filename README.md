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
remotes::install_github("tkcaccia/wsiTools")
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

If tile positions come from a CSV file, model output, or another annotation
tool, convert the coordinate list directly into a tile grid:

```r
coords <- data.frame(
  tile_id = c("spot_001", "spot_002"),
  x = c(10000, 12500),
  y = c(20000, 21500)
)

coord_grid <- wsi_tile_grid_from_coords(
  slide,
  coords,
  tile_size = 512,
  anchor = "center"
)

manifest <- wsi_tile_from_coords(
  slide,
  coords,
  output_dir = "coordinate_tiles",
  tile_size = 512,
  anchor = "center",
  bounds = "trim"
)
```

## Interactive preview

```r
viewer <- wsi_viewer(slide, width = 1600)
```

`wsi_viewer()` creates a self-contained HTML viewer from a backend-generated
thumbnail. It supports pan, zoom, and level-0 coordinate readout without loading
the full slide into R memory.

The interactive toolbar is organized into menus: `Navigate`, `Annotations`,
`GeoJSON`, `View`, and, when enabled, `Stains`. These menus group pan/select
modes, fit and 1:1 zoom, ROI and label toggles, ROI opacity, previous/next ROI
navigation, a side window listing all GeoJSON geometries, crosshair display,
coordinate copying, polygon drawing, and GeoJSON export. Use `GeoJSON` to open
the geometry list; each row shows the geometry type, bounds, point count,
source, and id. Use `Draw ROI`, click polygon vertices, double-click or press
`Finish`, then use `Save GeoJSON`. In `Brush` mode, painting with no selected
annotation creates a new ROI; painting with an annotation selected extends it;
holding `Alt` while brushing removes from the selected annotation. Use `Edit`
to move vertices, double-click an edge to insert a vertex, and Backspace/Delete
to remove the active vertex. The `Name`, `Class`, and `Custom class` controls
update the selected annotation label and category before GeoJSON export. In a
static browser viewer this opens the
browser's normal save/download flow rather than silently writing to a server
path.

For a live R workflow, use `wsi_viewer_live()`. This starts an optional local
`httpuv` bridge so browser-side annotations and analyses are posted back to the
current R session automatically:

```r
session <- wsi_viewer_live(
  slide,
  mode = "tiles",
  name = "viewer_state"
)

# After drawing ROIs, measuring distances, importing GeoJSON, or adding
# segmentation overlays in the browser:
viewer_state_rois
viewer_state_measurements
viewer_state_segmentation
wsi_viewer_state(session)$rois
```

In plain R sessions the live loop is serviced until you press Esc or Ctrl+C.
The synced objects remain available in the chosen R environment.

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

### H&E BTF interactive viewer and conversion

For an H&E image saved as BigTIFF/BTF, the repository includes a runnable
example at `inst/examples/he-btf-viewer-convert.R`. It opens metadata only,
writes a tiled interactive HTML viewer, and converts the BTF to a compressed
pyramidal OME-TIFF.

```r
Sys.setenv(WSITOOLS_HE_BTF = "/path/to/he_image.btf")
source(system.file("examples/he-btf-viewer-convert.R", package = "wsiTools"))
```

From the package source tree:

```sh
WSITOOLS_HE_BTF="/path/to/he_image.btf" Rscript inst/examples/he-btf-viewer-convert.R
```

The example requires libvips (`vips` and `vipsheader`) and does not overwrite
converted output unless `WSITOOLS_OVERWRITE=TRUE` is set.

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

The `Stains` menu adds an `IHC` toggle, separate hematoxylin and HRP/DAB
visibility controls, color pickers, and gain sliders. In tiled mode the browser
recolors only the visible Deep Zoom tiles, so the full WSI is not loaded into R
memory.

For brightfield multiplex IHC, define up to three optical-density stain
channels and open the selectable multi-channel viewer:

```r
multi_channels <- wsi_stain_channels(
  name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
  vector = list(
    c(0.650, 0.704, 0.286),
    c(0.268, 0.570, 0.776),
    c(0.213, 0.851, 0.477)
  ),
  colour = c("#4b3f99", "#8b5a2b", "#d73027")
)

multi_viewer <- wsi_viewer_multi_ihc(
  slide,
  channels = multi_channels,
  mode = "tiles",
  output = "sample_multi_ihc_viewer.html",
  tile_dir = "sample_viewer_tiles"
)
```

The `Stains` menu exposes the `mIHC` toggle plus a checkbox, colour picker, and
gain slider for each channel. The default vectors are only starting values; use
assay-specific stain vectors for quantitative work.

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

## Practical pathology workflows

Compare two slides or derived images side by side. Zoom, pan, and cursor
position are synchronized by default, and ROI or mask overlays can be supplied
for either side:

```r
viewer_compare(
  "sample_he.svs",
  "sample_ihc.svs",
  sync = TRUE,
  roi1 = "he_annotations.geojson",
  output = "he_ihc_compare.html"
)
```

OME-Zarr inputs are opened as lightweight metadata-backed slide handles. The
first implementation reads dimensions, levels, and NGFF metadata without
decoding full image chunks:

```r
zarr <- open_omezarr("sample.ome.zarr")
omezarr_metadata("sample.ome.zarr")
```

ROI annotations can be imported from QuPath-compatible GeoJSON, labelled,
written back to GeoJSON, and overlaid in the viewer:

```r
rois <- read_geojson("annotations.geojson")
rois <- wsi_set_roi_class(rois, "tumour", roi_id = rois$roi_id[1])
write_geojson(rois, "annotations_relabelled.geojson", overwrite = TRUE)

viewer_add_rois(slide, rois, output = "slide_rois.html")
```

Optional external segmentation tools remain outside the dependency tree. Export
an ROI crop for a tool such as StarDist or Cellpose, then import GeoJSON
polygons, CSV/TSV centroids, or a mask image as an overlay. Centroid tables are
drawn as cell markers in the viewer:

```r
export_roi_crop(slide, rois, "roi_crop.png", roi_id = rois$roi_id[1])
segmentation <- import_segmentation("model_output.geojson")
viewer_add_segmentation(slide, segmentation, output = "segmentation.html")

cells <- import_segmentation("stardist_centroids.csv")
viewer_add_segmentation(slide, cells, output = "stardist_cells.html", cell_radius = 8)
```

For StarDist, `wsiTools` keeps Python optional. In the interactive viewer,
select or brush an ROI, open the Segmentation menu, export the selected ROI,
run StarDist on the ROI crop, then load the result GeoJSON or CSV/TSV centroids
back into the viewer.
From R, the same workflow can be scripted with a user-supplied StarDist command:

```r
result <- stardist_segment_roi(
  slide,
  rois,
  output_dir = "stardist_roi",
  roi_id = rois$roi_id[1],
  command = "python",
  args = c("run_stardist.py", "{input}", "{output}", "{model}"),
  model = "2D_versatile_he"
)

viewer_add_segmentation(slide, result$segmentation, output = "stardist_overlay.html")
```

The live viewer can also start segmentation directly on the selected ROI. Start
the viewer with `stardist = TRUE`; wsiTools starts the local R endpoint and
wires the `Start StarDist` button automatically:

```r
slide <- wsi_open("sample.svs")
session <- wsi_viewer_live(
  slide,
  mode = "tiles",
  stardist = TRUE,
  stardist_command = "python",
  stardist_args = c("run_stardist.py", "{input}", "{output}", "{model}"),
  output = "slide_with_stardist_runner.html"
)
```

In the viewer, select or brush an ROI, open `Segmentation`, and press
`Start StarDist`. The selected ROI is sent to the local endpoint, StarDist runs
on the ROI crop, and returned cell polygons are added as overlays. If
`stardist-predict2d` is already on `PATH`, you can omit `stardist_command` and
`stardist_args`.

The same StarDist bridge is available from the command line. From a source
checkout use `./exec/wsitools`; from an installed package you can resolve the
script path with `system.file()`:

```sh
WSITOOLS_BIN="$(Rscript -e 'cat(system.file("exec", "wsitools", package = "wsiTools"))')"

"$WSITOOLS_BIN" stardist-roi \
  --image sample.svs \
  --roi selected_roi.geojson \
  --output-dir stardist_roi \
  --command python \
  --arg run_stardist.py \
  --arg '{input}' \
  --arg '{output}' \
  --arg '{model}' \
  --model 2D_versatile_he \
  --overwrite
```

Other CLI commands include:

```sh
"$WSITOOLS_BIN" backends
"$WSITOOLS_BIN" stardist-image --input roi_crop.png --output cells.geojson --command python --arg run_stardist.py --arg '{input}' --arg '{output}'
"$WSITOOLS_BIN" translate-rois --input cells_crop.geojson --output cells_slide.geojson --dx 10000 --dy 20000 --overwrite
```

A complete selected-ROI example is available at
`inst/examples/stardist-selected-roi-cli.R`. It opens a slide, overlays the ROI
GeoJSON, exports the ROI crop, optionally runs StarDist, and prints the
equivalent `wsitools stardist-roi` command:

```sh
WSITOOLS_IMAGE="/path/to/slide.svs" \
WSITOOLS_ROI_GEOJSON="/path/to/selected_roi.geojson" \
Rscript inst/examples/stardist-selected-roi-cli.R
```

For patch extraction, `extract_tiles()` accepts fixed tile size and stride. It
returns coordinates without reading pixels unless `output_dir` is supplied:

```r
grid <- extract_tiles(slide, roi = rois, tile_size = 512, stride = 256, save_images = FALSE)
manifest <- extract_tiles(slide, roi = rois, tile_size = 512, stride = 512, output_dir = "roi_tiles")
```

Basic measurement and registration helpers cover manual pathology analysis
workflows:

```r
cells <- data.frame(x = c(1000, 1200, 3000), y = c(800, 900, 1800))
measure_cell_density(cells, rois, pixel_size = wsi_mpp(slide))
summarise_rois(rois, cells = cells, pixel_size = wsi_mpp(slide), file = "roi_summary.csv")

transform <- estimate_transform(landmarks1, landmarks2)
transformed_rois <- transform_rois(rois, transform)
```

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
- ROI GeoJSON writing, class labels, and viewer overlay helpers
- side-by-side comparison viewer
- OME-Zarr metadata-backed opening
- optional segmentation import/export bridge
- StarDist polygon and centroid cell overlays in the HTML viewer
- stride-based tile extraction wrapper
- basic measurements, tissue class summaries, and affine ROI transforms
- S3 print, summary, and plot methods
- testthat suite

Planned:

- native OpenSlide C/Rcpp bindings with external-pointer finalizers
- OME-Zarr chunk decoding for true tiled pixel display
- associated image reads
- richer ROI geometry filtering and polygon masking
- parallel tile extraction
- advanced tissue segmentation
