# Examples Gallery

These examples are meant to be copied, pasted, and edited. Replace file paths
with files on your computer. For installation and backend checks, see
[installation](installation.md) and [backend setup](backends.md).

Start most sessions with:

```r
library(wsiTools)

wsi_backends()
wsi_setup_report()
```

## Installed Copy-Paste Scripts

wsiTools ships copy-paste scripts in `inst/examples/`. After installation, get
their paths with `system.file()`:

```r
system.file("examples/open_single_svs_live.R", package = "wsiTools")
system.file("examples/open_czi_project_live.R", package = "wsiTools")
system.file("examples/open_spatialexperiment_four_slide_live.R", package = "wsiTools")
system.file("examples/open_cellphenotyper_project_live.R", package = "wsiTools")
system.file("examples/open_he_mihc_overlay_live.R", package = "wsiTools")
```

Run one directly from R:

```r
Sys.setenv(WSITOOLS_SVS = "/path/to/sample.svs")
source(system.file("examples/open_single_svs_live.R", package = "wsiTools"))
```

Each script documents the environment variables it accepts, so the same file
can be used from RStudio, `Rscript`, or a remote desktop.

For the full selected-ROI StarDist, Mesmer, mask import, and cell export
workflow, see [cell segmentation](cell-segmentation.md).
For Seurat, Giotto, SpatialExperiment, and CellPhenotyper workflows, see
[spatial omics](spatial-omics.md).
For the `.wsiproject` directory layout and reproducible project state, see
[project format](projects.md).

## Open The Built-In Demo

Use this when you do not have an SVS, CZI, OME-TIFF, Seurat object, or
CellPhenotyper project available yet:

```r
library(wsiTools)

demo <- wsi_demo_viewer(open = TRUE)

demo$path
demo$viewer
demo$files
```

The generated demo directory contains:

- mock slide metadata;
- a tiny PNG and tiny TIFF;
- fake ROI GeoJSON;
- fake cell centroids CSV;
- fake tile grid CSV;
- fake spatial spots, expression, and PCA files;
- an HTML viewer with annotations, cells, and spatial spots.

## Open SVS

```r
library(wsiTools)

viewer <- wsi_open_viewer("/path/to/sample.svs")
```

Inspect metadata:

```r
slide <- wsi_open("/path/to/sample.svs")
wsi_info(slide)
wsi_levels(slide)
wsi_mpp(slide)
```

Close when done:

```r
viewer$stop()
wsi_close(slide)
```

## Open CZI

For CZI files, the project viewer is usually the easiest first view because a
single CZI can contain several scenes/sections:

```r
library(wsiTools)

html <- wsi_viewer_project(
  images = "/path/to/image.czi",
  output = "czi_project_viewer.html",
  czi_sections = TRUE,
  open = TRUE,
  overwrite = TRUE
)
```

Check CZI-capable backends:

```r
wsi_has_native_czi()
wsi_has_bioformats()
wsi_backends()
```

## Open OME-TIFF

```r
library(wsiTools)

slide <- wsi_open("/path/to/image.ome.tif")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE
)

viewer$open()
```

If the OME-TIFF is a channel image to overlay on another slide, see
[mIHC channel overlay](#mihc-channel-overlay).

## Open Multiple Images As A Project

```r
library(wsiTools)

images <- c(
  "/path/to/section_1.svs",
  "/path/to/section_2.svs",
  "/path/to/section_3.czi",
  "/path/to/probabilities.ome.tif"
)

html <- wsi_viewer_project(
  images = images,
  output = "case_project_viewer.html",
  open = TRUE,
  overwrite = TRUE
)
```

Annotations are stored separately for each project image/section.

## Open Seurat + Image

```r
library(wsiTools)
library(Seurat)

# seurat_obj <- readRDS("/path/to/seurat_object.rds")

viewer <- wsi_viewer_seurat(
  seurat = seurat_obj,
  image = "/path/to/high_resolution_tissue_image.tif",
  dynamic_tiles = TRUE,
  mode = "tiles",
  open = TRUE,
  wait = FALSE
)

viewer$get_rois()
viewer$get_annotation_spots()
```

In live mode, type a gene name in the Seurat menu to retrieve that gene from R
without embedding the full expression matrix in the browser.

## Open Giotto + Image

```r
library(wsiTools)

# giotto_obj <- readRDS("/path/to/giotto_object.rds")

viewer <- wsi_viewer_giotto(
  giotto = giotto_obj,
  image = "/path/to/high_resolution_tissue_image.tif",
  dynamic_tiles = TRUE,
  mode = "tiles",
  open = TRUE,
  wait = FALSE
)

viewer$get_rois()
viewer$get_annotation_spots()
```

## Open SpatialExperiment + Image

```r
library(wsiTools)

# spe <- readRDS("/path/to/spatialexperiment_object.rds")

viewer <- wsi_viewer_spatialexperiment(
  spe = spe,
  image = "/path/to/high_resolution_tissue_image.tif",
  dynamic_tiles = TRUE,
  mode = "tiles",
  open = TRUE,
  wait = FALSE
)

viewer$get_rois()
viewer$get_annotation_spots()
```

## Open CellPhenotyper Project

CellPhenotyper projects are opened from the output directory or from
`00_execution/project_outputs.tsv`.

```r
library(wsiTools)

project <- wsi_read_cellphenotyper_project(
  "/path/to/CellPhenotyper_outputs"
)

viewer <- wsi_viewer_cellphenotyper(
  project,
  live = TRUE,
  mode = "tiles",
  open = TRUE,
  wait = FALSE
)

viewer$get_segmentation()
viewer$get_rois()
```

When a GigaTIME probability OME-TIFF is present, the channels are controlled
from the `Stains` menu. CellPhenotyper cells and annotations are available from
the corresponding top menus.

## Load GeoJSON

```r
library(wsiTools)

slide <- wsi_open("/path/to/sample.svs")
rois <- wsi_read_geojson("/path/to/qupath_annotations.geojson")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  roi = rois
)

viewer$open()
```

For an existing live viewer:

```r
rois <- wsi_read_geojson("/path/to/qupath_annotations.geojson")
viewer$add_rois(rois)
```

## Draw Annotations

```r
library(wsiTools)

slide <- wsi_open("/path/to/sample.svs")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = FALSE
)

# Draw polygons or brush annotations in the browser.
# Then retrieve them from R:
rois <- viewer$get_rois()
selected <- viewer$get_selected_roi()
```

Use the live viewer if you want browser annotations to return automatically to
R. Static `file://` viewers do not synchronize back to R.

## Export GeoJSON

```r
rois <- viewer$get_rois()

write_geojson(
  rois,
  file = "edited_annotations.geojson",
  overwrite = TRUE
)
```

Round trip with QuPath:

```r
rois <- wsi_read_geojson("qupath_annotations.geojson")
viewer$add_rois(rois)

edited <- viewer$get_rois()
write_geojson(edited, "edited_for_qupath.geojson", overwrite = TRUE)
```

## Extract Tiles

Generate a tile grid without reading tiles:

```r
grid <- wsi_tile_grid(
  slide,
  tile_size = 512,
  overlap = 0,
  level = 0,
  include_partial = FALSE
)

head(grid)
```

Write tiles:

```r
manifest <- wsi_export_tiles(
  slide,
  grid = grid,
  output_dir = "tiles",
  format = "png",
  overwrite = FALSE
)
```

High-level extraction:

```r
manifest <- wsi_tile(
  slide,
  output_dir = "tiles",
  tile_size = 512,
  overlap = 0,
  level = 0,
  tissue_mask = TRUE,
  tissue_threshold = 0.1,
  format = "png",
  overwrite = FALSE
)
```

If an annotation is selected in a live spatial viewer:

```r
selected_roi <- viewer$get_selected_roi()

manifest <- extract_tiles(
  slide,
  roi = selected_roi,
  tile_size = 512,
  stride = 512,
  skip_background = TRUE,
  output_dir = "roi_tiles"
)
```

## Run StarDist On Selected ROI

StarDist is optional. wsiTools does not install or load it automatically.

```r
wsi_has_stardist()
```

Open a live viewer with selected-ROI segmentation enabled:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_engines = c("stardist_he", "stardist_ihc"),
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32,
  open = TRUE,
  wait = FALSE
)
```

Draw/select an ROI in the browser and use `Cells > Run selected ROI`, or run
from R:

```r
roi <- viewer$get_selected_roi()

job <- viewer$run_segmentation_async(
  roi = roi,
  engine = "stardist_he",
  tiles_x = 32,
  tiles_y = 32
)

result <- job$result()
viewer$add_segmentation(result$segmentation)
viewer$get_segmentation()
```

The memory-safe pattern is ROI crop first, then tiled StarDist on the crop.
Never run a model on the full WSI loaded into R memory.

## Load Cell Mask

Load a mask as an overlay:

```r
mask <- import_segmentation(
  "/path/to/cell_mask.png",
  type = "mask"
)

viewer$add_segmentation(mask)
```

Convert mask components to cell ROI polygons:

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

GeoJSON and centroid tables can also be imported:

```r
cells_geojson <- import_segmentation("/path/to/stardist_cells.geojson")
cells_csv <- import_segmentation("/path/to/cell_centroids.csv")
```

## H&E Stain Deconvolution

Open an H&E slide with hematoxylin, eosin, and residual controls in the
`Stains` menu:

```r
library(wsiTools)

slide <- wsi_open("/path/to/he_slide.svs")

html <- wsi_viewer_he(
  slide,
  mode = "tiles",
  method = "macenko",
  output = "he_deconvolution_viewer.html",
  open = TRUE,
  overwrite = TRUE
)
```

Read and deconvolve one small region in R:

```r
he_region <- wsi_deconvolve_he_region(
  slide,
  x = 10000,
  y = 20000,
  width = 512,
  height = 512,
  level = 0,
  method = "fixed"
)
```

Use `method = "macenko"` or `method = "vahadane"` when stain vectors should be
estimated from a thumbnail rather than fixed defaults.

## mIHC Channel Overlay

Open an H&E base slide with a registered mIHC OME-TIFF overlay:

```r
library(wsiTools)

viewer <- wsi_viewer_he_mihc(
  he = "/path/to/he_slide.svs",
  mihc = "/path/to/mihc_probs.ome.tif",
  registration = "/path/to/registration_shift.json",
  channel_names = c("DAPI", "CD3", "CD20", "PanCK", "CD68"),
  colours = c("#3B82F6", "#22C55E", "#F97316", "#E11D48", "#A855F7"),
  opacity = 0.55,
  visible = TRUE,
  open = TRUE,
  wait = FALSE
)
```

Use the `Stains` menu to toggle the H&E base and individual mIHC channels.
The full images remain file-backed; the viewer requests tiles as needed.

For brightfield multiplex IHC deconvolution:

```r
slide <- wsi_open("/path/to/multiplex_ihc.svs")

channels <- wsi_stain_channels(
  name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
  vector = list(
    c(0.650, 0.704, 0.286),
    c(0.268, 0.570, 0.776),
    c(0.213, 0.851, 0.477)
  ),
  colour = c("#4b3f99", "#8b5a2b", "#d73027")
)

wsi_viewer_multi_ihc(
  slide,
  mode = "tiles",
  channels = channels,
  output = "multi_ihc_viewer.html",
  open = TRUE,
  overwrite = TRUE
)
```
