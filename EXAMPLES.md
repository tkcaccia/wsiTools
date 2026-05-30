# wsiTools Examples

This page collects practical examples for opening, viewing, annotating,
converting, and analysing whole-slide or large microscopy images with
wsiTools. The main README focuses on installation and backend setup; this page
focuses on workflows.

wsiTools is designed for memory-efficient image access. The examples below use
metadata, thumbnails, tiles, dynamic tile endpoints, and region reads. They do
not intentionally load complete level-0 whole-slide images into R memory.

## Before Running Examples

Start every new machine with:

```r
library(wsiTools)
wsi_backends()
```

Useful optional backends are:

- `libvips`: fast thumbnails, Deep Zoom tiles, pyramidal TIFF/OME-TIFF export.
- `openslide`: common pathology WSI metadata and region reads.
- `httpuv`: live viewer bridge between browser and R.
- `magick`: fallback previews for ordinary image formats.
- native CZI/libCZIAPI or Bio-Formats Java helper: Zeiss CZI previews and
  metadata.

Use this to inspect the setup plan:

```r
wsi_setup()
```

## Installed Example Scripts

After installing the package, examples can be inspected with:

```r
system.file("examples", package = "wsiTools")
```

Run a bundled example like this:

```r
source(system.file("examples/first-run-guided-example.R", package = "wsiTools"))
```

Bundled scripts include:

| Script | Purpose |
| --- | --- |
| `first-run-guided-example.R` | Opens a small mock project with ROIs, cells, mask annotations, and tile grid. |
| `he-btf-viewer-convert.R` | Opens a simple H&E image and converts it with libvips. |
| `he-estimated-fast-viewer.R` | Opens a fast H&E tiled viewer when libvips is available. |
| `he-gigatime-overlay-viewer.R` | Overlays GigaTIME/mIHC channels on a matched H&E image. |
| `sapc0052-ihc-deconvolution.R` | Demonstrates brightfield IHC deconvolution into hematoxylin and DAB/HRP-like channels. |
| `czi-project-viewer.R` | Opens one or more CZI images as a project viewer. |
| `cellphenotyper-project-viewer.R` | Opens CellPhenotyper outputs, including H&E, cells, GigaTIME channels, KODAMA, and GrandQC where available. |
| `stardist-selected-roi-cli.R` | Shows selected-ROI crop export and optional StarDist command-line integration. |
| `live-viewer-sync-example.R` | Demonstrates the live R/browser state bridge. |
| `project-session-example.R` | Saves and restores viewer project state. |
| `pathology-viewer-workflow.R` | Combines annotations, measurements, tiles, and export in one pathology workflow. |
| `tile-preview-layer-example.R` | Shows a tile grid overlay before extracting tiles. |
| `open-trajectory-undo-viewer.R` | Demonstrates trajectory drawing and undo behaviour. |

## Open One Image

Use this for a single SVS, TIFF/BTF, OME-TIFF, PNG, JPEG, or other backend
readable image:

```r
library(wsiTools)

path <- file.choose()
slide <- wsi_open(path)

wsi_info(slide)
wsi_levels(slide)

html <- wsi_viewer(
  slide,
  mode = "tiles",
  output = "single_image_viewer.html",
  tile_dir = "single_image_viewer_tiles",
  overwrite = TRUE,
  open = TRUE
)

wsi_close(slide)
```

Use `mode = "thumbnail"` for a small static preview. Use `mode = "tiles"` for
full-resolution zooming when libvips can generate Deep Zoom tiles.

## Live Viewer With R Feedback

Static viewers are self-contained HTML files. They can display images and
annotations, but they do not automatically update R objects while you interact
with the browser.

Use a live viewer when annotations, measurements, segmentation selections, or
PCA selections must come back to R:

```r
library(wsiTools)

slide <- wsi_open("sample.svs")

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  dynamic_tiles = TRUE,
  transport = "auto",
  open = TRUE,
  wait = FALSE
)

viewer$get_rois()
viewer$get_measurements()
viewer$get_selected_rois()
viewer$capabilities()

wsi_close(slide)
```

The live bridge uses `httpuv`. WebSocket transport is used when available, with
HTTP POST/polling as fallback.

## Multiple Images or CZI Sections

Use `wsi_viewer_project()` when a case contains several images, CZI files, or
multiple CZI scenes/sections:

```r
library(wsiTools)

images <- c(
  "/Users/stefano/Downloads/2025_10_24__1328.czi",
  "/Users/stefano/Downloads/2025_10_24__1329.czi",
  "/Users/stefano/Downloads/AP-GY-26-04_HE.svs"
)

html <- wsi_viewer_project(
  images,
  output = "/Users/stefano/Documents/viewer/czi_project_wsiTools_viewer.html",
  czi_sections = TRUE,
  overwrite = TRUE,
  open = TRUE
)
```

The Project panel keeps annotations separate for each image or section.
`czi_sections = TRUE` is the default and lists CZI scenes/sections separately.

## H&E Plus mIHC or GigaTIME Overlay

When you have an H&E image and a registered multiplex/OME-TIFF probability
image, open the H&E as the base layer and the mIHC image as tiled channels:

```r
library(wsiTools)

he <- "/Users/stefano/Downloads/AP-GY-26-04_HE.svs"
mihc <- "/Users/stefano/Documents/CellPhenotyper/remote_previews/apgy2604_ometiff_jpeg_pyramid/gigatime_probs.ome.tif"
shift <- "/Users/stefano/Documents/CellPhenotyper/remote_previews/apgy2604_registration/shift.json"

viewer <- wsi_viewer_he_mihc(
  he = he,
  mihc = mihc,
  mode = "tiles",
  channel_names = c("DAPI", "PD-1"),
  colours = c("#3B82F6", "#F97316"),
  visible = c(FALSE, TRUE),
  opacity = 0.55,
  registration = shift,
  base_layer_name = "H&E",
  open = TRUE,
  wait = FALSE
)
```

`wsi_viewer_he_mihc()` opens the H&E slide, prepares the mIHC channel sources,
and starts the live viewer.

Channel visibility is controlled from the top **Stains** menu. Channels that do
not belong to the current visualized image should not be shown.

## Brightfield IHC Deconvolution

Use this for a brightfield IHC image where hematoxylin and DAB/HRP-like stain
channels should be inspected separately:

```r
library(wsiTools)

slide <- wsi_open("/path/to/IHC_or_SAPC_0052.svs")

viewer <- wsi_viewer_multi_ihc(
  slide,
  mode = "tiles",
  output = "sapc0052_ihc_viewer.html",
  overwrite = TRUE,
  open = TRUE
)

wsi_close(slide)
```

For H&E, use `stain = "he"`:

```r
slide <- wsi_open("/path/to/he_slide.svs")

wsi_viewer(
  slide,
  mode = "tiles",
  stain = "he",
  output = "he_deconvolution_viewer.html",
  overwrite = TRUE,
  open = TRUE
)

wsi_close(slide)
```

Browser-side stain display is intended for visual exploration. For quantitative
analysis, use region-based deconvolution/export functions on selected ROIs or
tiles.

## GeoJSON Annotation Round Trip

```r
library(wsiTools)

slide <- wsi_open("sample.svs")
rois <- wsi_read_geojson("qupath_annotations.geojson")

html <- wsi_viewer(
  slide,
  mode = "tiles",
  roi = rois,
  output = "annotated_viewer.html",
  overwrite = TRUE,
  open = TRUE
)

edited <- wsi_read_geojson("annotated_viewer_annotations.geojson")
wsi_write_geojson(edited, "edited_for_qupath.geojson")

wsi_close(slide)
```

Use the live viewer when you need edited annotations to return to R immediately:

```r
viewer <- wsi_viewer_live(slide, roi = rois, wait = FALSE)
viewer$get_rois()
```

## Tile Grid and Tile Extraction

Generate coordinates without reading pixels:

```r
slide <- wsi_open("sample.svs")

grid <- wsi_tile_grid(
  slide,
  tile_size = 512,
  overlap = 0,
  level = 0,
  include_partial = FALSE
)

head(grid)
```

Extract tiles later:

```r
manifest <- wsi_tile(
  slide,
  output_dir = "tiles",
  tile_size = 512,
  level = 0,
  tissue_mask = TRUE,
  overwrite = FALSE
)

wsi_close(slide)
```

Tile manifests preserve provenance columns such as tile ID, file, coordinates,
level, row, column, and tissue fraction when available.

## OME-TIFF or Pyramidal TIFF Conversion

```r
library(wsiTools)

wsi_convert(
  input = "sample.svs",
  output = "sample.ome.tiff",
  format = "ome-tiff",
  pyramid = TRUE,
  compression = "lzw",
  overwrite = FALSE
)
```

Create a pyramidal TIFF/OME-TIFF explicitly:

```r
wsi_pyramid(
  input = "large_image.tif",
  output = "large_image_pyramid.ome.tif",
  tile_size = 512,
  compression = "lzw",
  ome = TRUE,
  overwrite = FALSE
)
```

These functions use libvips when available and never silently overwrite output
files.

## CellPhenotyper Project Viewer

CellPhenotyper projects are discovered from `00_execution/project_outputs.tsv`.
wsiTools resolves the H&E input image, StarDist/cell tables, GigaTIME
probability OME-TIFF, KODAMA outputs, and GrandQC GeoJSON where present.

```r
library(wsiTools)

project_dir <- "/Users/stefano/Documents/CellPhenotyper_1927zoom_newUNI2_GigaTIME_20260529/results_1927zoom_full_20260527_215129"

project <- wsi_read_cellphenotyper_project(project_dir)
project

viewer <- wsi_viewer_cellphenotyper(
  project,
  output = file.path(project$root, "cellphenotyper_wsiTools_viewer.html"),
  mode = "tiles",
  gigatime_overlay = TRUE,
  open = TRUE,
  wait = FALSE,
  overwrite = TRUE
)
```

The top menus provide:

- **Cells**: show or hide StarDist/cell segmentation overlays.
- **Stains**: show or hide GigaTIME/mIHC channels.
- **KODAMA**: import refined KODAMA GeoJSON and open the KODAMA plot.
- **Artifacts**: import GrandQC GeoJSON regions where available.

## Seurat / Visium Spatial PCA Viewer

This links a Seurat spatial object to an external high-resolution tissue image.
The viewer overlays spots on the tissue and opens a PCA plot from the top
**Seurat** menu. Lasso selection on the PCA plot highlights the matching spots
on the tissue image; live mode syncs the selection back to R.

```r
library(wsiTools)
library(Seurat)
library(SeuratData)

brain <- LoadData("stxBrain", type = "anterior1")
brain <- SCTransform(brain, assay = "Spatial", verbose = FALSE)
brain <- RunPCA(brain, assay = "SCT", verbose = FALSE)

viewer <- wsi_viewer_seurat(
  brain,
  image = "/Users/stefano/Downloads/V1_Mouse_Brain_Sagittal_Anterior_image.tif",
  image_name = "anterior1",
  coordinate_transform = "x_neg_y_y_neg_x",
  reduction = "pca",
  dims = c(1, 2),
  mode = "tiles",
  output = "/Users/stefano/Documents/viewer/seurat_mouse_brain_pca_viewer.html",
  overwrite = TRUE,
  open = TRUE
)
```

Live version:

```r
viewer <- wsi_viewer_seurat(
  brain,
  image = "/Users/stefano/Downloads/V1_Mouse_Brain_Sagittal_Anterior_image.tif",
  image_name = "anterior1",
  coordinate_transform = "x_neg_y_y_neg_x",
  reduction = "pca",
  dims = c(1, 2),
  live = TRUE,
  dynamic_tiles = TRUE,
  output = "/Users/stefano/Documents/viewer/seurat_mouse_brain_pca_live.html",
  overwrite = TRUE,
  open = TRUE,
  wait = FALSE
)

viewer$get_state()$seurat_selection
```

By default, wsiTools first tries to use the coordinates and scale factors stored
inside the Seurat object. The external mouse-brain TIFF above is oriented
differently from the Seurat spot coordinate frame, so the example uses
`coordinate_transform = "x_neg_y_y_neg_x"`, equivalent to `x1 = image_height - y`
and `y1 = image_width - x`. This corresponds to the orientation correction
`x1 = -y` and `y1 = -x`, with offsets added so browser coordinates remain
positive. If the external image needs only a vertical flip, use
`coordinate_transform = "flip_y"`. If it needs a 90-degree clockwise correction,
use `coordinate_transform = "x_y_y_neg_x"`, equivalent to `x1 = y` and
`y1 = image_width - x`. If spots appear shifted or scaled, pass the original
Space Ranger `spatial/` directory so wsiTools can use `tissue_positions.csv`
and `scalefactors_json.json`. If the external image is a resized/cropped
derivative of the 10x full-resolution image, inspect the linked object and
override the coordinate scaling:

```r
linked <- wsi_link_seurat_image(
  brain,
  image = "/Users/stefano/Downloads/V1_Mouse_Brain_Sagittal_Anterior_image.tif",
  image_name = "anterior1",
  spatial_dir = "/Users/stefano/Downloads/spatial",
  coordinate_transform = "x_neg_y_y_neg_x",
  coordinate_scale = "custom",
  scale_x = 2.0,
  scale_y = 2.0
)

wsi_viewer_seurat(brain, image = linked$image_path, linked = linked, open = TRUE)
```

## Selected-ROI StarDist Workflow

StarDist is optional and external. wsiTools should still install and open
without it. A typical workflow is:

```r
slide <- wsi_open("sample.svs")

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  open = TRUE,
  wait = FALSE
)

# Draw/select an ROI in the browser, then return to R:
roi <- viewer$get_selected_rois()

crop_file <- export_roi_crop(
  slide,
  roi = roi,
  file = "selected_roi_for_stardist.tif"
)

# Run external segmentation only if configured.
# Then import results and push them back into the viewer.
cells <- import_segmentation("stardist_output.geojson")
viewer$add_segmentation(cells)

wsi_close(slide)
```

## Project Save and Restore

```r
slide <- wsi_open("sample.svs")
viewer <- wsi_viewer_live(slide, wait = FALSE)

# Interact in the browser, then save state.
viewer$save_project("case_01.wsiproject")

# Later:
project <- wsi_read_project("case_01.wsiproject")
restore_project_state(viewer, project)

wsi_close(slide)
```

Project state is intended to store slide path, viewer viewport, annotations,
selected ROI IDs, trajectories, layers, measurements, segmentation, stain and
channel settings, tile source metadata, and provenance where available.

## Troubleshooting

### The viewer opens but the image is low resolution

Use `mode = "tiles"` and make sure libvips is available:

```r
wsi_backends()
wsi_has_vips()
```

Static thumbnail/project previews are intentionally downsampled. Full-resolution
zooming requires precomputed Deep Zoom tiles or a live dynamic tile server.

### The browser says stain selection needs canvas pixel access

Open the viewer through `http://127.0.0.1` or `http://localhost` instead of a
`file://` URL. Live viewers do this automatically.

### CZI opens slowly or shows only a backend message

Check the native CZI bridge and Bio-Formats helper:

```r
wsi_has_native_czi()
wsi_backends()
```

For first visualization, wsiTools prefers OpenSlide/libvips when they can read
the file, then native libCZI/libCZIAPI, then the Bio-Formats Java helper. The
legacy Python CZI path is disabled unless you explicitly opt in.

### StarDist button is disabled or reports not configured

StarDist is not mandatory. Configure it only when you need segmentation:

```r
wsi_install_stardist(install = FALSE)
```

Then install/configure the external command and rerun:

```r
wsi_has_stardist()
```

### Nothing comes back to R after drawing annotations

Use `wsi_viewer_live()` or a live wrapper such as
`wsi_viewer_cellphenotyper(..., live = TRUE)`. Static HTML viewers cannot
automatically update R objects.
