# wsiTools

wsiTools is an R toolkit for memory-efficient WSI access and preprocessing
through region-based reading, tiling, pyramidal image handling, conversion and
export.

Whole-slide images can be extremely large, so wsiTools is designed around
backend metadata, pyramid levels, explicit region reads, tile manifests, and
streaming conversion. It does not load complete level-0 slides into R memory by
default.

## Installation

### Quick install from GitHub

Start with the lightweight R package. This installs the core package only;
large WSI backends are checked at runtime and are not downloaded silently.

```r
install.packages("remotes", repos = "https://cloud.r-project.org")

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE
)

library(wsiTools)
packageVersion("wsiTools")
wsi_backends()
```

From a local checkout:

```r
install.packages(".", repos = NULL, type = "source")
```

The current GitHub package is designed to install without OpenSlide, libvips,
Python, StarDist, Cellpose, Bio-Formats, or OME-Zarr pixel readers. It also does
not require Rtools just to install on Windows. If future versions add optional
C++ acceleration, the README and setup checks will say so explicitly.

### Windows clean reinstall

If a previous installation was interrupted, remove the stale lock and retry:

```r
lib <- Sys.getenv("R_LIBS_USER")
unlink(file.path(lib, "00LOCK-wsiTools"), recursive = TRUE, force = TRUE)
unlink(file.path(lib, "wsiTools"), recursive = TRUE, force = TRUE)

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE
)
```

If R asks whether to update packages, choose `3: None` unless you deliberately
want to update your local package library. This avoids accidental long updates
on managed Windows workstations.

### Optional viewer packages

The package loads without these, but the live viewer and previews are more
useful when they are installed:

```r
install.packages(
  c("magick", "httpuv", "callr"),
  repos = "https://cloud.r-project.org"
)
```

Use `sf` only if you need polygon-aware spatial operations:

```r
install.packages("sf", repos = "https://cloud.r-project.org")
```

## System dependencies

wsiTools can be installed without WSI system libraries, but backend-dependent
functions need the corresponding tools at runtime:

- OpenSlide command-line tools for OpenSlide-backed metadata and region reads.
- libvips command-line tools (`vips`, `vipsheader`) for conversion, pyramids,
  thumbnails, cropping, and export.
- Bio-Formats command-line tools are planned for future microscopy format
  support.
- StarDist, Cellpose, or other segmentation engines remain external commands or
  services; wsiTools can export ROI crops and import their outputs without
  depending on Python.
- `httpuv`, `magick`, and `sf` are optional R packages used only for live viewer
  bridges, image previews, and polygon-aware ROI operations.
- `callr` is optional and is used only when non-blocking background jobs are
  requested.
- OME-Zarr metadata reading is lightweight; full chunked pixel decoding should
  remain an optional backend capability.

Check your local capabilities after installing:

```r
library(wsiTools)
wsi_backends()
```

New users can ask wsiTools for a setup plan. This is the recommended second
step after installation:

```r
plan <- wsi_setup()
plan
```

This reports missing optional R packages and system tools, and prints copyable
commands for Homebrew, apt, dnf, or conda when one is detected. It does not
install anything unless you explicitly opt in:

```r
# Review the plan first
wsi_setup()

# Show the install plan without running it
wsi_install_backends(install = FALSE)
```

### Automatic optional backend installer

`wsi_install_backends()` is the convenience helper for first-time setup. It can
install optional R packages such as `magick`, `httpuv`, and `callr`, and it can
run supported system package managers for tools such as libvips, OpenSlide, and
ImageMagick. These tools are optional runtime backends: wsiTools never installs
them silently during `install.packages()` or `remotes::install_github()`.

```r
library(wsiTools)

# Recommended: inspect what is missing first
wsi_setup()

# Let wsiTools choose a supported installer for this platform
wsi_install_backends(method = "auto")

# macOS with Homebrew
wsi_install_backends(method = "homebrew")

# Ubuntu/Debian. sudo is never used unless you opt in.
wsi_install_backends(method = "apt", allow_sudo = TRUE)

# Fedora
wsi_install_backends(method = "dnf", allow_sudo = TRUE)

# Windows pieces supported by winget
wsi_install_backends(
  tools = c("libvips", "imagemagick"),
  method = "winget"
)

# Conda or mamba environment
wsi_install_backends(
  tools = c("openslide", "libvips", "imagemagick"),
  method = "conda"
)
```

In interactive R sessions, wsiTools asks before running system commands. On
Debian/Ubuntu or Fedora, system installs may require `sudo`; wsiTools will not
run those commands unless you explicitly set `allow_sudo = TRUE`. Restart R or
RStudio after installing system tools, then check:

```r
wsi_backends()
```

OpenSlide on Windows is still best installed through conda-forge, MSYS2/vcpkg,
or the official binary distribution, then added to `PATH`. The winget helper is
useful for libvips and ImageMagick, but it does not currently install OpenSlide
for Windows.

Typical manual backend installs are:

```sh
# macOS
brew install vips openslide

# Ubuntu/Debian
sudo apt install libvips-tools openslide-tools

# Fedora
sudo dnf install vips-tools openslide-tools
```

On Windows, install libvips/OpenSlide with a system package manager such as
winget, MSYS2, vcpkg, or the official binary distributions, then make sure the
tool directories are on `PATH`. Re-run `wsi_backends()` afterwards.

StarDist is a Python/TensorFlow tool, so it is not installed silently during
`install.packages()` or `remotes::install_github()`. To enable the viewer's
`Run segmentation` button, run the explicit first-time setup helper:

```r
wsi_install_stardist(method = "conda")
```

This creates or updates a dedicated `wsitools-stardist` environment, writes a
small `stardist-predict2d` wrapper in the user wsiTools configuration
directory, and makes `wsi_has_stardist()` detect it automatically. Use
`wsi_install_stardist(install = FALSE)` to see the commands without running
them. The helper checks free disk space first because the conda StarDist package
can pull in a large TensorFlow stack.

Other Python tools such as Cellpose are best installed in a dedicated conda/pip
environment and then exposed to wsiTools through environment variables or
function arguments.

## First-run guided example

After installing from GitHub, create a small mock pathology project that opens
without OpenSlide, libvips, Python, or real WSI files:

```r
library(wsiTools)

example <- wsi_first_run_example(open = TRUE)
example$path
head(example$tile_grid)
```

The example directory contains a mock slide project, ROI GeoJSON, a labelled
mask converted to annotations, synthetic cell segmentation overlays, a tile
manifest, and a self-contained HTML viewer. You can also run the installed
script:

```r
source(system.file("examples/first-run-guided-example.R", package = "wsiTools"))
```

For a live viewer, inspect the capabilities of that specific session before
starting optional tools:

```r
slide <- wsi_open("sample.svs")
viewer <- wsi_viewer_live(slide, wait = FALSE)
viewer$capabilities()
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

## Multiple samples in one viewer

Use `wsi_viewer_project()` when a case contains several images, serial
sections, or multi-scene microscopy files such as CZI. The function writes a
single HTML viewer with a left-side **Project** panel for switching between
samples and scenes. Large source files remain on disk; the viewer embeds
downsampled previews so the full image is not loaded into R memory.
Annotations are stored separately for each project image/section, so ROIs drawn
on one tissue section do not appear on a different section.

```r
library(wsiTools)

samples <- c(
  "case_01_section_01.czi",
  "case_01_section_02.czi",
  "case_01_section_03.czi"
)

html <- wsi_viewer_project(
  samples,
  output = "case_01_project_viewer.html",
  width = 4096,
  overwrite = TRUE
)
html
```

For sharper preview zoom, increase `width`; for smaller HTML files, decrease
it. CZI previews are optional and require a Python executable with
`aicspylibczi`, `numpy`, and `Pillow`:

```r
Sys.setenv(WSITOOLS_CZI_PYTHON = "/path/to/python")
wsi_has_czi_python()
```

You can also add related images to a normal slide viewer:

```r
slide <- wsi_open("case_01_he.svs")

wsi_viewer(
  slide,
  mode = "tiles",
  output = "case_01_viewer.html",
  tile_dir = "case_01_viewer_tiles",
  project_images = c("case_01_ihc.czi", "case_01_serial_section.czi")
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
`Finish`, then use `Save GeoJSON`. In `Brush` mode, each normal stroke creates
a new automatically named annotation. If the painted area touches an existing
annotation with the same label, the regions merge; holding `Alt` on
Windows/Linux or `Command` on macOS while brushing removes from the selected
annotation. The compact ROI report does not open
automatically; select an ROI and click `ROI summary` when you want to inspect
area, cell count, and density. Use `Edit`
to move vertices, double-click an edge to insert a vertex, and Backspace/Delete
to remove the active vertex. Drawn, painted, and edited annotation regions are
kept class-exclusive: annotations with the same class label merge into one
multi-part annotation, while areas already occupied by a different class label
are clipped from the new or refined annotation. The brush refinement controls can smooth a
boundary, simplify it with a pixel tolerance, fill holes, merge checked
annotations from the left panel, and split a multi-part annotation into
separate ROIs. The Class Presets and History sections in the annotation panel
can be maximized with their `Maximize` buttons and restored with `Esc` or
`Restore`. The toolbar `Class`, `Custom class`, and `Set next class` controls
set the class for the next drawn or painted annotation; selecting an ROI does
not overwrite these controls, and changing them does not relabel the selected
annotation. Use the annotation manager controls to change an existing
annotation before GeoJSON export. Annotation colors
are category-driven: all ROIs with the same class label share the same class
color, including imported GeoJSON and newly painted annotations. `Ctrl+Z` undoes
annotation edits and `Ctrl+Shift+Z`/`Ctrl+Y` redoes them, with the last 10
committed states retained in each direction. In a static browser viewer this opens the
browser's normal save/download flow rather than silently writing to a server
path.

For a live R workflow, use `wsi_viewer_live()`. The ordinary static
`wsi_viewer()` writes an HTML file; it can export GeoJSON from the browser, but
it cannot automatically update R objects after the file is opened. The live
viewer starts an optional local `httpuv` bridge so browser-side annotations,
measurements, channel settings, and analyses are posted back to the current R
session automatically. WebSocket sync is used when available, with HTTP
POST/polling retained as a fallback:

```r
session <- wsi_viewer_live(
  slide,
  mode = "tiles",
  name = "viewer_state",
  wait = FALSE
)

# After drawing ROIs, measuring distances, importing GeoJSON, or adding
# segmentation overlays in the browser:
session$capabilities()
session$get_rois()
session$get_selected_roi()
session$get_selected_rois()
session$get_measurements()
session$get_roi_summary()
session$get_cell_summary()
session$get_class_summary()
session$get_ihc_summary()
session$get_ihc_class_summary()
session$get_segmentation()
session$get_layers()
session$get_channel_settings()

# After deconvolving a selected ROI or crop in R:
session$measure_ihc_intensity(patch_channels, dab_threshold = 0.1)

# Register R callbacks before interacting with the viewer:
session$on("roi_created", function(roi) {
  print(roi)
})

session$on("roi_selected", function(roi) {
  crop <- export_roi_crop(slide, roi, file = tempfile(fileext = ".png"))
})

session$on("segmentation_finished", function(cells) {
  print(summarise_rois(session$get_rois(), cells = cells))
})

# R can also push overlays back into the open viewer:
session$add_rois(read_geojson("qupath_annotations.geojson"))
session$add_segmentation(data.frame(cell_id = "cell_1", x = 1200, y = 900))
session$add_layer("tumour ROIs", session$get_rois())
session$add_layer("StarDist cells", session$get_segmentation())
session$add_layer("DAB intensity", matrix(runif(100), nrow = 10), opacity = 0.4)
session$set_layer_visible("StarDist cells", TRUE)

# Optional channel-tile layers can also be pushed from R:
dab_tiles <- wsi_channel_source(
  "DAB tiles",
  type = "deepzoom",
  tile_url_base = "dab_tiles/slide_files",
  width = slide$dimensions[["width"]],
  height = slide$dimensions[["height"]],
  max_level = 12,
  opacity = 0.5,
  colour = "#8b5a2b"
)
session$add_channel_source(dab_tiles)
session$set_channel_opacity("DAB tiles", 0.35)

# Save a reproducible project snapshot:
session$save_project("case_001.wsiproject")
```

In plain R sessions, call `session$service()` periodically, or start with
`wait = TRUE` for a blocking live loop that runs until you press Esc or Ctrl+C.
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

This writes browser-readable tiles next to the HTML file. Tiled mode uses
OpenSeadragon for image rendering, tile caching, smooth transitions, and
coordinate-stable overlays; zooming requests higher-resolution tiles instead of
magnifying a thumbnail.

OpenSeadragon is a browser tile viewer: it cannot read raw SVS, OME-TIFF, or
BTF pixels directly. wsiTools therefore provides two tile backends. Static
Deep Zoom tiles are precomputed with libvips and are fastest for shared HTML
viewers. The live viewer can alternatively serve tiles on demand from R without
precomputing a pyramid:

```r
session <- wsi_viewer_live(
  slide,
  mode = "tiles",
  dynamic_tiles = TRUE,
  dynamic_tile_format = "jpg",
  transport = "auto",
  wait = FALSE
)
```

The dynamic tile server exposes URLs of the form
`/tiles/{slide_id}/{level}/{x}/{y}.jpg`, caches generated tiles in a temporary
directory, and removes that cache when `wsi_viewer_stop(session)` is called.
Tiles are produced from region reads through the available libvips/OpenSlide
backend; the full WSI is not loaded into R memory.

### H&E plus mIHC channel overlays

Use `wsi_viewer_he_mihc()` to open an H&E WSI as the base tiled image and
overlay a matching mIHC OME-TIFF/probability image as independent channel
layers. Each mIHC page is served as a dynamic OpenSeadragon layer with
visibility, opacity, colour, gain, and contrast controls synced to the live R
session.

```r
viewer <- wsi_viewer_he_mihc(
  he = "AP-GY-26-04_HE.svs",
  mihc = "gigatime_probs.ome.tif",
  registration = "shift.json",
  dynamic_tiles = FALSE,
  tile_dir = "AP-GY-26-04_HE_deepzoom",
  transport = "auto",
  wait = FALSE
)

viewer$get_channel_settings()
```

For the smoothest interaction, precompute/reuse the H&E Deep Zoom tiles as
shown above. If no tile pyramid is available, set `dynamic_tiles = TRUE`;
`wsi_viewer_he_mihc()` then uses live JPEG base tiles by default as a fallback.

The optional `registration` JSON is used to place the mIHC crop in H&E
level-0 coordinates.

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
visibility controls, color pickers, opacity, gain, and contrast-window sliders.
In tiled mode the browser recolors only the visible OpenSeadragon tiles, so the
full WSI is not loaded into R memory.

For H&E patches, `wsiTools` can separate hematoxylin/eosin channels and
perform lightweight stain normalisation. Macenko-style estimation and an
experimental Vahadane-style NMF estimator are implemented in pure R for
already-small regions, tiles, or thumbnails:

```r
patch <- wsi_read_region(
  slide,
  x = 10000,
  y = 20000,
  width = 1024,
  height = 1024
)

he_channels <- wsi_deconvolve_he(patch)
stain_matrix <- wsi_estimate_stain_matrix(patch, method = "macenko")

normalized <- wsi_normalize_stains(
  patch,
  method = "macenko",
  target_matrix = wsi_he_stain_matrix()
)
```

For WSI workflows, normalise only the requested region or apply the function
inside a tile loop:

```r
normalized_region <- wsi_stain_normalize_region(
  slide,
  x = 10000,
  y = 20000,
  width = 1024,
  height = 1024,
  method = "macenko"
)
```

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

The `Stains` menu exposes the `mIHC` toggle plus visibility, colour, opacity,
gain, and contrast controls for each channel. The default vectors are only
starting values; use assay-specific stain vectors for quantitative work.

## Conversion example

```r
wsi_convert(
  input = "sample.svs",
  output = "sample.ome.tiff",
  format = "ome-tiff",
  pyramid = TRUE,
  compression = "lzw",
  tile_size = 512,
  depth = "onepixel",
  region_shrink = "mean",
  predictor = "horizontal"
)

wsi_export_ome_tiff(
  input = "sample.svs",
  output = "sample.ome.tiff",
  compression = "lzw",
  overwrite = TRUE
)
```

OME-TIFF export uses libvips when available and writes tiled pyramids with
SubIFD pyramid layers and TIFF metadata properties for OME-oriented workflows.
The exact codec support depends on the installed libvips/libtiff build.

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

Annotation masks can be imported as ROI polygons too. This is intended for
already-small annotation or segmentation masks, not for loading a full WSI into
R memory:

```r
mask_rois <- read_mask_annotations(
  "tumour_mask.png",
  threshold = 0.5,
  class_map = c("1" = "tumour"),
  origin = c(x = 0, y = 0),
  scale = c(x = 1, y = 1)
)

wsi_viewer_roi(slide, mask_rois, output = "tumour_mask_viewer.html", open = FALSE)
# In a live session, use: session$add_rois(mask_rois)
write_geojson(mask_rois, "tumour_mask_annotations.geojson", overwrite = TRUE)
```

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
first implementation reads dimensions, axes, chunks, dtype, transforms, levels,
and NGFF metadata without decoding full image chunks:

```r
zarr <- open_omezarr("sample.ome.zarr")
meta <- omezarr_metadata("sample.ome.zarr")
meta$axis_table
meta$levels
```

ROI annotations can be imported from QuPath-compatible GeoJSON, labelled,
written back to GeoJSON, and overlaid in the viewer:

```r
rois <- read_geojson("annotations.geojson")
rois <- wsi_set_roi_class(rois, "tumour", roi_id = rois$roi_id[1])
write_geojson(rois, "annotations_relabelled.geojson", overwrite = TRUE)

viewer_add_rois(slide, rois, output = "slide_rois.html")
```

The GeoJSON reader/writer preserves QuPath polygon and multipolygon
annotations, including object ids, object type, classification names and
colors, measurements, lock state, raw properties, and top-level GeoJSON
metadata where possible.

For a live viewer round trip, keep the annotations as `wsi_roi` objects:

```r
rois <- read_geojson("qupath_annotations.geojson")
viewer$add_rois(rois)

# After visual edits in the viewer:
edited <- viewer$get_rois()
write_geojson(edited, "edited_for_qupath.geojson", overwrite = TRUE)
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

# Convert a model mask image into editable ROI annotations:
mask_rois <- import_segmentation("model_mask.png", mask_as_rois = TRUE, threshold = 0.5)
viewer$add_rois(mask_rois)
```

For StarDist, `wsiTools` keeps Python optional but provides a first-run setup
helper:

```r
wsi_install_stardist(method = "conda")
```

After that, `wsi_viewer_live(..., stardist = TRUE)` can wire the viewer's
`Run segmentation` button automatically. In the interactive viewer, select or
brush an ROI, open the Segmentation menu, run StarDist on the ROI crop, then
load or receive the result GeoJSON or CSV/TSV centroids back into the viewer.
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
the viewer with `stardist = TRUE`; when a StarDist command is available,
wsiTools starts the local R endpoint and wires the `Run segmentation` button
automatically:

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
`Run segmentation`. The selected ROI is sent to the local endpoint, StarDist
runs on only that ROI crop, and returned cell polygons or centroids are added
as overlays. The selected ROI and imported cells are also written directly into
the live R session, so the normal R-side result is:

```r
cells <- session$get_segmentation()
selected_roi <- session$get_selected_roi()
selected_rois <- session$get_selected_rois()
roi_summary <- session$get_roi_summary()
cell_summary <- session$get_cell_summary()
```

The last run metadata, including crop/output paths, is available in
`wsi_viewer_state(session)$last_segmentation` and the companion object
`wsi_viewer_live_state_last_segmentation`. If `stardist-predict2d` is already
on `PATH`, you can omit `stardist_command` and `stardist_args`. If no StarDist
command is available, the viewer still opens and the Segmentation menu remains
usable for selected-ROI export and result import.

Long-running work can also run as a non-blocking background job when the
optional `callr` package is installed:

```r
# Preview candidate tiles in the open viewer without reading pixel data.
# Inspect the locked grid overlay, then export exactly that preview.
preview <- session$preview_tiles(
  tile_size = 512,
  stride = 512
)

tiles <- session$extract_tile_preview(
  output_dir = "confirmed_tiles",
  format = "png"
)

job <- session$run_segmentation_async(
  command = "python",
  args = c("run_stardist.py", "{input}", "{output}", "{model}")
)

job$status()
cells <- job$result()

tile_job <- session$run_tiles_async(
  tile_size = 512,
  stride = 512,
  save_images = FALSE
)
tiles <- tile_job$result()

convert_job <- session$run_conversion_async(
  output = "sample.ome.tiff",
  format = "ome-tiff",
  compression = "lzw",
  overwrite = TRUE
)
convert_job$status()
```

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

Live viewer selections can be used directly for reproducible ROI tile
extraction:

```r
rois <- session$get_selected_rois()

tiles <- extract_tiles(
  slide,
  roi = rois,
  tile_size = 512,
  stride = 512,
  skip_background = TRUE,
  split = c(train = 0.7, valid = 0.3),
  seed = 1,
  save_images = FALSE
)
```

For ML pipelines, ROI-aware sampling can keep tile provenance, balance classes,
skip background, write a CSV manifest, and create a reproducible
train/validation split:

```r
manifest <- extract_tiles(
  slide,
  roi = rois,
  output_dir = "ml_tiles",
  tile_size = 512,
  stride = 512,
  sampling = "balanced",
  tiles_per_class = 500,
  skip_background = TRUE,
  tissue_threshold = 0.2,
  split = "train_validation",
  train_fraction = 0.8,
  seed = 2026,
  manifest_file = "ml_tiles/manifest.csv"
)
```

Artifact filtering is optional and tile-based. It reads candidate regions one at
a time, computes lightweight quality-control metrics, and can either flag or
drop tiles with blur, out-of-focus appearance, pen-like marks, fold-like dark
saturated regions, bubble-like bright regions, or very bright/dark content:

```r
manifest <- extract_tiles(
  slide,
  roi = rois,
  tile_size = 512,
  stride = 512,
  skip_background = TRUE,
  whitespace_filter = TRUE,
  artifact_filter = TRUE,
  artifact_action = "flag",
  save_images = FALSE
)

clean_tiles <- subset(manifest, !artifact_flag & !whitespace_flag)
```

The first QC step should usually be tissue/background detection from a
low-resolution thumbnail. `wsi_tissue_mask()` uses HSV saturation and brightness
thresholds, never reads the full WSI into memory, and returns the logical mask
plus tissue percentage, approximate tissue area, a whole-tissue bounding box,
and connected-component bounding boxes:

```r
tissue <- wsi_tissue_mask(
  slide,
  thumbnail_width = 2048,
  saturation_threshold = 0.05,
  brightness_threshold = 0.8
)

tissue$tissue_percentage
tissue$tissue_area
tissue$component_bboxes
```

Pen and ink marks can be screened with a second thumbnail- or tile-level pass.
`wsi_detect_pen_marks()` uses transparent RGB dominance rules for blue, green,
and red pen/marker pixels, plus a dark edge-rich component rule for black ink.
If a tissue mask is supplied, or estimated from the same small image, the result
reports the percentage of tissue affected:

```r
thumb <- wsi_thumbnail(slide, width = 2048, format = "array")
tissue <- wsi_detect_tissue(thumb)
ink <- wsi_detect_pen_marks(thumb, tissue_mask = tissue)

ink$pen_percentage
ink$tissue_affected_percentage
ink$component_bboxes
```

Blur and out-of-focus regions can be scored with the variance of the
Laplacian. `wsi_detect_blur()` works on one tile, thumbnail, or small region,
while `wsi_focus_heatmap()` reads a tile grid one region at a time and returns
both a numeric focus heatmap and a blurry-tile mask:

```r
focus_tile <- wsi_detect_blur(patch, threshold = 0.001)

focus <- wsi_focus_heatmap(
  slide,
  tile_size = 512,
  threshold = 0.001,
  tissue_mask = tissue
)

focus$slide_focus_score
focus$blurry_tile_fraction
focus$heatmap
```

Basic stain-quality screening uses tissue-region colour statistics:
saturation, brightness, mean RGB, and RGB optical density. `wsi_detect_stain_quality()`
returns a staining score plus low-stain and over-stain masks. The slide-level
helper reads one tile at a time and returns heatmaps suitable for QC review:

```r
stain_tile <- wsi_detect_stain_quality(patch, tissue_mask = patch_tissue)

stain <- wsi_stain_quality_heatmap(
  slide,
  tile_size = 512,
  tissue_mask = tissue,
  low_saturation_threshold = 0.08,
  high_brightness_threshold = 0.88
)

stain$slide_staining_score
stain$low_stain_tile_fraction
stain$over_stain_tile_fraction
stain$stain_score_heatmap
```

Tissue fold screening is also available as candidate detection, not a
definitive classifier. `wsi_detect_fold_candidates()` combines high optical
density, high saturation, low brightness, local edge content, and connected
component filtering. `wsi_fold_candidate_heatmap()` applies the same rule
tile-by-tile for slide QC:

```r
fold_tile <- wsi_detect_fold_candidates(patch, tissue_mask = patch_tissue)

folds <- wsi_fold_candidate_heatmap(
  slide,
  tile_size = 512,
  tissue_mask = tissue,
  high_od_threshold = 1.2,
  saturation_threshold = 0.25,
  brightness_threshold = 0.45
)

folds$fold_candidate_tile_fraction
folds$fold_fraction_heatmap
folds$tiles[, c("tile_id", "fold_candidate", "fold_fraction")]
```

Air bubbles can be screened as a second-stage candidate detector. The rule is
deliberately conservative: bright low-saturation centres must also have a
sharp surrounding edge/ring and approximately round connected-component
geometry. This helps avoid treating ordinary white background as a bubble:

```r
bubble_tile <- wsi_detect_bubble_candidates(patch, tissue_mask = patch_tissue)

bubbles <- wsi_bubble_candidate_heatmap(
  slide,
  tile_size = 512,
  tissue_mask = tissue,
  brightness_threshold = 0.85,
  saturation_threshold = 0.18,
  min_ring_contrast = 0.05
)

bubbles$bubble_candidate_tile_fraction
bubbles$bubble_fraction_heatmap
bubbles$tiles[, c("tile_id", "bubble_candidate", "bubble_fraction")]
```

Dust and debris screening focuses on small, dark, sharply defined connected
components. `wsi_detect_dust_candidates()` reports an object mask plus
per-object summaries, and when a tissue mask is provided it separates objects
on tissue, background, and tissue edges:

```r
dust_tile <- wsi_detect_dust_candidates(
  patch,
  tissue_mask = patch_tissue,
  max_area = 200,
  max_diameter = 40
)

dust <- wsi_dust_candidate_heatmap(
  slide,
  tile_size = 512,
  tissue_mask = tissue,
  brightness_threshold = 0.30,
  min_contrast = 0.08
)

dust$dust_candidate_tile_fraction
dust$dust_fraction_heatmap
dust$tiles[, c("tile_id", "dust_candidate", "dust_object_count")]
```

The interactive viewer also includes an **Artifacts** menu for quick
viewport-level screening. It can inspect the currently rendered view for
obvious blur, pen-like marks, folds, bubbles, or very bright/dark content and
optionally create an `artefact` ROI. This is intended for manual QC while
viewing; reproducible ML tile export should still use `artifact_filter = TRUE`
in the tile manifest workflow.

Whitespace/background labelling is separate from artifact detection. It adds
manifest columns such as `whitespace_fraction`, `whitespace_flag`,
`background_fraction`, and `background_flag` using a simple bright,
low-saturation detector:

```r
manifest <- extract_tiles(
  slide,
  tile_size = 512,
  stride = 512,
  whitespace_filter = TRUE,
  whitespace_action = "flag",
  save_images = FALSE
)
```

Basic measurement and registration helpers cover manual pathology analysis
workflows:

```r
cells <- data.frame(x = c(1000, 1200, 3000), y = c(800, 900, 1800))
measure_cell_density(cells, rois, pixel_size = wsi_mpp(slide))
summarise_rois(rois, cells = cells, pixel_size = wsi_mpp(slide), file = "roi_summary.csv")

patch_channels <- wsi_deconvolve_region(
  slide,
  x = 10000,
  y = 20000,
  width = 2048,
  height = 2048
)

ihc_roi <- measure_ihc_intensity(
  patch_channels,
  rois = rois,
  image_origin = c(x = 10000, y = 20000),
  dab_threshold = 0.1,
  pixel_size = wsi_mpp(slide)
)
ihc_class <- summarise_ihc_intensity(ihc_roi)

cell_counts <- wsi_cell_counts(
  slide,
  segmentation = cells,
  channels = patch_channels,
  rois = rois,
  image_origin = c(x = 10000, y = 20000),
  positive_threshold = c(hrp_dab = 0.1),
  pixel_size = wsi_mpp(slide),
  output_dir = "cell_counts",
  overwrite = TRUE
)

report <- measurement_report(
  rois,
  cells = cells,
  stains = patch_channels,
  image_origin = c(x = 10000, y = 20000),
  pixel_size = wsi_mpp(slide),
  output_dir = "measurement_report",
  prefix = "case_001"
)

transform <- estimate_transform(landmarks1, landmarks2)
transformed_rois <- transform_rois(rois, transform)
```

The report writes separate CSV tables for ROI area/density, class summaries,
nearest-neighbour distances, cell-to-boundary distances, and hematoxylin/HRP-DAB
intensity summaries when stain channels are supplied. `measure_ihc_intensity()`
adds practical IHC columns such as `ihc_dab_mean`,
`ihc_dab_positive_area_px2`, `ihc_hematoxylin_density`, and
`ihc_dab_h_ratio`. `wsi_cell_counts()` creates a per-cell table, a cell-by-
channel counts matrix, ROI counts, class counts, and optional CSV exports from
segmentation coordinates plus already-read stain/channel matrices.

Save the analysis as a reopenable wsiTools project:

```r
project <- wsi_project(slide)
project$viewer_state <- session$get_state()
project$rois <- session$get_rois()
project$measurements <- session$get_measurements()
project$segmentation <- session$get_segmentation()
project$stain_settings <- list(method = "H-DAB", channels = c("hematoxylin", "DAB"))
project$tile_manifest <- manifest
project$metadata <- list(case_id = "case_001")
project$processing_provenance <- list(
  steps = list(
    list(name = "manual annotation", tool = "wsiTools viewer"),
    list(name = "tile extraction", tile_size = 512, stride = 512)
  )
)

wsi_save_project(project, "case_001.wsiproject")
reopened <- wsi_read_project("case_001.wsiproject")
reopened$rois
reopened$tile_manifest

case_report <- wsi_case_report(
  reopened,
  output_dir = "case_001_report",
  overwrite = TRUE
)
case_report$html
case_report$files
```

The project directory contains a `project.json` index plus sidecar GeoJSON and
CSV files, so analyses can be versioned, inspected, and reopened without
embedding image pixels. `wsi_case_report()` adds a lightweight HTML report and
CSV tables for ROI areas, class percentages, cell densities, stain intensity
summaries, tile counts, and processing provenance.

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

- optional OpenSlide native bindings outside the CRAN-light core, if needed
- OME-Zarr chunk decoding for true tiled pixel display
- associated image reads
- richer ROI geometry filtering and polygon masking
- parallel tile extraction
- advanced tissue segmentation
