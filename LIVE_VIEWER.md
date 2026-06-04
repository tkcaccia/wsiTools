# wsiTools Live Viewer Guide

This guide explains how to run the wsiTools viewer in live mode, how the
browser communicates with R, and how to avoid the common synchronization
mistakes that happen on remote computers.

Use this page when you want annotations, measurements, selected spots,
prediction results, proximity results, tile previews, or project state created
in the browser to come back automatically to R.

## The Main Idea

wsiTools has two viewer styles:

| Viewer | Function | What It Does | R Synchronization |
| --- | --- | --- | --- |
| Static viewer | `wsi_viewer()` | Writes an HTML file for viewing images and annotations. | No automatic feedback to R after the file is opened. |
| Live viewer | `wsi_viewer_live()` | Writes an HTML file and starts a local R web service using `httpuv`. | Yes. Browser events are sent back to the active R session. |

The live viewer has two parts:

1. A viewer HTML page, for example:

```text
sample_live_viewer.html
```

2. A local R synchronization endpoint, for example:

```text
http://127.0.0.1:8788/viewer-state
```

Open the HTML viewer page in the browser. Do not open `/viewer-state` as the
main page. `/viewer-state` is only a JSON endpoint used internally by the
viewer.

## Minimal Live Viewer

Run this from R or RStudio on the same computer that will open the browser:

```r
library(wsiTools)

slide <- wsi_open("sample.svs")

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  output = "sample_live_viewer.html",
  open = TRUE,
  wait = FALSE
)

viewer$get_rois()
viewer$get_measurements()
viewer$get_selected_roi()
```

Keep the R session alive while the browser is open. If R is closed, the HTML may
still display static content, but live synchronization stops.

## RStudio Workflow

In RStudio, use `wait = FALSE`. This returns a live viewer session object so you
can keep using the console.

```r
library(wsiTools)

slide <- wsi_open("sample.svs")

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  output = "sample_live_viewer.html",
  open = TRUE,
  wait = FALSE,
  name = "case_001_viewer"
)

# After drawing or editing in the browser:
rois <- viewer$get_rois()
measurements <- viewer$get_measurements()
annotation_spots <- viewer$get_annotation_spots()
```

Useful live methods include:

```r
viewer$get_state()
viewer$get_rois()
viewer$get_selected_roi()
viewer$get_selected_rois()
viewer$get_measurements()
viewer$get_annotation_spots()
viewer$get_prediction()
viewer$get_proximity()
viewer$get_channel_settings()
viewer$add_rois(read_geojson("annotations.geojson"))
viewer$add_layer("my spots", spots)
viewer$run_segmentation_async(engine = "stardist_he")
viewer$save_project("case_001.wsiproject")
viewer$capabilities()
```

## Optional Selected-ROI Cell Segmentation

The live viewer can expose a top `Cells` menu for selected-ROI segmentation.
StarDist and Mesmer remain external optional tools; wsiTools crops only the
selected ROI and imports the resulting GeoJSON, CSV/TSV, or mask output.

```r
viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  dynamic_tiles = TRUE,
  stardist = TRUE,
  stardist_args = c(
    "--input", "{input}",
    "--output", "{output}",
    "--model", "{model}",
    "--tiles", "{tiles_y}", "{tiles_x}"
  ),
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32,
  wait = FALSE
)
```

In the browser, draw/select one ROI and use `Cells > Run selected ROI`. From R,
the same workflow is:

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
```

The `Cells` menu can also load existing cell GeoJSON, CSV/TSV centroids, or
PNG/JPEG/WebP masks. For TIFF masks, use R:

```r
cells <- import_segmentation("cell_mask.tif", mask_as_rois = TRUE, threshold = 0.5)
viewer$add_segmentation(cells)
```

## Rscript Workflow

When running from a terminal with `Rscript`, use `wait = TRUE`. This keeps the
R process alive and services browser events.

```r
library(wsiTools)

slide <- wsi_open("sample.svs")

viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  output = "sample_live_viewer.html",
  open = TRUE,
  wait = TRUE,
  name = "case_001_viewer"
)
```

Run it with:

```bash
Rscript open_live_viewer.R
```

Do not close that terminal while using the live viewer.

## Static Tiles Versus Dynamic Tiles

OpenSeadragon is a browser tile viewer. It does not read raw SVS, OME-TIFF, BTF,
CZI, or NDPI files directly. wsiTools gives the browser tiles in one of two
ways.

### Fast Shared Viewer: Prebuilt Deep Zoom Tiles

This is the fastest option for repeated viewing.

```r
viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  output = "sample_live_viewer.html",
  tile_dir = "sample_live_viewer_tiles",
  dynamic_tiles = FALSE,
  open = TRUE,
  wait = FALSE
)
```

The first run may create the tile pyramid. Later runs reuse it unless you ask to
rebuild.

### On-Demand Viewer: Dynamic Tiles

Use this when you do not want to precompute tiles.

```r
viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  dynamic_tiles = TRUE,
  dynamic_tile_format = "jpg",
  open = TRUE,
  wait = FALSE
)
```

Dynamic tiles are generated only for requested regions and cached in a temporary
directory. They are removed when the viewer session is stopped.

## The Correct URL To Open

When the live viewer starts, R prints messages similar to:

```text
wsiTools live viewer written to /path/to/sample_live_viewer.html
wsiTools live viewer sync listening at http://127.0.0.1:8788/viewer-state
WebSocket sync available at ws://127.0.0.1:8788/viewer-state with HTTP polling fallback.
```

Open the viewer HTML:

```text
file:///path/to/sample_live_viewer.html
```

or, if you serve the folder through a local HTTP server:

```text
http://127.0.0.1:8795/sample_live_viewer.html
```

Do not use this as the viewer page:

```text
http://127.0.0.1:8788/viewer-state
```

That endpoint should return JSON such as:

```json
{"ok":true,"roi_count":0,"measurement_count":0}
```

Seeing JSON means the R bridge is alive; it does not mean you opened the viewer.

## Remote Computer Rule

`127.0.0.1` always means "this same computer".

If R is running on a remote Ubuntu workstation, and Firefox is also running on
that same remote workstation, open the viewer in Firefox on the remote
workstation. No SSH tunnel is needed.

If R is running on a remote workstation but the browser is running on your Mac
or Windows laptop, then `127.0.0.1` in your laptop browser points to your laptop,
not to the remote R session. You need an SSH tunnel.

## Remote Ubuntu With Firefox

This is the simplest remote workflow when the user is operating the remote
desktop directly.

On the remote Ubuntu computer:

```bash
Rscript /path/to/open_live_viewer.R
```

Keep that terminal open.

In Firefox on the same Ubuntu desktop, open the generated viewer HTML. For
example:

```text
file:///mnt/sata_ssd/benchmarks/fourslide_spatialexperiments/save/spatialexperiment_four_slide_live_viewer.html
```

If the viewer files are served through a local HTTP server on the remote
machine, open:

```text
http://127.0.0.1:8795/spatialexperiment_four_slide_live_viewer.html
```

Do not open:

```text
http://127.0.0.1:8797/viewer-state
```

That is only the synchronization endpoint.

## Remote R, Local Browser With SSH Tunnel

Use this when R is running on a remote computer but the browser is on your local
Mac or Windows machine.

Suppose the remote live viewer reports:

```text
viewer HTML server: http://127.0.0.1:8795/viewer.html
sync server:        http://127.0.0.1:8797/viewer-state
```

From your local computer, create a tunnel:

```bash
ssh -L 8795:127.0.0.1:8795 -L 8797:127.0.0.1:8797 user@remote-host
```

Then open locally:

```text
http://127.0.0.1:8795/viewer.html
```

The HTML page will connect to:

```text
http://127.0.0.1:8797/viewer-state
```

through the tunnel.

If the live viewer chooses another port because the requested port is busy,
tunnel the actual port printed by R. For example, if R prints port `8799`, then
tunnel `8799`, not the original requested port.

## Checking Whether Sync Is Alive

From the same machine where the browser is running, test:

```bash
curl http://127.0.0.1:8797/viewer-state
```

A healthy response looks like:

```json
{"ok":true,"event":null,"roi_count":0,"measurement_count":0}
```

If `curl` fails:

- The R session may not be running.
- The viewer may have moved to a different port.
- You may be testing from the wrong computer.
- The SSH tunnel may be missing or stopped.

## Finding The Actual Live Port

The requested port may not always be the port used. If port `8788` is already
busy, wsiTools tries another port.

Look at the R startup message:

```text
wsiTools live viewer sync listening at http://127.0.0.1:8797/viewer-state
```

The correct live sync port is `8797`.

On Linux, you can also inspect listening ports:

```bash
ss -ltnp | grep 879
```

On macOS:

```bash
lsof -nP -iTCP -sTCP:LISTEN | grep 879
```

## Four-Slide SpatialExperiment Live Example

This example is useful for Visium/SpatialExperiment benchmarks with one image
per tissue section.

```r
library(wsiTools)

base_dir <- "/mnt/sata_ssd/benchmarks/fourslide_spatialexperiments"
object_file <- file.path(base_dir, "obejct.RData")
save_dir <- file.path(base_dir, "save")
dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

load(object_file) # expected object: spe_sub

images <- c(
  `151507` = file.path(base_dir, "151507_full_image.tif"),
  `151508` = file.path(base_dir, "151508_full_image.tif"),
  `151509` = file.path(base_dir, "151509_full_image.tif"),
  `151510` = file.path(base_dir, "151510_full_image.tif")
)

linked <- wsiTools:::wsi_link_spatialexperiment_project_sections(
  spe = spe_sub,
  images = images,
  sample_ids = names(images),
  image_names = names(images),
  reduction = "PCA",
  coordinate_scale = "none",
  spot_radius = NULL
)

labels <- paste("SpatialExperiment", names(images))
names(linked) <- labels

output <- file.path(save_dir, "spatialexperiment_four_slide_live_viewer.html")
tile_dir <- file.path(save_dir, "spatialexperiment_four_slide_tiles")

records <- wsiTools:::wsi_seurat_project_records(
  linked = linked,
  output = output,
  labels = labels,
  mode = "tiles",
  tile_dir = tile_dir,
  tile_size = 512,
  tile_format = "jpg",
  quality = 90,
  rebuild = FALSE,
  tile_overlap = 1L
)

first <- linked[[1]]
first_record <- records[[1]]
first_layer <- wsiTools:::wsi_seurat_spots_layer(first)
first_layer$project_scoped <- TRUE

viewer <- wsi_viewer_live(
  slide = first$slide,
  name = "spatialexperiment_four_slide_live",
  port = 8794,
  transport = "auto",
  dynamic_tiles = FALSE,
  output = output,
  overwrite = TRUE,
  open = TRUE,
  wait = TRUE,
  mode = "tiles",
  tile_size = first_record$tile_size,
  tile_format = first_record$tile_format,
  tile_url_base = first_record$tile_url_base,
  tile_url_style = first_record$tile_url_style,
  tile_overlap = first_record$tile_overlap,
  max_level = first_record$max_level,
  project_images = records,
  layers = list(first_layer),
  seurat = first,
  prediction_context = wsi_prediction_context(spatial = first),
  proximity_context = wsi_proximity_context(spatial = first)
)
```

For the Chiamaka benchmark, the saved script is:

```bash
Rscript /mnt/sata_ssd/benchmarks/fourslide_spatialexperiments/open_spatialexperiment_four_slide_live.R
```

If using Firefox directly on Chiamaka Ubuntu, open:

```text
http://127.0.0.1:8795/spatialexperiment_four_slide_live_viewer.html
```

If using a browser on another computer, tunnel the viewer file server and the
actual live sync port printed by R.

## Seurat, Giotto, And SpatialExperiment Live Viewers

Spatial object viewers use the same live bridge.

```r
viewer <- wsi_viewer_seurat(
  brain,
  image = "tissue.tif",
  live = TRUE,
  mode = "tiles",
  open = TRUE
)
```

```r
viewer <- wsi_viewer_giotto(
  giotto_object,
  image = "tissue.tif",
  live = TRUE,
  mode = "tiles",
  open = TRUE
)
```

```r
viewer <- wsi_viewer_spatialexperiment(
  spe,
  image = "tissue.tif",
  live = TRUE,
  mode = "tiles",
  open = TRUE
)
```

In live mode, gene values are fetched from R one gene at a time. The full
expression matrix is not embedded in the browser. This keeps the viewer lighter
and allows the user to type a gene name in the top spatial-object menu.

## What Comes Back To R

The live session keeps synchronized copies of:

- annotations and ROIs;
- selected ROI IDs;
- annotation-to-spot associations;
- distance and area measurements;
- cell or spot selections;
- prediction overlays;
- proximity analysis results;
- visible layers;
- channel and stain settings;
- project state and autosave state;
- viewer history where available.

Typical retrieval code:

```r
rois <- viewer$get_rois()
spots_in_rois <- viewer$get_annotation_spots()
measurements <- viewer$get_measurements()
prediction <- viewer$get_prediction()
proximity <- viewer$get_proximity()

write_geojson(rois, "edited_rois.geojson", overwrite = TRUE)
write.csv(spots_in_rois, "annotation_spots.csv", row.names = FALSE)
```

## Autosave

Use `autosave_path` to save project state while the viewer is open:

```r
viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  autosave = TRUE,
  autosave_path = "case_001.wsiproject",
  autosave_interval = 5,
  wait = FALSE
)
```

The project can include slide path, viewport, ROIs, measurements, trajectories,
layers, segmentation/cell overlays, channel settings, and processing state.

## Troubleshooting

### I see JSON instead of the image viewer

You opened `/viewer-state`. Open the HTML file or HTTP-served viewer page
instead.

Wrong:

```text
http://127.0.0.1:8797/viewer-state
```

Right:

```text
http://127.0.0.1:8795/spatialexperiment_four_slide_live_viewer.html
```

### The viewer opens but says synchronization failed

Check the live endpoint from the same machine as the browser:

```bash
curl http://127.0.0.1:8797/viewer-state
```

If the browser is local and R is remote, create or repair the SSH tunnel.

### The port changed

Use the port printed by R:

```text
wsiTools live viewer sync listening at http://127.0.0.1:8797/viewer-state
```

Do not assume the originally requested port was used.

### Firefox on remote Ubuntu cannot sync

Make sure both are true:

- the R terminal is still running on that Ubuntu computer;
- Firefox opened the viewer page, not `/viewer-state`.

If the viewer was opened from a file URL and the browser blocks local access,
serve the folder:

```bash
cd /path/to/viewer/folder
python3 -m http.server 8795 --bind 127.0.0.1
```

Then open:

```text
http://127.0.0.1:8795/viewer.html
```

### The image loads but zoom is low resolution

Use tiled mode and libvips-generated Deep Zoom tiles:

```r
viewer <- wsi_viewer_live(
  slide,
  mode = "tiles",
  tile_dir = "viewer_tiles",
  dynamic_tiles = FALSE
)
```

Thumbnail mode is fast but not full-resolution.

### The viewer is slow

Prefer prebuilt Deep Zoom tiles for repeated use. Dynamic tiles are convenient
but can be slower because R must generate requested tiles on demand.

### Gene lookup does not work

Use a live spatial viewer:

```r
wsi_viewer_seurat(..., live = TRUE)
wsi_viewer_giotto(..., live = TRUE)
wsi_viewer_spatialexperiment(..., live = TRUE)
```

The original R object must stay available in the active R session.

### Prediction menu is unavailable

Prediction needs a live spatial or CellPhenotyper viewer and the optional
`fastPLS` package:

```r
remotes::install_github("tkcaccia/fastPLS")
```

Then reopen the live viewer.

### Cells run button says StarDist or Mesmer is not configured

Install or configure the external model command only when you need live
selected-ROI segmentation:

```r
wsi_install_stardist(install = FALSE)
wsi_install_stardist(method = "conda")

# Or point to an existing wrapper:
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist_wrapper")
Sys.setenv(WSITOOLS_MESMER_COMMAND = "/path/to/mesmer_wrapper")

wsi_cell_segmentation_engines()
```

For low RAM use, keep the workflow ROI-aware and pass tiled model arguments,
for example `{tiles_y}` and `{tiles_x}` set to 32.

## Checklist

Before reporting a live sync problem, check:

1. Did you open the viewer HTML page, not `/viewer-state`?
2. Is the R terminal or RStudio session still running?
3. Does `curl http://127.0.0.1:<port>/viewer-state` return `{"ok":true,...}`?
4. Are the browser and R running on the same computer?
5. If not, did you create an SSH tunnel for the actual printed sync port?
6. Did the viewer choose a different port because the requested port was busy?
7. Are you using tiled mode for full-resolution zoom?
8. Are optional packages such as `httpuv` and `magick` installed?
