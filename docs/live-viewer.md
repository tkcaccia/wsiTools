# Live Viewer Guide

The live viewer is the mode to use when the browser and R need to stay
connected. Use it when annotations, selected ROIs, measurements, segmentation
results, spots, trajectories, or project state created in the browser should
come back automatically to the active R session.

## Minimal Live Viewer

```r
library(wsiTools)

slide <- wsi_open("/path/to/image.svs")

viewer <- wsi_viewer_live(slide)
viewer$open()
```

After drawing or editing in the browser, retrieve results from R:

```r
viewer$get_rois()
viewer$get_selected_roi()
viewer$get_measurements()
viewer$get_segmentation()
```

Keep the R session alive while the browser is open. If R stops, the browser may
still show the last static page, but live synchronization stops.

## Use Tiled Mode For Full Resolution

For whole-slide images, use tiled OpenSeadragon viewing:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE
)

viewer$open()
```

The browser cannot read raw SVS, NDPI, CZI, BTF, or OME-TIFF pixels directly.
It needs image tiles. wsiTools can use prebuilt Deep Zoom tiles or a dynamic
tile server, depending on the slide and available backends.

## What Live Synchronization Means

`wsi_viewer_live()` starts a local `httpuv` web service from R. The browser
sends validated viewer events back to R, such as:

- ROI created, updated, selected, or deleted
- brush stroke committed
- measurement created
- trajectory edited
- viewport changed
- layer or channel settings changed
- segmentation result loaded
- project state updated

Useful session methods include:

```r
viewer$get_state()
viewer$get_rois()
viewer$get_selected_roi()
viewer$get_selected_rois()
viewer$get_measurements()
viewer$get_segmentation()
viewer$get_channel_settings()
viewer$get_annotation_spots()
viewer$add_rois(read_geojson("annotations.geojson"))
viewer$add_segmentation(import_segmentation("cells.geojson"))
viewer$save_project("case_01.wsiproject")
viewer$capabilities()
```

## Static Viewer Versus Live Viewer

| Viewer | Function | Opens As | Automatic Feedback To R |
|---|---|---|---|
| Static viewer | `wsi_viewer()` | usually `file://...html` | No |
| Live viewer | `wsi_viewer_live()` | `http://127.0.0.1:<port>/...` | Yes |

Static viewers are useful for simple inspection or sharing an HTML file. They
do not keep a running R bridge after the file is opened.

Live viewers are for interactive analysis. They require the R process to keep
running because R is serving the viewer state, event endpoint, and optional
dynamic tiles.

## Common Failure: The Viewer Opens As `file://`

If the viewer opens as a local file path such as:

```text
file:///Users/name/project/viewer.html
```

live synchronization will not work. A `file://` page is a static browser page;
it cannot reliably send annotations and measurements back into R.

Use:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()
```

Then open the `http://127.0.0.1:<port>` URL printed by R. Do not open
`/viewer-state` directly. `/viewer-state` is an internal JSON endpoint used by
the viewer, not the viewer page.

## RStudio Workflow

In RStudio, keep the console usable by opening the viewer without blocking:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = FALSE,
  name = "case_01"
)

viewer$get_rois()
viewer$get_measurements()
```

If the browser does not open automatically, copy the printed
`http://127.0.0.1:<port>` URL into the browser.

## Rscript Workflow

When using `Rscript`, keep the process alive:

```r
library(wsiTools)

slide <- wsi_open("/path/to/image.svs")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = TRUE,
  name = "case_01"
)
```

Run:

```bash
Rscript open_live_viewer.R
```

Do not close the terminal while using the viewer.

## Remote Desktop Or Server Workflow

If R runs on a remote machine, open the URL in a browser that can reach the same
machine and port. For SSH, use port forwarding:

```bash
ssh -L 8794:127.0.0.1:8794 user@remote-host
```

Then open:

```text
http://127.0.0.1:8794
```

If the remote browser opens `/viewer-state`, go back to the viewer HTML URL
printed by R.

## Selected-ROI Cell Segmentation

When optional StarDist or Mesmer commands are configured, the live viewer can
run segmentation on a selected ROI crop instead of the full WSI:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_engines = c("stardist_he", "stardist_ihc", "mesmer_dapi"),
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32
)

viewer$open()
```

In the browser, draw/select an ROI and use the `Cells` menu. In R:

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

StarDist and Mesmer are optional runtime tools. Check:

```r
wsi_has_stardist()
wsi_has_mesmer()
```

## Troubleshooting Checklist

Run:

```r
wsi_backends()
wsi_setup_report()
wsi_diagnose()
viewer$capabilities()
viewer$get_state()
```

Common causes of failed live synchronization:

- the viewer was opened as `file://` instead of `http://127.0.0.1:<port>`;
- the R session was closed or busy;
- the browser opened `/viewer-state` instead of the viewer page;
- a firewall or SSH setup blocks the local `httpuv` port;
- an old browser tab is still open after restarting the R viewer session;
- the installed package version is older than the viewer HTML generated by R.

The safest reset is to close old viewer tabs, restart R, recreate the live
viewer, and open the fresh `http://127.0.0.1:<port>` URL printed by R.

For more known error messages and fixes, see [troubleshooting](troubleshooting.md).
