# Open One Image

This is the shortest practical workflow for opening one whole-slide image in
wsiTools. The friendly entry point chooses the backend, static/live viewer,
tiled/thumbnail mode, and browser launch behavior without loading the full
slide into R memory.

No image file yet? Run:

```r
demo <- wsi_demo_viewer(open = TRUE)
demo$path
```

## Minimal Viewer

```r
library(wsiTools)

viewer <- wsi_open_viewer("/path/to/image.svs")
```

Renderer selection is identical for direct R and Tauri launches:

```r
viewer <- wsi_open_viewer(
  "/path/to/image.svs",
  renderer = "auto" # WebGL/WebGL2, with Canvas fallback
)
```

Use `renderer = "gpu"` to require WebGL or `renderer = "cpu"` to force the
Canvas fallback. Tiled close zoom always retains access to the source maximum
pyramid level (level-0/full-resolution pixels).

If live mode is available, this returns a `wsi_viewer_session`. If live mode is
not available, it returns the static HTML viewer path.

For a static viewer only:

```r
html <- wsi_open_viewer(
  "/path/to/image.svs",
  live = "no",
  open = TRUE
)
```

If the live viewer prints a `http://127.0.0.1:<port>` URL, open that viewer URL.
Do not open `/viewer-state` directly; that endpoint is for browser-to-R
synchronization, not for viewing the slide.

## Manual Control

Use the lower-level functions when you want to keep explicit control of the
slide object:

```r
slide <- wsi_open("/path/to/image.svs")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE
)

viewer$open()
```

## Check Metadata

Before viewing or tiling, check what the backend can read:

```r
wsi_info(slide)
wsi_levels(slide)
wsi_mpp(slide)
wsi_objective_power(slide)
wsi_properties(slide)
```

Backend availability can be checked with:

```r
wsi_backends()
wsi_setup_report()
```

## Close The Slide Or Viewer

When you are done with a slide object:

```r
wsi_close(slide)
```

For a live viewer session:

```r
viewer$stop()
```

If the browser window is still open after stopping the session, close the
browser tab or create a new live viewer.

## Reopen The Same Image

If you stopped the viewer but still have the slide object:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()
```

If you closed the slide object, reopen it from the file path:

```r
viewer <- wsi_open_viewer("/path/to/image.svs")
```

## Static Viewer

Use a static viewer when you only need an HTML file and do not need automatic
feedback into R:

```r
html <- wsi_viewer(
  slide,
  tiled = TRUE
)

html
```

Static viewers are convenient for sharing or simple inspection. They do not
push annotations, selections, measurements, or viewer state back to R
automatically. For more detail, see the [live viewer guide](live-viewer.md).

## Export A TIFF Crop

In a live viewer, use **View > Save view TIFF** to export the visible slide
region, or **Annotations > Save ROI TIFF** to export the selected annotation
bounding box. These exports are written by R in the R working directory unless
you enter another output directory in the viewer.

For scripted workflows, use the same backend export directly:

```r
wsi_export_region(
  slide,
  x = 10000,
  y = 20000,
  width = 2048,
  height = 2048,
  output = "region.tif",
  format = "tiff"
)
```

## Export Tiles

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

Export tiles to a folder:

```r
manifest <- wsi_export_tiles(
  slide,
  grid = grid,
  output_dir = "tiles",
  format = "png",
  overwrite = FALSE
)

head(manifest)
```

Or use the higher-level tile extraction helper:

```r
manifest <- wsi_tile(
  slide,
  output_dir = "tiles",
  tile_size = 512,
  overlap = 0,
  level = 0,
  format = "png",
  overwrite = FALSE
)
```

The returned manifest records tile coordinates, dimensions, level, row/column
indices, tissue fraction when available, and output filenames.

## Export A Region

Export one rectangular region:

```r
wsi_export_region(
  slide,
  x = 10000,
  y = 20000,
  width = 1024,
  height = 1024,
  level = 0,
  output = "region.png",
  format = "png"
)
```

The coordinates are level-0 slide coordinates unless the function documentation
states otherwise.

## Read A Small Region Into R

Only read small regions into R memory:

```r
patch <- wsi_read_region(
  slide,
  x = 10000,
  y = 20000,
  width = 512,
  height = 512,
  level = 0,
  format = "array"
)
```

Do not use region reading to load an entire WSI level into R.

## Common Problems

### The viewer opens but shows no image

Check:

```r
file.exists("/path/to/image.svs")
wsi_backends()
wsi_info(slide)
```

For SVS/NDPI/SCN/MRXS, OpenSlide or libvips is usually needed. For CZI, native
CZI or Bio-Formats may be needed.

### Full resolution is not available

Use tiled mode:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()
```

The browser needs tiles for full-resolution zooming. Opening a low-resolution
thumbnail or preview image will not expose level-0 detail.

### Annotations do not return to R

Use the live viewer, not a static viewer:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()

viewer$get_rois()
```

Static HTML files do not maintain a live R bridge.
