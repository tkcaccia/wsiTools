# Troubleshooting

This page lists common wsiTools installation and viewer problems in a practical
format. Most fixes start by checking the package version, backend status, and
live viewer state.

Run these commands before asking for help:

```r
library(wsiTools)

packageVersion("wsiTools")
wsi_backends()
wsi_setup_report()
wsi_diagnose()
```

For live viewer problems, also run:

```r
viewer$capabilities()
viewer$get_state()
```

## Package Not Installed

Problem:

```text
Error in library(wsiTools) : there is no package called 'wsiTools'
```

Meaning:

The package is not installed in the R library used by the current R session.

Check:

```r
.libPaths()
packageVersion("wsiTools")
```

Fix:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE
)

library(wsiTools)
```

If you installed into a different R version or library, restart R and check
`.libPaths()`.

## `00LOCK-wsiTools` On Windows

Problem:

```text
ERROR: failed to lock directory ... for modifying
Try removing .../00LOCK-wsiTools
```

Meaning:

A previous installation was interrupted and left a lock directory in the R
package library.

Check:

```r
Sys.getenv("R_LIBS_USER")
list.files(Sys.getenv("R_LIBS_USER"), pattern = "00LOCK")
```

Fix:

```r
lib <- Sys.getenv("R_LIBS_USER")
unlink(file.path(lib, "00LOCK-wsiTools"), recursive = TRUE, force = TRUE)
unlink(file.path(lib, "wsiTools"), recursive = TRUE, force = TRUE)

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE,
  INSTALL_opts = "--no-multiarch"
)
```

## Windows `Makeconf:296` Compilation Error

Problem:

```text
make: *** [C:/PROGRA~1/R/.../Makeconf:296: native-czi.o] Error 1
ERROR: compilation failed for package 'wsiTools'
```

Meaning:

The package compilation failed. This is usually caused by an Rtools mismatch,
an old GitHub checkout, a stale source package cache, or a native CZI compile
issue on that workstation.

Check:

```r
sessionInfo()
.libPaths()
Sys.which(c("git", "make", "gcc", "g++"))
pkgbuild::has_build_tools(debug = TRUE)
```

Fix:

```r
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE,
  INSTALL_opts = "--no-multiarch"
)
```

If the native CZI bridge is the only blocker and you need a core install first:

```r
Sys.setenv(WSITOOLS_DISABLE_NATIVE_CZI = "1")

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE,
  INSTALL_opts = "--no-multiarch"
)
```

Then report the full install output plus:

```r
wsi_setup_report()
```

## CZI Opens But Image Is Not Displayed

Problem:

The viewer opens, but the CZI image area is blank, white, black, or only shows a
backend message.

Meaning:

The CZI file was detected, but no working CZI pixel reader or preview path was
available for that specific file/scene.

Check:

```r
file.exists("/path/to/image.czi")
wsi_has_native_czi()
wsi_has_bioformats()
wsi_backends()
```

Fix:

Use the project viewer first, because CZI files often contain multiple scenes:

```r
wsi_viewer_project(
  images = "/path/to/image.czi",
  czi_sections = TRUE,
  open = TRUE,
  overwrite = TRUE
)
```

If native CZI is installed but not found:

```r
Sys.setenv(WSITOOLS_LIBCZIAPI = "/full/path/to/libCZIAPI")
wsi_has_native_czi()
```

If native CZI is unavailable, install/configure Bio-Formats as a fallback.

## Bio-Formats Missing

Problem:

```text
Bio-Formats was not found on PATH
```

Meaning:

The optional Bio-Formats command-line tools or Java helper are not available.
wsiTools can still open formats supported by other backends, but broad
microscopy fallback is missing.

Check:

```r
wsi_has_bioformats()
wsi_backends()
wsi_setup_report()
```

Fix:

Install Bio-Formats tools or configure the Java helper. With conda:

```r
wsi_install_backends(
  tools = "bioformats",
  method = "conda"
)
```

If conda reports Terms of Service errors, use a conda-forge/miniforge setup or
install OME bftools manually.

## Image Is Slow

Problem:

The image opens, but zooming and panning are slow.

Meaning:

The viewer may be using low-resolution previews, on-demand dynamic tiles, a
slow backend, or a very large uncompressed overlay.

Check:

```r
wsi_has_vips()
wsi_has_openslide()
wsi_info(slide)
viewer$capabilities()
```

Fix:

Use tiled viewing:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  dynamic_tiles = FALSE
)

viewer$open()
```

For repeated viewing, prefer prebuilt static Deep Zoom tiles when possible. Use
dynamic tiles mainly when prebuilt tiles are unavailable or for live channel
overlays.

## Black Tiles Or Tile Gaps

Problem:

You see black flashes, black tile lines, or small gaps between tiles.

Meaning:

The browser is waiting for tiles, tiles are generated slowly, or overlay tiles
do not align perfectly with the base tile grid.

Check:

```r
wsi_has_vips()
viewer$capabilities()
```

Fix:

Prefer prebuilt tiles for stable viewing:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  dynamic_tiles = FALSE
)

viewer$open()
```

If using dynamic tiles, use JPEG for brightfield base tiles and keep overlays
limited to channels that are needed:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  dynamic_tiles = TRUE,
  dynamic_tile_format = "jpg"
)
```

## Live Sync Not Working

Problem:

You draw annotations or measurements in the browser, but R does not receive
them.

Meaning:

The browser is not connected to the active R `httpuv` live session, or the R
session has stopped.

Check:

```r
viewer$get_state()
viewer$get_rois()
viewer$capabilities()
```

Fix:

Create a live viewer and keep R running:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = FALSE
)

viewer$open()
```

Open the printed `http://127.0.0.1:<port>` viewer URL. Do not open the
`/viewer-state` endpoint as the viewer page.

## Unsupported Viewer State Field

Problem:

```text
Viewer state payload contains unsupported field: selected_object
```

Meaning:

The browser viewer HTML and the installed R package are out of sync. The
browser is sending a field that the R-side event validator does not recognize.

Check:

```r
packageVersion("wsiTools")
viewer$get_state()
```

Fix:

Update the package and regenerate the viewer HTML:

```r
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE
)

library(wsiTools)
slide <- wsi_open("/path/to/image.svs")
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()
```

Close old browser tabs before opening the regenerated viewer.

## StarDist Not Configured

Problem:

```text
StarDist is not configured
```

Meaning:

wsiTools can orchestrate StarDist on a selected ROI crop, but no StarDist
command was found.

Check:

```r
wsi_has_stardist()
Sys.getenv("WSITOOLS_STARDIST_COMMAND")
```

Fix:

Install/configure StarDist externally, or point wsiTools to an existing wrapper:

```r
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist-command")
wsi_has_stardist()
```

For low-RAM use, segment only a selected ROI and use tiled execution:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32
)
```

## Scale Bar Unavailable

Problem:

The viewer does not show a micrometre scale bar or reports that scale is
unavailable.

Meaning:

The slide metadata does not contain reliable microns-per-pixel information, or
the backend could not read it.

Check:

```r
wsi_mpp(slide)
wsi_properties(slide)
wsi_info(slide)
```

Fix:

Use a file/backend that exposes MPP metadata, or supply calibration through the
workflow that knows the imaging platform. For Visium data, spot spacing can
help estimate scale: standard Visium spots are 55 micrometres in diameter with
100 micrometre center-to-center spacing.

## Spots Not Aligned

Problem:

Spatial transcriptomics spots appear shifted, flipped, rotated, or delayed
relative to the tissue image.

Meaning:

The coordinate transform does not match the image orientation or scale, or the
viewer is using a preview/image different from the one used to compute spatial
coordinates.

Check:

```r
wsi_info(slide)
wsi_mpp(slide)
```

For Seurat/Giotto/SpatialExperiment linking, inspect the transform arguments
you used, especially `flip` and `rotation`.

Fix:

Use the high-resolution tissue image associated with the object, then adjust
transform parameters explicitly:

```r
viewer <- wsi_viewer_seurat(
  seurat_obj,
  image = "/path/to/high_resolution_tissue_image.tif",
  dynamic_tiles = TRUE,
  flip = "vertical",
  rotation = 90
)
```

If that is wrong, try `flip = "horizontal"` or `rotation = 180`/`270` depending
on how the spots differ from the tissue.

## Browser Opens Static File Instead Of Live URL

Problem:

The browser URL starts with:

```text
file://
```

Meaning:

You opened a static HTML viewer. Static viewers do not synchronize
annotations, measurements, or selections back to R.

Check:

Look at the browser address bar. Live viewers should open as:

```text
http://127.0.0.1:<port>/...
```

Fix:

Use `wsi_viewer_live()`:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = FALSE
)

viewer$open()
```

Keep the R session running while using the viewer.

## What To Paste In A GitHub Issue

When reporting a problem, paste:

```r
sessionInfo()
packageVersion("wsiTools")
wsi_backends()
wsi_setup_report()
wsi_diagnose()
```

For image-specific problems:

```r
file.exists("/path/to/image")
file.info("/path/to/image")
```

For live viewer problems:

```r
viewer$capabilities()
viewer$get_state()
```
