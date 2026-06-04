# Installing wsiTools

This page explains how to install the R package and the optional runtime tools
used by wsiTools. The core R package is intentionally lightweight: it can be
installed without OpenSlide, libvips, Bio-Formats, StarDist, Mesmer, Python, or
large model files. Those tools are detected at runtime with `wsi_backends()`.

For a plain-language explanation of what each optional backend does, see the
[backend setup guide](backends.md). After installation, the fastest test is the
[one-image quickstart](open-one-image.md), followed by the
[examples gallery](examples.md). If something fails, see
[troubleshooting](troubleshooting.md).

## R Package Installation

Install the current GitHub version:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE
)

library(wsiTools)
wsi_backends()
wsi_setup_report()
wsi_diagnose(live_test = FALSE)
```

If you are reinstalling after local development or after a failed install, use:

```r
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE
)
```

The commands `wsi_setup_report()` and `wsi_diagnose()` are read-only.
`wsi_setup_report()` prints the package version, R session, backend
availability, optional R packages, copyable setup commands, and relevant
environment variables. `wsi_diagnose()` adds executable paths, live-viewer
startup checks, browser/R sync self-tests when requested, and suggested fixes.

## Windows Installation

1. Install R from CRAN.
2. Install the matching Rtools version for your R release.
3. Restart R or RStudio.
4. Install wsiTools:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")

remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE,
  INSTALL_opts = "--no-multiarch"
)

library(wsiTools)
wsi_backends()
wsi_setup_report()
```

If installation fails because a previous attempt left a lock directory:

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

Useful Windows checks:

```r
sessionInfo()
Sys.which(c("git", "make", "gcc", "g++"))
pkgbuild::has_build_tools(debug = TRUE)
wsi_setup_report()
```

Optional Windows tools can be installed manually or with package managers. For
example, if `winget` is available:

```powershell
winget install libvips.libvips
winget install ImageMagick.ImageMagick
```

OpenSlide on Windows is often easiest to install manually and then add to
`PATH`. If native CZI support is needed and the library is not discoverable,
set `WSITOOLS_LIBCZIAPI` to the full path of the libCZIAPI shared library.

## macOS Installation

Install the R package:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("tkcaccia/wsiTools", upgrade = "never", build_vignettes = FALSE)
```

Optional system tools with Homebrew:

```bash
brew install openslide vips imagemagick
```

Then restart R and check:

```r
library(wsiTools)
wsi_backends()
wsi_setup_report()
```

For CZI first visualization through the native bridge, install/configure ZEISS
libCZI/libCZIAPI and check:

```r
wsi_has_native_czi()
```

## Ubuntu Installation

Install system build tools and common WSI backends:

```bash
sudo apt update
sudo apt install -y build-essential r-base-dev git
sudo apt install -y openslide-tools libvips-tools imagemagick
```

Install the R package:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("tkcaccia/wsiTools", upgrade = "never", build_vignettes = FALSE)

library(wsiTools)
wsi_backends()
wsi_setup_report()
```

On remote desktops or servers, live viewing needs the browser to reach the
`http://127.0.0.1:<port>` URL printed by `wsi_viewer_live()`. If you connect
through SSH, use port forwarding.

## Optional Backend Installation

The core package does not silently install large external tools. Use
`wsi_backends()` to see what is available and `wsi_dependency_plan()` to print
copyable commands for your platform:

```r
wsi_backends()
wsi_dependency_plan(method = "manual", include_optional = TRUE)
wsi_dependency_plan(method = "auto", include_optional = TRUE)
```

To ask wsiTools to run supported setup commands, opt in explicitly:

```r
wsi_install_backends(
  tools = c("openslide", "libvips", "imagemagick", "bioformats"),
  method = "auto"
)
```

For conda or mamba, prefer conda-forge/OME channels and avoid default Anaconda
channels if Terms of Service are not accepted on the machine:

```r
wsi_install_backends(
  tools = c("openslide", "libvips", "imagemagick", "bioformats"),
  method = "conda"
)
```

StarDist and Mesmer are optional external cell-segmentation engines. They are
not required for package installation or image viewing. Check them with:

```r
wsi_has_stardist()
wsi_has_mesmer()
```

If you already have a StarDist command or wrapper, point wsiTools to it:

```r
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist-command")
wsi_has_stardist()
```

If you already have a Mesmer/DeepCell command or wrapper, point wsiTools to it:

```r
Sys.setenv(WSITOOLS_MESMER_COMMAND = "/path/to/mesmer-command")
wsi_has_mesmer()
```

## How To Test Installation

Run these commands first:

```r
library(wsiTools)
packageVersion("wsiTools")
wsi_backends()
wsi_setup_report()
wsi_diagnose(live_test = FALSE)
```

Then test one image:

```r
slide <- wsi_open("sample.svs")
wsi_info(slide)
wsi_viewer(slide)
```

For full-resolution browser viewing, use tiled/live mode:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()
```

If you only want a static HTML file:

```r
html <- wsi_viewer(slide, tiled = TRUE)
html
```

## Backend Helper Functions

Use these functions to understand what wsiTools can do on the current machine:

```r
wsi_has_vips()
wsi_has_openslide()
wsi_has_native_czi()
wsi_has_stardist()
wsi_has_mesmer()
```

- `wsi_has_vips()` is `TRUE` when `vips` and `vipsheader` are on `PATH`.
  libvips is used for conversion, pyramids, thumbnails, export, and fast tile
  generation.
- `wsi_has_openslide()` is `TRUE` when OpenSlide command-line tools are on
  `PATH`. OpenSlide is useful for SVS, NDPI, SCN, MRXS, and other WSI formats
  supported by the local OpenSlide build.
- `wsi_has_native_czi()` is `TRUE` when the optional native CZI bridge can load
  ZEISS libCZI/libCZIAPI.
- `wsi_has_stardist()` is `TRUE` when an external StarDist command is
  configured and discoverable. wsiTools uses it only when the user explicitly
  starts selected-ROI cell segmentation.
- `wsi_has_mesmer()` is `TRUE` when an external Mesmer/DeepCell command is
  configured and discoverable. It is optional and intended for DAPI/mIHC cell
  segmentation workflows.

## Common Installation Errors

### `there is no package called 'wsiTools'`

The package is not installed in the active R library. Reinstall:

```r
remotes::install_github("tkcaccia/wsiTools", upgrade = "never", build_vignettes = FALSE)
```

Then restart R and run:

```r
library(wsiTools)
packageVersion("wsiTools")
```

### `failed to lock directory ... 00LOCK-wsiTools`

Remove the stale lock and reinstall:

```r
lib <- Sys.getenv("R_LIBS_USER")
unlink(file.path(lib, "00LOCK-wsiTools"), recursive = TRUE, force = TRUE)
unlink(file.path(lib, "wsiTools"), recursive = TRUE, force = TRUE)
```

### Windows compilation fails

Check Rtools:

```r
Sys.which(c("git", "make", "gcc", "g++"))
pkgbuild::has_build_tools(debug = TRUE)
```

Then reinstall with:

```r
remotes::install_github(
  "tkcaccia/wsiTools",
  upgrade = "never",
  build_vignettes = FALSE,
  force = TRUE,
  INSTALL_opts = "--no-multiarch"
)
```

### `No available backend could open this file`

The file may need an optional backend. Run:

```r
wsi_backends()
wsi_setup_report()
wsi_diagnose(live_test = FALSE)
```

For CZI, check:

```r
wsi_has_native_czi()
wsi_has_bioformats()
```

For SVS/NDPI/SCN/MRXS, check:

```r
wsi_has_openslide()
wsi_has_vips()
```

### Live viewer opens but R does not receive annotations

Use `wsi_viewer_live()`, not a static `file://` HTML viewer:

```r
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()
```

Open the printed `http://127.0.0.1:<port>` viewer URL, not `/viewer-state`.

### StarDist or Mesmer is not configured

This means the optional model command is missing. The R package can still open
images and use CellPhenotyper outputs. To enable selected-ROI segmentation:

```r
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist-command")
Sys.setenv(WSITOOLS_MESMER_COMMAND = "/path/to/mesmer-command")
wsi_has_stardist()
wsi_has_mesmer()
```

For low-RAM StarDist runs, segment only a selected ROI and use tiled execution
arguments such as `{tiles_y}` and `{tiles_x}` in your command wrapper.

### Conda Terms of Service errors

If conda reports that default Anaconda channels require Terms of Service
acceptance, either accept those terms manually or use a conda-forge/miniforge
setup. wsiTools' conda setup plan uses `--override-channels` with conda-forge
and OME where possible, but existing conda configuration can still affect some
machines.
