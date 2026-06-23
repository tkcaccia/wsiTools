# Backend Setup

wsiTools is designed so the R package can install first, even on machines that
do not yet have whole-slide image libraries or model tools. Backends are
optional runtime capabilities: they are checked when you open, convert, tile,
or segment an image. They are not mandatory R package dependencies.

Start every setup check with:

```r
library(wsiTools)

wsi_backends()
wsi_setup_report()
wsi_diagnose(live_test = FALSE)
```

Use the `wsi_has_*()` helpers for specific capabilities:

```r
wsi_has_vips()
wsi_has_openslide()
wsi_has_native_czi()
wsi_has_stardist()
wsi_has_mesmer()
```

## Backend Summary

| Backend | Needed for |
|---|---|
| libvips | conversion, pyramids, fast tiles |
| OpenSlide | SVS/NDPI/SCN/MRXS reading |
| native CZI | fast CZI viewing |
| Bio-Formats | broad microscopy fallback |
| StarDist | optional cell segmentation |
| Mesmer | optional DAPI/mIHC segmentation |

## What Runtime Capability Means

The package itself is the R interface, viewer code, metadata handling,
annotation logic, project state, and wrappers. Runtime backends are external
tools that wsiTools can use when they are present.

This has three practical consequences:

- `install_github("tkcaccia/wsiTools")` should not need OpenSlide, libvips,
  Bio-Formats, StarDist, Mesmer, or large model files.
- `wsi_backends()` tells you which advanced features are available on the
  current computer.
- If a backend is missing, only the functions that need that backend should
  fail, with an informative message.

## libvips

libvips is the preferred backend for large-image conversion and tile
preparation. It is useful for:

- pyramidal TIFF and OME-TIFF export
- fast thumbnails and region export
- Deep Zoom / tiled viewer preparation
- image conversion between TIFF, OME-TIFF, PNG, JPEG, and pyramidal TIFF

Check:

```r
wsi_has_vips()
```

This returns `TRUE` when both `vips` and `vipsheader` are on `PATH`.

Common install commands:

```bash
# macOS
brew install vips

# Ubuntu
sudo apt install -y libvips-tools
```

On Windows, install libvips manually or with `winget`:

```powershell
winget install libvips.libvips
```

## OpenSlide

OpenSlide is used for many digital pathology whole-slide image formats. It is
especially relevant for:

- Aperio SVS
- Hamamatsu NDPI
- Leica SCN
- MIRAX MRXS
- generic pyramidal TIFF variants supported by the local OpenSlide build

Check:

```r
wsi_has_openslide()
```

This returns `TRUE` when `openslide-show-properties` is available on `PATH`.

Common install commands:

```bash
# macOS
brew install openslide

# Ubuntu
sudo apt install -y openslide-tools
```

OpenSlide support is backend-dependent. Do not assume every SVS, NDPI, SCN, or
MRXS variant will open on every machine.

## Native CZI

The native CZI backend is intended for faster CZI first visualization without
using Python. It calls ZEISS libCZI/libCZIAPI through the package's compiled
bridge.

In live CZI viewers, wsiTools keeps a small native reader/accessor handle open
for the viewer session and requests only the tile or region needed by
OpenSeadragon. The full CZI scene is not loaded into R memory. Generated tiles
are cached temporarily and the native handle/cache are cleaned up when the live
viewer session stops. Set `WSITOOLS_CZI_PERSISTENT_TILE_READER=false` only when
debugging a native CZI installation problem.

Check:

```r
wsi_has_native_czi()
```

If the library is installed but not found automatically, set:

```r
Sys.setenv(WSITOOLS_LIBCZIAPI = "/full/path/to/libCZIAPI")
wsi_has_native_czi()
```

On Windows, the equivalent path usually points to a `.dll`; on macOS, a
`.dylib`; on Linux, a `.so`.

If native CZI support is unavailable, wsiTools may still open some CZI files
through Bio-Formats or other fallback paths, but performance and first
visualization behavior may differ.

## Bio-Formats

Bio-Formats is the broad microscopy fallback. It is useful for formats that are
not reliably covered by OpenSlide, libvips, ImageMagick, or native CZI.

Bio-Formats can help with:

- CZI fallback metadata/region access
- microscopy formats outside classical pathology WSI
- conversion workflows where the OME Bio-Formats tools support the file

Check:

```r
wsi_has_bioformats()
wsi_backends()
```

wsiTools can detect command-line Bio-Formats tools such as `showinf` and
`bfconvert`, and it can also use the optional Java helper when configured.

Bio-Formats is powerful but not always the fastest option for first
visualization. For CZI, prefer native CZI when available; use Bio-Formats as a
broad compatibility fallback.

## StarDist

StarDist is optional and is used only when the user explicitly starts
cell-segmentation workflows. It is not required to install wsiTools or open
images.

In wsiTools, StarDist is intended for selected-ROI segmentation, not whole-slide
segmentation in memory. The low-RAM pattern is:

1. select or draw an ROI in the viewer;
2. crop only that ROI;
3. run StarDist on the crop;
4. use tiled StarDist execution for large crops;
5. import polygons, centroids, masks, or GeoJSON back into the viewer/R.

Check:

```r
wsi_has_stardist()
```

Configure an existing StarDist command:

```r
Sys.setenv(WSITOOLS_STARDIST_COMMAND = "/path/to/stardist-command")
wsi_has_stardist()
```

For live viewer use:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  stardist = TRUE,
  segmentation_engines = c("stardist_he", "stardist_ihc"),
  segmentation_tiles_x = 32,
  segmentation_tiles_y = 32
)

viewer$open()
```

Then draw/select an ROI and run segmentation from the `Cells` menu.

## Mesmer

Mesmer is optional and intended for DAPI/mIHC cell-segmentation workflows. Like
StarDist, it should be configured as an external command or wrapper and run on
selected ROI crops rather than full whole-slide images.

Check:

```r
wsi_has_mesmer()
```

Configure an existing Mesmer/DeepCell command:

```r
Sys.setenv(WSITOOLS_MESMER_COMMAND = "/path/to/mesmer-command")
wsi_has_mesmer()
```

In the viewer, Mesmer-style workflows are exposed through the `Cells` menu when
the live segmentation endpoint is enabled and a Mesmer command is available.

## Choosing A Backend

Use this practical order:

1. For SVS/NDPI/SCN/MRXS pathology slides, try OpenSlide/libvips.
2. For conversion, pyramids, and fast tiled export, prefer libvips.
3. For CZI first visualization, prefer native CZI when available.
4. For broad microscopy fallback, use Bio-Formats.
5. For cell segmentation, use already-configured StarDist or Mesmer commands
   on selected ROI crops.

## Diagnostic Commands For Issues

When asking for help, paste the output of:

```r
library(wsiTools)
packageVersion("wsiTools")
wsi_backends()
wsi_setup_report()
wsi_diagnose()
```

For a specific image, also include:

```r
normalizePath("your_image_file")
file.info("your_image_file")
```

Do not attach large WSI files to a GitHub issue unless requested. A small
representative test image or the exact backend error is usually more useful.
