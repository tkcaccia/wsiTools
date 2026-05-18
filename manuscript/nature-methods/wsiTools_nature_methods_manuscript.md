# wsiTools: memory-efficient whole-slide image access, preprocessing and annotation interoperability in R

## Authors

Stefano [Surname]^1,*; [Co-author names]^2

^1 [Affiliation]  
^2 [Affiliation]  
* Correspondence: [email]

## Article Type

Target journal: Nature Methods  
Proposed type: Resource or Article  
Status: draft for presubmission inquiry; quantitative validation still required.

## Abstract

Whole-slide images (WSIs) are essential to digital pathology, spatial omics and
machine-learning patch extraction, but their multi-gigapixel scale and
heterogeneous vendor formats make reproducible analysis difficult in R. We
present `wsiTools`, an open-source R toolkit for memory-efficient WSI access
and preprocessing through region-based reading, tiled processing, pyramidal
image handling, conversion and export. The package provides backend capability
checks and a lightweight slide abstraction over OpenSlide, libvips and
metadata-level OME-Zarr support. It exposes functions for metadata extraction,
region reading, thumbnails, tile grids, tile extraction, BTF/SVS-to-OME-TIFF
conversion, QuPath-compatible GeoJSON ROI import/export, interactive tiled
viewing, H&E/IHC comparison, immunohistochemistry color deconvolution,
segmentation-overlay import, spatial measurements and simple serial-section
registration. `wsiTools` does not load entire level-0 slides into memory by
default; instead, it delegates large-image operations to established WSI
backends and records reproducible manifests for downstream analysis. We
demonstrate its use in practical pathology workflows including H&E BigTIFF
visualization and conversion, SAPC 0052 immunohistochemistry deconvolution,
ROI-aware tiling and class-level tissue summaries. [Insert quantitative
benchmark results before submission.] `wsiTools` is available at
https://github.com/tkcaccia/wsiTools.

## Introduction

Digital pathology has transformed histology from a microscope-centered practice
into a data-intensive imaging discipline. A single WSI can contain billions of
pixels, multiple pyramid levels, scanner-specific metadata and associated label
or macro images. This scale is both an opportunity and a computational
obstacle: many downstream workflows require small patches, ROI-specific crops,
tiled training data, pyramidal conversion or spatial summaries, but loading the
entire slide into memory is usually infeasible.

Several open-source tools address parts of this problem. OpenSlide provides a C
library for reading whole-slide images and supports many virtual slide formats,
with the caveat that backend support varies by scanner and format. libvips
provides efficient streaming operations for large images and is widely used for
conversion, thumbnailing and pyramidal image generation. QuPath has become a
practical open-source desktop environment for bioimage and pathology annotation,
including ROI creation and analysis. OME-Zarr extends bioimaging data access
toward chunked, cloud-compatible multidimensional arrays. These tools are
powerful, but R users often need to connect them manually with downstream
bioinformatics, spatial transcriptomics and statistical analysis workflows.

`wsiTools` was developed to bridge this gap. Its design principle is simple:
WSI workflows in R should be scriptable and reproducible without treating a
whole slide as an ordinary in-memory image. The package therefore uses backend
metadata, region reads, tiles, pyramid levels and streaming conversion whenever
possible. It also treats annotations and derived outputs as first-class
analysis objects, enabling GeoJSON ROI round-trips, tile manifests,
segmentation overlays and measurement summaries to remain linked to slide
coordinates.

## Results

### A backend-aware slide abstraction for R

`wsiTools` introduces a lightweight S3 `wsi_slide` object returned by
`wsi_open()`. The object stores the file path, selected backend, level-0
dimensions, pyramid levels, downsample factors, metadata properties, associated
image names and backend-specific information. Opening a slide reads metadata
only and does not load pixel data from the full image. Users can inspect
available backends with `wsi_backends()`, including OpenSlide, libvips,
Bio-Formats placeholders and ImageMagick/magick preview support.

The package is deliberately backend-aware rather than format-claiming. For
example, Aperio SVS, Hamamatsu NDPI, Leica SCN, MIRAX MRXS, Philips TIFF,
Ventana BIF, generic pyramidal TIFF, OME-TIFF and DICOM WSI may be supported
when the local backend supports them, but `wsiTools` reports capabilities and
raises informative errors rather than guaranteeing universal compatibility.

### Region-based reading, thumbnails and tile manifests

The core API reads only explicitly requested regions. `wsi_read_region()` uses
level-0 coordinates and returns arrays, rasters or magick images when the
backend supports the request. `wsi_thumbnail()` creates low-resolution previews
through libvips rather than by loading full-resolution images. `wsi_tile_grid()`
generates tile coordinates without reading image pixels, while `wsi_tile()` and
`wsi_export_tiles()` export the requested regions to disk and return tile
manifests with coordinates, pyramid level, row, column, tissue fraction and
file paths.

Coordinate-list workflows are supported through `wsi_tile_grid_from_coords()`,
`wsi_tile_from_coords()` and `extract_tiles()`, enabling patch extraction from
model outputs, manually curated coordinate tables or spatial omics spot
locations. The tile manifest provides a reproducible contract between raw WSIs
and downstream machine-learning pipelines.

### Conversion and pyramidal export

Large-image conversion is delegated to libvips through safe command wrappers.
`wsi_convert()` converts WSI or large image inputs to TIFF, pyramidal TIFF,
OME-TIFF, PNG or JPEG, and `wsi_pyramid()` provides a focused interface for
creating tiled pyramidal TIFF/OME-TIFF outputs. Output files are never silently
overwritten. In a representative H&E BTF workflow, the script
`inst/examples/he-btf-viewer-convert.R` opens a local BigTIFF/BTF image,
creates a tiled interactive viewer and converts the source image to compressed
pyramidal OME-TIFF. [Insert conversion benchmark table.]

### Interactive viewers and ROI interoperability

`wsiTools` includes lightweight HTML viewers designed for practical pathology
inspection. `wsi_viewer()` supports thumbnail and full-resolution tiled modes,
with pan, zoom, coordinate readout, menu-organized controls, crosshairs, ROI
overlays, a geometry list and polygon drawing. Drawn regions can be exported as
GeoJSON. `wsi_viewer_roi()` and `viewer_add_rois()` overlay QuPath-compatible
GeoJSON annotations, while `write_geojson()` exports modified ROI objects back
to GeoJSON.

`viewer_compare()` provides side-by-side image comparison with synchronized
zoom and pan, optional independent navigation, linked cursor position and
annotation or mask overlays. This is intended for H&E/IHC comparison,
original/processed image review, WSI/segmentation inspection and serial-section
quality control. [Insert screenshot and usability example.]

### Immunohistochemistry deconvolution

Brightfield IHC workflows often require separation of hematoxylin and
chromogenic marker signal. `wsi_deconvolve_ihc()` performs optical-density
color deconvolution on already-small images, and `wsi_deconvolve_region()`
combines region-based WSI reading with H-DAB deconvolution. The interactive
IHC viewer exposes channel visibility, color and gain controls. For multiplex
brightfield assays, `wsi_stain_channels()` and `wsi_deconvolve_multi_ihc()`
support one to three stain vectors at a time, reflecting the constraints of RGB
brightfield data.

We applied the workflow to the local SAPC 0052.svs image. The script
`run_sapc0052_deconvolution.R` opens the Aperio SVS image through libvips,
selects a tissue-containing region from a low-resolution tissue mask, reads a
1024 x 1024 patch and writes hematoxylin, HRP/DAB, composite and RDS channel
outputs. [Insert visual panel and quantitative channel summary.]

### Segmentation, measurement and registration scaffolding

`wsiTools` provides optional interfaces for external segmentation workflows
without making StarDist, Cellpose or other model frameworks mandatory.
`export_roi_crop()` writes ROI crops for external analysis,
`import_segmentation()` reads GeoJSON, CSV centroids or mask images, and
`viewer_add_segmentation()` displays supported outputs as overlays. The package
also includes basic spatial measurements: point distances, cell-to-ROI-boundary
distances, nearest-neighbor distances, ROI distances, cell density and
class-level ROI summaries. Simple affine registration from manual landmarks is
available through `estimate_transform()` and `transform_rois()`.

## Discussion

`wsiTools` focuses on a specific need: making large pathology image
preprocessing practical and reproducible from R. Rather than replacing mature
viewers such as QuPath or low-level libraries such as OpenSlide and libvips, it
wraps their strengths in an R-native API and connects them to tile manifests,
ROI objects, spatial summaries and downstream statistical analysis. This
design is especially useful for bioinformatics groups that already use R for
spatial omics, pathology metadata, model evaluation and reproducible reporting.

The current implementation also has limitations. Native OpenSlide C bindings
are planned but not yet implemented; the first milestone uses command-line
OpenSlide/libvips paths. OME-Zarr support currently reads metadata and pyramid
levels but does not yet decode pixel chunks. Polygon-aware ROI tiling is partly
scaffolded and should be expanded beyond bounding-box workflows. The
side-by-side comparison viewer currently uses thumbnail/placeholder image
sources for simple comparisons, while fully tiled synchronized comparison is a
priority for future work. Finally, Nature Methods-level evaluation will require
formal benchmarks across slide formats, scanners, operating systems and
representative downstream tasks.

Despite these limitations, `wsiTools` provides a practical and extensible
foundation for memory-efficient WSI preprocessing in R. Its emphasis on
backend capability checks, region-based operations, tiled manifests and open
annotation formats should help make computational pathology pipelines more
transparent and reproducible.

## Methods

### Software architecture

`wsiTools` is implemented as an R package using S3 classes and roxygen2
documentation. The package keeps mandatory dependencies light: `cli`,
`jsonlite`, `grDevices`, `graphics` and `utils`. Optional functionality uses
system libraries and suggested R packages such as `magick`, `sf`, OpenSlide
command-line tools and libvips command-line tools.

### Backend detection

Backend availability is detected with command-line checks for OpenSlide,
libvips and Bio-Formats-related tools. `wsi_backends()` returns a data frame
with backend name, installation status, version, capabilities and notes. This
design allows the package to work when only one backend is installed and to
fail informatively when a requested backend is unavailable.

### Slide metadata

`wsi_open()` validates the input path, chooses a backend and returns a
`wsi_slide` object. For libvips-backed images, dimensions and metadata are read
using `vipsheader`. For OpenSlide-backed images, metadata is read using
OpenSlide command-line tools when installed. OME-Zarr metadata are parsed from
local Zarr/NGFF metadata files without reading chunk data.

### Region reading and tiling

All region reads use level-0 coordinates, with width and height expressed in
output pixels at the selected pyramid level. Tile grids are generated from
slide dimensions, tile size, overlap or stride, pyramid level and optional
regions. Tile export calls the same region-reading pathway and records output
metadata in a manifest data frame.

### Conversion

`wsi_convert()` constructs libvips command arguments with shell-safe quoting and
uses `system2()` without invoking a shell. TIFF outputs can be tiled,
pyramidal, BigTIFF and OME-oriented when the local libvips build supports the
requested options.

### ROI and GeoJSON

GeoJSON import uses `jsonlite` to parse FeatureCollection, Feature or geometry
objects. Polygon and multipolygon coordinates are retained as list columns,
with ROI id, name, class, geometry type, bounding box and coordinate reference
metadata when available. GeoJSON export writes FeatureCollections with
QuPath-style `classification$name` properties.

### Color deconvolution

IHC deconvolution converts RGB values to optical density using
`OD = -log(max(I, epsilon))`. User-provided stain vectors are normalized and
completed to a three-vector basis using a residual vector when needed. The
inverse stain matrix estimates per-pixel concentration channels. Outputs can
be returned as concentration matrices or recolored pseudo-images.

### Testing

The package includes testthat tests for backend checks, opening errors,
metadata, region validation, tile grids, coordinate-based tiling, OME-Zarr
metadata parsing, GeoJSON import/export, viewer HTML generation, stain
deconvolution, segmentation import, measurements and affine ROI transforms.
`R CMD check --as-cran` currently reports no errors or warnings in the local
development environment; remaining NOTEs reflect new submission status,
environment time verification and R-generated HTML manual validation warnings.

## Data Availability

No new protected patient dataset is distributed with this software draft.
Example scripts support public H&E BTF images and local non-identifiable WSI
files. Before submission, benchmark datasets and accession information should
be listed here. If human tissue images are used, ethical approval and data-use
conditions must be documented.

## Code Availability

The software is available at https://github.com/tkcaccia/wsiTools under the MIT
license. Before submission, create a versioned release and archive it with a
permanent DOI. Include exact commit hash, R version, package version, system
dependency versions, benchmark scripts and test data.

## Competing Interests

[Insert competing interests statement.]

## Acknowledgements

[Insert funding, institutional and contributor acknowledgements.]

## References

1. Nature Methods. Aims & Scope. https://www.nature.com/nmeth/aims
2. Nature Methods. Content Types. https://www.nature.com/nmeth/content
3. Nature Methods. Submission Guidelines. https://www.nature.com/nmeth/submission-guidelines
4. Bankhead, P. et al. QuPath: Open source software for digital pathology image
   analysis. Scientific Reports 7, 16878 (2017). https://doi.org/10.1038/s41598-017-17204-5
5. OpenSlide. OpenSlide: a C library for reading whole-slide images.
   https://openslide.org/
6. OpenSlide. Virtual slide formats understood by OpenSlide.
   https://openslide.org/formats/
7. libvips. Cite libvips. https://www.libvips.org/API/8.16/Cite.html
8. Moore, J. et al. OME-NGFF: a next-generation file format for expanding
   bioimaging data-access strategies. Nature Methods 18, 1496-1498 (2021).
   https://doi.org/10.1038/s41592-021-01326-w

