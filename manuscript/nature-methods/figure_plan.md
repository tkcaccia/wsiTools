# Figure Plan

## Figure 1. Overview of wsiTools

Purpose: show the package architecture and intended pathology workflow.

Panels:

- A. Inputs: SVS, NDPI, SCN, MRXS, BIF, pyramidal TIFF, BTF, OME-TIFF and
  OME-Zarr metadata.
- B. Backend layer: OpenSlide for WSI metadata/regions, libvips for thumbnails,
  conversion, pyramids and Deep Zoom tiles.
- C. R-facing API: `wsi_open()`, `wsi_info()`, `wsi_read_region()`,
  `wsi_tile_grid()`, `wsi_tile()`, `wsi_convert()`, `wsi_viewer()`.
- D. Outputs: tile manifests, OME-TIFF, GeoJSON, segmentation overlays,
  measurements and summaries.

Needed data/artifacts:

- Diagram generated from package functions and example files.

## Figure 2. Memory-efficient WSI access and conversion

Purpose: demonstrate that workflows operate by metadata, pyramid level, region
and tile rather than whole-slide loading.

Panels:

- A. Memory profile for opening a slide, reading a 512 x 512 region and
  generating a tile grid.
- B. Runtime for thumbnail generation and tile extraction across file sizes.
- C. Conversion benchmark from BTF/SVS to pyramidal OME-TIFF using libvips.
- D. Output file size comparison by compression method.

Needed benchmark:

- At least 10 representative slides across Aperio SVS, BigTIFF/BTF and
  OME-TIFF.
- Compare with a baseline workflow such as direct full-image load attempts,
  command-line-only libvips scripts, or Python/OpenSlide scripts.

## Figure 3. Interactive pathology viewer and annotation interoperability

Purpose: show practical use for ROI selection, GeoJSON, H&E/IHC comparison and
segmentation overlays.

Panels:

- A. Tiled full-resolution viewer screenshot.
- B. ROI list side panel with QuPath-compatible GeoJSON polygons.
- C. Drawn ROI exported to GeoJSON and re-imported.
- D. Side-by-side comparison viewer for H&E versus IHC or original versus
  segmentation mask.

Needed data/artifacts:

- Screenshots from browser-based viewer using non-identifiable example slides.
- QuPath-exported GeoJSON and wsiTools-exported GeoJSON round-trip example.

## Figure 4. Case studies

Purpose: demonstrate biological/pathology relevance.

Panels:

- A. H&E BTF image opened and converted to pyramidal OME-TIFF.
- B. SAPC 0052.svs IHC region deconvolved into hematoxylin and HRP/DAB
  channels.
- C. ROI-aware tile manifest for machine-learning patch extraction.
- D. Class-level ROI summaries and cell-density measurements after importing
  external segmentation outputs.

Needed data/artifacts:

- Non-identifiable H&E BTF image.
- IHC slide region outputs.
- Example segmentation CSV/GeoJSON.
- Summary table exported by `summarise_rois()`.

## Extended Data

- Extended Data Fig. 1: backend capability table and informative error examples.
- Extended Data Fig. 2: CRAN/R CMD check and test coverage summary.
- Extended Data Fig. 3: OME-Zarr metadata parsing example.
- Extended Data Fig. 4: reproducibility workflow from raw WSI to tile manifest.

