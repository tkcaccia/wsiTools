# Cover Letter Draft

Dear Editors,

Please consider our manuscript, "`wsiTools`: memory-efficient whole-slide image
access, preprocessing and annotation interoperability in R", for publication in
Nature Methods.

Digital pathology and spatial biology workflows increasingly depend on
whole-slide images (WSIs), yet common analysis environments in R remain poorly
equipped for direct, reproducible work with multi-gigapixel image files. Users
often move between desktop viewers, command-line converters, Python scripts,
manual annotation tools and downstream R analysis, increasing friction and
reducing reproducibility. `wsiTools` provides an open-source R toolkit that
unifies memory-efficient WSI access, tiled processing, conversion, ROI-aware
workflows and lightweight interactive visualization without loading complete
level-0 slides into memory.

The package contributes:

- a backend-aware S3 slide abstraction over OpenSlide, libvips and OME-Zarr
  metadata;
- region-based reading, thumbnails, cropping, tile-grid generation and tile
  extraction;
- libvips-based pyramidal TIFF/OME-TIFF conversion;
- QuPath-compatible GeoJSON ROI import/export and viewer overlays;
- interactive tiled viewers for slide inspection, H&E/IHC comparison, ROI
  creation and stain-channel exploration;
- brightfield immunohistochemistry deconvolution for hematoxylin and HRP/DAB;
- interfaces for external segmentation results, spatial measurements, class
  summaries and simple serial-section registration.

We believe `wsiTools` will be useful to digital pathology, spatial omics,
bioinformatics and machine-learning researchers who need transparent,
scriptable and reproducible WSI preprocessing pipelines in R. The manuscript is
accompanied by open-source code, documentation, examples, test data and
benchmark scripts.

This manuscript is not under consideration elsewhere. All authors have approved
the submission. The authors declare [competing interests statement].

Sincerely,

[Corresponding author name]  
[Institution]  
[Email]

