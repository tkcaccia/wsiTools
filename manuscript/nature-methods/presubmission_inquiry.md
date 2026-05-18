# Presubmission Inquiry Draft

Dear Nature Methods Editors,

We are writing to inquire whether you would consider a manuscript describing
`wsiTools`, an open-source R toolkit for memory-efficient access,
preprocessing, annotation interoperability, tiling, conversion, and exploratory
visualization of whole-slide pathology images.

Whole-slide images are central to computational pathology, spatial omics and
machine-learning patch extraction, but their size and proprietary formats make
reproducible analysis difficult in R. `wsiTools` addresses this gap by exposing
a region-based, backend-aware interface to OpenSlide and libvips, together with
tile manifests, pyramidal TIFF/OME-TIFF conversion, QuPath-compatible GeoJSON
ROI import/export, interactive tiled viewers, color deconvolution for
brightfield immunohistochemistry, and optional bridges to external segmentation
tools. The package is designed to avoid loading complete level-0 slides into R
memory and instead uses pyramid levels, tiled processing and streaming backend
operations.

We envisage the manuscript as a Resource or software Article. The accompanying
validation package will benchmark metadata extraction, region reading,
thumbnail generation, tile extraction, BTF/SVS-to-OME-TIFF conversion, ROI-aware
tiling and stain deconvolution across representative Aperio SVS, BigTIFF/BTF,
OME-TIFF and OME-Zarr examples. We will also provide reproducible case studies
showing integration with QuPath GeoJSON annotations, H&E/IHC comparison, and
machine-learning patch manifests for downstream pathology pipelines.

The software is open source under the MIT license and available at
https://github.com/tkcaccia/wsiTools. A versioned release, archived DOI, test
data and benchmark scripts will accompany the submission.

We would be grateful for your advice on whether this manuscript would be
suitable for consideration by Nature Methods.

Sincerely,

[Corresponding author name]  
[Institution]  
[Email]

