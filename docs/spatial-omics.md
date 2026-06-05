# Spatial Omics

wsiTools links high-resolution tissue images with spatial omics objects without
copying the full expression matrix into the browser. In live mode, the browser
keeps image coordinates, selected spots, selected annotations, and dimensional
reduction selections synchronized with the active R session.

Supported project types:

- Seurat spatial objects;
- Giotto spatial objects;
- SpatialExperiment objects, including multi-slide projects;
- CellPhenotyper output folders.

The common pattern is live by default:

```r
library(wsiTools)

viewer <- wsi_viewer_spatial(
  object,
  image = "/path/to/high_resolution_image.tif",
  dynamic_tiles = TRUE
)

viewer$open()
```

In live mode, these R methods are useful for Seurat, Giotto, and
SpatialExperiment viewers:

```r
viewer$get_selected_spots()
viewer$get_spot_annotation_table()
viewer$colour_spots_by_gene("Mbp")
```

`viewer$colour_spots_by_gene("Mbp")` sends the gene name to the browser, which
uses the live R gene endpoint to fetch only that gene. The full expression
matrix is kept in R.

## Seurat

Example using a Seurat Visium object:

```r
library(wsiTools)
library(Seurat)

brain <- readRDS("/path/to/seurat_spatial_object.rds")
image <- "/path/to/high_resolution_tissue_image.tif"

viewer <- wsi_viewer_seurat(
  brain,
  image = image,
  dynamic_tiles = TRUE,
  coordinate_flip = "none",
  coordinate_rotation = 0
)

viewer$open()
```

Colour spots by a gene from R:

```r
viewer$colour_spots_by_gene("Mbp")
```

Retrieve selected spots after drawing a lasso in the dimensionality-reduction
plot:

```r
selected <- viewer$get_selected_spots()
head(selected)
```

Associate spots with drawn annotations:

```r
spot_annotations <- viewer$get_spot_annotation_table()
utils::write.csv(
  spot_annotations,
  "seurat_spot_annotation_table.csv",
  row.names = FALSE
)
```

If the tissue coordinates are not aligned, adjust the transform parameters:

```r
viewer <- wsi_viewer_seurat(
  brain,
  image = image,
  dynamic_tiles = TRUE,
  coordinate_flip = "vertical",
  coordinate_rotation = 90
)
```

## Giotto

Use the same architecture for Giotto. Giotto remains optional; wsiTools uses
Giotto accessors when the package is installed and can also work with explicit
coordinates and embeddings.

```r
library(wsiTools)
library(Giotto)

gobject <- readRDS("/path/to/giotto_object.rds")
image <- "/path/to/high_resolution_tissue_image.tif"

viewer <- wsi_viewer_giotto(
  gobject,
  image = image,
  dynamic_tiles = TRUE,
  reduction = "PCA"
)

viewer$open()
```

Colour by a gene:

```r
viewer$colour_spots_by_gene("Mbp")
```

Get selected spots and annotation assignments:

```r
selected <- viewer$get_selected_spots()
spot_annotations <- viewer$get_spot_annotation_table()
```

If a Giotto object is unusual or accessors are not available, provide
coordinates and embeddings explicitly:

```r
viewer <- wsi_viewer_giotto(
  gobject,
  image = image,
  coordinates = giotto_coordinates,
  embeddings = giotto_pca,
  dynamic_tiles = TRUE
)
```

## SpatialExperiment

Single-slide SpatialExperiment example:

```r
library(wsiTools)
library(SpatialExperiment)

spe <- readRDS("/path/to/spatialexperiment_object.rds")
image <- "/path/to/high_resolution_tissue_image.tif"

viewer <- wsi_viewer_spatialexperiment(
  spe,
  image = image,
  dynamic_tiles = TRUE,
  reduction = "PCA"
)

viewer$open()
```

Gene colouring and selections:

```r
viewer$colour_spots_by_gene("Mbp")
selected <- viewer$get_selected_spots()
spot_annotations <- viewer$get_spot_annotation_table()
```

Multi-slide SpatialExperiment projects should be opened as one viewer project
so each section has its own image, spots, annotations, and viewer state:

```r
images <- c(
  section_1 = "/path/to/section_1.tif",
  section_2 = "/path/to/section_2.tif",
  section_3 = "/path/to/section_3.tif",
  section_4 = "/path/to/section_4.tif"
)

viewer <- wsi_viewer_spatialexperiment_project(
  spe,
  images = images,
  sample_ids = names(images),
  mode = "tiles",
  output = "spatialexperiment_four_slide_viewer.html",
  open = TRUE,
  wait = FALSE,
  overwrite = TRUE
)
```

For live R feedback on one section at a time, open that section with
`wsi_viewer_spatialexperiment(...)`. When a live viewer is used and
the reduction plot window is set to all tissues, selected spots should remain a
project-wide selection:

```r
selected <- viewer$get_selected_spots()
```

## CellPhenotyper

CellPhenotyper projects are opened from an output directory or from
`00_execution/project_outputs.tsv`. wsiTools reads the manifest, opens the
input image, overlays GigaTIME channels when available, loads cells, and can
display KODAMA/MedSAM refined GeoJSON regions.

```r
library(wsiTools)

project <- wsi_read_cellphenotyper_project(
  "/path/to/CellPhenotyper_outputs"
)

viewer <- wsi_viewer_cellphenotyper(
  project,
  live = TRUE,
  dynamic_tiles = FALSE,
  gigatime_overlay = TRUE
)

viewer$open()
```

Retrieve cell overlays and summaries:

```r
cells <- viewer$get_segmentation()
cell_summary <- viewer$get_cell_summary()
roi_summary <- viewer$get_roi_summary()
```

Retrieve KODAMA plot selections:

```r
kodama_selection <- viewer$get_kodama_selection()
```

CellPhenotyper is cell-based rather than spot-based, so use
`get_segmentation()`, `get_cell_summary()`, and `get_kodama_selection()` for
cell selections. `get_selected_spots()` and `colour_spots_by_gene()` are meant
for Seurat, Giotto, and SpatialExperiment spot overlays.

## Annotation To Spot Table

After drawing annotations in a live spatial viewer, export the spot/annotation
relationship:

```r
spot_annotations <- viewer$get_spot_annotation_table()

utils::write.csv(
  spot_annotations,
  "spot_annotation_table.csv",
  row.names = FALSE
)
```

The table contains annotation identifiers/classes plus spot identifiers and
slide coordinates when the browser has calculated the associations.

## Tile Extraction Around Spots

Spatial viewers can preview and export spot-centered tiles from the R session.
If an annotation is selected, only tiles inside that annotation are generated.

In the browser, open the Seurat/Giotto/SpatialExperiment menu and use the tile
window. From R, retrieve the latest preview:

```r
tile_preview <- viewer$get_tile_preview()
```

For scripted tile extraction, use the spatial tile helpers or ordinary WSI tile
functions with the selected annotation:

```r
rois <- viewer$get_selected_rois()
```

## Prediction And Proximity

Spatial viewers can expose prediction and proximity workflows when live R is
available. These results are ordinary R data frames:

```r
prediction <- viewer$get_prediction()
proximity <- viewer$get_proximity()
```

The prediction menu is intended for Seurat, Giotto, SpatialExperiment, and
CellPhenotyper projects where annotation-defined training and test sets are
available.

## Troubleshooting

Problem: gene colouring does not work.

Meaning: the viewer is probably static, the live R session is not running, or
the object does not expose expression values for that gene.

Check:

```r
wsi_backends()
viewer$capabilities()
```

Fix: open the object with the default live viewer, keep R running, and call:

```r
viewer$colour_spots_by_gene("Mbp")
```

Problem: spots are shifted relative to the tissue image.

Fix: adjust coordinate transforms:

```r
coordinate_flip = "vertical"
coordinate_rotation = 90
```

For Visium, if microns-per-pixel metadata are unavailable, wsiTools can infer
scale from spot spacing where possible. Standard Visium spots are 55 microns in
diameter with 100 microns center-to-center spacing.

Problem: all tissue selections disappear when switching sections.

Meaning: the viewer may be in current-tissue selection mode.

Fix: use the all-tissues option in the dimensionality-reduction plot window
when you want a selection to remain project-wide.
