# Project Format

wsiTools projects are saved as `.wsiproject` directories. A project records the
analysis state and lightweight tabular/annotation outputs, but it does not copy
or embed whole-slide image pixels. Slide images remain external files referenced
by path.

The project format is intended for:

- reopening an analysis session;
- preserving annotations, measurements, segmentation overlays, and tile
  manifests;
- tracking stain/channel settings and viewer state;
- recording processing provenance for reproducible pipelines;
- keeping project contents inspectable as JSON, GeoJSON, and CSV files.

## Basic Workflow

Create a project from a live viewer:

```r
library(wsiTools)

slide <- wsi_open("/path/to/case_001.svs")
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()

# After drawing annotations or running analysis:
viewer$save_project("case_001.wsiproject")
```

Or build a project explicitly from R objects:

```r
project <- wsi_project(
  slide = slide,
  viewer_state = viewer$get_state(),
  rois = viewer$get_rois(),
  measurements = viewer$get_measurements(),
  segmentation = viewer$get_segmentation(),
  tile_manifest = viewer$get_tile_preview(),
  metadata = list(case_id = "case_001"),
  processing_provenance = list(
    steps = list(
      list(name = "manual annotation", tool = "wsiTools viewer"),
      list(name = "tile preview", tile_size = 512)
    )
  )
)

wsi_save_project(project, "case_001.wsiproject", overwrite = TRUE)
```

Reopen later:

```r
project <- wsi_read_project("case_001.wsiproject")
project$rois
project$tile_manifest
```

Restore into a live viewer:

```r
slide <- wsi_open(project$slide_path)
viewer <- wsi_viewer_live(slide, tiled = TRUE)
viewer$open()

restore_project_state(viewer, project)
```

## Directory Layout

A typical `.wsiproject` directory looks like this:

```text
case_001.wsiproject/
  project.json
  rois.geojson
  segmentation.geojson
  measurements.csv
  measurements/
    roi_summary.csv
    class_summary.csv
    cell_summary.csv
    ihc_summary.csv
  tile_manifests/
    tile_manifest.csv
```

Not every file is always present. wsiTools writes sidecar files only when the
corresponding object exists.

## `project.json`

`project.json` is the project index. It stores:

- schema name and version;
- creation time;
- wsiTools package version;
- slide identity and slide path;
- a compact viewer-state summary;
- stain/channel settings;
- paths to sidecar files;
- user metadata;
- processing provenance.

The slide path is a reference to the source image. The WSI itself is not copied
into the project directory.

## Slide Paths

The project stores slide metadata such as:

- original file path;
- backend used when the project was saved;
- slide dimensions;
- pyramid levels when available;
- microns-per-pixel when available;
- objective power when available.

When reopening a project:

```r
project <- wsi_read_project("case_001.wsiproject", open_slide = TRUE)
```

`open_slide = TRUE` tries to reopen the recorded slide path. If the slide has
moved, update the path in your workflow or reopen the slide manually and restore
the project state into a new live viewer.

## Annotations

Manual annotations and imported GeoJSON annotations are stored as
`rois.geojson` when present. This file is intended to remain compatible with
QuPath-style polygon workflows where possible.

Annotations can include:

- ROI identifiers;
- annotation names;
- classes or labels;
- colours;
- polygon or multipolygon geometry;
- measurements attached to annotation properties where available.

Read them directly:

```r
project <- wsi_read_project("case_001.wsiproject")
rois <- project$rois
```

Export them again:

```r
write_geojson(project$rois, "edited_annotations.geojson", overwrite = TRUE)
```

## Trajectories

Trajectory objects are saved as part of the viewer-state summary in
`project.json`. They include the trajectory coordinates and display/editing
state needed to restore them in a live viewer.

Trajectory data are useful for:

- analysing gradients across tissue;
- proximity analysis;
- path-based region inspection;
- documenting manually drawn tissue axes.

## Segmentation

Cell or object segmentation overlays are stored separately from annotation ROIs.
Depending on the input, wsiTools may write:

- `segmentation.geojson` for polygon or centroid-converted cell overlays;
- `segmentation.csv` for tabular centroid outputs.

Segmentation may come from:

- CellPhenotyper projects;
- StarDist or Mesmer outputs;
- imported GeoJSON;
- centroid CSV/TSV tables;
- mask images converted to ROI polygons.

Access after reopening:

```r
project <- wsi_read_project("case_001.wsiproject")
cells <- project$segmentation
```

## Measurements

Measurement outputs are saved as CSV files when available. A simple project may
contain `measurements.csv`. A richer measurement report may create a
`measurements/` directory with multiple tables, for example:

- ROI summary;
- class summary;
- cell summary;
- IHC intensity summary;
- distance measurements;
- density measurements.

Retrieve in R:

```r
measurements <- project$measurements
```

Generate a report:

```r
case_report <- wsi_case_report(
  project,
  output_dir = "case_001_report",
  overwrite = TRUE
)
```

## Channel Settings

The project stores stain and channel display settings so the viewer can restore
how overlays were shown. This can include:

- visible/hidden channels;
- opacity;
- colour;
- gain and contrast settings when available;
- H&E, H-DAB, or mIHC channel preferences;
- dynamic or precomputed channel tile-source metadata.

Channel settings are small metadata records. The channel image files or tile
pyramids remain external and are referenced by path or tile-source metadata.

## Viewer State

The viewer state summary records browser-side state that is useful for
reopening a session:

- selected ROI IDs;
- selected object;
- layers;
- segmentation overlay summary;
- measurements;
- trajectories;
- stain/channel state;
- tile-source metadata;
- viewport position and zoom when available;
- annotation history when available;
- tile preview state.

Use:

```r
state <- viewer$get_state()
```

to inspect the current live state before saving.

## Tile Manifests

Tile manifests are stored as CSV files in `tile_manifests/`. A manifest can
record:

- tile ID;
- x/y coordinates;
- width and height;
- pyramid level;
- row and column;
- downsample factor;
- associated ROI;
- tissue fraction or QC fields when available;
- output filename;
- train/validation split;
- seed and sampling/provenance fields.

Tile manifests do not require reading tile pixels. They are useful for ML
pipelines because they preserve exactly which coordinates were selected for
export.

Example:

```r
tiles <- extract_tiles(
  slide,
  roi = viewer$get_selected_rois(),
  tile_size = 512,
  stride = 512,
  save_images = FALSE,
  seed = 1
)

project <- wsi_project(
  slide = slide,
  tile_manifest = tiles
)

wsi_save_project(project, "tiles_only.wsiproject", overwrite = TRUE)
```

## Provenance

`processing_provenance` records reproducibility information. wsiTools adds basic
package and session metadata, and users can add workflow-specific entries:

```r
project <- wsi_project(
  slide = slide,
  rois = viewer$get_rois(),
  processing_provenance = list(
    pipeline = "manual_roi_tile_export",
    operator = "analyst_01",
    steps = list(
      list(name = "open slide", backend = slide$backend),
      list(name = "draw annotations", tool = "wsiTools live viewer"),
      list(name = "extract tiles", tile_size = 512, stride = 512)
    )
  )
)
```

Good provenance entries include:

- software versions;
- model names and versions;
- command-line arguments;
- random seeds;
- tile sizes and thresholds;
- annotation sources;
- input and output paths;
- date/time and operator notes.

## Autosave

Live viewers can autosave into a `.wsiproject` folder:

```r
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  autosave = TRUE,
  autosave_path = "case_001_autosave.wsiproject"
)

viewer$open()
```

Autosave helps protect:

- annotations;
- selected ROI;
- measurements;
- stain settings;
- visible layers;
- viewport state;
- segmentation overlays.

## What Is Not Stored

To keep projects lightweight and Git-friendly, `.wsiproject` does not store:

- full WSI pixel data;
- large SVS/CZI/OME-TIFF files;
- complete expression matrices unless explicitly exported elsewhere;
- external model files or Python environments;
- full Deep Zoom tile pyramids unless the user stores those separately.

Store large image files and model outputs in a stable project directory, then
keep the `.wsiproject` as the lightweight index that points to them.

## Practical Recommendation

For a reproducible analysis, keep this structure:

```text
case_001/
  images/
    case_001.svs
  analysis/
    case_001.wsiproject/
    case_001_report/
  tiles/
  models/
```

Then use relative or stable absolute paths whenever possible. This makes the
project easier to reopen on another workstation or remote server.
