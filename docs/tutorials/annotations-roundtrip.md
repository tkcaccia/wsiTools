# Annotate and round-trip

**Outcome:** draw annotations in the live browser, retrieve them in R, save them as
slide-coordinate GeoJSON, and add the saved annotations back to the viewer.

## 1. Start a live tiled viewer

```r
library(wsiTools)

slide <- wsi_open("/path/to/sample.svs")
viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = FALSE
)
viewer$open()
```

## 2. Draw an annotation

In the browser:

1. choose the polygon or brush annotation tool;
2. draw one region over tissue;
3. assign a meaningful class or label when needed; and
4. keep the annotation selected for the next check.

## 3. Retrieve browser state in R

```r
rois <- viewer$get_rois()
selected <- viewer$get_selected_roi()

length(rois)
selected
```

The live session is the important part: a static `file://` viewer cannot push
annotation changes back to R automatically.

## 4. Save GeoJSON

```r
write_geojson(
  rois,
  "annotations.geojson",
  overwrite = TRUE
)
```

wsiTools writes exported coordinates in the original slide coordinate system,
not in the current browser zoom or pan coordinate system.

## 5. Prove the round-trip

Remove or start a fresh viewer, then read and add the saved annotations:

```r
restored <- read_geojson("annotations.geojson")
viewer$add_rois(restored)
```

Use GeoJSON as the primary interchange format for polygon annotations. When
moving between tools, inspect coordinate units, image identity, and class names
rather than assuming every application preserves all metadata identically.

## 6. Finish

```r
viewer$stop()
wsi_close(slide)
```

## Checkpoint

The round-trip is complete when the saved GeoJSON reappears at the same slide
location and `viewer$get_rois()` returns the restored annotation.

## Next

Use the annotation to limit downstream work in
[Segment a selected ROI](segment-selected-roi.md), or create a tissue-aware tile
manifest in [Detect tissue and extract tiles](tissue-and-tiles.md).
