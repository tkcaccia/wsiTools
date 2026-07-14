# Open your first slide

**Outcome:** open a whole-slide image in a tiled live viewer, inspect its metadata,
and release the viewer and slide when finished.

## 1. Check the installation

```r
library(wsiTools)

wsi_backends()
wsi_setup_report()
wsi_diagnose(live_test = FALSE)
```

The checks report optional capabilities; they do not silently install system
software or model stacks.

## 2. Start without sample data

Use the built-in demonstration when no slide is available:

```r
demo <- wsi_demo_viewer(open = TRUE)

demo$path
demo$files
```

This creates synthetic metadata, small images, annotations, cells, spots, and a
viewer. It does not download a clinical slide.

## 3. Open a real image

The shortest entry point lets wsiTools choose an available backend and viewing
mode:

```r
viewer <- wsi_open_viewer("/path/to/sample.svs")
```

For explicit control, keep the slide and live session as separate objects:

```r
slide <- wsi_open("/path/to/sample.svs")

viewer <- wsi_viewer_live(
  slide,
  tiled = TRUE,
  open = TRUE,
  wait = FALSE
)

viewer$open()
```

Keep the R session running while using a live viewer. Open the
`http://127.0.0.1:<port>` viewer URL printed by R, not the internal
`/viewer-state` endpoint.

## 4. Inspect the slide before analysis

```r
wsi_info(slide)
wsi_levels(slide)
wsi_mpp(slide)
wsi_objective_power(slide)
wsi_properties(slide)
```

These calls help confirm dimensions, pyramid levels, pixel size, and scanner
metadata before choosing regions or tile sizes.

## 5. Close resources cleanly

```r
viewer$stop()
wsi_close(slide)
```

## Checkpoint

You have completed the tutorial when:

- the browser shows a navigable image or the synthetic demonstration;
- metadata and pyramid levels print in R;
- the live viewer uses an HTTP URL; and
- the viewer and slide close without leaving a running session.

## Next

Continue with [Annotate and round-trip](annotations-roundtrip.md), or read the
full [one-image guide](../open-one-image.md) for static viewing, TIFF crops, and
tile export options.
