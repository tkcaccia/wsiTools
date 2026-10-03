# Export and save results

**Outcome:** collect the live viewer state into explicit files that can be checked,
versioned, and reused in a later analysis session.

This tutorial assumes an active live viewer named `viewer`.

## 1. Retrieve current state

```r
rois <- viewer$get_rois()
measurements <- viewer$get_measurements()
annotation_spots <- viewer$get_annotation_spots()
segmentation <- viewer$get_segmentation()
state <- viewer$get_state()
```

Inspect the returned objects before writing them. A viewer may legitimately
return `NULL` or an empty result for a feature that has not been used.

## 2. Write annotations

```r
if (length(rois)) {
  write_geojson(
    rois,
    "case_01_annotations.geojson",
    overwrite = TRUE
  )
}
```

## 3. Write tabular results

```r
if (!is.null(measurements) && NROW(measurements)) {
  write.csv(
    measurements,
    "case_01_measurements.csv",
    row.names = FALSE
  )
}

if (!is.null(annotation_spots) && NROW(annotation_spots)) {
  write.csv(
    annotation_spots,
    "case_01_annotation_spots.csv",
    row.names = FALSE
  )
}
```

Use stable case identifiers in file names and keep slide identity, package
version, and coordinate units with the exported analysis.

## 4. Save project state

```r
viewer$save_project("case_01.wsiproject")
```

The project stores reusable viewer and analysis state in a directory structure.
See the [project format guide](../projects.md) before moving or archiving it.
Large source images and external model files may remain referenced rather than
copied, depending on the workflow.

## 5. Record the software environment

```r
writeLines(
  capture.output(sessionInfo()),
  "case_01_sessionInfo.txt"
)

wsi_setup_report()
```

For a formal pipeline, also record the input image checksum and the exact
wsiTools commit or release used to create results.

## Checkpoint

The export is complete when another analyst can identify the source image,
reopen the project or GeoJSON, inspect tabular results, and see the R environment
used for the analysis.
