# First-run guided example for wsiTools.
#
# Run after installing from GitHub:
# source(system.file("examples/first-run-guided-example.R", package = "wsiTools"))

library(wsiTools)

out_dir <- Sys.getenv(
  "WSITOOLS_FIRST_RUN_DIR",
  unset = file.path(tempdir(), "wsiTools-first-run-guided")
)

example <- wsi_first_run_example(
  path = out_dir,
  open = interactive(),
  overwrite = TRUE
)

print(example)

project <- wsi_read_project(example$path)

cat("\nProject manifest:\n")
print(project$manifest$schema)

cat("\nROI classes:\n")
print(table(example$rois$class, useNA = "ifany"))

cat("\nSynthetic segmentation centroids:\n")
print(utils::head(example$centroids))

cat("\nCoordinate-only tile grid:\n")
print(utils::head(example$tile_grid))

cat("\nViewer HTML:\n")
cat(example$viewer, "\n")
