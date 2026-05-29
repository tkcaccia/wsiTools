# Open a CellPhenotyper output folder in the wsiTools viewer.
# The input H&E image and GigaTIME panel are resolved from
# 00_execution/project_outputs.tsv.

library(wsiTools)

project_dir <- Sys.getenv(
  "WSITOOLS_CELLPHENOTYPER_PROJECT",
  "/Users/stefano/Documents/CellPhenotyper_1927zoom_full_20260527_215129_outputs"
)
viewer_dir <- Sys.getenv("WSITOOLS_VIEWER_DIR", "/Users/stefano/Documents/viewer")
dir.create(viewer_dir, recursive = TRUE, showWarnings = FALSE)

project <- wsi_read_cellphenotyper_project(project_dir)
print(project)

message("H&E input from manifest: ", project$input_image)
if (!is.na(project$files$gigatime_panel) && nzchar(project$files$gigatime_panel)) {
  message("GigaTIME panel from manifest: ", project$files$gigatime_panel)
} else {
  message("No GigaTIME channel panel was found in project_outputs.tsv.")
}

html <- wsi_viewer_cellphenotyper(
  project,
  output = file.path(viewer_dir, "cellphenotyper_project_viewer.html"),
  tile_dir = file.path(viewer_dir, "cellphenotyper_project_viewer_tiles"),
  mode = "tiles",
  overwrite = TRUE,
  open = interactive()
)

message("Viewer written to: ", html)
message("Use the left Project panel to switch between the H&E input and GigaTIME channel panel.")
