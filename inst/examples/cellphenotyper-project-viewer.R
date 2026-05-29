# Open a CellPhenotyper output folder in the wsiTools viewer.
# The input H&E image and GigaTIME probability OME-TIFF are resolved from
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
if (!is.na(project$files$gigatime_probs) && nzchar(project$files$gigatime_probs)) {
  message("GigaTIME OME-TIFF from manifest: ", project$files$gigatime_probs)
} else {
  message("No GigaTIME probability OME-TIFF was found in project_outputs.tsv.")
}
if (is.data.frame(project$files$kodama_geojson) && nrow(project$files$kodama_geojson)) {
  message("KODAMA/MedSAM refined GeoJSON files: ", nrow(project$files$kodama_geojson))
  print(project$files$kodama_geojson[, c("label", "path"), drop = FALSE])
} else {
  message("No KODAMA/MedSAM refined GeoJSON was found in project_outputs.tsv.")
}

viewer <- wsi_viewer_cellphenotyper(
  project,
  output = file.path(viewer_dir, "cellphenotyper_project_viewer.html"),
  tile_dir = file.path(viewer_dir, "cellphenotyper_project_viewer_tiles"),
  mode = "tiles",
  overwrite = TRUE,
  open = interactive(),
  wait = FALSE
)

assign("cellphenotyper_viewer", viewer, envir = .GlobalEnv)
message("Viewer object saved as `cellphenotyper_viewer`.")
message("Use the top Stains menu to show/hide GigaTIME marker channels over the H&E.")
message("Use the top KODAMA menu to load MedSAM-refined KODAMA GeoJSON regions as annotations.")
message("Inspect channel settings with: cellphenotyper_viewer$get_channel_settings()")
