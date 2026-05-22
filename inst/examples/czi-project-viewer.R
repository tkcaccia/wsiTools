# Example: open multiple CZI samples/scenes in one wsiTools project viewer.
#
# Large CZI files remain on disk. The viewer embeds downsampled previews and
# lists all available samples/scenes in the left Project panel.
#
# Usage:
#   WSITOOLS_CZI_FILES="/path/a.czi:/path/b.czi" Rscript inst/examples/czi-project-viewer.R
#
# Optional:
#   WSITOOLS_CZI_PYTHON="/path/to/python-with-aicspylibczi"
#   WSITOOLS_CZI_PROJECT_PREVIEW_WIDTH="4096"
#   WSITOOLS_CZI_PROJECT_OUTPUT="case_project_viewer.html"

library(wsiTools)

czi_files_env <- Sys.getenv("WSITOOLS_CZI_FILES", "")
if (!nzchar(czi_files_env)) {
  stop(
    "Set WSITOOLS_CZI_FILES to one or more CZI paths separated by ",
    .Platform$path.sep,
    call. = FALSE
  )
}

czi_files <- strsplit(czi_files_env, .Platform$path.sep, fixed = TRUE)[[1]]
czi_files <- czi_files[nzchar(czi_files)]
missing <- czi_files[!file.exists(czi_files)]
if (length(missing)) {
  stop("Missing CZI file(s):\n", paste(missing, collapse = "\n"), call. = FALSE)
}

preview_width <- as.integer(Sys.getenv("WSITOOLS_CZI_PROJECT_PREVIEW_WIDTH", "4096"))
if (is.na(preview_width) || preview_width < 512L) {
  preview_width <- 4096L
}

output <- Sys.getenv("WSITOOLS_CZI_PROJECT_OUTPUT", "czi_project_wsiTools_viewer.html")

message("Backend status:")
print(wsi_backends())
message("CZI preview target width: ", preview_width, " px")

html <- wsi_viewer_project(
  czi_files,
  output = output,
  open = interactive(),
  overwrite = TRUE,
  width = preview_width,
  title = "CZI project viewer"
)

message("Viewer written to: ", normalizePath(html, winslash = "/", mustWork = FALSE))
