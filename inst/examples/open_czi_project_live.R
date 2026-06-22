# Open one or more CZI files as a wsiTools project viewer.
#
# Full-resolution live CZI viewing uses the optional native CZI backend. If the
# native backend is not available, this script falls back to the lighter project
# preview viewer unless WSITOOLS_CZI_REQUIRE_LIVE=true.
#
# From R:
#   Sys.setenv(WSITOOLS_CZI_FILES = paste(c("/path/a.czi", "/path/b.czi"), collapse = .Platform$path.sep))
#   source(system.file("examples/open_czi_project_live.R", package = "wsiTools"))
#
# From a shell:
#   WSITOOLS_CZI_FILES="/path/a.czi:/path/b.czi" Rscript inst/examples/open_czi_project_live.R
#
# Optional environment variables:
#   WSITOOLS_CZI_FILES        CZI paths separated by .Platform$path.sep.
#   WSITOOLS_OUTPUT           Output HTML path. Default: open_czi_project_live.html.
#   WSITOOLS_CZI_SECTIONS     true/false. Default: true.
#   WSITOOLS_CZI_CHANNEL      Zero-based CZI channel. Default: 0.
#   WSITOOLS_CZI_PREVIEW      lazy/all. Default: lazy for faster opening.
#   WSITOOLS_CZI_REQUIRE_LIVE true/false. Default: false.
#   WSITOOLS_OPEN             true/false. Default: true.
#   WSITOOLS_WAIT             true/false. Default: true for Rscript, false for interactive R.

library(wsiTools)

example_bool <- function(name, default = FALSE) {
  value <- Sys.getenv(name, if (isTRUE(default)) "true" else "false")
  tolower(value) %in% c("true", "1", "yes", "y", "on")
}

example_paths <- function(name) {
  value <- Sys.getenv(name, "")
  if (!nzchar(value)) {
    stop("Set ", name, " to one or more CZI paths separated by .Platform$path.sep.", call. = FALSE)
  }
  paths <- strsplit(value, .Platform$path.sep, fixed = TRUE)[[1]]
  paths <- normalizePath(paths[nzchar(paths)], winslash = "/", mustWork = FALSE)
  missing <- paths[!file.exists(paths)]
  if (length(missing)) {
    stop("Missing CZI file(s):\n", paste(missing, collapse = "\n"), call. = FALSE)
  }
  paths
}

czi_files <- example_paths("WSITOOLS_CZI_FILES")
output <- Sys.getenv("WSITOOLS_OUTPUT", file.path(getwd(), "open_czi_project_live.html"))
sections <- example_bool("WSITOOLS_CZI_SECTIONS", TRUE)
require_live <- example_bool("WSITOOLS_CZI_REQUIRE_LIVE", FALSE)
open_browser <- example_bool("WSITOOLS_OPEN", TRUE)
wait <- example_bool("WSITOOLS_WAIT", !interactive())
channel <- suppressWarnings(as.integer(Sys.getenv("WSITOOLS_CZI_CHANNEL", "0")))
if (is.na(channel) || channel < 0L) {
  channel <- 0L
}
preview <- tolower(Sys.getenv("WSITOOLS_CZI_PREVIEW", "lazy"))
if (!preview %in% c("lazy", "all")) {
  preview <- "lazy"
}

message("Backend status:")
print(wsi_backends())

if (wsi_has_native_czi()) {
  message("Opening full-resolution live CZI project with native CZI tiles.")
  viewer <- wsi_viewer_czi_project_live(
    czi_files,
    output = output,
    open = open_browser,
    overwrite = TRUE,
    sections = sections,
    czi_preview = preview,
    channel = channel,
    transport = "auto",
    wait = wait,
    name = "czi_project_live_viewer_state"
  )
  assign("czi_project_live_viewer", viewer, envir = .GlobalEnv)
  message("Viewer object saved as `czi_project_live_viewer`.")
  message("Stop the live viewer with: czi_project_live_viewer$stop()")
} else if (isTRUE(require_live)) {
  stop(
    "Native CZI backend is not available. Run wsi_install_native_czi() or set ",
    "WSITOOLS_CZI_REQUIRE_LIVE=false to allow the preview fallback.",
    call. = FALSE
  )
} else {
  message("Native CZI backend is not available; opening static project preview fallback.")
  html <- wsi_viewer_project(
    czi_files,
    output = output,
    open = open_browser,
    overwrite = TRUE,
    czi_sections = sections,
    title = "CZI project viewer"
  )
  assign("czi_project_viewer_html", html, envir = .GlobalEnv)
  message("Viewer HTML saved as `czi_project_viewer_html`: ", normalizePath(html, winslash = "/", mustWork = FALSE))
}
