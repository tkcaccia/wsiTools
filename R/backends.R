#' Check available WSI backends and optional runtime components
#'
#' `wsi_backends()` reports whether optional command-line backends and runtime
#' helpers are available. The package can be installed with no WSI backend
#' present; backend-dependent functions fail with informative errors when a
#' required tool is missing.
#'
#' @return A data frame with backend name, installation status, version,
#'   capabilities, and notes.
#' @export
#' @examples
#' wsi_backends()
wsi_backends <- function() {
  data.frame(
    backend = c(
      "openslide",
      "libvips",
      "bioformats",
      "bioformats_java",
      "imagemagick",
      "native_czi",
      "aicspylibczi",
      "httpuv",
      "callr",
      "stardist",
      "cellpose"
    ),
    installed = c(
      wsi_has_openslide(),
      wsi_has_vips(),
      wsi_has_bioformats(),
      wsi_has_bioformats_java(),
      wsi_has_imagemagick(),
      wsi_has_native_czi(),
      wsi_has_czi_python(),
      requireNamespace("httpuv", quietly = TRUE),
      wsi_has_callr(),
      wsi_has_stardist(),
      wsi_has_cellpose()
    ),
    version = c(
      wsi_command_version("openslide-show-properties"),
      wsi_command_version("vips"),
      wsi_bioformats_version(),
      wsi_bioformats_java_version(),
      wsi_first_available_version(wsi_command_version("magick"), wsi_optional_package_version("magick")),
      wsi_native_czi_version(),
      wsi_czi_python_version(),
      wsi_optional_package_version("httpuv"),
      wsi_optional_package_version("callr"),
      wsi_command_version(wsi_default_stardist_command()),
      wsi_command_version(wsi_default_cellpose_command())
    ),
    capabilities = c(
      "metadata, pyramid levels, region reads via openslide-write-png when available",
      "large-image conversion, thumbnails, cropping, pyramidal TIFF export",
      "optional microscopy format bridge for CZI and other Bio-Formats-readable files",
      "optional Bio-Formats region reader through a small Java ImageReader helper",
      "ordinary image metadata, thumbnails, and level-0 crop previews",
      "optional native CZI region/tile reader through ZEISS libCZI",
      "optional CZI mosaic metadata and lightweight preview generation",
      "live R viewer bridge, browser-to-R events, selected-ROI segmentation endpoint",
      "non-blocking background jobs for segmentation, tiling, conversion, and pyramids",
      "optional external selected-ROI cell segmentation command",
      "optional external selected-ROI cell segmentation command"
    ),
    notes = c(
      "Requires OpenSlide command-line tools for this milestone; native C bindings are planned.",
      "Requires vips and vipsheader on PATH.",
      "Optional backend; install Bio-Formats command-line tools (`showinf`/`bfconvert`) for metadata and conversion workflows.",
      "Optional first-visualization helper. Requires Java plus `bioformats_package.jar`; set WSITOOLS_BIOFORMATS_JAR when needed.",
      "Optional fallback for standard TIFF/PNG/JPEG previews; install libvips/OpenSlide for WSI-scale tiled viewing.",
      "Optional direct CZI backend. Install ZEISS libCZI/libCZIAPI and set WSITOOLS_LIBCZIAPI if it is not on the dynamic library path.",
      "Legacy optional fallback. Only used for CZI previews when WSITOOLS_CZI_ALLOW_PYTHON=true.",
      "Suggested R package; not required for static viewers or package installation.",
      "Suggested R package; not required unless async jobs are requested.",
      "Optional command. Set WSITOOLS_STARDIST_COMMAND or pass command/args when running.",
      "Optional command. Set WSITOOLS_CELLPOSE_COMMAND or import Cellpose results manually."
    ),
    stringsAsFactors = FALSE
  )
}

#' @rdname wsi_backends
#' @return `TRUE` or `FALSE` for `wsi_has_*()` helpers.
#' @export
wsi_has_openslide <- function() {
  wsi_command_exists("openslide-show-properties")
}

#' @rdname wsi_backends
#' @export
wsi_has_vips <- function() {
  wsi_command_exists("vips") && wsi_command_exists("vipsheader")
}

#' @rdname wsi_backends
#' @export
wsi_has_bioformats <- function() {
  wsi_has_bioformats_java() ||
    wsi_command_exists(wsi_bioformats_command("bfconvert")) ||
    wsi_command_exists(wsi_bioformats_command("showinf"))
}

#' @rdname wsi_backends
#' @param python Optional Python executable used to check the legacy CZI
#'   preview bridge. When `NULL`, `WSITOOLS_CZI_PYTHON` and then `python3` are
#'   checked.
#' @export
wsi_has_czi_python <- function(python = NULL) {
  python <- wsi_czi_python_command(python)
  if (!wsi_command_exists(python)) {
    return(FALSE)
  }
  out <- tryCatch(
    suppressWarnings(system2(
      python,
      args = c("-c", shQuote("import aicspylibczi, numpy, PIL")),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  identical(as.integer(status), 0L)
}

#' @rdname wsi_backends
#' @param command Optional command to check for command-backed helpers such as
#'   Cellpose.
#' @export
wsi_has_cellpose <- function(command = NULL) {
  command <- wsi_default_cellpose_command(command)
  is.character(command) && length(command) == 1L && nzchar(command) && wsi_command_exists(command)
}

wsi_optional_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    return(NA_character_)
  }
  as.character(utils::packageVersion(package))
}

wsi_first_available_version <- function(...) {
  versions <- c(...)
  versions <- versions[!is.na(versions) & nzchar(versions)]
  if (length(versions)) versions[[1L]] else NA_character_
}

wsi_default_cellpose_command <- function(command = NULL) {
  if (!is.null(command)) {
    return(command)
  }
  env_command <- Sys.getenv("WSITOOLS_CELLPOSE_COMMAND", unset = "")
  if (nzchar(env_command)) {
    return(env_command)
  }
  if (wsi_command_exists("cellpose")) {
    return("cellpose")
  }
  ""
}

wsi_bioformats_version <- function() {
  java <- wsi_bioformats_java_version()
  if (!is.na(java)) {
    return(java)
  }
  bfconvert <- wsi_bioformats_command("bfconvert")
  showinf <- wsi_bioformats_command("showinf")
  if (wsi_command_exists(bfconvert)) {
    return(wsi_command_version(bfconvert, "-version"))
  }
  if (wsi_command_exists(showinf)) {
    return(wsi_command_version(showinf, "-version"))
  }
  NA_character_
}

wsi_bioformats_java_command <- function(command = NULL) {
  if (!is.null(command)) {
    return(command)
  }
  env <- Sys.getenv("WSITOOLS_JAVA", unset = "")
  if (nzchar(env)) {
    return(env)
  }
  java <- Sys.which("java")
  if (nzchar(java)) unname(java) else "java"
}

wsi_bioformats_javac_command <- function(command = NULL) {
  if (!is.null(command)) {
    return(command)
  }
  env <- Sys.getenv("WSITOOLS_JAVAC", unset = "")
  if (nzchar(env)) {
    return(env)
  }
  javac <- Sys.which("javac")
  if (nzchar(javac)) unname(javac) else "javac"
}

wsi_bioformats_java_jar <- function(jar = NULL) {
  if (!is.null(jar) && nzchar(jar)) {
    return(jar)
  }
  env <- Sys.getenv("WSITOOLS_BIOFORMATS_JAR", unset = "")
  if (nzchar(env)) {
    return(env)
  }
  home <- Sys.getenv("WSITOOLS_BIOFORMATS_HOME", unset = "")
  candidates <- character()
  if (nzchar(home)) {
    candidates <- c(
      file.path(home, "bioformats_package.jar"),
      file.path(home, "loci_tools.jar"),
      file.path(home, "artifacts", "bioformats_package.jar")
    )
  }
  candidates <- c(
    candidates,
    file.path(getwd(), "bioformats_package.jar"),
    file.path(getwd(), "loci_tools.jar")
  )
  existing <- candidates[file.exists(candidates)]
  if (length(existing)) {
    return(existing[[1L]])
  }
  ""
}

wsi_has_bioformats_java <- function(jar = NULL, java = NULL) {
  jar <- wsi_bioformats_java_jar(jar)
  java <- wsi_bioformats_java_command(java)
  nzchar(jar) && file.exists(jar) && wsi_command_exists(java)
}

wsi_bioformats_java_version <- function(jar = NULL, java = NULL) {
  if (!wsi_has_bioformats_java(jar = jar, java = java)) {
    return(NA_character_)
  }
  out <- tryCatch(
    wsi_bioformats_java_run(c("version"), jar = jar, java = java),
    error = function(err) character()
  )
  if (!length(out)) {
    return("Bio-Formats Java helper")
  }
  parsed <- tryCatch(jsonlite::fromJSON(paste(out, collapse = "\n")), error = function(err) NULL)
  version <- parsed$bioformats_version %||% NA_character_
  if (is.na(version) || !nzchar(version)) {
    return("Bio-Formats Java helper")
  }
  paste("Bio-Formats", version)
}

wsi_bioformats_command <- function(command = c("bfconvert", "showinf")) {
  command <- match.arg(command)
  path <- Sys.which(command)
  if (nzchar(path)) {
    return(unname(path))
  }
  home <- Sys.getenv("WSITOOLS_BIOFORMATS_HOME", unset = "")
  if (nzchar(home)) {
    candidate <- file.path(home, command)
    if (.Platform$OS.type == "windows" && !file.exists(candidate)) {
      candidate <- paste0(candidate, ".bat")
    }
    if (file.exists(candidate)) {
      return(candidate)
    }
  }
  command
}

wsi_czi_python_command <- function(python = NULL) {
  if (!is.null(python)) {
    return(python)
  }
  env_python <- Sys.getenv("WSITOOLS_CZI_PYTHON", unset = "")
  if (nzchar(env_python)) {
    return(env_python)
  }
  py <- Sys.which("python3")
  if (nzchar(py)) unname(py) else "python3"
}

wsi_czi_python_version <- function(python = NULL) {
  python <- wsi_czi_python_command(python)
  if (!wsi_command_exists(python)) {
    return(NA_character_)
  }
  out <- tryCatch(
    suppressWarnings(system2(
      python,
      args = c("-c", shQuote("import aicspylibczi; print('aicspylibczi ' + getattr(aicspylibczi, '__version__', 'unknown'))")),
      stdout = TRUE,
      stderr = TRUE
    )),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L) || !length(out)) {
    return(NA_character_)
  }
  trimws(out[[1L]])
}

wsi_choose_open_backend <- function() {
  if (wsi_has_openslide()) {
    return("openslide")
  }
  if (wsi_has_vips()) {
    return("vips")
  }
  if (wsi_has_imagemagick()) {
    return("imagemagick")
  }
  wsi_abort(
    "No WSI/image backend is available. Install OpenSlide, libvips, or ImageMagick, then retry.",
    class = "wsi_backend_unavailable"
  )
}

wsi_choose_region_backend <- function(slide, backend = c("auto", "vips", "openslide", "native_czi", "bioformats", "imagemagick")) {
  backend <- match.arg(backend)
  if (backend != "auto") {
    return(backend)
  }
  if (identical(slide$backend, "native_czi")) {
    return("native_czi")
  }
  if (wsi_has_vips()) {
    return("vips")
  }
  if (identical(slide$backend, "openslide") && wsi_command_exists("openslide-write-png")) {
    return("openslide")
  }
  if (identical(slide$backend, "bioformats")) {
    return("bioformats")
  }
  if (identical(slide$backend, "imagemagick")) {
    return("imagemagick")
  }
  slide$backend
}
