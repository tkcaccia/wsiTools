#' Check for native CZI support
#'
#' `wsi_has_native_czi()` reports whether the optional native CZI reader is
#' available. The intended native backend is ZEISS libCZI through a small C/C++
#' bridge. It is kept optional so the CRAN-safe R package can still install on
#' systems without libCZI.
#'
#' @return `TRUE` when native CZI symbols are available, otherwise `FALSE`.
#' @export
#' @examples
#' wsi_has_native_czi()
wsi_has_native_czi <- function() {
  if (!wsi_native_available("wsi_native_czi_available")) {
    return(FALSE)
  }
  isTRUE(tryCatch(.Call("wsi_native_czi_available", PACKAGE = "wsiTools"), error = function(err) FALSE))
}

wsi_native_czi_version <- function() {
  if (!wsi_native_available("wsi_native_czi_version")) {
    return(NA_character_)
  }
  as.character(tryCatch(.Call("wsi_native_czi_version", PACKAGE = "wsiTools"), error = function(err) NA_character_))
}

wsi_native_czi_project_preview <- function(path, width = 768, height = NULL) {
  if (!wsi_has_native_czi()) {
    wsi_abort(
      paste(
        "Native CZI preview is not available in this build.",
        "The planned implementation will use ZEISS libCZI through a small C/C++ bridge.",
        "For now, configure the optional `aicspylibczi` bridge for first visualization.",
        sep = "\n"
      ),
      class = "wsi_backend_unavailable"
    )
  }
  wsi_abort("Native CZI preview is not implemented in this build.", class = "wsi_backend_unavailable")
}

wsi_czi_project_preview <- function(path, width = 768, height = NULL) {
  if (wsi_has_native_czi()) {
    preview <- tryCatch(
      wsi_native_czi_project_preview(path, width = width, height = height),
      error = function(err) err
    )
    if (!inherits(preview, "error") && !is.null(preview)) {
      preview$backend <- "native_czi"
      return(preview)
    }
  }

  preview <- tryCatch(
    wsi_czi_python_project_preview(path, width = width),
    error = function(err) err
  )
  if (!inherits(preview, "error") && !is.null(preview)) {
    preview$backend <- "aicspylibczi"
    return(preview)
  }

  NULL
}
