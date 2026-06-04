#' Open a whole-slide image
#'
#' `wsi_open()` validates a file path, chooses a backend, and returns a
#' lightweight slide handle. It reads metadata and pyramid information only; it
#' does not load the full slide into R memory.
#'
#' @param path Path to a WSI or large image file.
#' @param backend Backend to use. `"auto"` detects OME-Zarr directories and CZI
#'   files, then prefers OpenSlide when available and falls back to libvips.
#'
#' @return An S3 object of class `wsi_slide`.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' wsi_info(slide)
#' wsi_close(slide)
#' }
wsi_open <- function(path, backend = c("auto", "openslide", "vips", "native_czi", "bioformats", "omezarr", "imagemagick")) {
  backend <- match.arg(backend)
  path <- wsi_validate_input_path(path)
  is_czi <- wsi_is_czi_path(path)

  if (backend == "auto") {
    if (wsi_is_omezarr_path(path)) {
      return(wsi_open_backend_or_abort(path, "omezarr", requested_backend = "auto", is_czi = is_czi))
    }
    candidates <- wsi_auto_backend_candidates(is_czi = is_czi)
    if (!length(candidates)) {
      wsi_abort(
        wsi_backend_failure_message(
          path = path,
          requested_backend = "auto",
          candidates = candidates,
          errors = character(),
          is_czi = is_czi
        ),
        class = "wsi_backend_unavailable"
      )
    }

    errors <- character()
    for (candidate in candidates) {
      slide <- tryCatch(
        switch(
          candidate,
          openslide = wsi_openslide_open(path),
          vips = wsi_vips_open(path),
          native_czi = wsi_native_czi_open(path),
          bioformats = wsi_bioformats_open(path),
          imagemagick = wsi_imagemagick_open(path)
        ),
        error = function(err) {
          errors[[candidate]] <<- conditionMessage(err)
          NULL
        }
      )
      if (!is.null(slide)) {
        return(slide)
      }
    }

    wsi_abort(
      wsi_backend_failure_message(
        path = path,
        requested_backend = "auto",
        candidates = candidates,
        errors = errors,
        is_czi = is_czi
      ),
      class = "wsi_backend_unavailable"
    )
  }

  wsi_open_backend_or_abort(path, backend, requested_backend = backend, is_czi = is_czi)
}

wsi_open_backend_or_abort <- function(path, backend, requested_backend = backend, is_czi = wsi_is_czi_path(path)) {
  tryCatch(
    wsi_open_backend(path, backend),
    error = function(err) {
      wsi_abort(
        wsi_backend_failure_message(
          path = path,
          requested_backend = requested_backend,
          candidates = backend,
          errors = stats::setNames(conditionMessage(err), backend),
          is_czi = is_czi
        ),
        class = "wsi_backend_unavailable"
      )
    }
  )
}

wsi_open_backend <- function(path, backend) {
  switch(
    backend,
    openslide = wsi_openslide_open(path),
    vips = wsi_vips_open(path),
    native_czi = wsi_native_czi_open(path),
    bioformats = wsi_bioformats_open(path),
    omezarr = open_omezarr(path),
    imagemagick = wsi_imagemagick_open(path)
  )
}

wsi_auto_backend_candidates <- function(is_czi,
                                        has_openslide = wsi_has_openslide(),
                                        has_vips = wsi_has_vips(),
                                        has_native_czi = wsi_has_native_czi(),
                                        has_bioformats = wsi_has_bioformats(),
                                        has_imagemagick = wsi_has_imagemagick()) {
  c(
    if (isTRUE(has_openslide)) "openslide",
    if (isTRUE(has_vips)) "vips",
    if (isTRUE(is_czi) && isTRUE(has_native_czi)) "native_czi",
    if (isTRUE(is_czi) && isTRUE(has_bioformats)) "bioformats",
    if (!isTRUE(is_czi) && isTRUE(has_imagemagick)) "imagemagick"
  )
}

wsi_backend_failure_message <- function(path, requested_backend, candidates = character(),
                                        errors = character(), is_czi = FALSE) {
  file_type <- if (isTRUE(is_czi)) "CZI" else "Image"
  candidate_lines <- wsi_backend_candidate_lines(
    path = path,
    requested_backend = requested_backend,
    candidates = candidates,
    errors = errors,
    is_czi = is_czi
  )
  paste(
    sprintf("%s could not be opened.", file_type),
    sprintf("File: %s", normalizePath(path, winslash = "/", mustWork = FALSE)),
    sprintf("Requested backend: %s", requested_backend),
    "",
    "Backends tried or considered:",
    paste0("  - ", candidate_lines, collapse = "\n"),
    "",
    "How to check:",
    "  wsi_backends()",
    "  wsi_diagnose(live_test = FALSE)",
    "",
    "How to install or fix:",
    paste0("  ", wsi_backend_fix_lines(is_czi = is_czi, requested_backend = requested_backend), collapse = "\n"),
    if (isTRUE(is_czi)) paste0("\n", wsi_czi_backend_message(path)) else "",
    sep = "\n"
  )
}

wsi_backend_candidate_lines <- function(path, requested_backend, candidates, errors, is_czi) {
  backend_order <- if (identical(requested_backend, "auto")) {
    if (isTRUE(is_czi)) {
      c("openslide", "vips", "native_czi", "bioformats_java", "bioformats", "imagemagick")
    } else {
      c("openslide", "vips", "imagemagick")
    }
  } else {
    requested_backend
  }

  backend_order <- unique(c(backend_order, names(errors), candidates))
  backend_order <- backend_order[nzchar(backend_order)]
  vapply(
    backend_order,
    function(backend) wsi_backend_candidate_line(backend, candidates, errors, is_czi),
    character(1)
  )
}

wsi_backend_candidate_line <- function(backend, candidates, errors, is_czi) {
  if (backend %in% names(errors)) {
    return(sprintf("%s: failed - %s", backend, wsi_first_error_line(errors[[backend]])))
  }
  if (identical(backend, "imagemagick") && isTRUE(is_czi)) {
    if (wsi_has_imagemagick()) {
      return("imagemagick: available, but not used for CZI because ImageMagick commonly has no CZI delegate")
    }
    return("imagemagick: unavailable, and not a CZI backend")
  }
  available <- wsi_backend_available(backend)
  if (isTRUE(available)) {
    if (backend %in% candidates) {
      return(sprintf("%s: available but did not open the file", backend))
    }
    return(sprintf("%s: available but not selected for this file", backend))
  }
  sprintf("%s: unavailable - %s", backend, wsi_backend_unavailable_reason(backend))
}

wsi_backend_available <- function(backend) {
  switch(
    backend,
    openslide = wsi_has_openslide(),
    vips = wsi_has_vips(),
    native_czi = wsi_has_native_czi(),
    bioformats_java = wsi_has_bioformats_java(),
    bioformats = wsi_has_bioformats(),
    imagemagick = wsi_has_imagemagick(),
    omezarr = TRUE,
    FALSE
  )
}

wsi_backend_unavailable_reason <- function(backend) {
  switch(
    backend,
    openslide = "install OpenSlide command-line tools so `openslide-show-properties` is on PATH",
    vips = "install libvips command-line tools so `vips` and `vipsheader` are on PATH",
    native_czi = "install/configure ZEISS libCZI/libCZIAPI, then check `wsi_has_native_czi()`",
    bioformats_java = "install Java plus `bioformats_package.jar`, set `WSITOOLS_BIOFORMATS_JAR`, then check `wsi_has_bioformats_java()`",
    bioformats = "install OME Bio-Formats command-line tools so `showinf`/`bfconvert` are on PATH",
    imagemagick = "install ImageMagick CLI or the optional R package `magick`",
    omezarr = "input was not recognized as an OME-Zarr directory",
    "backend is not available"
  )
}

wsi_backend_fix_lines <- function(is_czi, requested_backend) {
  if (isTRUE(is_czi)) {
    return(c(
      "wsi_install_native_czi(install = FALSE)",
      "wsi_install_backends(tools = \"bioformats\", install = FALSE)",
      "wsi_install_backends(\"bioformats\")  # after reviewing the install plan",
      "Sys.setenv(WSITOOLS_BIOFORMATS_JAR = \"/path/to/bioformats_package.jar\")"
    ))
  }

  if (identical(requested_backend, "openslide")) {
    return(c(
      "wsi_install_backends(tools = \"openslide\", install = FALSE)",
      "wsi_install_backends(\"openslide\")  # after reviewing the install plan"
    ))
  }
  if (identical(requested_backend, "vips")) {
    return(c(
      "wsi_install_backends(tools = \"libvips\", install = FALSE)",
      "wsi_install_backends(\"libvips\")  # after reviewing the install plan"
    ))
  }
  if (identical(requested_backend, "imagemagick")) {
    return(c(
      "wsi_install_backends(tools = \"imagemagick\", install = FALSE)",
      "wsi_install_backends(\"imagemagick\")  # after reviewing the install plan"
    ))
  }
  if (identical(requested_backend, "bioformats")) {
    return(c(
      "wsi_install_backends(tools = \"bioformats\", install = FALSE)",
      "wsi_install_backends(\"bioformats\")  # after reviewing the install plan"
    ))
  }

  c(
    "wsi_install_backends(tools = c(\"openslide\", \"libvips\", \"imagemagick\"), install = FALSE)",
    "wsi_install_backends(\"libvips\")  # example: install one needed backend after reviewing the plan"
  )
}

wsi_first_error_line <- function(x) {
  lines <- strsplit(wsi_clean_text(x), "\n", fixed = TRUE)[[1L]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines)) lines[[1L]] else "backend returned an empty error message"
}

#' Close a slide handle
#'
#' This is a no-op for command-line backed slides in the first milestone, but it
#' is part of the public API so future native OpenSlide pointers can be released
#' explicitly.
#'
#' @param slide A `wsi_slide` object.
#'
#' @return The slide object, invisibly.
#' @export
wsi_close <- function(slide) {
  wsi_check_slide(slide)
  slide$closed <- TRUE
  invisible(slide)
}

#' Slide metadata
#'
#' @param slide A `wsi_slide` object.
#'
#' @return `wsi_info()` returns a named list. `wsi_levels()` returns a data
#'   frame. `wsi_properties()` returns a named list. `wsi_mpp()` and
#'   `wsi_objective_power()` return numeric metadata when available.
#' @export
wsi_info <- function(slide) {
  wsi_check_slide(slide)
  structure(
    list(
      path = slide$path,
      backend = slide$backend,
      vendor = slide$metadata$vendor %||% NA_character_,
      dimensions = slide$dimensions,
      level_count = nrow(slide$levels),
      levels = wsi_levels(slide),
      mpp = wsi_mpp(slide),
      objective_power = wsi_objective_power(slide),
      tile_size = slide$properties[["openslide.level[0].tile-width"]] %||% NA_character_,
      associated_images = slide$associated_images,
      properties = slide$properties
    ),
    class = "wsi_info"
  )
}

#' @rdname wsi_info
#' @export
wsi_levels <- function(slide) {
  wsi_check_slide(slide)
  slide$levels
}

#' @rdname wsi_info
#' @export
wsi_properties <- function(slide) {
  wsi_check_slide(slide)
  slide$properties
}

#' @rdname wsi_info
#' @export
wsi_mpp <- function(slide) {
  wsi_check_slide(slide)
  props <- slide$properties
  x <- suppressWarnings(as.numeric(props[["openslide.mpp-x"]] %||% props[["mpp-x"]] %||% NA_real_))
  y <- suppressWarnings(as.numeric(props[["openslide.mpp-y"]] %||% props[["mpp-y"]] %||% NA_real_))
  if (is.na(x) && is.na(y)) {
    return(c(x = NA_real_, y = NA_real_))
  }
  c(x = x, y = y)
}

#' @rdname wsi_info
#' @export
wsi_objective_power <- function(slide) {
  wsi_check_slide(slide)
  props <- slide$properties
  suppressWarnings(as.numeric(
    props[["openslide.objective-power"]] %||%
      props[["aperio.AppMag"]] %||%
      props[["objective-power"]] %||%
      NA_real_
  ))
}

#' Associated slide images
#'
#' @param slide A `wsi_slide` object.
#'
#' @return `wsi_associated_images()` returns character names such as `"label"`
#'   and `"macro"` when the backend reports them. `wsi_read_associated_image()`
#'   returns an image when supported.
#' @export
wsi_associated_images <- function(slide) {
  wsi_check_slide(slide)
  slide$associated_images
}

#' @rdname wsi_associated_images
#' @param name Associated image name.
#' @export
wsi_read_associated_image <- function(slide, name) {
  wsi_check_slide(slide)
  if (!name %in% slide$associated_images) {
    wsi_abort(sprintf("Associated image `%s` is not reported by this backend.", name))
  }
  wsi_abort("Reading associated images is not implemented in the first milestone. TODO: wire OpenSlide native or CLI support.")
}

#' Create a mock slide for tests and examples
#'
#' @param width,height Level-0 dimensions.
#' @param levels Downsample factors for pyramid levels.
#'
#' @return A fake `wsi_slide` object.
#' @keywords internal
wsi_mock_slide <- function(width = 10000, height = 8000, levels = c(1, 4, 16)) {
  level_df <- data.frame(
    level = seq_along(levels) - 1L,
    width = ceiling(width / levels),
    height = ceiling(height / levels),
    downsample = levels,
    stringsAsFactors = FALSE
  )
  wsi_make_slide(
    path = NA_character_,
    backend = "mock",
    dimensions = c(width = width, height = height),
    levels = level_df,
    properties = list(
      "openslide.vendor" = "mock",
      "openslide.mpp-x" = "0.25",
      "openslide.mpp-y" = "0.25",
      "openslide.objective-power" = "40"
    ),
    metadata = list(vendor = "mock"),
    associated_images = character()
  )
}
