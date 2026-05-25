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

  if (backend == "auto") {
    if (wsi_is_omezarr_path(path)) {
      return(open_omezarr(path))
    }
    is_czi <- wsi_is_czi_path(path)
    candidates <- c(
      if (is_czi && wsi_has_native_czi()) "native_czi",
      if (wsi_has_openslide()) "openslide",
      if (wsi_has_vips()) "vips",
      if (is_czi && wsi_has_bioformats()) "bioformats",
      if (!is_czi && wsi_has_imagemagick()) "imagemagick"
    )
    if (!length(candidates)) {
      if (is_czi) {
        wsi_abort(wsi_czi_backend_message(path), class = "wsi_backend_unavailable")
      }
      wsi_abort(
        "No WSI/image backend is available. Install OpenSlide, libvips, or ImageMagick, then retry.",
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
      paste(
        "No available backend could open this file.",
        paste(sprintf("%s: %s", names(errors), errors), collapse = "\n"),
        if (is_czi) paste0("\n", wsi_czi_backend_message(path)) else ""
      )
    )
  }

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
