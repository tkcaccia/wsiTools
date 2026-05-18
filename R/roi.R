wsi_geojson_features <- function(x) {
  if (identical(x$type, "FeatureCollection")) {
    return(x$features %||% list())
  }
  if (identical(x$type, "Feature")) {
    return(list(x))
  }
  if (!is.null(x$geometry)) {
    return(list(list(type = "Feature", geometry = x$geometry, properties = x$properties %||% list())))
  }
  wsi_abort("GeoJSON must be a FeatureCollection, Feature, or object with a geometry field.")
}

wsi_collect_points <- function(coords) {
  if (is.null(coords)) {
    return(matrix(numeric(), ncol = 2))
  }
  if (is.numeric(coords) && length(coords) >= 2L) {
    return(matrix(as.numeric(coords[1:2]), ncol = 2))
  }
  if (is.list(coords) && length(coords) >= 2L && all(vapply(coords[1:2], is.numeric, logical(1)))) {
    return(matrix(c(as.numeric(coords[[1L]]), as.numeric(coords[[2L]])), ncol = 2))
  }
  if (is.list(coords)) {
    mats <- lapply(coords, wsi_collect_points)
    mats <- mats[vapply(mats, nrow, integer(1)) > 0L]
    if (length(mats)) {
      return(do.call(rbind, mats))
    }
  }
  matrix(numeric(), ncol = 2)
}

wsi_roi_name <- function(properties, index) {
  classification <- properties$classification$name %||% properties$class %||% properties$classification %||% NA_character_
  properties$name %||% properties$label %||% properties$annotation_label %||% classification %||% sprintf("roi_%d", index)
}

wsi_empty_roi <- function() {
  roi <- data.frame(
    roi_id = character(),
    name = character(),
    class = character(),
    geometry_type = character(),
    xmin = numeric(),
    ymin = numeric(),
    xmax = numeric(),
    ymax = numeric(),
    crs = character(),
    stringsAsFactors = FALSE
  )
  roi$coordinates <- I(list())
  class(roi) <- c("wsi_roi", class(roi))
  roi
}

wsi_roi_from_geojson <- function(geojson) {
  features <- wsi_geojson_features(geojson)

  if (!length(features)) {
    return(wsi_empty_roi())
  }

  rows <- lapply(seq_along(features), function(i) {
    feature <- features[[i]]
    geometry <- feature$geometry %||% list()
    properties <- feature$properties %||% list()
    points <- wsi_collect_points(geometry$coordinates)
    if (!nrow(points)) {
      wsi_abort(sprintf("GeoJSON feature %d does not contain polygon coordinates.", i))
    }
    classification <- properties$classification$name %||% properties$class %||% properties$classification %||% NA_character_
    data.frame(
      roi_id = as.character(feature$id %||% properties$id %||% i),
      name = as.character(wsi_roi_name(properties, i)),
      class = as.character(classification %||% NA_character_),
      geometry_type = as.character(geometry$type %||% NA_character_),
      xmin = min(points[, 1L]),
      ymin = min(points[, 2L]),
      xmax = max(points[, 1L]),
      ymax = max(points[, 2L]),
      crs = as.character(geojson$crs$properties$name %||% NA_character_),
      stringsAsFactors = FALSE
    )
  })

  roi <- do.call(rbind, rows)
  roi$coordinates <- I(lapply(features, function(feature) feature$geometry$coordinates))
  class(roi) <- c("wsi_roi", class(roi))
  roi
}

#' Read QuPath-style GeoJSON annotations
#'
#' Reads GeoJSON annotations and returns lightweight ROI metadata with list
#' columns for geometry coordinates. Polygon and multipolygon coordinates are
#' supported in the first milestone.
#'
#' @param path GeoJSON file path.
#'
#' @return A `wsi_roi` data frame.
#' @export
wsi_read_geojson <- function(path) {
  path <- wsi_validate_input_path(path)
  geojson <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  wsi_roi_from_geojson(geojson)
}

#' @rdname wsi_read_geojson
#' @param file GeoJSON file path.
#' @export
read_geojson <- function(file) {
  wsi_read_geojson(file)
}

#' Write ROI annotations as GeoJSON
#'
#' Writes `wsi_roi` objects as QuPath-compatible GeoJSON FeatureCollections.
#'
#' @param rois A `wsi_roi` object.
#' @param file Output GeoJSON path.
#' @param overwrite Whether to overwrite an existing file.
#'
#' @return The output path, invisibly.
#' @export
write_geojson <- function(rois, file, overwrite = FALSE) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  file <- wsi_validate_output_path(file, overwrite = overwrite)
  features <- lapply(seq_len(nrow(rois)), function(i) {
    class_name <- rois$class[[i]]
    properties <- list(
      name = rois$name[[i]],
      label = rois$name[[i]],
      classification = list(name = class_name %||% "annotation"),
      class = class_name
    )
    list(
      type = "Feature",
      id = rois$roi_id[[i]],
      properties = properties,
      geometry = list(
        type = rois$geometry_type[[i]],
        coordinates = rois$coordinates[[i]]
      )
    )
  })
  geojson <- list(type = "FeatureCollection", features = features)
  jsonlite::write_json(geojson, file, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(file)
}

#' @rdname write_geojson
#' @export
wsi_write_geojson <- write_geojson

#' Assign pathology labels to ROIs
#'
#' @param rois A `wsi_roi` object.
#' @param label Label to assign, for example `"tumour"`, `"stroma"`,
#'   `"necrosis"`, `"normal"`, `"artefact"`, or `"exclusion"`.
#' @param roi_id Optional ROI ids to update. When `NULL`, all ROIs are updated.
#'
#' @return A modified `wsi_roi` object.
#' @export
wsi_set_roi_class <- function(rois, label, roi_id = NULL) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    wsi_abort("`label` must be a single non-empty character value.")
  }
  idx <- if (is.null(roi_id)) {
    seq_len(nrow(rois))
  } else {
    match(as.character(roi_id), rois$roi_id)
  }
  if (anyNA(idx)) {
    wsi_abort("Some `roi_id` values were not found in `rois`.")
  }
  rois$class[idx] <- label
  rois
}

#' Add ROI overlays to a viewer
#'
#' Convenience wrapper for creating a viewer with ROI overlays. Static HTML
#' viewers cannot be modified in-place, so pass a slide and this function will
#' create a new viewer with the ROI overlay.
#'
#' @param viewer A `wsi_slide` object.
#' @param rois A `wsi_roi` object or GeoJSON path.
#' @param ... Additional arguments passed to [wsi_viewer()].
#'
#' @return The HTML viewer path, invisibly.
#' @export
viewer_add_rois <- function(viewer, rois, ...) {
  if (!inherits(viewer, "wsi_slide")) {
    wsi_abort("`viewer_add_rois()` currently expects a `wsi_slide`; recreate static HTML viewers with `wsi_viewer(slide, roi = rois)`.")
  }
  wsi_viewer(viewer, roi = rois, ...)
}

#' @rdname viewer_add_rois
#' @export
wsi_viewer_add_rois <- viewer_add_rois

wsi_require_sf_for_roi <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    wsi_abort(
      "ROI cropping/tiling requires the optional package `sf` for polygon-aware operations. Install `sf` or use ROI bounding boxes manually.",
      class = "wsi_missing_dependency"
    )
  }
  invisible(TRUE)
}

#' Crop ROI bounding boxes
#'
#' Polygon-aware masking is planned; the first milestone crops each ROI bounding
#' box and requires `sf` so future geometry behavior is explicit.
#'
#' @param slide A `wsi_slide` object.
#' @param roi ROI object from [wsi_read_geojson()].
#' @param output_dir Optional output directory.
#' @param level Pyramid level.
#' @param format Output format.
#'
#' @return A list of returned crops, or a data frame of output files.
#' @export
wsi_crop_roi <- function(slide, roi, output_dir = NULL, level = 0,
                         format = c("tiff", "png", "jpeg")) {
  wsi_check_slide(slide)
  wsi_require_sf_for_roi()
  format <- match.arg(format)
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be an object returned by `wsi_read_geojson()`.")
  }

  crops <- vector("list", nrow(roi))
  if (!is.null(output_dir) && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  for (i in seq_len(nrow(roi))) {
    width <- roi$xmax[[i]] - roi$xmin[[i]]
    height <- roi$ymax[[i]] - roi$ymin[[i]]
    if (is.null(output_dir)) {
      crops[[i]] <- wsi_crop(slide, roi$xmin[[i]], roi$ymin[[i]], width, height, level = level)
    } else {
      output <- file.path(output_dir, sprintf("%s.%s", roi$roi_id[[i]], wsi_format_extension(format)))
      wsi_crop(slide, roi$xmin[[i]], roi$ymin[[i]], width, height, level = level, output = output, format = format)
      crops[[i]] <- output
    }
  }

  if (is.null(output_dir)) {
    return(crops)
  }
  data.frame(roi_id = roi$roi_id, file = unlist(crops), stringsAsFactors = FALSE)
}

#' Tile ROI bounding boxes
#'
#' @param slide A `wsi_slide` object.
#' @param roi ROI object from [wsi_read_geojson()].
#' @param output_dir Output directory.
#' @param tile_size,overlap,level Tiling parameters.
#' @param format Output format.
#'
#' @return A tile manifest data frame.
#' @export
wsi_tile_roi <- function(slide, roi, output_dir, tile_size = 512, overlap = 0,
                         level = 0, format = c("png", "jpeg", "tiff")) {
  wsi_check_slide(slide)
  wsi_require_sf_for_roi()
  format <- match.arg(format)
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be an object returned by `wsi_read_geojson()`.")
  }

  manifests <- vector("list", nrow(roi))
  for (i in seq_len(nrow(roi))) {
    region <- c(
      x = roi$xmin[[i]],
      y = roi$ymin[[i]],
      width = roi$xmax[[i]] - roi$xmin[[i]],
      height = roi$ymax[[i]] - roi$ymin[[i]]
    )
    manifest <- wsi_tile(
      slide,
      output_dir = output_dir,
      tile_size = tile_size,
      overlap = overlap,
      level = level,
      region = region,
      format = format,
      prefix = roi$roi_id[[i]]
    )
    manifest$roi_id <- roi$roi_id[[i]]
    manifests[[i]] <- manifest
  }

  out <- do.call(rbind, manifests)
  class(out) <- c("wsi_tile_manifest", setdiff(class(out), "wsi_tile_manifest"))
  out
}
