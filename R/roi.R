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
  if (is.list(coords) && length(coords) == 2L && all(vapply(coords, function(x) is.numeric(x) && length(x) == 1L, logical(1)))) {
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

wsi_geojson_missing <- function(x) {
  is.null(x) ||
    length(x) == 0L ||
    (!is.list(x) && length(x) == 1L && is.na(x))
}

wsi_geojson_scalar <- function(x, default = NA_character_) {
  if (wsi_geojson_missing(x)) {
    return(default)
  }
  if (is.list(x)) {
    if (length(x) == 1L) {
      return(wsi_geojson_scalar(x[[1L]], default = default))
    }
    return(default)
  }
  if (length(x) != 1L || is.na(x)) {
    return(default)
  }
  as.character(x)
}

wsi_geojson_logical <- function(x) {
  if (wsi_geojson_missing(x)) {
    return(NA)
  }
  if (is.logical(x) && length(x) == 1L) {
    return(x)
  }
  if (is.numeric(x) && length(x) == 1L && is.finite(x)) {
    return(!identical(as.numeric(x), 0))
  }
  value <- tolower(wsi_geojson_scalar(x, default = NA_character_))
  if (is.na(value)) {
    return(NA)
  }
  if (value %in% c("true", "t", "yes", "y", "1")) {
    return(TRUE)
  }
  if (value %in% c("false", "f", "no", "n", "0")) {
    return(FALSE)
  }
  NA
}

wsi_geojson_list <- function(x) {
  if (is.list(x)) {
    return(x)
  }
  list()
}

wsi_qupath_classification <- function(properties) {
  classification <- wsi_geojson_list(properties$classification)
  if (length(classification)) {
    return(classification)
  }
  value <- wsi_geojson_scalar(properties$classification, default = NA_character_)
  if (!is.na(value) && nzchar(value)) {
    return(list(name = value))
  }
  list()
}

wsi_qupath_class_name <- function(properties) {
  classification <- wsi_qupath_classification(properties)
  value <- wsi_geojson_scalar(classification$name, default = NA_character_)
  if (!is.na(value) && nzchar(value)) {
    return(value)
  }
  value <- wsi_geojson_scalar(properties$class, default = NA_character_)
  if (!is.na(value) && nzchar(value)) {
    return(value)
  }
  NA_character_
}

wsi_color_component <- function(x, names) {
  lower <- tolower(names(x) %||% character())
  idx <- match(names, lower)
  idx <- idx[!is.na(idx)]
  if (!length(idx)) {
    return(NA_real_)
  }
  value <- x[[idx[[1L]]]]
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) {
    return(NA_real_)
  }
  value
}

wsi_color_to_hex <- function(x) {
  if (wsi_geojson_missing(x)) {
    return(NA_character_)
  }
  if (is.character(x)) {
    value <- trimws(x[[1L]])
    if (grepl("^#?[0-9A-Fa-f]{6}$", value)) {
      return(paste0("#", toupper(sub("^#", "", value))))
    }
    if (grepl("^0x[0-9A-Fa-f]{6,8}$", value)) {
      value <- substr(value, nchar(value) - 5L, nchar(value))
      rgb <- strtoi(value, base = 16L)
      return(sprintf("#%06X", rgb))
    }
    return(NA_character_)
  }
  if (is.numeric(x) && length(x) >= 1L && is.finite(x[[1L]])) {
    rgb <- as.numeric(x[[1L]]) %% 16777216
    red <- floor(rgb / 65536) %% 256
    green <- floor(rgb / 256) %% 256
    blue <- rgb %% 256
    return(sprintf("#%02X%02X%02X", as.integer(red), as.integer(green), as.integer(blue)))
  }
  if (is.list(x)) {
    red <- wsi_color_component(x, c("r", "red"))
    green <- wsi_color_component(x, c("g", "green"))
    blue <- wsi_color_component(x, c("b", "blue"))
    if (all(is.finite(c(red, green, blue)))) {
      if (max(red, green, blue) <= 1) {
        red <- red * 255
        green <- green * 255
        blue <- blue * 255
      }
      return(sprintf("#%02X%02X%02X", as.integer(red), as.integer(green), as.integer(blue)))
    }
  }
  NA_character_
}

wsi_qupath_color <- function(properties) {
  classification <- wsi_qupath_classification(properties)
  candidates <- list(
    properties$color,
    properties$colour,
    properties$colorRGB,
    properties$color_rgb,
    classification$color,
    classification$colour,
    classification$colorRGB,
    classification$color_rgb
  )
  for (candidate in candidates) {
    value <- wsi_color_to_hex(candidate)
    if (!is.na(value) && nzchar(value)) {
      return(value)
    }
  }
  NA_character_
}

wsi_qupath_measurements <- function(properties) {
  if (is.list(properties$measurements)) {
    return(properties$measurements)
  }
  list()
}

wsi_geojson_crs_name <- function(geojson) {
  wsi_geojson_scalar(geojson$crs$properties$name, default = NA_character_)
}

wsi_geojson_foreign_members <- function(geojson) {
  if (!is.list(geojson)) {
    return(list())
  }
  foreign <- geojson[setdiff(names(geojson), c("type", "features", "crs"))]
  if (is.null(foreign)) {
    return(list())
  }
  foreign
}

wsi_apply_geojson_attributes <- function(roi, geojson) {
  attr(roi, "geojson_crs") <- geojson$crs %||% NULL
  attr(roi, "geojson_foreign_members") <- wsi_geojson_foreign_members(geojson)
  roi
}

wsi_roi_name <- function(properties, index) {
  classification <- wsi_qupath_class_name(properties)
  name <- wsi_geojson_scalar(properties$name, default = NA_character_)
  if (!is.na(name) && nzchar(name)) {
    return(name)
  }
  label <- wsi_geojson_scalar(properties$label, default = NA_character_)
  if (!is.na(label) && nzchar(label)) {
    return(label)
  }
  annotation_label <- wsi_geojson_scalar(properties$annotation_label, default = NA_character_)
  if (!is.na(annotation_label) && nzchar(annotation_label)) {
    return(annotation_label)
  }
  if (!is.na(classification) && nzchar(classification)) {
    return(classification)
  }
  sprintf("roi_%d", index)
}

wsi_empty_roi <- function(geojson = list()) {
  roi <- data.frame(
    roi_id = character(),
    name = character(),
    class = character(),
    object_type = character(),
    color = character(),
    classification_color = character(),
    is_locked = logical(),
    geometry_type = character(),
    xmin = numeric(),
    ymin = numeric(),
    xmax = numeric(),
    ymax = numeric(),
    crs = character(),
    stringsAsFactors = FALSE
  )
  roi$coordinates <- I(list())
  roi$measurements <- I(list())
  roi$properties <- I(list())
  roi$geometry <- I(list())
  roi$feature <- I(list())
  class(roi) <- c("wsi_roi", class(roi))
  wsi_apply_geojson_attributes(roi, geojson)
}

wsi_roi_from_geojson <- function(geojson) {
  features <- wsi_geojson_features(geojson)

  if (!length(features)) {
    return(wsi_empty_roi(geojson))
  }

  rows <- lapply(seq_along(features), function(i) {
    feature <- features[[i]]
    geometry <- feature$geometry %||% list()
    properties <- feature$properties %||% list()
    properties <- wsi_geojson_list(properties)
    points <- wsi_collect_points(geometry$coordinates)
    if (!nrow(points)) {
      wsi_abort(sprintf("GeoJSON feature %d does not contain polygon coordinates.", i))
    }
    classification <- wsi_qupath_class_name(properties)
    object_type <- wsi_geojson_scalar(
      properties$objectType %||% properties$object_type,
      default = NA_character_
    )
    color <- wsi_qupath_color(properties)
    data.frame(
      roi_id = wsi_geojson_scalar(feature$id %||% properties$id %||% properties$objectId, default = as.character(i)),
      name = wsi_roi_name(properties, i),
      class = classification,
      object_type = object_type,
      color = color,
      classification_color = color,
      is_locked = wsi_geojson_logical(properties$isLocked %||% properties$locked),
      geometry_type = wsi_geojson_scalar(geometry$type, default = NA_character_),
      xmin = min(points[, 1L]),
      ymin = min(points[, 2L]),
      xmax = max(points[, 1L]),
      ymax = max(points[, 2L]),
      crs = wsi_geojson_crs_name(geojson),
      stringsAsFactors = FALSE
    )
  })

  roi <- do.call(rbind, rows)
  roi$coordinates <- I(lapply(features, function(feature) feature$geometry$coordinates))
  roi$measurements <- I(lapply(features, function(feature) wsi_qupath_measurements(feature$properties %||% list())))
  roi$properties <- I(lapply(features, function(feature) wsi_geojson_list(feature$properties %||% list())))
  roi$geometry <- I(lapply(features, function(feature) wsi_geojson_list(feature$geometry %||% list())))
  roi$feature <- I(features)
  class(roi) <- c("wsi_roi", class(roi))
  wsi_apply_geojson_attributes(roi, geojson)
}

#' Read QuPath-style GeoJSON annotations
#'
#' Reads GeoJSON annotations and returns lightweight ROI metadata with list
#' columns for geometry coordinates. Polygon and multipolygon annotations are
#' supported, and QuPath metadata such as object ids, object type,
#' classification labels/colors, measurements, and raw properties are preserved
#' for round-trip export.
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

wsi_roi_column_value <- function(rois, column, index, default = NULL) {
  if (!column %in% names(rois)) {
    return(default)
  }
  value <- rois[[column]][[index]]
  if (wsi_geojson_missing(value)) {
    return(default)
  }
  value
}

wsi_roi_feature_for_write <- function(rois, index) {
  feature <- wsi_roi_column_value(rois, "feature", index, default = list())
  wsi_geojson_list(feature)
}

wsi_roi_properties_for_write <- function(rois, index) {
  properties <- wsi_roi_column_value(rois, "properties", index, default = list())
  properties <- wsi_geojson_list(properties)

  name <- wsi_geojson_scalar(wsi_roi_column_value(rois, "name", index), default = NA_character_)
  if (!is.na(name) && nzchar(name)) {
    properties$name <- name
    properties$label <- name
  }

  class_name <- wsi_geojson_scalar(wsi_roi_column_value(rois, "class", index), default = NA_character_)
  if (is.na(class_name) || !nzchar(class_name)) {
    class_name <- wsi_qupath_class_name(properties)
  }
  if (is.na(class_name) || !nzchar(class_name)) {
    class_name <- "annotation"
  }

  classification <- wsi_qupath_classification(properties)
  classification$name <- class_name
  color <- wsi_geojson_scalar(
    wsi_roi_column_value(rois, "classification_color", index, default = wsi_roi_column_value(rois, "color", index)),
    default = NA_character_
  )
  if (!is.na(color) && nzchar(color)) {
    existing_color <- wsi_qupath_color(properties)
    color_changed <- !is.na(existing_color) && nzchar(existing_color) &&
      !identical(toupper(existing_color), toupper(color))
    if (isTRUE(color_changed)) {
      classification$color <- color
      classification$colorRGB <- NULL
      classification$color_rgb <- NULL
      classification$colour <- NULL
    } else if (is.null(classification$color) && is.null(classification$colorRGB)) {
      classification$color <- color
    }
  }
  properties$classification <- classification
  properties$class <- class_name

  object_type <- wsi_geojson_scalar(wsi_roi_column_value(rois, "object_type", index), default = NA_character_)
  if (!is.na(object_type) && nzchar(object_type)) {
    properties$objectType <- object_type
  }

  is_locked <- wsi_roi_column_value(rois, "is_locked", index, default = NULL)
  if (!is.null(is_locked)) {
    is_locked <- wsi_geojson_logical(is_locked)
    if (!is.na(is_locked)) {
      properties$isLocked <- is_locked
    }
  }

  measurements <- wsi_roi_column_value(rois, "measurements", index, default = NULL)
  if (is.list(measurements)) {
    properties$measurements <- measurements
  }

  properties
}

wsi_roi_geometry_for_write <- function(rois, index) {
  geometry <- wsi_roi_column_value(rois, "geometry", index, default = list())
  geometry <- wsi_geojson_list(geometry)
  geometry_type <- wsi_geojson_scalar(wsi_roi_column_value(rois, "geometry_type", index), default = NA_character_)
  if (is.na(geometry_type) || !nzchar(geometry_type)) {
    geometry_type <- wsi_geojson_scalar(geometry$type, default = NA_character_)
  }
  if (is.na(geometry_type) || !nzchar(geometry_type)) {
    wsi_abort(sprintf("ROI row %d is missing `geometry_type`.", index))
  }
  coordinates <- wsi_roi_column_value(rois, "coordinates", index, default = NULL)
  if (is.null(coordinates)) {
    wsi_abort(sprintf("ROI row %d is missing GeoJSON coordinates.", index))
  }
  geometry$type <- geometry_type
  geometry$coordinates <- coordinates
  geometry
}

wsi_geojson_for_rois <- function(rois, features) {
  foreign <- attr(rois, "geojson_foreign_members", exact = TRUE)
  foreign <- wsi_geojson_list(foreign)
  foreign <- foreign[setdiff(names(foreign), c("type", "features", "crs"))]
  out <- c(list(type = "FeatureCollection"), foreign)
  crs <- attr(rois, "geojson_crs", exact = TRUE)
  if (!is.null(crs)) {
    out$crs <- crs
  }
  out$features <- features
  out
}

#' Write ROI annotations as GeoJSON
#'
#' Writes `wsi_roi` objects as QuPath-compatible GeoJSON FeatureCollections.
#' Polygon and multipolygon coordinates are written without flattening, and
#' QuPath metadata read by [read_geojson()] is preserved where possible.
#'
#' @param rois A `wsi_roi` object.
#' @param file Output GeoJSON path.
#' @param overwrite Whether to overwrite an existing file.
#' @param class_presets Optional class preset table from
#'   [wsi_roi_class_presets()]. When supplied, preset colours are written into
#'   QuPath-compatible classifications.
#' @param respect_export_rules Whether to drop ROIs whose class preset has
#'   `export = FALSE` or `export_rule = "exclude"`.
#'
#' @return The output path, invisibly.
#' @export
write_geojson <- function(rois, file, overwrite = FALSE, class_presets = NULL,
                          respect_export_rules = FALSE) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  if (!is.null(class_presets)) {
    rois <- wsi_apply_roi_class_presets(rois, class_presets, update_existing = TRUE)
    rois <- wsi_filter_roi_export(rois, class_presets, respect_export_rules = respect_export_rules)
  }
  file <- wsi_validate_output_path(file, overwrite = overwrite)
  features <- lapply(seq_len(nrow(rois)), function(i) {
    feature <- wsi_roi_feature_for_write(rois, i)
    feature$type <- "Feature"
    feature$id <- wsi_geojson_scalar(wsi_roi_column_value(rois, "roi_id", i), default = as.character(i))
    feature$properties <- wsi_roi_properties_for_write(rois, i)
    feature$geometry <- wsi_roi_geometry_for_write(rois, i)
    feature
  })
  geojson <- wsi_geojson_for_rois(rois, features)
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
  if ("properties" %in% names(rois)) {
    for (i in idx) {
      properties <- wsi_geojson_list(rois$properties[[i]])
      classification <- wsi_qupath_classification(properties)
      classification$name <- label
      properties$classification <- classification
      properties$class <- label
      rois$properties[[i]] <- properties
    }
  }
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
#' @param whitespace_filter Whether to compute optional whitespace/background
#'   metrics before export.
#' @param whitespace_action `"flag"` keeps all tiles and appends whitespace
#'   columns; `"drop"` removes rows where `whitespace_flag` is `TRUE`.
#' @param whitespace_options Thresholds from [wsi_whitespace_options()].
#' @param artifact_filter Whether to compute optional tile artifact metrics
#'   before export.
#' @param artifact_action `"flag"` keeps all tiles and appends artifact columns;
#'   `"drop"` removes rows where `artifact_flag` is `TRUE`.
#' @param artifact_options Thresholds from [wsi_artifact_options()].
#'
#' @return A tile manifest data frame.
#' @export
wsi_tile_roi <- function(slide, roi, output_dir, tile_size = 512, overlap = 0,
                         level = 0, format = c("png", "jpeg", "tiff"),
                         whitespace_filter = FALSE,
                         whitespace_action = c("flag", "drop"),
                         whitespace_options = wsi_whitespace_options(),
                         artifact_filter = FALSE,
                         artifact_action = c("flag", "drop"),
                         artifact_options = wsi_artifact_options()) {
  wsi_check_slide(slide)
  wsi_require_sf_for_roi()
  format <- match.arg(format)
  whitespace_action <- match.arg(whitespace_action)
  artifact_action <- match.arg(artifact_action)
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
      whitespace_filter = whitespace_filter,
      whitespace_action = whitespace_action,
      whitespace_options = whitespace_options,
      artifact_filter = artifact_filter,
      artifact_action = artifact_action,
      artifact_options = artifact_options,
      prefix = roi$roi_id[[i]]
    )
    manifest$roi_id <- roi$roi_id[[i]]
    manifests[[i]] <- manifest
  }

  out <- do.call(rbind, manifests)
  class(out) <- c("wsi_tile_manifest", setdiff(class(out), "wsi_tile_manifest"))
  out
}
