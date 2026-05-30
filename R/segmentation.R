wsi_roi_bbox <- function(roi, roi_id = NULL) {
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a `wsi_roi` object.")
  }
  if (!nrow(roi)) {
    wsi_abort("`roi` does not contain any regions.")
  }
  idx <- if (is.null(roi_id)) {
    if (nrow(roi) > 1L) {
      wsi_warn("Multiple ROIs were supplied; using the first ROI. Pass `roi_id` to choose a specific region.")
    }
    1L
  } else {
    match(as.character(roi_id), roi$roi_id)
  }
  if (is.na(idx)) {
    wsi_abort(sprintf("ROI id `%s` was not found.", roi_id))
  }
  c(
    x = roi$xmin[[idx]],
    y = roi$ymin[[idx]],
    width = roi$xmax[[idx]] - roi$xmin[[idx]],
    height = roi$ymax[[idx]] - roi$ymin[[idx]]
  )
}

wsi_segmentation_type <- function(file, type = c("auto", "geojson", "csv", "mask")) {
  type <- match.arg(type)
  if (!identical(type, "auto")) {
    return(type)
  }
  ext <- tolower(tools::file_ext(file))
  if (ext %in% c("geojson", "json")) {
    return("geojson")
  }
  if (ext %in% c("csv", "tsv", "txt")) {
    return("csv")
  }
  if (ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
    return("mask")
  }
  wsi_abort("Could not infer segmentation type from file extension. Use `type = \"geojson\"`, `\"csv\"`, or `\"mask\"`.")
}

wsi_centroid_columns <- function(data) {
  lower <- tolower(names(data))
  x <- match("x", lower)
  y <- match("y", lower)
  if (is.na(x)) {
    x <- match("centroid_x", lower)
  }
  if (is.na(y)) {
    y <- match("centroid_y", lower)
  }
  if (is.na(x) || is.na(y)) {
    return(NULL)
  }
  c(x = x, y = y)
}

wsi_centroid_circle_coordinates <- function(x, y, radius = 8, n = 24L) {
  angles <- seq(0, 2 * pi, length.out = as.integer(n) + 1L)
  ring <- lapply(angles, function(angle) {
    unname(c(x + cos(angle) * radius, y + sin(angle) * radius))
  })
  list(ring)
}

wsi_centroid_id <- function(data, index) {
  candidates <- c("cell_id", "id", "object_id", "label")
  lower <- tolower(names(data))
  idx <- match(candidates, lower)
  idx <- idx[!is.na(idx)]
  if (length(idx)) {
    value <- data[[idx[[1L]]]][[index]]
    if (!is.na(value) && nzchar(as.character(value))) {
      return(as.character(value))
    }
  }
  sprintf("cell_%d", index)
}

wsi_offset_centroids <- function(segmentation, dx = 0, dy = 0) {
  if (!inherits(segmentation, "wsi_segmentation_centroids")) {
    wsi_abort("`segmentation` must contain centroid coordinates.")
  }
  out <- segmentation
  out$x <- out$x + dx
  out$y <- out$y + dy
  out
}

wsi_default_stardist_command <- function(command = NULL) {
  if (!is.null(command)) {
    return(command)
  }
  env_command <- Sys.getenv("WSITOOLS_STARDIST_COMMAND", unset = "")
  if (nzchar(env_command)) {
    return(env_command)
  }
  wrapper <- tryCatch(wsi_stardist_wrapper_path(), error = function(err) "")
  if (nzchar(wrapper) && wsi_command_exists(wrapper)) {
    return(wrapper)
  }
  if (wsi_command_exists("stardist-predict2d")) {
    return("stardist-predict2d")
  }
  ""
}

wsi_stardist_setup_command <- function(command = "stardist-predict2d") {
  command <- command %||% "stardist-predict2d"
  if (!nzchar(command)) {
    command <- "stardist-predict2d"
  }
  sprintf(
    "Sys.setenv(WSITOOLS_STARDIST_COMMAND = %s)",
    encodeString(command, quote = "\"")
  )
}

wsi_stardist_not_configured_message <- function(command = NULL,
                                                context = c("generic", "viewer", "image")) {
  context <- match.arg(context)
  command <- command %||% ""
  requested <- if (nzchar(command)) {
    sprintf("\nRequested command: `%s`.", command)
  } else {
    ""
  }
  example <- switch(
    context,
    viewer = paste0(
      "\nCopyable R command suggestion:\n",
      "  wsi_install_stardist(method = \"conda\")\n",
      "  viewer <- wsi_viewer_live(slide, stardist = TRUE, wait = FALSE)"
    ),
    image = paste0(
      "\nCopyable R command suggestion:\n",
      "  wsi_install_stardist(method = \"conda\")\n",
      "  stardist_segment_image(input, output)"
    ),
    generic = paste0(
      "\nCopyable R command suggestion:\n",
      "  wsi_install_stardist(method = \"conda\")\n",
      "  # or configure an existing command:\n",
      "  ", wsi_stardist_setup_command()
    )
  )
  paste0(
    "No StarDist command was found. Install/configure it, ",
    "or load a segmentation GeoJSON/CSV instead.",
    requested,
    example
  )
}

wsi_stardist_default_args <- function(prob_thresh = NULL, nms_thresh = NULL) {
  args <- c("--input", "{input}", "--output", "{output}", "--model", "{model}")
  if (!is.null(prob_thresh)) {
    args <- c(args, "--prob-thresh", "{prob_thresh}")
  }
  if (!is.null(nms_thresh)) {
    args <- c(args, "--nms-thresh", "{nms_thresh}")
  }
  args
}

wsi_template_args <- function(args, values) {
  out <- as.character(args)
  for (key in names(values)) {
    value <- values[[key]]
    value <- if (is.null(value) || is.na(value)) "" else as.character(value)
    out <- gsub(sprintf("{%s}", key), value, out, fixed = TRUE)
  }
  out
}

wsi_offset_coordinates <- function(coords, dx = 0, dy = 0) {
  if (is.numeric(coords) && length(coords) >= 2L) {
    out <- coords
    out[[1L]] <- out[[1L]] + dx
    out[[2L]] <- out[[2L]] + dy
    return(out)
  }
  if (is.list(coords) && length(coords) >= 2L &&
      is.numeric(coords[[1L]]) && is.numeric(coords[[2L]]) &&
      length(coords[[1L]]) == 1L && length(coords[[2L]]) == 1L) {
    out <- coords
    out[[1L]] <- out[[1L]] + dx
    out[[2L]] <- out[[2L]] + dy
    return(out)
  }
  if (is.list(coords)) {
    return(lapply(coords, wsi_offset_coordinates, dx = dx, dy = dy))
  }
  coords
}

#' Translate ROI coordinates
#'
#' Adds an x/y offset to ROI coordinates. This is useful when segmentation
#' polygons were produced on an ROI crop and need to be mapped back to level-0
#' slide coordinates.
#'
#' @param rois A `wsi_roi` object.
#' @param dx,dy Offsets to add to x/y coordinates.
#'
#' @return A translated `wsi_roi` object.
#' @export
wsi_translate_rois <- function(rois, dx = 0, dy = 0) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  if (!is.numeric(dx) || length(dx) != 1L || is.na(dx) || !is.finite(dx)) {
    wsi_abort("`dx` must be a single finite number.")
  }
  if (!is.numeric(dy) || length(dy) != 1L || is.na(dy) || !is.finite(dy)) {
    wsi_abort("`dy` must be a single finite number.")
  }
  rois$coordinates <- I(lapply(rois$coordinates, wsi_offset_coordinates, dx = dx, dy = dy))
  rois$xmin <- rois$xmin + dx
  rois$xmax <- rois$xmax + dx
  rois$ymin <- rois$ymin + dy
  rois$ymax <- rois$ymax + dy
  rois
}

#' @rdname wsi_translate_rois
#' @export
translate_rois <- wsi_translate_rois

#' Convert segmentation outputs to ROI overlays
#'
#' Converts imported segmentation outputs into `wsi_roi` polygons that can be
#' overlaid in [wsi_viewer()]. Polygon GeoJSON segmentations are returned
#' unchanged. Centroid tables are converted to small circular cell markers.
#'
#' @param segmentation A segmentation object returned by [import_segmentation()]
#'   or `stardist_segment_*()$segmentation`.
#' @param radius Radius, in slide pixels, for centroid cell markers.
#' @param label Class label used for centroid cell markers.
#'
#' @return A `wsi_roi` object.
#' @export
wsi_segmentation_to_rois <- function(segmentation, radius = 8, label = "cell") {
  if (inherits(segmentation, "wsi_roi")) {
    return(segmentation)
  }
  if (!inherits(segmentation, "wsi_segmentation_centroids")) {
    wsi_abort("Only polygon GeoJSON and centroid segmentation tables can be converted to ROI overlays.")
  }
  radius <- wsi_check_scalar_number(radius, "radius", allow_zero = FALSE)
  if (!nrow(segmentation)) {
    wsi_abort("Segmentation centroid table is empty.")
  }
  if (!is.character(label) || length(label) != 1L || is.na(label) || !nzchar(label)) {
    wsi_abort("`label` must be a single non-empty character value.")
  }

  ids <- vapply(seq_len(nrow(segmentation)), function(i) wsi_centroid_id(segmentation, i), character(1))
  coords <- lapply(seq_len(nrow(segmentation)), function(i) {
    wsi_centroid_circle_coordinates(segmentation$x[[i]], segmentation$y[[i]], radius = radius)
  })
  roi <- data.frame(
    roi_id = ids,
    name = ids,
    class = label,
    geometry_type = "Polygon",
    xmin = segmentation$x - radius,
    ymin = segmentation$y - radius,
    xmax = segmentation$x + radius,
    ymax = segmentation$y + radius,
    crs = NA_character_,
    stringsAsFactors = FALSE
  )
  roi$coordinates <- I(coords)
  class(roi) <- c("wsi_segmentation_rois", "wsi_segmentation", "wsi_roi", class(roi))
  attr(roi, "source_file") <- attr(segmentation, "source_file", exact = TRUE)
  roi
}

#' @rdname wsi_segmentation_to_rois
#' @export
segmentation_to_rois <- wsi_segmentation_to_rois

#' Check StarDist command availability
#'
#' Checks whether a StarDist command-line entry point is available. StarDist is
#' optional and is never required to install or load wsiTools.
#'
#' @param command Optional command to check. If omitted, the
#'   `WSITOOLS_STARDIST_COMMAND` environment variable is used, followed by
#'   `stardist-predict2d` when present on `PATH`.
#'
#' @return `TRUE` or `FALSE`.
#' @export
wsi_has_stardist <- function(command = NULL) {
  command <- wsi_default_stardist_command(command)
  is.character(command) && length(command) == 1L && nzchar(command) && wsi_command_exists(command)
}

wsi_stardist_result <- function(input, output, segmentation = NULL,
                                command = NULL, args = character(),
                                command_output = character(), crop = NULL,
                                slide_output = NULL, roi_id = NULL,
                                bbox = NULL, status = "complete") {
  structure(
    list(
      input = input,
      crop = crop,
      output = output,
      slide_output = slide_output,
      segmentation = segmentation,
      command = command,
      args = args,
      command_output = command_output,
      roi_id = roi_id,
      bbox = bbox,
      status = status
    ),
    class = "wsi_stardist_result"
  )
}

#' @export
print.wsi_stardist_result <- function(x, ...) {
  cat("<wsi_stardist_result>\n")
  cat(sprintf("  status: %s\n", x$status %||% "unknown"))
  if (!is.null(x$crop)) {
    cat(sprintf("  crop:   %s\n", x$crop))
  }
  if (!is.null(x$output)) {
    cat(sprintf("  output: %s\n", x$output))
  }
  if (!is.null(x$slide_output)) {
    cat(sprintf("  mapped: %s\n", x$slide_output))
  }
  if (!is.null(x$segmentation)) {
    n <- if (is.data.frame(x$segmentation)) nrow(x$segmentation) else NA_integer_
    if (!is.na(n)) {
      cat(sprintf("  objects: %d\n", n))
    }
  }
  invisible(x)
}

#' Run StarDist on an image crop
#'
#' Runs an optional external StarDist command on a crop image and imports the
#' result when it is written as GeoJSON, CSV/TSV centroids, or an image mask.
#' wsiTools does not depend on StarDist; pass `command` and `args` for the
#' Python/CLI environment available on your machine.
#'
#' @param input Input crop image path.
#' @param output Expected StarDist output path.
#' @param model StarDist model identifier passed into `{model}` placeholders.
#' @param command External command. If omitted, uses
#'   `WSITOOLS_STARDIST_COMMAND` or `stardist-predict2d` when available.
#' @param args Command arguments. Placeholders `{input}`, `{output}`, `{model}`,
#'   `{prob_thresh}`, and `{nms_thresh}` are substituted safely before calling
#'   [system2()].
#' @param output_type Output type for [import_segmentation()].
#' @param prob_thresh,nms_thresh Optional StarDist thresholds.
#' @param overwrite Whether to overwrite `output`.
#' @param run If `FALSE`, return the command plan without running StarDist.
#'
#' @return A `wsi_stardist_result` object.
#' @export
stardist_segment_image <- function(input, output,
                                   model = "2D_versatile_he",
                                   command = NULL,
                                   args = NULL,
                                   output_type = c("auto", "geojson", "csv", "mask"),
                                   prob_thresh = NULL,
                                   nms_thresh = NULL,
                                   overwrite = FALSE,
                                   run = TRUE) {
  input <- wsi_validate_input_path(input)
  output_type <- match.arg(output_type)
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  if (!is.null(prob_thresh)) {
    prob_thresh <- wsi_check_scalar_number(prob_thresh, "prob_thresh")
  }
  if (!is.null(nms_thresh)) {
    nms_thresh <- wsi_check_scalar_number(nms_thresh, "nms_thresh")
  }

  command <- wsi_default_stardist_command(command)
  if (is.null(args)) {
    args <- wsi_stardist_default_args(prob_thresh = prob_thresh, nms_thresh = nms_thresh)
  }
  args <- wsi_template_args(
    args,
    list(
      input = input,
      output = output,
      model = model,
      prob_thresh = prob_thresh,
      nms_thresh = nms_thresh
    )
  )

  if (!isTRUE(run)) {
    return(wsi_stardist_result(
      input = input,
      output = output,
      command = command,
      args = args,
      status = "planned"
    ))
  }

  if (!nzchar(command) || !wsi_command_exists(command)) {
    wsi_abort(
      wsi_stardist_not_configured_message(command, context = "image"),
      class = "wsi_backend_unavailable"
    )
  }

  command_output <- wsi_run_command(
    command,
    args = args,
    error_message = "StarDist segmentation failed."
  )
  if (!file.exists(output)) {
    wsi_abort(sprintf("StarDist completed but did not create the expected output file: %s", output))
  }

  segmentation <- import_segmentation(output, type = output_type)
  wsi_stardist_result(
    input = input,
    output = output,
    segmentation = segmentation,
    command = command,
    args = args,
    command_output = command_output,
    status = "complete"
  )
}

#' @rdname stardist_segment_image
#' @export
wsi_stardist_segment_image <- stardist_segment_image

#' Export an ROI crop for external analysis
#'
#' Crops the bounding box of a selected ROI without loading the full slide into
#' R memory. This is useful for sending a selected region to optional tools such
#' as StarDist, Cellpose, or other external segmentation pipelines.
#'
#' @param image A `wsi_slide` object or a path that can be opened by
#'   [wsi_open()].
#' @param roi A `wsi_roi` object.
#' @param file Output image path.
#' @param roi_id Optional ROI id. When omitted, the first ROI is used.
#' @param level Pyramid level.
#' @param format Output image format.
#' @param overwrite Whether to overwrite `file`.
#' @param backend Region export backend.
#'
#' @return The output path, invisibly.
#' @export
export_roi_crop <- function(image, roi, file, roi_id = NULL, level = 0,
                            format = c("png", "tiff", "jpeg"),
                            overwrite = FALSE,
                            backend = c("auto", "vips", "openslide")) {
  format <- match.arg(format)
  backend <- match.arg(backend)
  region <- wsi_roi_bbox(roi, roi_id = roi_id)

  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)
  slide <- if (inherits(image, "wsi_slide")) {
    image
  } else if (is.character(image) && length(image) == 1L) {
    opened <- wsi_open(image)
    opened
  } else {
    wsi_abort("`image` must be a `wsi_slide` object or a path that can be opened by `wsi_open()`.")
  }

  wsi_crop(
    slide,
    x = region[["x"]],
    y = region[["y"]],
    width = region[["width"]],
    height = region[["height"]],
    level = level,
    output = file,
    format = format,
    backend = backend,
    overwrite = overwrite
  )
  invisible(file)
}

#' @rdname export_roi_crop
#' @export
wsi_export_roi_crop <- export_roi_crop

#' Run StarDist on a selected ROI
#'
#' Exports the bounding box of a selected ROI, runs an optional external
#' StarDist command on the crop, imports the segmentation output, and for
#' GeoJSON outputs maps crop-local coordinates back to level-0 slide
#' coordinates. Centroid CSV/TSV outputs are also mapped back to slide
#' coordinates when `translate_geojson = TRUE`.
#'
#' @param image A `wsi_slide` object or a path that can be opened by
#'   [wsi_open()].
#' @param roi A `wsi_roi` object containing the selected ROI.
#' @param output_dir Directory for the ROI crop and StarDist output.
#' @param roi_id Optional ROI id. When omitted, the first ROI is used.
#' @param level Pyramid level used for the exported crop.
#' @param crop_format Crop image format.
#' @param output Optional expected StarDist output path. Defaults to a GeoJSON
#'   path in `output_dir`.
#' @param translate_geojson Whether to translate crop-local GeoJSON polygons to
#'   slide coordinates using the selected ROI bounding box origin.
#' @inheritParams stardist_segment_image
#' @inheritParams export_roi_crop
#'
#' @return A `wsi_stardist_result` object.
#' @export
stardist_segment_roi <- function(image, roi, output_dir, roi_id = NULL,
                                 level = 0,
                                 crop_format = c("png", "tiff", "jpeg"),
                                 model = "2D_versatile_he",
                                 command = NULL,
                                 args = NULL,
                                 output = NULL,
                                 output_type = c("auto", "geojson", "csv", "mask"),
                                 prob_thresh = NULL,
                                 nms_thresh = NULL,
                                 translate_geojson = TRUE,
                                 overwrite = FALSE,
                                 run = TRUE,
                                 backend = c("auto", "vips", "openslide")) {
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a `wsi_roi` object.")
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    wsi_abort(sprintf("Could not create output directory: %s", output_dir))
  }

  crop_format <- match.arg(crop_format)
  output_type <- match.arg(output_type)
  backend <- match.arg(backend)
  region <- wsi_roi_bbox(roi, roi_id = roi_id)
  idx <- if (is.null(roi_id)) 1L else match(as.character(roi_id), roi$roi_id)
  roi_label <- roi$roi_id[[idx]]
  stem <- gsub("[^A-Za-z0-9_.-]+", "_", roi_label)
  if (!nzchar(stem)) {
    stem <- "roi"
  }

  crop_file <- file.path(output_dir, sprintf("%s_crop.%s", stem, wsi_format_extension(crop_format)))
  output <- output %||% file.path(output_dir, sprintf("%s_stardist.geojson", stem))

  export_roi_crop(
    image = image,
    roi = roi,
    file = crop_file,
    roi_id = roi_label,
    level = level,
    format = crop_format,
    overwrite = overwrite,
    backend = backend
  )

  result <- stardist_segment_image(
    input = crop_file,
    output = output,
    model = model,
    command = command,
    args = args,
    output_type = output_type,
    prob_thresh = prob_thresh,
    nms_thresh = nms_thresh,
    overwrite = overwrite,
    run = run
  )
  result$crop <- crop_file
  result$roi_id <- roi_label
  result$bbox <- region

  if (!isTRUE(run)) {
    result$status <- "crop_exported"
    return(result)
  }

  if (isTRUE(translate_geojson)) {
    if (inherits(result$segmentation, "wsi_roi")) {
      translated <- wsi_translate_rois(
        result$segmentation,
        dx = unname(region[["x"]]),
        dy = unname(region[["y"]])
      )
      slide_output <- file.path(
        output_dir,
        sprintf("%s_stardist_slide.geojson", stem)
      )
      write_geojson(translated, slide_output, overwrite = TRUE)
      class(translated) <- unique(c("wsi_segmentation_rois", "wsi_segmentation", class(translated)))
      attr(translated, "source_file") <- slide_output
      result$slide_output <- slide_output
      result$segmentation <- translated
    } else if (inherits(result$segmentation, "wsi_segmentation_centroids")) {
      translated <- wsi_offset_centroids(
        result$segmentation,
        dx = unname(region[["x"]]),
        dy = unname(region[["y"]])
      )
      slide_output <- file.path(
        output_dir,
        sprintf("%s_stardist_slide.csv", stem)
      )
      utils::write.csv(translated, slide_output, row.names = FALSE)
      attr(translated, "source_file") <- slide_output
      result$slide_output <- slide_output
      result$segmentation <- translated
    }
  }

  result
}

#' @rdname stardist_segment_roi
#' @export
wsi_stardist_segment_roi <- stardist_segment_roi

#' Import external segmentation outputs
#'
#' Imports lightweight outputs from optional external tools. GeoJSON files are
#' read as polygon ROIs, CSV/TSV files with `x`/`y` or `centroid_x`/`centroid_y`
#' columns are treated as centroids, and image files are treated as mask
#' overlays. No external model package is required.
#'
#' @param file Segmentation output file.
#' @param type Input type. `"auto"` infers from file extension.
#' @param mask_as_rois For image masks, convert connected non-background mask
#'   components to `wsi_roi` polygon annotations with [wsi_mask_to_rois()]
#'   instead of returning a mask-overlay object.
#' @param ... Additional arguments passed to [wsi_mask_to_rois()] when
#'   `mask_as_rois = TRUE`.
#'
#' @return A segmentation object.
#' @export
import_segmentation <- function(file, type = c("auto", "geojson", "csv", "mask"),
                                mask_as_rois = FALSE, ...) {
  file <- wsi_validate_input_path(file)
  type <- wsi_segmentation_type(file, type)

  if (identical(type, "geojson")) {
    roi <- wsi_read_geojson(file)
    class(roi) <- c("wsi_segmentation_rois", "wsi_segmentation", class(roi))
    attr(roi, "source_file") <- file
    return(roi)
  }

  if (identical(type, "csv")) {
    ext <- tolower(tools::file_ext(file))
    data <- if (ext %in% c("tsv", "txt")) {
      utils::read.delim(file, stringsAsFactors = FALSE, check.names = FALSE)
    } else {
      utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
    }
    cols <- wsi_centroid_columns(data)
    if (is.null(cols)) {
      out <- structure(
        list(type = "table", path = file, data = data),
        class = c("wsi_segmentation_table", "wsi_segmentation")
      )
      return(out)
    }
    data$x <- suppressWarnings(as.numeric(data[[cols[["x"]]]]))
    data$y <- suppressWarnings(as.numeric(data[[cols[["y"]]]]))
    if (anyNA(data$x) || anyNA(data$y)) {
      wsi_abort("Segmentation centroid coordinates must be numeric.")
    }
    class(data) <- c("wsi_segmentation_centroids", "wsi_segmentation", class(data))
    attr(data, "source_file") <- file
    return(data)
  }

  if (isTRUE(mask_as_rois)) {
    roi <- wsi_read_mask_annotations(file, ...)
    class(roi) <- c("wsi_segmentation_rois", "wsi_segmentation", class(roi))
    attr(roi, "source_file") <- file
    return(roi)
  }

  structure(
    list(type = "mask", path = file),
    class = c("wsi_segmentation_mask", "wsi_segmentation")
  )
}

#' @rdname import_segmentation
#' @export
wsi_import_segmentation <- import_segmentation

wsi_segmentation_geojson_text <- function(segmentation, cell_radius = 8) {
  rois <- wsi_segmentation_to_rois(segmentation, radius = cell_radius)
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp), add = TRUE)
  write_geojson(rois, tmp, overwrite = TRUE)
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

wsi_segmentation_geojson_object <- function(segmentation, cell_radius = 8) {
  text <- wsi_segmentation_geojson_text(segmentation, cell_radius = cell_radius)
  jsonlite::fromJSON(text, simplifyVector = FALSE)
}

wsi_named_numeric_list <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  values <- as.numeric(x)
  names(values) <- names(x)
  as.list(values)
}

wsi_stardist_response_body <- function(result, cell_radius = 8) {
  metadata <- list(
    message = sprintf(
      "StarDist completed for ROI %s.",
      result$roi_id %||% "selected ROI"
    ),
    crop = result$crop %||% result$input %||% NULL,
    output = result$output %||% NULL,
    slide_output = result$slide_output %||% NULL,
    roi_id = result$roi_id %||% NULL,
    bbox = wsi_named_numeric_list(result$bbox),
    status = result$status %||% "complete",
    segmentation_type = class(result$segmentation)[[1L]] %||% NULL
  )

  overlay <- tryCatch(
    wsi_segmentation_geojson_object(result$segmentation, cell_radius = cell_radius),
    error = function(err) {
      metadata$message <<- paste0(
        metadata$message,
        " The output was imported, but live viewer overlay currently requires ",
        "polygon GeoJSON or centroid CSV/TSV output. ",
        conditionMessage(err)
      )
      NULL
    }
  )
  if (!is.null(overlay)) {
    metadata$geojson <- overlay
  }
  metadata
}

wsi_http_json_response <- function(status = 200L, body = list(), content_type = "application/json") {
  text <- if (is.character(body) && length(body) == 1L) {
    body
  } else {
    jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")
  }
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = content_type,
      "Access-Control-Allow-Origin" = "*",
      "Access-Control-Allow-Methods" = "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers" = "Content-Type"
    ),
    body = text
  )
}

wsi_http_request_body <- function(req) {
  input <- req$rook.input
  if (is.null(input) || !is.function(input$read)) {
    return("")
  }
  body <- input$read()
  if (is.raw(body)) {
    return(rawToChar(body))
  }
  paste(as.character(body), collapse = "\n")
}

wsi_http_query_params <- function(query = NULL) {
  if (is.null(query) || !nzchar(query)) {
    return(list())
  }
  parts <- strsplit(query, "&", fixed = TRUE)[[1L]]
  values <- list()
  for (part in parts) {
    if (!nzchar(part)) {
      next
    }
    kv <- strsplit(part, "=", fixed = TRUE)[[1L]]
    key <- utils::URLdecode(kv[[1L]] %||% "")
    value <- utils::URLdecode(kv[[2L]] %||% "")
    if (nzchar(key)) {
      values[[key]] <- value
    }
  }
  values
}

#' Start a local StarDist ROI segmentation endpoint
#'
#' Starts an optional local HTTP endpoint for the interactive viewer's
#' `Run segmentation` button. The viewer posts the selected ROI GeoJSON to the
#' endpoint. The endpoint crops that ROI, runs [stardist_segment_roi()], converts
#' polygon or centroid segmentation output to GeoJSON cell overlays, and returns
#' the result and run metadata to the browser.
#'
#' This function requires the optional `httpuv` package and an external StarDist
#' command or script. It is not required for installing or loading wsiTools.
#'
#' @param image A `wsi_slide` object or slide path accepted by [wsi_open()].
#' @param output_dir Directory for ROI crops and StarDist outputs.
#' @param host,port Local host and first port to try for the endpoint.
#' @param path URL path used for segmentation requests.
#' @param max_tries Number of subsequent ports to try if `port` is busy.
#' @param model,command,args,output_type,prob_thresh,nms_thresh,overwrite,level
#'   Arguments passed to [stardist_segment_roi()].
#' @param crop_format Crop image format passed to [stardist_segment_roi()].
#' @param backend Region export backend passed to [stardist_segment_roi()].
#' @param cell_radius Radius used when returning centroid outputs as cell
#'   GeoJSON overlays.
#' @param state Optional live viewer state. When supplied, the endpoint records
#'   the selected ROI and imported segmentation directly in the R session before
#'   returning the overlay to the browser.
#' @param wait If `TRUE`, run the httpuv event loop until interrupted.
#'
#' @return A `wsi_stardist_server` object with the service URL.
#' @export
#'
#' @examples
#' \dontrun{
#' server <- wsi_stardist_server(
#'   "sample.svs",
#'   output_dir = "stardist_server",
#'   command = "python",
#'   args = c("run_stardist.py", "{input}", "{output}", "{model}")
#' )
#'
#' slide <- wsi_open("sample.svs")
#' wsi_viewer(
#'   slide,
#'   mode = "tiles",
#'   segmentation_run_url = server$url
#' )
#' }
wsi_stardist_server <- function(image,
                                output_dir = "wsi_stardist_server",
                                host = "127.0.0.1",
                                port = 8787,
                                path = "/segment",
                                max_tries = 20L,
                                model = "2D_versatile_he",
                                command = NULL,
                                args = NULL,
                                output_type = c("auto", "geojson", "csv", "mask"),
                                prob_thresh = NULL,
                                nms_thresh = NULL,
                                overwrite = TRUE,
                                level = 0,
                                crop_format = c("png", "tiff", "jpeg"),
                                backend = c("auto", "vips", "openslide"),
                                cell_radius = 8,
                                state = NULL,
                                wait = FALSE) {
  if (!requireNamespace("httpuv", quietly = TRUE)) {
    wsi_abort(
      "Starting a local StarDist segmentation endpoint requires the optional package `httpuv`.",
      class = "wsi_missing_dependency"
    )
  }
  output_type <- match.arg(output_type)
  crop_format <- match.arg(crop_format)
  backend <- match.arg(backend)
  cell_radius <- wsi_check_scalar_number(cell_radius, "cell_radius", allow_zero = FALSE)
  port <- as.integer(wsi_check_scalar_number(port, "port", allow_zero = FALSE))
  max_tries <- as.integer(wsi_check_scalar_number(max_tries, "max_tries", allow_zero = TRUE))
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    wsi_abort(sprintf("Could not create output directory: %s", output_dir))
  }
  if (!startsWith(path, "/")) {
    path <- paste0("/", path)
  }
  if (!is.null(state) && !inherits(state, "wsi_viewer_state")) {
    wsi_abort("`state` must be `NULL` or a live `wsi_viewer_state` object.")
  }

  app <- list(
    call = function(req) {
      method <- req$REQUEST_METHOD %||% "GET"
      request_path <- req$PATH_INFO %||% "/"
      if (identical(method, "OPTIONS")) {
        return(wsi_http_json_response(status = 204L, body = ""))
      }
      if (!identical(request_path, path)) {
        return(wsi_http_json_response(status = 404L, body = list(error = "Not found.")))
      }
      if (!identical(method, "POST")) {
        return(wsi_http_json_response(status = 405L, body = list(error = "Use POST with selected ROI GeoJSON.")))
      }

      tryCatch({
        body <- wsi_http_request_body(req)
        if (!nzchar(body)) {
          wsi_abort("Request body did not contain selected ROI GeoJSON.")
        }
        roi_file <- tempfile(fileext = ".geojson")
        on.exit(unlink(roi_file), add = TRUE)
        writeLines(body, roi_file, useBytes = TRUE)
        roi <- read_geojson(roi_file)
        if (!is.null(state)) {
          wsi_viewer_state_set_selected_roi(
            state,
            roi,
            event = "segmentation_requested",
            detail = list(engine = "stardist", output_dir = output_dir)
          )
        }
        result <- stardist_segment_roi(
          image = image,
          roi = roi,
          output_dir = output_dir,
          level = level,
          crop_format = crop_format,
          model = model,
          command = command,
          args = args,
          output_type = output_type,
          prob_thresh = prob_thresh,
          nms_thresh = nms_thresh,
          translate_geojson = TRUE,
          overwrite = overwrite,
          run = TRUE,
          backend = backend
        )
        if (!is.null(state)) {
          wsi_viewer_state_add_segmentation_result(
            state,
            result,
            cell_radius = cell_radius
          )
        }
        wsi_http_json_response(
          body = wsi_stardist_response_body(result, cell_radius = cell_radius)
        )
      }, error = function(err) {
        wsi_http_json_response(
          status = 500L,
          body = list(error = conditionMessage(err))
        )
      })
    }
  )

  last_error <- NULL
  server <- NULL
  used_port <- NULL
  for (candidate in seq.int(port, port + max_tries)) {
    server <- try(httpuv::startServer(host, candidate, app), silent = TRUE)
    if (!inherits(server, "try-error")) {
      used_port <- candidate
      break
    }
    last_error <- conditionMessage(attr(server, "condition"))
    server <- NULL
  }
  if (is.null(server)) {
    wsi_abort(sprintf(
      "Could not start StarDist endpoint near port %d: %s",
      port,
      last_error %||% "unknown error"
    ))
  }
  url <- sprintf("http://%s:%d%s", host, used_port, path)
  out <- structure(
    list(server = server, url = url, host = host, port = used_port, path = path, state = state),
    class = "wsi_stardist_server"
  )
  message("wsiTools StarDist endpoint listening at ", url)
  message("Create the viewer with `segmentation_run_url = \"", url, "\"`.")
  if (isTRUE(wait)) {
    message("Press Ctrl+C or Esc to stop the segmentation endpoint.")
    on.exit(httpuv::stopServer(server), add = TRUE)
    repeat httpuv::service(100)
  }
  out
}

#' @export
print.wsi_stardist_server <- function(x, ...) {
  cat("<wsi_stardist_server>\n")
  cat(sprintf("  url: %s\n", x$url))
  cat("  stop with: httpuv::stopServer(x$server)\n")
  invisible(x)
}

#' Add segmentation overlays to a viewer
#'
#' Displays imported segmentation results with the existing viewer tools. ROI
#' segmentations are overlaid in [wsi_viewer()]. Centroid tables are converted
#' to small circular cell overlays. Mask segmentations are shown in a
#' side-by-side comparison with the mask overlaid on the right panel.
#'
#' @param viewer A `wsi_slide` object.
#' @param segmentation A segmentation object or path accepted by
#'   [import_segmentation()].
#' @param output Optional HTML output path.
#' @param open Whether to open the HTML viewer.
#' @param cell_radius Radius, in slide pixels, for centroid cell markers.
#' @param ... Additional arguments passed to [wsi_viewer()] or
#'   [viewer_compare()].
#'
#' @return The HTML viewer path, invisibly.
#' @export
viewer_add_segmentation <- function(viewer, segmentation, output = NULL,
                                    open = interactive(), cell_radius = 8, ...) {
  if (!inherits(viewer, "wsi_slide")) {
    wsi_abort("`viewer` must be a `wsi_slide` object.")
  }
  if (is.character(segmentation) && length(segmentation) == 1L) {
    segmentation <- import_segmentation(segmentation)
  }

  if (inherits(segmentation, "wsi_roi")) {
    return(wsi_viewer(viewer, roi = segmentation, output = output, open = open, ...))
  }

  if (inherits(segmentation, "wsi_segmentation_centroids")) {
    rois <- wsi_segmentation_to_rois(segmentation, radius = cell_radius)
    return(wsi_viewer(viewer, roi = rois, output = output, open = open, ...))
  }

  if (inherits(segmentation, "wsi_segmentation_mask")) {
    return(viewer_compare(
      viewer,
      viewer,
      sync = TRUE,
      mask2 = segmentation$path,
      output = output,
      open = open,
      title = "wsiTools segmentation overlay",
      ...
    ))
  }

  wsi_abort("Unsupported segmentation object.")
}

#' @rdname viewer_add_segmentation
#' @export
wsi_viewer_add_segmentation <- viewer_add_segmentation
