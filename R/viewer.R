wsi_html_escape <- function(x) {
  x <- wsi_clean_text(as.character(x %||% ""))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

wsi_image_data_uri <- function(path, mime = "image/png") {
  size <- file.info(path)$size
  if (is.na(size) || size <= 0) {
    wsi_abort(sprintf("Could not read viewer image file: %s", path))
  }
  bytes <- readBin(path, what = "raw", n = size)
  paste0("data:", mime, ";base64,", jsonlite::base64_enc(bytes))
}

wsi_mock_viewer_data_uri <- function(width, height) {
  svg <- sprintf(
    paste0(
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">",
      "<rect width=\"100%%\" height=\"100%%\" fill=\"#f7f7f7\"/>",
      "<path d=\"M0 0L%d %dM%d 0L0 %d\" stroke=\"#d0d0d0\" stroke-width=\"4\"/>",
      "<text x=\"50%%\" y=\"50%%\" dominant-baseline=\"middle\" text-anchor=\"middle\" ",
      "font-family=\"sans-serif\" font-size=\"28\" fill=\"#555\">mock WSI thumbnail</text>",
      "</svg>"
    ),
    width, height, width, height, width, height, width, height
  )
  paste0("data:image/svg+xml;base64,", jsonlite::base64_enc(charToRaw(svg)))
}

wsi_viewer_thumbnail_data_uri <- function(slide, width, height = NULL) {
  if (identical(slide$backend, "mock")) {
    thumb_height <- height %||% max(1L, as.integer(round(width * slide$dimensions[["height"]] / slide$dimensions[["width"]])))
    return(wsi_mock_viewer_data_uri(width, thumb_height))
  }
  if (identical(slide$backend, "omezarr")) {
    return(wsi_omezarr_placeholder_data_uri(slide, width = width))
  }

  if (identical(slide$backend, "imagemagick") && !wsi_has_vips()) {
    tmp <- tempfile(fileext = ".png")
    on.exit(unlink(tmp), add = TRUE)
    wsi_imagemagick_thumbnail_file(slide, tmp, width = width, height = height)
    return(wsi_image_data_uri(tmp, mime = "image/png"))
  }

  if (!wsi_has_vips()) {
    wsi_abort(
      wsi_backend_action_message(
        "Interactive viewing requires libvips for real slides in this milestone.",
        backend = "vips"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  args <- c("thumbnail", slide$path, tmp, as.character(width))
  if (!is.null(height)) {
    args <- c(args, "--height", as.character(height))
  }
  wsi_run_command("vips", args = args, error_message = "libvips failed to create a viewer thumbnail.")
  wsi_image_data_uri(tmp, mime = "image/png")
}

wsi_viewer_navigator_timeout <- function() {
  timeout <- suppressWarnings(as.numeric(Sys.getenv("WSITOOLS_NAVIGATOR_TIMEOUT", unset = "8")))
  if (!is.finite(timeout) || timeout < 0) {
    timeout <- 8
  }
  timeout
}

wsi_viewer_navigator_data_uri <- function(slide, width = 512) {
  timeout <- wsi_viewer_navigator_timeout()
  if (timeout == 0) {
    return(NULL)
  }
  if (identical(slide$backend, "mock") || identical(slide$backend, "omezarr") ||
      (identical(slide$backend, "imagemagick") && !wsi_has_vips())) {
    return(tryCatch(
      wsi_viewer_thumbnail_data_uri(slide, width = width, height = NULL),
      error = function(err) NULL
    ))
  }
  if (!wsi_has_vips()) {
    return(NULL)
  }

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  args <- c("thumbnail", slide$path, tmp, as.character(width))
  output <- tryCatch(
    suppressWarnings(system2("vips", args = wsi_system2_args(args), stdout = TRUE, stderr = TRUE, timeout = timeout)),
    error = function(err) structure(conditionMessage(err), status = 1L)
  )
  status <- attr(output, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L) || !file.exists(tmp)) {
    wsi_warn(
      sprintf(
        "Skipping navigator preview because libvips did not create it within %s second%s. Full-resolution tiled viewing is still available.",
        format(timeout, trim = TRUE, scientific = FALSE),
        if (identical(timeout, 1)) "" else "s"
      )
    )
    return(NULL)
  }
  tryCatch(wsi_image_data_uri(tmp, mime = "image/png"), error = function(err) NULL)
}

wsi_url_encode_path <- function(path) {
  utils::URLencode(path, reserved = FALSE)
}

wsi_file_url <- function(path) {
  paste0("file://", wsi_url_encode_path(normalizePath(path, winslash = "/", mustWork = FALSE)))
}

wsi_default_tile_dir <- function(output) {
  file.path(
    dirname(output),
    paste0(tools::file_path_sans_ext(basename(output)), "_tiles")
  )
}

wsi_tile_base_url <- function(tile_dir, output) {
  tile_files <- file.path(tile_dir, "slide_files")
  output_dir <- normalizePath(dirname(output), winslash = "/", mustWork = FALSE)
  tile_parent <- normalizePath(dirname(tile_dir), winslash = "/", mustWork = FALSE)

  if (identical(output_dir, tile_parent)) {
    return(wsi_url_encode_path(file.path(basename(tile_dir), "slide_files")))
  }

  wsi_file_url(tile_files)
}

wsi_dz_max_level <- function(width, height) {
  ceiling(log2(max(width, height)))
}

wsi_dz_suffix <- function(tile_format, quality) {
  if (identical(tile_format, "jpg")) {
    return(sprintf(".jpg[Q=%d]", as.integer(quality)))
  }
  ".png"
}

wsi_dzi_overlap <- function(dzi_file, default = 0L) {
  if (!file.exists(dzi_file)) {
    return(as.integer(default))
  }
  line <- tryCatch(readLines(dzi_file, warn = FALSE, n = 3L), error = function(err) character())
  text <- paste(line, collapse = " ")
  value <- suppressWarnings(as.integer(sub(".*\\bOverlap=\"([0-9]+)\".*", "\\1", text)))
  if (length(value) != 1L || is.na(value) || !is.finite(value) || value < 0L) {
    return(as.integer(default))
  }
  as.integer(value)
}

wsi_dzi_dimensions <- function(dzi_file) {
  if (!file.exists(dzi_file)) {
    return(c(width = NA_real_, height = NA_real_))
  }
  text <- tryCatch(paste(readLines(dzi_file, warn = FALSE), collapse = " "), error = function(err) "")
  width <- suppressWarnings(as.numeric(sub(".*\\bWidth=\"([0-9.]+)\".*", "\\1", text)))
  height <- suppressWarnings(as.numeric(sub(".*\\bHeight=\"([0-9.]+)\".*", "\\1", text)))
  c(
    width = if (length(width) == 1L && is.finite(width)) width else NA_real_,
    height = if (length(height) == 1L && is.finite(height)) height else NA_real_
  )
}

wsi_deepzoom_metadata_file <- function(tile_dir) {
  file.path(tile_dir, "slide.wsiTools.json")
}

wsi_deepzoom_env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) {
    return(isTRUE(default))
  }
  tolower(value) %in% c("1", "true", "yes", "y", "on")
}

wsi_deepzoom_slide_signature <- function(slide) {
  path <- as.character(slide$path %||% "")
  path_norm <- if (nzchar(path)) normalizePath(path, winslash = "/", mustWork = FALSE) else ""
  info <- if (nzchar(path) && file.exists(path)) file.info(path) else NULL
  list(
    path = path_norm,
    basename = basename(path_norm),
    size = if (!is.null(info)) unname(as.numeric(info$size)) else NA_real_,
    mtime = if (!is.null(info)) format(info$mtime, tz = "UTC", usetz = TRUE) else NA_character_,
    width = unname(as.numeric(slide$dimensions[["width"]] %||% NA_real_)),
    height = unname(as.numeric(slide$dimensions[["height"]] %||% NA_real_)),
    backend = as.character(slide$backend %||% "")
  )
}

wsi_deepzoom_metadata <- function(slide, tile_size, tile_overlap, tile_format, quality) {
  c(
    wsi_deepzoom_slide_signature(slide),
    list(
      tile_size = as.integer(tile_size),
      tile_overlap = as.integer(tile_overlap),
      tile_format = as.character(tile_format),
      quality = as.integer(quality),
      wsiTools_version = as.character(utils::packageVersion("wsiTools")),
      created = format(Sys.time(), tz = "UTC", usetz = TRUE)
    )
  )
}

wsi_write_deepzoom_metadata <- function(tile_dir, metadata) {
  metadata_file <- wsi_deepzoom_metadata_file(tile_dir)
  tryCatch(
    jsonlite::write_json(metadata, metadata_file, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    error = function(err) {
      wsi_warn(sprintf("Could not write Deep Zoom cache metadata `%s`: %s", metadata_file, conditionMessage(err)))
    }
  )
  invisible(metadata_file)
}

wsi_read_deepzoom_metadata <- function(tile_dir) {
  metadata_file <- wsi_deepzoom_metadata_file(tile_dir)
  if (!file.exists(metadata_file)) {
    return(NULL)
  }
  tryCatch(jsonlite::read_json(metadata_file, simplifyVector = TRUE), error = function(err) NULL)
}

wsi_deepzoom_metadata_matches <- function(existing, expected) {
  if (is.null(existing)) {
    return(FALSE)
  }
  keys <- c("path", "size", "mtime", "width", "height", "backend",
            "tile_size", "tile_overlap", "tile_format", "quality")
  for (key in keys) {
    lhs <- existing[[key]]
    rhs <- expected[[key]]
    if (is.null(lhs) || is.null(rhs)) {
      return(FALSE)
    }
    if (key %in% c("size", "width", "height")) {
      if (!isTRUE(all.equal(as.numeric(lhs), as.numeric(rhs), tolerance = 1e-8))) {
        return(FALSE)
      }
    } else if (!identical(as.character(lhs), as.character(rhs))) {
      return(FALSE)
    }
  }
  TRUE
}

wsi_deepzoom_cache_status <- function(slide, tile_dir, dzi_file, tile_files,
                                      tile_size, tile_overlap, tile_format, quality) {
  if (!file.exists(dzi_file) || !dir.exists(tile_files)) {
    return(list(valid = FALSE, reason = "missing"))
  }
  expected <- wsi_deepzoom_metadata(slide, tile_size, tile_overlap, tile_format, quality)
  existing <- wsi_read_deepzoom_metadata(tile_dir)
  if (wsi_deepzoom_metadata_matches(existing, expected)) {
    return(list(valid = TRUE, reason = "metadata", expected = expected, existing = existing))
  }

  dzi_dims <- wsi_dzi_dimensions(dzi_file)
  expected_dims <- c(width = expected$width, height = expected$height)
  dims_match <- isTRUE(all.equal(unname(dzi_dims), unname(expected_dims), tolerance = 1e-8))
  if (is.null(existing) && dims_match &&
      isTRUE(wsi_deepzoom_env_flag("WSITOOLS_TRUST_LEGACY_DEEPZOOM_CACHE", FALSE))) {
    return(list(valid = TRUE, reason = "legacy-trusted", expected = expected, existing = existing))
  }
  reason <- if (is.null(existing)) "missing metadata" else "metadata mismatch"
  list(valid = FALSE, reason = reason, expected = expected, existing = existing)
}

wsi_create_deepzoom_tiles <- function(slide, tile_dir, tile_size = 512,
                                      tile_overlap = 1,
                                      tile_format = c("jpg", "png"),
                                      quality = 90, rebuild = FALSE) {
  tile_format <- match.arg(tile_format)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
  if (tile_overlap >= tile_size) {
    wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
  }
  quality <- as.integer(wsi_check_scalar_number(quality, "quality", allow_zero = FALSE))
  if (quality > 100L) {
    wsi_abort("`quality` must be between 1 and 100.")
  }

  if (identical(slide$backend, "mock")) {
    wsi_abort("Full-resolution tiled viewing is not available for mock slides.")
  }
  if (!wsi_has_vips()) {
    wsi_abort(
      wsi_backend_action_message(
        "Full-resolution tiled viewing requires libvips.",
        backend = "vips"
      ),
      class = "wsi_backend_unavailable"
    )
  }

  if (!dir.exists(tile_dir)) {
    dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(tile_dir)) {
    wsi_abort(sprintf("Could not create tile directory: %s", tile_dir))
  }

  dzi_base <- file.path(tile_dir, "slide")
  dzi_file <- paste0(dzi_base, ".dzi")
  tile_files <- paste0(dzi_base, "_files")

  if (file.exists(dzi_file) && dir.exists(tile_files) && !isTRUE(rebuild)) {
    cache_status <- wsi_deepzoom_cache_status(
      slide = slide,
      tile_dir = tile_dir,
      dzi_file = dzi_file,
      tile_files = tile_files,
      tile_size = tile_size,
      tile_overlap = tile_overlap,
      tile_format = tile_format,
      quality = quality
    )
    if (isTRUE(cache_status$valid)) {
      return(list(dzi = dzi_file, tiles = tile_files, overlap = wsi_dzi_overlap(dzi_file, default = tile_overlap)))
    }
    wsi_warn(sprintf(
      "Existing Deep Zoom tiles in `%s` do not match the requested image (%s); rebuilding the cache.",
      tile_dir,
      cache_status$reason %||% "stale cache"
    ))
    rebuild <- TRUE
  }

  if (isTRUE(rebuild)) {
    if (file.exists(dzi_file)) {
      unlink(dzi_file)
    }
    if (dir.exists(tile_files)) {
      unlink(tile_files, recursive = TRUE)
    }
    metadata_file <- wsi_deepzoom_metadata_file(tile_dir)
    if (file.exists(metadata_file)) {
      unlink(metadata_file)
    }
  } else if (file.exists(dzi_file) || dir.exists(tile_files)) {
    wsi_abort(
      sprintf(
        "Deep Zoom output already exists in `%s`. Use `rebuild = TRUE` to replace it.",
        tile_dir
      ),
      class = "wsi_output_exists"
    )
  }

  args <- c(
    "dzsave",
    slide$path,
    dzi_base,
    "--layout",
    "dz",
    "--tile-size",
    as.character(tile_size),
    "--overlap",
    as.character(tile_overlap),
    "--suffix",
    wsi_dz_suffix(tile_format, quality)
  )

  wsi_run_command(
    "vips",
    args = args,
    error_message = "libvips failed to create Deep Zoom tiles for the interactive viewer."
  )

  if (!file.exists(dzi_file) || !dir.exists(tile_files)) {
    wsi_abort("libvips completed but did not create the expected Deep Zoom output.")
  }

  wsi_write_deepzoom_metadata(
    tile_dir,
    wsi_deepzoom_metadata(slide, tile_size, tile_overlap, tile_format, quality)
  )

  list(dzi = dzi_file, tiles = tile_files, overlap = tile_overlap)
}

wsi_viewer_point <- function(point) {
  if (is.numeric(point) && length(point) >= 2L) {
    return(list(x = unname(as.numeric(point[[1L]])), y = unname(as.numeric(point[[2L]]))))
  }
  if (is.list(point) && length(point) >= 2L &&
      is.numeric(point[[1L]]) && is.numeric(point[[2L]])) {
    return(list(x = unname(as.numeric(point[[1L]][[1L]])), y = unname(as.numeric(point[[2L]][[1L]]))))
  }
  NULL
}

wsi_viewer_ring <- function(ring) {
  if (is.matrix(ring) || is.data.frame(ring)) {
    if (ncol(ring) < 2L) {
      return(list())
    }
    points <- lapply(seq_len(nrow(ring)), function(i) {
      list(x = unname(as.numeric(ring[i, 1L])), y = unname(as.numeric(ring[i, 2L])))
    })
  } else if (is.list(ring)) {
    points <- lapply(ring, wsi_viewer_point)
    points <- points[!vapply(points, is.null, logical(1))]
  } else {
    points <- list()
  }

  if (length(points) < 3L) {
    return(list())
  }
  points
}

wsi_viewer_roi_rings <- function(geometry_type, coordinates) {
  groups <- wsi_viewer_roi_ring_groups(geometry_type, coordinates)
  if (!length(groups)) {
    return(list())
  }
  unlist(groups, recursive = FALSE)
}

wsi_viewer_roi_ring_groups <- function(geometry_type, coordinates) {
  geometry_type <- tolower(geometry_type %||% "")

  if (identical(geometry_type, "polygon")) {
    rings <- lapply(coordinates, wsi_viewer_ring)
    rings <- rings[vapply(rings, length, integer(1)) >= 3L]
    return(if (length(rings)) list(rings) else list())
  }

  if (identical(geometry_type, "multipolygon")) {
    groups <- lapply(coordinates, function(polygon) {
      polygon_rings <- lapply(polygon, wsi_viewer_ring)
      polygon_rings[vapply(polygon_rings, length, integer(1)) >= 3L]
    })
    return(groups[vapply(groups, length, integer(1)) > 0L])
  }

  list()
}

wsi_viewer_ring_area <- function(points) {
  if (length(points) < 3L) {
    return(NA_real_)
  }
  x <- vapply(points, `[[`, numeric(1), "x")
  y <- vapply(points, `[[`, numeric(1), "y")
  if (x[[1L]] != x[[length(x)]] || y[[1L]] != y[[length(y)]]) {
    x <- c(x, x[[1L]])
    y <- c(y, y[[1L]])
  }
  abs(sum(x[-1L] * y[-length(y)] - x[-length(x)] * y[-1L]) / 2)
}

wsi_viewer_polygon_area <- function(rings) {
  if (!length(rings)) {
    return(NA_real_)
  }
  areas <- vapply(rings, wsi_viewer_ring_area, numeric(1))
  if (all(is.na(areas))) {
    return(NA_real_)
  }
  outer <- areas[[1L]]
  holes <- if (length(areas) > 1L) sum(areas[-1L], na.rm = TRUE) else 0
  max(0, outer - holes)
}

wsi_viewer_ring_groups_area <- function(groups) {
  if (!length(groups)) {
    return(NA_real_)
  }
  areas <- vapply(groups, wsi_viewer_polygon_area, numeric(1))
  if (all(is.na(areas))) {
    return(NA_real_)
  }
  sum(areas, na.rm = TRUE)
}

wsi_viewer_point_count <- function(coordinates) {
  nrow(wsi_collect_points(coordinates))
}

wsi_viewer_hex_to_rgba <- function(hex, alpha = 0.15) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1L], rgb[2L], rgb[3L], alpha)
}

wsi_viewer_class_presets_payload <- function(presets = NULL) {
  presets <- wsi_normalize_roi_class_presets(presets)
  lapply(seq_len(nrow(presets)), function(i) {
    list(
      class = presets$class[[i]],
      label = presets$label[[i]],
      color = presets$color[[i]],
      export = isTRUE(presets$export[[i]]),
      export_rule = presets$export_rule[[i]]
    )
  })
}

wsi_viewer_class_options <- function(presets = NULL) {
  presets <- wsi_normalize_roi_class_presets(presets)
  paste0(
    vapply(seq_len(nrow(presets)), function(i) {
      paste0(
        "<option value=\"", wsi_html_escape(presets$class[[i]]), "\">",
        wsi_html_escape(presets$label[[i]]),
        "</option>"
      )
    }, character(1)),
    collapse = ""
  )
}

wsi_viewer_roi_colour <- function(roi, index, fallback) {
  if (!"color" %in% names(roi)) {
    return(fallback)
  }
  colour <- roi$color[[index]]
  if (is.na(colour) || !nzchar(colour)) {
    return(fallback)
  }
  ok <- tryCatch(
    {
      grDevices::col2rgb(colour)
      TRUE
    },
    error = function(err) FALSE
  )
  if (isTRUE(ok)) colour else fallback
}

wsi_viewer_roi_features <- function(roi = NULL, fill_alpha = 0.15,
                                    class_presets = NULL) {
  if (is.null(roi)) {
    return(list())
  }
  if (is.character(roi) && length(roi) == 1L) {
    roi <- wsi_read_geojson(roi)
  }
  if (!inherits(roi, "wsi_roi")) {
    wsi_abort("`roi` must be a GeoJSON path or an object returned by `wsi_read_geojson()`.")
  }

  palette <- c("#00BFC4", "#F8766D", "#7CAE00", "#C77CFF", "#E69F00", "#56B4E9", "#CC79A7")
  class_presets <- wsi_normalize_roi_class_presets(class_presets)
  preset_lookup <- stats::setNames(seq_len(nrow(class_presets)), tolower(class_presets$class))
  features <- list()

  for (i in seq_len(nrow(roi))) {
    roi_class <- as.character(roi$class[[i]] %||% "annotation")
    fallback_colour <- wsi_roi_class_colour(
      roi_class,
      presets = class_presets,
      fallback = palette[((i - 1L) %% length(palette)) + 1L]
    )
    preset_idx <- preset_lookup[tolower(roi_class)]
    if (!is.na(preset_idx)) {
      fallback_colour <- class_presets$color[[unname(preset_idx)]]
    }
    colour <- if (nzchar(trimws(roi_class))) {
      fallback_colour
    } else {
      wsi_viewer_roi_colour(roi, i, fallback_colour)
    }
    ring_groups <- wsi_viewer_roi_ring_groups(roi$geometry_type[[i]], roi$coordinates[[i]])
    rings <- if (length(ring_groups)) ring_groups[[1L]] else list()
    add_groups <- if (length(ring_groups) > 1L) ring_groups[-1L] else list()
    drawable <- length(ring_groups) > 0L
    properties <- if ("properties" %in% names(roi)) wsi_geojson_list(roi$properties[[i]]) else list()
    geometry <- if ("geometry" %in% names(roi)) wsi_geojson_list(roi$geometry[[i]]) else list()
    if (!length(geometry)) {
      geometry <- list(
        type = as.character(roi$geometry_type[[i]] %||% NA_character_),
        coordinates = roi$coordinates[[i]]
      )
    }
    feature <- if ("feature" %in% names(roi)) wsi_geojson_list(roi$feature[[i]]) else list()
    measurements <- if ("measurements" %in% names(roi)) roi$measurements[[i]] else list()
    features[[length(features) + 1L]] <- list(
      id = as.character(roi$roi_id[[i]]),
      name = as.character(roi$name[[i]] %||% roi$roi_id[[i]]),
      label = as.character(roi$name[[i]] %||% roi$roi_id[[i]]),
      class = as.character(roi$class[[i]] %||% NA_character_),
      visible = TRUE,
      locked = if ("is_locked" %in% names(roi)) isTRUE(roi$is_locked[[i]]) else FALSE,
      geometry_type = as.character(roi$geometry_type[[i]] %||% NA_character_),
      source = "geojson",
      drawable = drawable,
      point_count = wsi_viewer_point_count(roi$coordinates[[i]]),
      area = if (drawable) wsi_viewer_ring_groups_area(ring_groups) else NA_real_,
      bbox = list(
        xmin = unname(as.numeric(roi$xmin[[i]])),
        ymin = unname(as.numeric(roi$ymin[[i]])),
        xmax = unname(as.numeric(roi$xmax[[i]])),
        ymax = unname(as.numeric(roi$ymax[[i]]))
      ),
      coordinates = roi$coordinates[[i]],
      colour = colour,
      original_colour = colour,
      fill = wsi_viewer_hex_to_rgba(colour, alpha = fill_alpha),
      rings = rings,
      add_groups = add_groups,
      measurements = measurements,
      properties = properties,
      geometry = geometry,
      feature = feature
    )
  }

  features
}

wsi_viewer_styles <- function(background = "#101010") {
  paste0(
    "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:", background, ";color:#f1f1f1;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;}\n",
    "#viewer{display:block;width:100vw;height:100vh;background:", background, ";cursor:grab;}\n",
    "#viewer.osdViewer{position:fixed;inset:0;overflow:hidden;}\n",
    "#viewer .openseadragon-canvas canvas{image-rendering:auto;}\n",
    "#overlay{position:fixed;inset:0;width:100vw;height:100vh;z-index:4;background:transparent;cursor:grab;touch-action:none;}\n",
    "#multiViewGrid{position:fixed;inset:0;z-index:5;display:none;grid-template-columns:1fr;grid-template-rows:1fr;gap:3px;padding:3px;box-sizing:border-box;background:#050505;pointer-events:auto;}\n",
    "body.multiViewActive #multiViewGrid{display:grid;}\n",
    "body.multiViewActive #viewer.osdViewer{visibility:hidden;pointer-events:none;}\n",
    "body.multiViewActive #overlay{display:none;}\n",
    "#multiViewGrid.layout2{grid-template-columns:repeat(2,minmax(0,1fr));grid-template-rows:1fr;}\n",
    "#multiViewGrid.layout4{grid-template-columns:repeat(2,minmax(0,1fr));grid-template-rows:repeat(2,minmax(0,1fr));}\n",
    "#multiViewGrid.layout6{grid-template-columns:repeat(3,minmax(0,1fr));grid-template-rows:repeat(2,minmax(0,1fr));}\n",
    "#multiViewGrid.layoutCustom{grid-template-columns:repeat(var(--multi-view-cols,2),minmax(0,1fr));grid-template-rows:repeat(var(--multi-view-rows,2),minmax(0,1fr));}\n",
    ".multiViewPane{position:relative;overflow:hidden;background:#070707;border:1px solid rgba(255,255,255,.16);border-radius:4px;min-width:0;min-height:0;}\n",
    ".multiViewPane.blank{background:#090b10;border-style:dashed;border-color:rgba(148,163,184,.42);}\n",
    ".multiViewPane.active{border-color:#5eead4;box-shadow:inset 0 0 0 1px rgba(94,234,212,.75);}\n",
    ".multiViewPane.dropTarget{border-color:#facc15;box-shadow:inset 0 0 0 2px rgba(250,204,21,.78);}\n",
    ".multiViewPaneViewer{position:absolute!important;inset:0!important;width:100%!important;height:100%!important;min-height:0;}\n",
    "body.multiViewActive .multiViewPaneViewer{pointer-events:none;}\n",
    ".multiViewPaneViewer>.openseadragon-container,.multiViewPaneViewer .openseadragon-canvas{width:100%!important;height:100%!important;}\n",
    ".multiViewPaneBlank{position:absolute;inset:0;z-index:3;display:flex;align-items:center;justify-content:center;padding:18px;text-align:center;color:#cbd5e1;font-size:13px;line-height:1.35;background:linear-gradient(135deg,rgba(15,23,42,.78),rgba(2,6,23,.72));pointer-events:none;}\n",
    ".multiViewPaneOverlay{position:absolute;inset:0;z-index:20;width:100%;height:100%;background:transparent;touch-action:none;cursor:grab;pointer-events:auto!important;}\n",
    ".multiViewPaneOverlay.dragging{cursor:grabbing;}\n",
    ".multiViewPaneOverlay.selecting,.multiViewPaneOverlay.drawing,.multiViewPaneOverlay.editing,.multiViewPaneOverlay.measuring,.multiViewPaneOverlay.trajectory,.multiViewPaneOverlay.screenshot{cursor:crosshair;}\n",
    ".multiViewPaneOverlay.brushing,.multiViewPaneOverlay.brush-add,.multiViewPaneOverlay.brush-subtract{cursor:none;}\n",
    ".multiViewPaneOverlay.cursor-blocked{cursor:not-allowed;}\n",
    ".multiViewPaneTitle{position:absolute;left:8px;top:8px;z-index:2;padding:4px 7px;border-radius:999px;background:rgba(0,0,0,.62);border:1px solid rgba(255,255,255,.20);font-size:11px;line-height:1;color:#e5e7eb;pointer-events:none;}\n",
    ".multiViewResizeLayer{position:absolute;inset:0;z-index:70;pointer-events:none;}\n",
    ".multiViewResizeHandle{position:absolute;pointer-events:auto;background:rgba(0,0,0,.01);touch-action:none;}\n",
    ".multiViewResizeHandle::after{content:'';position:absolute;background:rgba(94,234,212,.38);opacity:.72;transition:opacity .12s ease,background .12s ease,box-shadow .12s ease;box-shadow:0 0 0 1px rgba(0,0,0,.30);}\n",
    ".multiViewResizeHandle:hover::after,body.multiViewResizing .multiViewResizeHandle.active::after{opacity:1;background:rgba(94,234,212,.92);box-shadow:0 0 0 1px rgba(0,0,0,.45),0 0 12px rgba(94,234,212,.42);}\n",
    ".multiViewResizeHandle.col{top:3px;bottom:3px;width:22px;transform:translateX(-50%);cursor:col-resize;}\n",
    ".multiViewResizeHandle.col::after{left:9px;top:0;bottom:0;width:4px;border-radius:999px;}\n",
    ".multiViewResizeHandle.row{left:3px;right:3px;height:22px;transform:translateY(-50%);cursor:row-resize;}\n",
    ".multiViewResizeHandle.row::after{top:9px;left:0;right:0;height:4px;border-radius:999px;}\n",
    "body.multiViewResizing{cursor:grabbing!important;user-select:none;}\n",
    "#viewer.dragging,#overlay.dragging{cursor:grabbing;}\n",
    "#viewer.selecting,#viewer.drawing,#viewer.editing,#overlay.selecting,#overlay.drawing,#overlay.editing{cursor:crosshair;}\n",
    "#viewer.measuring,#viewer.trajectory,#overlay.measuring,#overlay.trajectory{cursor:crosshair;}\n",
    "#viewer.screenshot,#overlay.screenshot{cursor:crosshair;}\n",
    "#viewer.brushing,#overlay.brushing,#viewer.brush-add,#overlay.brush-add,#viewer.brush-subtract,#overlay.brush-subtract{cursor:none;}\n",
    "#viewer.cursor-blocked,#overlay.cursor-blocked{cursor:not-allowed;}\n",
    ".bar{position:fixed;left:12px;right:12px;top:12px;display:flex;gap:8px;align-items:center;pointer-events:none;z-index:30;}\n",
    ":root{--wsi-side-panel-bg:rgba(18,18,18,.94);}\n",
    ".panel{background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;backdrop-filter:blur(6px);pointer-events:auto;}\n",
    ".titleLine{display:flex;align-items:center;gap:8px;min-width:0;}\n",
    ".title{font-weight:600;max-width:34vw;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".meta{font-size:12px;color:#d2d2d2;}\n",
    ".unsavedIndicator{display:none;align-items:center;gap:5px;flex:0 0 auto;border:1px solid rgba(250,204,21,.45);border-radius:999px;background:rgba(113,63,18,.72);color:#fde68a;font-size:11px;line-height:1;padding:3px 7px;cursor:pointer;user-select:none;}\n",
    ".unsavedIndicator::before{content:'';width:7px;height:7px;border-radius:50%;background:#facc15;box-shadow:0 0 0 2px rgba(250,204,21,.18);}\n",
    ".unsavedIndicator.dirty{display:inline-flex;}\n",
    ".unsavedIndicator:focus-visible{outline:2px solid #facc15;outline-offset:2px;}\n",
    ".spacer{flex:1;}\n",
    ".tools{display:flex;gap:6px;align-items:center;flex-wrap:wrap;justify-content:flex-end;position:relative;}\n",
    ".sep{width:1px;height:22px;background:rgba(255,255,255,.18);display:inline-block;}\n",
    ".navPanButton{width:30px;height:28px;padding:5px;display:inline-flex;align-items:center;justify-content:center;}\n",
    ".iconMove{width:17px;height:17px;display:block;}\n",
    ".navDock{position:fixed;right:14px;top:50%;transform:translateY(-50%);z-index:32;display:flex;flex-direction:column;gap:6px;padding:7px;}\n",
    ".navDock button{width:42px;height:36px;padding:0;display:flex;align-items:center;justify-content:center;font-weight:650;}\n",
    ".navDock .imageTransformButton{font-size:0;}\n",
    ".navDock .navIcon{width:18px;height:18px;display:block;}\n",
    ".navDock .screenshotButton{font-size:0;}\n",
    ".imageTransformRefreshing .openseadragon-canvas canvas,.imageTransformRefreshing .openseadragon-canvas img{opacity:0!important;}\n",
    "button{appearance:none;border:1px solid rgba(255,255,255,.24);background:#252525;color:#f2f2f2;border-radius:5px;padding:6px 9px;font-size:13px;line-height:1;}\n",
    "button:hover{background:#333;}\n",
    "button.active{background:#0f766e;border-color:#5eead4;color:#fff;}\n",
    "button:disabled{opacity:.38;cursor:not-allowed;}\n",
    ".toolMenu{position:relative;}\n",
    ".toolMenu summary{list-style:none;appearance:none;display:inline-flex;align-items:center;gap:7px;border:1px solid rgba(255,255,255,.24);background:#252525;color:#f2f2f2;border-radius:5px;padding:6px 9px;font-size:13px;line-height:1;cursor:pointer;user-select:none;}\n",
    ".toolMenu summary::-webkit-details-marker{display:none;}\n",
    ".toolMenu summary::after{content:'';width:6px;height:6px;border-right:1.6px solid #bdbdbd;border-bottom:1.6px solid #bdbdbd;transform:rotate(45deg) translate(-1px,-1px);transform-origin:center;transition:transform .14s ease,border-color .14s ease;}\n",
    ".toolMenu[open] summary{background:#333;border-color:#5eead4;}\n",
    ".toolMenu[open] summary::after{border-color:#5eead4;transform:rotate(225deg) translate(-1px,-1px);}\n",
    "#jobSyncIndicator{display:inline-flex;align-items:center;justify-content:flex-start;gap:6px;flex:0 0 158px;width:158px;min-width:158px;max-width:158px;box-sizing:border-box;border-radius:999px;padding:5px 9px;background:rgba(37,37,37,.94);border:1px solid rgba(255,255,255,.22);color:#e5e7eb;font-size:12px;line-height:1;white-space:nowrap;overflow:hidden;}\n",
    "#jobSyncIndicator:hover{background:#333;border-color:#5eead4;}\n",
    "#jobSyncIndicator .syncDot{width:8px;height:8px;border-radius:50%;background:#94a3b8;box-shadow:0 0 0 2px rgba(148,163,184,.16);flex:0 0 auto;}\n",
    "#jobSyncIndicator .syncLabel{font-weight:650;overflow:hidden;text-overflow:ellipsis;flex:0 1 auto;min-width:0;}\n",
    "#jobSyncIndicator .syncDetail{color:#cbd5e1;overflow:hidden;text-overflow:ellipsis;flex:1 1 auto;min-width:0;max-width:none;}\n",
    "#jobSyncIndicator.off .syncDot{background:#64748b;box-shadow:0 0 0 2px rgba(100,116,139,.18);}\n",
    "#jobSyncIndicator.pending .syncDot{background:#facc15;box-shadow:0 0 0 2px rgba(250,204,21,.22);}\n",
    "#jobSyncIndicator.running .syncDot{background:#60a5fa;box-shadow:0 0 0 2px rgba(96,165,250,.22);animation:wsiSyncPulse 1.25s ease-in-out infinite;}\n",
    "#jobSyncIndicator.completed .syncDot,#jobSyncIndicator.idle .syncDot{background:#2dd4bf;box-shadow:0 0 0 2px rgba(45,212,191,.20);}\n",
    "#jobSyncIndicator.failed{border-color:rgba(248,113,113,.55);background:rgba(127,29,29,.82);color:#fecaca;}\n",
    "#jobSyncIndicator.failed .syncDot{background:#f87171;box-shadow:0 0 0 2px rgba(248,113,113,.24);}\n",
    "@keyframes wsiSyncPulse{0%,100%{opacity:.55;transform:scale(.92)}50%{opacity:1;transform:scale(1.18)}}\n",
    ".menuBody{position:absolute;right:0;top:calc(100% + 7px);z-index:20;min-width:230px;max-width:min(360px,calc(100vw - 24px));max-height:calc(100vh - 108px);overflow:auto;background:rgba(18,18,18,.96);border:1px solid rgba(255,255,255,.18);border-radius:6px;padding:8px;box-shadow:0 16px 36px rgba(0,0,0,.34);display:flex;flex-direction:column;gap:6px;}\n",
    ".menuGrid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px;}\n",
    ".menuBody button{width:100%;text-align:center;}\n",
    ".menuBody label.control{justify-content:space-between;gap:10px;min-height:28px;}\n",
    ".proximityControlGrid{display:grid;grid-template-columns:repeat(auto-fit,minmax(160px,1fr));gap:7px;margin:2px 0 4px;align-items:start;}\n",
    ".proximityControlGrid label.control{display:flex;flex-direction:column;align-items:stretch;justify-content:flex-start;gap:4px;min-width:0;min-height:0;line-height:1.2;}\n",
    ".proximityControlGrid select{width:100%;box-sizing:border-box;min-width:0;}\n",
    ".proximityControlGrid select[multiple]{min-height:76px;}\n",
    ".proximityControlGrid .proximityWide{grid-column:1 / -1;}\n",
    ".menuTitle{font-size:11px;text-transform:uppercase;letter-spacing:.04em;color:#a8a8a8;margin:2px 2px 0;}\n",
    ".menuHint{font-size:11px;color:#aaa;line-height:1.3;margin:0 2px 2px;}\n",
    "label.control{display:flex;gap:6px;align-items:center;color:#d7d7d7;font-size:12px;}\n",
    "input[type=range]{width:82px;accent-color:#5eead4;}\n",
    "#brushSizeValue{min-width:54px;text-align:right;color:#d7d7d7;font-size:11px;}\n",
    "input[type=color]{width:28px;height:22px;border:1px solid rgba(255,255,255,.28);border-radius:4px;background:transparent;padding:0;}\n",
    "input[type=checkbox]{accent-color:#5eead4;}\n",
    "input[type=text]{background:#202020;color:#f2f2f2;border:1px solid rgba(255,255,255,.24);border-radius:5px;padding:5px 7px;font-size:12px;min-width:0;width:160px;}\n",
    "select{background:#202020;color:#f2f2f2;border:1px solid rgba(255,255,255,.24);border-radius:5px;padding:5px 7px;font-size:12px;}\n",
    "#status{position:fixed;left:12px;bottom:12px;z-index:30;pointer-events:none;font-size:12px;color:#eee;background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;max-width:calc(100vw - 24px);}\n",
    "#scaleBar{position:fixed;left:50%;bottom:18px;transform:translateX(-50%);z-index:34;pointer-events:none;color:#f8fafc;text-align:center;font-size:12px;line-height:1;text-shadow:0 1px 2px rgba(0,0,0,.9);min-width:120px;}\n",
    "#scaleBar.unavailable{opacity:.68;}\n",
    "#scaleBarLine{height:6px;border-left:2px solid #fff;border-right:2px solid #fff;border-bottom:2px solid #fff;margin:0 auto 5px;box-shadow:0 1px 2px rgba(0,0,0,.85);}\n",
    "#scaleBar.unavailable #scaleBarLine{width:120px;border-color:rgba(255,255,255,.72);}\n",
    "#scaleBarLabel{display:inline-block;background:rgba(18,18,18,.8);border:1px solid rgba(255,255,255,.18);border-radius:999px;padding:3px 8px;}\n",
    "#toastStack{position:fixed;right:12px;bottom:184px;z-index:42;display:flex;flex-direction:column;gap:8px;align-items:flex-end;pointer-events:none;width:min(420px,calc(100vw - 24px));max-width:min(420px,calc(100vw - 24px));}\n",
    ".toast{pointer-events:auto;width:100%;max-width:100%;box-sizing:border-box;padding:9px 12px;border-radius:6px;border:1px solid rgba(255,255,255,.18);background:rgba(18,18,18,.94);color:#f8fafc;font-size:13px;line-height:1.25;box-shadow:0 14px 30px rgba(0,0,0,.36);opacity:0;transform:translateY(8px);transition:opacity .18s ease,transform .18s ease;cursor:pointer;}\n",
    ".toast-actionable{display:flex;align-items:center;gap:10px;}\n",
    ".toastMessage{min-width:0;display:block;overflow-wrap:anywhere;word-break:break-word;white-space:normal;max-height:9.5em;overflow:auto;}\n",
    ".toastAction{flex:0 0 auto;border-color:rgba(255,255,255,.38);background:rgba(255,255,255,.12);font-weight:650;padding:5px 7px;}\n",
    ".toastAction:hover{background:rgba(255,255,255,.2);}\n",
    ".toast.visible{opacity:1;transform:translateY(0);}\n",
    ".toast.leaving{opacity:0;transform:translateY(8px);}\n",
    ".toast-success{border-color:rgba(94,234,212,.55);background:rgba(15,118,110,.95);}\n",
    ".toast-warning{border-color:rgba(250,204,21,.55);background:rgba(113,63,18,.95);}\n",
    ".toast-error{border-color:rgba(248,113,113,.65);background:rgba(127,29,29,.95);}\n",
    ".toast-info{border-color:rgba(148,163,184,.45);}\n",
    "#commandPaletteBackdrop{position:fixed;inset:0;z-index:44;background:rgba(0,0,0,.32);display:none;pointer-events:auto;}\n",
    "#commandPaletteBackdrop.open{display:block;}\n",
    "#commandPalette{position:fixed;left:50%;top:12vh;transform:translateX(-50%);z-index:45;width:min(560px,calc(100vw - 28px));display:none;pointer-events:auto;padding:10px;box-shadow:0 24px 60px rgba(0,0,0,.48);}\n",
    "#commandPalette.open{display:block;}\n",
    ".commandPaletteHead{display:flex;align-items:center;justify-content:space-between;gap:12px;margin-bottom:8px;}\n",
    ".commandPaletteTitle{font-weight:650;}\n",
    ".commandPaletteHint{font-size:11px;color:#b8b8b8;}\n",
    "#commandPaletteSearch{width:100%;box-sizing:border-box;background:#111;color:#f8fafc;border:1px solid rgba(255,255,255,.24);border-radius:5px;padding:9px 10px;font-size:14px;margin-bottom:8px;}\n",
    ".commandPaletteList{display:flex;flex-direction:column;gap:5px;max-height:min(360px,58vh);overflow:auto;}\n",
    ".commandItem{display:grid;grid-template-columns:1fr auto;gap:6px 12px;width:100%;text-align:left;padding:9px 10px;border-radius:5px;background:rgba(255,255,255,.05);}\n",
    ".commandItem.active{border-color:#5eead4;background:rgba(20,184,166,.18);}\n",
    ".commandItem:disabled{opacity:.48;}\n",
    ".commandLabel{font-weight:600;line-height:1.2;}\n",
    ".commandMeta{font-size:11px;color:#b8b8b8;line-height:1.25;grid-column:1 / -1;}\n",
    ".commandKbd{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:10.5px;color:#e2e8f0;border:1px solid rgba(255,255,255,.18);border-radius:4px;padding:2px 5px;background:rgba(255,255,255,.06);}\n",
    "#screenshotDialogBackdrop,#annotationExportDialogBackdrop{position:fixed;inset:0;z-index:48;background:rgba(0,0,0,.38);display:none;pointer-events:auto;}\n",
    "#screenshotDialogBackdrop.open,#annotationExportDialogBackdrop.open{display:block;}\n",
    "#screenshotDialog,#annotationExportDialog{position:fixed;left:50%;top:50%;transform:translate(-50%,-50%);z-index:49;width:min(430px,calc(100vw - 28px));display:none;pointer-events:auto;padding:12px;box-shadow:0 24px 60px rgba(0,0,0,.52);}\n",
    "#screenshotDialog.open,#annotationExportDialog.open{display:block;}\n",
    ".screenshotDialogHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:10px;}\n",
    "#screenshotDialogTitle,#annotationExportDialogTitle{font-weight:650;color:#f8fafc;}\n",
    "#screenshotDialogSubtitle,#annotationExportDialogSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;}\n",
    ".screenshotDialogGrid{display:grid;grid-template-columns:120px minmax(0,1fr);gap:7px;align-items:center;margin:6px 0 8px;}\n",
    ".screenshotDialogGrid label{display:contents;color:#d7d7d7;font-size:12px;}\n",
    ".screenshotDialogGrid input,.screenshotDialogGrid select{width:100%;box-sizing:border-box;}\n",
    ".screenshotSaveLocation{font-size:11px;line-height:1.35;color:#cbd5e1;background:rgba(255,255,255,.055);border:1px solid rgba(255,255,255,.12);border-radius:5px;padding:7px;margin:4px 0 9px;}\n",
    ".screenshotOptionList{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px 10px;margin:6px 0 9px;}\n",
    ".screenshotOptionList label{font-size:12px;color:#e5e7eb;display:flex;gap:6px;align-items:center;min-width:0;}\n",
    ".screenshotDialogActions{display:flex;justify-content:flex-end;gap:8px;margin-top:10px;}\n",
    "#screenshotDialogSummary,#annotationExportDialogSummary{min-height:14px;}\n",
    "#shortcutHelpBackdrop{position:fixed;inset:0;z-index:46;background:rgba(0,0,0,.32);display:none;pointer-events:auto;}\n",
    "#shortcutHelpBackdrop.open{display:block;}\n",
    "#shortcutHelp{position:fixed;left:50%;top:8vh;transform:translateX(-50%);z-index:47;width:min(760px,calc(100vw - 28px));max-height:calc(100vh - 96px);display:none;pointer-events:auto;padding:12px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:auto;}\n",
    "#shortcutHelp.open{display:block;}\n",
    ".shortcutHelpHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;}\n",
    ".shortcutHelpTitle{font-weight:650;}\n",
    ".shortcutHelpHint{font-size:11px;color:#b8b8b8;margin-top:2px;}\n",
    ".shortcutHelpTabs{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px;margin:8px 0 10px;}\n",
    ".shortcutHelpTab{justify-content:center;border-color:rgba(255,255,255,.18);background:rgba(255,255,255,.055);}\n",
    ".shortcutHelpTab.active{border-color:#5eead4;background:rgba(20,184,166,.20);color:#f8fafc;}\n",
    ".helpPart{margin-top:12px;}\n",
    ".helpPart[hidden]{display:none!important;}\n",
    ".helpPartTitle{font-size:12px;text-transform:uppercase;letter-spacing:.045em;color:#5eead4;margin:0 0 7px;}\n",
    ".quickRecommendationList{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px 18px;}\n",
    ".viewerGuideGrid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:8px;margin-top:10px;}\n",
    ".viewerGuideSection{border:1px solid rgba(255,255,255,.12);border-radius:6px;background:rgba(255,255,255,.04);padding:8px;}\n",
    ".viewerGuideSection h3{margin:0 0 5px;font-size:12px;color:#f8fafc;}\n",
    ".viewerGuideList{margin:0;padding-left:17px;color:#e5e7eb;font-size:12px;line-height:1.35;}\n",
    ".viewerGuideList li{margin:3px 0;}\n",
    ".viewerGuideWide{grid-column:1 / -1;}\n",
    ".viewerGuideTroubleshooting{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px;}\n",
    ".viewerGuideIssue{border:1px solid rgba(255,255,255,.10);border-radius:5px;background:rgba(0,0,0,.18);padding:7px;font-size:12px;line-height:1.3;}\n",
    ".viewerGuideIssue strong{display:block;color:#facc15;margin-bottom:2px;}\n",
    ".shortcutList{display:grid;grid-template-columns:minmax(120px,auto) 1fr;gap:6px 12px;font-size:13px;line-height:1.25;}\n",
    ".shortcutList dt{margin:0;text-align:right;}\n",
    ".shortcutList dd{margin:0;color:#e5e7eb;}\n",
    ".shortcutHelpActions{display:flex;justify-content:flex-end;margin-top:10px;}\n",
    "#kodamaPlotWindow{position:fixed;right:18px;top:76px;z-index:43;width:min(520px,calc(100vw - 36px));height:min(480px,calc(100vh - 108px));min-width:360px;min-height:300px;max-width:calc(100vw - 36px);max-height:calc(100vh - 108px);display:none;pointer-events:auto;padding:10px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:hidden;resize:both;}\n",
    "#seuratPlotWindow{position:fixed;right:18px;top:76px;z-index:43;width:min(520px,calc(100vw - 36px));height:min(480px,calc(100vh - 108px));min-width:360px;min-height:300px;max-width:calc(100vw - 36px);max-height:calc(100vh - 108px);display:none;pointer-events:auto;padding:10px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:hidden;resize:both;}\n",
    "#predictionWindow{position:fixed;right:18px;top:88px;z-index:43;width:min(460px,calc(100vw - 36px));min-width:340px;max-width:calc(100vw - 36px);max-height:calc(100vh - 108px);display:none;pointer-events:auto;padding:10px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:auto;resize:both;}\n",
    "#proximityStatsWindow{position:fixed;right:18px;top:88px;z-index:43;width:min(620px,calc(100vw - 36px));min-width:360px;max-width:calc(100vw - 36px);max-height:calc(100vh - 108px);display:none;pointer-events:auto;padding:10px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:auto;resize:both;}\n",
    "#spatialTileWindow{position:fixed;right:18px;bottom:18px;z-index:43;width:min(460px,calc(100vw - 36px));min-width:340px;max-width:calc(100vw - 36px);max-height:calc(100vh - 108px);display:none;pointer-events:auto;padding:10px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:auto;resize:both;}\n",
    "#kodamaPlotWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#seuratPlotWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#predictionWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#proximityStatsWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#spatialTileWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#kodamaPlotWindow.moving{user-select:none;}\n",
    "#seuratPlotWindow.moving{user-select:none;}\n",
    "#predictionWindow.moving{user-select:none;}\n",
    "#proximityStatsWindow.moving{user-select:none;}\n",
    "#spatialTileWindow.moving{user-select:none;}\n",
    "#kodamaPlotWindow.open{display:flex;flex-direction:column;}\n",
    "#seuratPlotWindow.open{display:flex;flex-direction:column;}\n",
    "#predictionWindow.open{display:flex;flex-direction:column;}\n",
    "#proximityStatsWindow.open{display:flex;flex-direction:column;}\n",
    "#spatialTileWindow.open{display:flex;flex-direction:column;}\n",
    ".kodamaPlotHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    ".seuratPlotHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    ".predictionHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    ".proximityStatsHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    ".spatialTileHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    "#kodamaPlotTitle{font-weight:650;line-height:1.2;}\n",
    "#seuratPlotTitle{font-weight:650;line-height:1.2;}\n",
    "#predictionTitle{font-weight:650;line-height:1.2;}\n",
    "#proximityStatsTitle{font-weight:650;line-height:1.2;}\n",
    "#spatialTileTitle{font-weight:650;line-height:1.2;}\n",
    "#kodamaPlotSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    "#seuratPlotSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    "#predictionSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    "#proximityStatsSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    "#spatialTileSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    ".kodamaPlotTools{display:flex;gap:6px;flex-wrap:wrap;margin-bottom:8px;}\n",
    ".seuratPlotTools{display:flex;gap:6px;flex-wrap:wrap;margin-bottom:8px;}\n",
    ".predictionTools{display:flex;gap:6px;flex-wrap:wrap;margin:8px 0;}\n",
    ".proximityStatsTools{display:flex;gap:6px;flex-wrap:wrap;margin:8px 0;}\n",
    ".spatialTileTools{display:flex;gap:6px;flex-wrap:wrap;margin:8px 0;}\n",
    ".predictionForm{display:grid;grid-template-columns:1fr 1fr;gap:7px;align-items:end;}\n",
    ".predictionForm .wide{grid-column:1 / -1;}\n",
    ".predictionForm select[multiple]{min-height:94px;background:#111;color:#eee;border:1px solid rgba(255,255,255,.22);border-radius:5px;padding:5px;width:100%;box-sizing:border-box;}\n",
    ".proximityStatsTableWrap{overflow:auto;border:1px solid rgba(255,255,255,.12);border-radius:6px;background:rgba(0,0,0,.18);max-height:420px;}\n",
    ".proximityStatsTable{width:100%;border-collapse:collapse;font-size:11px;line-height:1.25;}\n",
    ".proximityStatsTable th,.proximityStatsTable td{padding:5px 6px;border-bottom:1px solid rgba(255,255,255,.08);text-align:left;white-space:nowrap;}\n",
    ".proximityStatsTable th{position:sticky;top:0;background:rgba(18,18,18,.98);color:#5eead4;z-index:1;}\n",
    ".proximityStatsFeature{color:#93c5fd;text-decoration:underline;text-underline-offset:2px;cursor:pointer;font-weight:650;}\n",
    ".spatialTileForm{display:grid;grid-template-columns:1fr 1fr;gap:7px;align-items:end;}\n",
    ".spatialTileForm .wide{grid-column:1 / -1;}\n",
    ".seuratPlotScope{display:flex;gap:4px;align-items:center;border-left:1px solid rgba(255,255,255,.14);padding-left:6px;margin-left:2px;}\n",
    ".seuratPlotScope button.active{background:#2563eb;border-color:#60a5fa;color:#fff;}\n",
    "#kodamaPlotViewport{position:relative;flex:1 1 auto;min-height:120px;background:#f8fafc;border-radius:6px;overflow:auto;display:flex;align-items:center;justify-content:center;}\n",
    "#seuratPlotViewport{position:relative;flex:1 1 auto;min-height:120px;background:#f8fafc;border-radius:6px;overflow:auto;display:flex;align-items:center;justify-content:center;}\n",
    "#kodamaPlotCanvas{max-width:100%;height:auto;display:block;cursor:crosshair;}\n",
    "#seuratPlotCanvas{max-width:100%;height:auto;display:block;cursor:crosshair;}\n",
    "#kodamaPlotLegend{display:flex;gap:6px;flex-wrap:wrap;margin-top:8px;max-height:84px;overflow:auto;}\n",
    "#seuratPlotLegend{display:flex;gap:6px;flex-wrap:wrap;margin-top:8px;max-height:84px;overflow:auto;}\n",
    "#kodamaPlotSelectionStatus{margin-bottom:6px;}\n",
    "#seuratPlotSelectionStatus{margin-bottom:6px;}\n",
    ".kodamaLegendItem{display:inline-flex;align-items:center;gap:5px;border:1px solid rgba(255,255,255,.14);border-radius:999px;background:rgba(255,255,255,.06);padding:3px 7px;font-size:11px;color:#e5e7eb;}\n",
    "#workspacePanel{position:fixed;left:12px;top:72px;width:420px;max-height:calc(100vh - 132px);display:flex;flex-direction:column;gap:8px;z-index:29;pointer-events:auto;}\n",
    "#workspaceResizeHandle{position:absolute;right:-6px;top:0;bottom:0;width:12px;cursor:ew-resize;z-index:3;touch-action:none;}\n",
    "#workspaceResizeHandle::after{content:'';position:absolute;right:4px;top:12px;bottom:12px;width:2px;border-radius:999px;background:rgba(255,255,255,.18);opacity:.45;transition:opacity .12s ease,background .12s ease;}\n",
    "#workspaceResizeHandle:hover::after,#workspacePanel.resizing #workspaceResizeHandle::after{opacity:1;background:#5eead4;}\n",
    "body.workspacePanelResizing{cursor:ew-resize;user-select:none;}\n",
    "#projectPanel,#roiPanel,#annotationHistory,#viewerLogPanel{position:relative;box-sizing:border-box;}\n",
    "#projectPanel{display:flex;flex-direction:column;flex:0 0 auto;max-height:min(38vh,320px);min-height:96px;overflow:hidden;}\n",
    "#projectPanel.closed{display:none;}\n",
    "#projectPanel.minimized{max-height:none;min-height:0;overflow:hidden;}\n",
    "#projectPanel.minimized #projectPanelBody{display:none;}\n",
    "#roiPanel{position:relative;width:auto;max-height:none;overflow:hidden;display:none;z-index:auto;pointer-events:auto;min-height:0;}\n",
    "#roiPanel.open{display:flex;flex:1 1 auto;min-height:0;flex-direction:column;}\n",
    "#roiPanel.minimized{width:auto;flex:0 0 auto;max-height:none;}\n",
    "#roiPanel.minimized #roiPanelBody{display:none;}\n",
    ".roiPanelHeader{display:flex;align-items:flex-start;justify-content:space-between;gap:10px;cursor:grab;user-select:none;}\n",
    ".roiPanelHeader:active{cursor:grabbing;}\n",
    ".roiPanelHeader:focus-visible{outline:2px solid #5eead4;outline-offset:3px;border-radius:4px;}\n",
    ".roiPanelMinimizeState{font-size:10.5px;color:#9fded8;text-align:right;line-height:1.2;max-width:92px;}\n",
    "#roiPanelBody{overflow:auto;min-height:0;padding-top:6px;padding-bottom:8px;}\n",
    "#projectPanel.resized,#annotationHistory.resized,#viewerLogPanel.resized{display:flex;flex-direction:column;overflow:hidden;}\n",
    "#projectPanel.resized #projectPanelBody,#annotationHistory.resized #annotationHistoryBody,#viewerLogPanel.resized #viewerLogBody{overflow:auto;max-height:none;flex:1 1 auto;min-height:0;padding-bottom:12px;}\n",
    "#roiPanel.resized #roiPanelBody{flex:1 1 auto;overflow:auto;}\n",
    ".sidePanelResizeHandle{position:absolute;left:10px;right:10px;bottom:2px;height:11px;cursor:ns-resize;z-index:5;touch-action:none;}\n",
    ".sidePanelResizeHandle::after{content:'';position:absolute;left:36%;right:36%;top:5px;height:2px;border-radius:999px;background:rgba(255,255,255,.22);opacity:.55;transition:opacity .12s ease,background .12s ease;}\n",
    ".sidePanelResizeHandle:hover::after,.resizingVertical>.sidePanelResizeHandle::after{opacity:1;background:#5eead4;}\n",
    ".minimized>.sidePanelResizeHandle,.closed>.sidePanelResizeHandle{display:none;}\n",
    "body.sidePanelResizing{cursor:ns-resize;user-select:none;}\n",
    "#selectionCard{position:fixed;left:444px;top:72px;width:260px;display:none;z-index:31;pointer-events:auto;}\n",
    "#selectionCard.open{display:block;}\n",
    ".selectionCardHead{display:flex;align-items:flex-start;gap:8px;margin-bottom:8px;}\n",
    ".selectionCardTitle{font-weight:650;line-height:1.2;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".selectionCardClass{font-size:11px;color:#b8b8b8;margin-top:2px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".selectionCardStats{display:grid;grid-template-columns:64px 1fr;gap:3px 8px;font-size:12px;line-height:1.3;margin:7px 0 9px;}\n",
    ".selectionCardStats span:nth-child(odd){color:#9f9f9f;}\n",
    ".selectionCardStats span:nth-child(even){color:#f1f5f9;text-align:right;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".selectionCardActions{display:grid;grid-template-columns:repeat(4,minmax(0,1fr));gap:5px;}\n",
    ".selectionCardActions button{padding:6px 5px;font-size:12px;}\n",
    ".sideTitle{font-weight:600;margin-bottom:3px;}\n",
    ".sideMeta{font-size:11px;color:#b8b8b8;margin-bottom:8px;line-height:1.35;}\n",
    ".projectPanel{margin:0;padding:8px 10px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);}\n",
    ".projectPanelHeader{display:flex;align-items:flex-start;justify-content:space-between;gap:10px;cursor:pointer;user-select:none;}\n",
    ".projectPanelHeader:focus-visible{outline:2px solid #5eead4;outline-offset:3px;border-radius:4px;}\n",
    ".projectPanelMinimizeState{font-size:10.5px;color:#9fded8;text-align:right;line-height:1.2;max-width:92px;}\n",
    ".panelHeaderActions{display:flex;align-items:flex-start;gap:6px;position:relative;z-index:8;}\n",
    ".panelCloseButton{width:22px;height:22px;min-width:22px;padding:0;border-radius:4px;font-size:14px;line-height:1;border:1px solid rgba(255,255,255,.16);background:rgba(255,255,255,.06);color:#eee;position:relative;z-index:9;}\n",
    ".panelCloseButton:hover{border-color:#5eead4;background:rgba(94,234,212,.14);}\n",
    "#projectPanelBody{padding-top:4px;padding-bottom:12px;overflow:auto;min-height:0;flex:1 1 auto;}\n",
    "#projectImageList,#projectSectionList{display:flex;flex-direction:column;gap:5px;}\n",
    ".projectItemRow{display:grid;grid-template-columns:minmax(0,1fr) 24px;gap:5px;align-items:stretch;}\n",
    ".projectItem,.projectSectionItem{display:grid;grid-template-columns:1fr auto;gap:4px 8px;width:100%;padding:7px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.045);color:#eee;text-align:left;}\n",
    ".projectItem{cursor:grab;}\n",
    ".projectItem:active{cursor:grabbing;}\n",
    ".projectItem.active,.projectSectionItem.active{border-color:#5eead4;background:rgba(94,234,212,.12);}\n",
    ".projectItem.dragging{opacity:.42;border-style:dashed;}\n",
    ".projectItem.dragOver{border-color:#facc15;background:rgba(250,204,21,.14);}\n",
    ".projectItem.unavailable,.projectSectionItem[disabled]{opacity:.62;cursor:not-allowed;}\n",
    ".projectItemClose{width:24px;min-width:24px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.045);color:#ddd;font-size:12px;line-height:1;}\n",
    ".projectItemClose:hover{border-color:#fb7185;background:rgba(251,113,133,.16);color:#fff;}\n",
    ".projectItemClose:disabled{opacity:.35;cursor:not-allowed;}\n",
    ".projectName{font-size:12.5px;font-weight:650;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".projectStatus{font-size:10.5px;color:#9fded8;text-align:right;white-space:nowrap;}\n",
    ".projectPath,.projectMessage{grid-column:1 / -1;font-size:10.5px;color:#aaa;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".projectMessage{color:#c8c8c8;white-space:normal;line-height:1.25;}\n",
    "#projectSectionList{margin-top:7px;padding-top:7px;border-top:1px solid rgba(255,255,255,.10);}\n",
    ".annotationEditor{display:flex;flex-direction:column;gap:6px;margin:8px 0 10px;padding:8px;border:1px solid rgba(255,255,255,.12);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".annotationEditor[hidden]{display:none!important;}\n",
    ".annotationEditorTitle{font-size:12px;font-weight:650;color:#f8fafc;line-height:1.2;}\n",
    ".annotationEditorHint{font-size:11px;color:#a8a8a8;line-height:1.3;margin-top:-2px;}\n",
    ".annotationEditor .control{justify-content:space-between;}\n",
    ".annotationBrushControls{display:flex;flex-direction:column;gap:4px;margin:2px 0;padding:7px;border:1px solid rgba(255,255,255,.10);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".annotationBrushControls input[type=range]{width:170px;}\n",
    "#brushZoomHint{margin-bottom:0;}\n",
    ".annotationActions{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:5px;}\n",
    ".annotationActions button{padding:6px 5px;font-size:12px;}\n",
    "#annotationHistory{display:block;flex:0 0 auto;max-height:min(30vh,280px);overflow:auto;margin:0;padding:8px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);}\n",
    "#annotationHistory.closed{display:none;}\n",
    "#annotationHistory.minimized{max-height:none;overflow:hidden;}\n",
    "#annotationHistory.minimized #annotationHistoryBody{display:none;}\n",
    "#viewerLogPanel{display:block;flex:0 0 auto;max-height:min(30vh,280px);overflow:auto;margin:0;padding:8px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);}\n",
    "#viewerLogPanel.closed{display:none;}\n",
    "#viewerLogPanel.minimized{max-height:none;overflow:hidden;}\n",
    "#viewerLogPanel.minimized #viewerLogBody{display:none;}\n",
    "#projectPanel.minimized,#roiPanel.minimized,#annotationHistory.minimized,#viewerLogPanel.minimized{height:auto!important;flex:0 0 auto!important;}\n",
    ".historyHead{display:flex;align-items:center;justify-content:space-between;gap:8px;margin-bottom:5px;}\n",
    ".historyHead .sideTitle{margin-bottom:0;}\n",
    ".historyHead button{padding:4px 7px;font-size:11px;}\n",
    ".historyActions{display:flex;align-items:center;gap:5px;flex-wrap:wrap;justify-content:flex-end;}\n",
    "#annotationSectionBackdrop{position:fixed;inset:0;z-index:40;background:rgba(0,0,0,.48);display:none;pointer-events:auto;}\n",
    "#annotationSectionBackdrop.open{display:block;}\n",
    "#annotationHistory.maximized{position:fixed;left:24px;right:24px;top:72px;bottom:24px;z-index:41;margin:0;padding:12px;display:flex;flex-direction:column;overflow:hidden;background:rgba(18,18,18,.98);border-color:rgba(94,234,212,.45);box-shadow:0 24px 80px rgba(0,0,0,.55);}\n",
    "#annotationHistory.maximized #annotationHistoryList{max-height:none;flex:1 1 auto;min-height:0;overflow:auto;}\n",
    "#annotationHistory.maximized .historyHead{position:sticky;top:0;background:rgba(18,18,18,.98);z-index:1;padding-bottom:6px;}\n",
    "#annotationHistoryList{display:flex;flex-direction:column;gap:5px;max-height:160px;overflow:auto;}\n",
    "#viewerLogList{display:flex;flex-direction:column;gap:5px;max-height:160px;overflow:auto;}\n",
    ".viewerLogActions{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:5px;margin:6px 0;}\n",
    ".viewerLogItem{border:1px solid rgba(255,255,255,.1);border-radius:5px;padding:6px;background:rgba(255,255,255,.045);}\n",
    ".viewerLogItem.warning{border-color:rgba(250,204,21,.38);background:rgba(113,63,18,.28);}\n",
    ".viewerLogItem.error{border-color:rgba(248,113,113,.45);background:rgba(127,29,29,.34);}\n",
    ".viewerLogHead{display:flex;align-items:center;gap:6px;font-size:11px;color:#cbd5e1;margin-bottom:3px;}\n",
    ".viewerLogLevel{font-weight:700;text-transform:uppercase;letter-spacing:.04em;}\n",
    ".viewerLogMessage{font-size:12px;line-height:1.35;color:#f8fafc;overflow-wrap:anywhere;}\n",
    ".viewerLogDetail{font-size:11px;color:#b8b8b8;line-height:1.3;margin-top:3px;overflow-wrap:anywhere;}\n",
    ".historyItem{border:1px solid rgba(255,255,255,.12);border-radius:5px;background:rgba(255,255,255,.045);padding:6px;}\n",
    ".historyAction{font-size:11.5px;font-weight:650;color:#e7e7e7;line-height:1.25;}\n",
    ".historyMeta{font-size:10.5px;color:#a8a8a8;line-height:1.25;margin-top:2px;word-break:break-word;}\n",
    ".annotationSearch{display:grid;grid-template-columns:1fr auto auto auto;gap:5px;align-items:center;margin:6px 0;}\n",
    ".annotationSearch input[type=text]{width:100%;box-sizing:border-box;}\n",
    ".annotationSearch select{max-width:98px;}\n",
    ".annotationSearch button{padding:5px 7px;font-size:12px;}\n",
    ".annotationListTools{display:flex;flex-wrap:wrap;gap:5px;align-items:center;margin:6px 0;}\n",
    ".annotationListTools button{padding:5px 7px;font-size:12px;}\n",
    ".annotationLabelHighlighter{margin:8px 0 10px;padding:8px;border:1px solid rgba(255,255,255,.10);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".annotationLabelHighlightList{display:flex;flex-direction:column;gap:4px;max-height:150px;overflow:auto;margin-top:6px;}\n",
    ".annotationLabelHighlightItem{display:grid;grid-template-columns:14px 12px minmax(0,1fr) auto;gap:7px;align-items:center;font-size:11px;color:#d7d7d7;line-height:1.25;}\n",
    ".annotationLabelHighlightItem input{margin:0;width:13px;height:13px;}\n",
    ".annotationLabelHighlightItem .labelName{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".annotationLabelHighlightItem .labelCount{color:#9ca3af;font-size:10px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;}\n",
    ".annotationLabelHighlightItem.active{color:#f8fafc;}\n",
    ".roiListEmpty{padding:10px 8px;color:#b8b8b8;font-size:12px;border:1px dashed rgba(255,255,255,.18);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".roiItem{display:block;width:100%;margin:6px 0;padding:8px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.04);color:#eee;text-align:left;}\n",
    ".roiItem.hidden{opacity:.55;}\n",
    ".layerItem.hidden{opacity:.55;}\n",
    ".roiItem.locked{border-color:rgba(250,204,21,.42);}\n",
    ".roiItem.active{border-color:#5eead4;background:rgba(20,184,166,.2);}\n",
    ".roiItem.highlighted{border-color:rgba(255,255,255,.55);box-shadow:inset 3px 0 0 var(--wsi-highlight-accent,#5eead4);background:rgba(255,255,255,.075);}\n",
    ".roiTop{display:flex;align-items:center;gap:8px;margin-bottom:5px;}\n",
    ".layerItem{display:block;width:100%;margin:6px 0;padding:8px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.04);color:#eee;text-align:left;}\n",
    ".layerItem.objectSelected{border-color:#facc15;background:rgba(250,204,21,.12);}\n",
    ".layerTop{display:flex;align-items:center;gap:8px;margin-bottom:6px;}\n",
    ".layerControls{display:grid;grid-template-columns:auto 1fr auto;gap:7px;align-items:center;font-size:11px;color:#cfcfcf;}\n",
    ".layerControls button{padding:5px 6px;font-size:11px;}\n",
    ".layerLegend{margin-top:7px;padding-top:7px;border-top:1px solid rgba(255,255,255,.10);}\n",
    ".layerLegendTitle{font-size:11px;color:#e5e7eb;margin-bottom:5px;display:flex;justify-content:space-between;gap:8px;}\n",
    ".layerLegendGradient{height:9px;border-radius:999px;border:1px solid rgba(255,255,255,.20);background:linear-gradient(90deg,#000,#fff);}\n",
    ".layerLegendTicks{display:flex;justify-content:space-between;gap:6px;margin-top:4px;color:#cbd5e1;font-size:10.5px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;}\n",
    ".layerLegendTicks span{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    "#proximityLegend{position:fixed;left:444px;bottom:28px;width:min(260px,calc(100vw - 468px));box-sizing:border-box;display:none;z-index:27;pointer-events:auto;padding:9px 10px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:var(--wsi-side-panel-bg);box-shadow:0 12px 36px rgba(0,0,0,.36);cursor:grab;user-select:none;touch-action:none;}\n",
    "#proximityLegend.open{display:block;}\n",
    "#proximityLegend.dragging{cursor:grabbing;}\n",
    "#proximityLegend .layerLegend{margin-top:0;padding-top:0;border-top:0;}\n",
    "@media(max-width:900px){#proximityLegend{left:12px;right:12px;bottom:58px;width:auto;}}\n",
    ".channelLegendPanel{display:none;margin:8px 0 10px;padding:8px;border:1px solid rgba(255,255,255,.10);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".channelLegendPanel.open{display:block;}\n",
    ".channelLegendTools{display:flex;gap:5px;align-items:center;margin:6px 0;}\n",
    ".channelLegendTools button{padding:4px 6px;font-size:11px;}\n",
    ".channelLegendList{display:flex;flex-direction:column;gap:4px;max-height:180px;overflow:auto;margin-top:6px;}\n",
    ".channelLegendItem{display:grid;grid-template-columns:14px 12px minmax(0,1fr) auto;gap:7px;align-items:center;font-size:11px;color:#d7d7d7;line-height:1.25;}\n",
    ".channelLegendItem input{margin:0;width:13px;height:13px;}\n",
    ".channelLegendItem.disabled{opacity:.48;}\n",
    ".channelLegendItem .legendLabel{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".channelLegendItem .legendValue{color:#9ca3af;font-size:10px;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;}\n",
    ".swatch{width:10px;height:10px;border-radius:50%;display:inline-block;flex:0 0 auto;}\n",
    ".roiName{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;font-weight:600;}\n",
    ".roiClass{color:#aaa;font-size:11px;margin-left:auto;max-width:90px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".roiControls{display:flex;flex-wrap:wrap;gap:5px;align-items:center;justify-content:flex-start;margin-top:7px;max-width:100%;overflow:hidden;}\n",
    ".roiControls button{padding:5px 6px;font-size:11px;flex:0 0 auto;max-width:100%;}\n",
    ".roiControls input[type=color]{width:24px;height:22px;flex:0 0 auto;}\n",
    ".roiSelect{margin:0;}\n",
    ".roiDetails{display:grid;grid-template-columns:76px 1fr;gap:2px 8px;font-size:11px;color:#cfcfcf;line-height:1.25;}\n",
    ".roiDetails span:nth-child(odd){color:#8f8f8f;}\n",
    ".roiDetails code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;white-space:normal;word-break:break-word;}\n",
    ".roiBadge{display:inline-block;border:1px solid rgba(255,255,255,.16);border-radius:4px;padding:1px 5px;font-size:10px;color:#d7d7d7;background:rgba(255,255,255,.06);}\n",
    "#measureList{display:flex;flex-direction:column;gap:5px;max-height:180px;overflow:auto;}\n",
    ".measureItem{display:block;width:100%;text-align:left;line-height:1.25;}\n",
    ".measureItem.active{border-color:#facc15;background:rgba(250,204,21,.14);}\n",
    ".measureItem code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;}\n",
    "#trajectoryAreaWidthValue{min-width:54px;text-align:right;color:#d7d7d7;font-size:11px;}\n",
    "#trajectoryList{display:flex;flex-direction:column;gap:5px;max-height:180px;overflow:auto;}\n",
    ".trajectoryItem{display:block;width:100%;text-align:left;line-height:1.25;}\n",
    ".trajectoryItem.active{border-color:#facc15;background:rgba(250,204,21,.14);}\n",
    ".trajectoryItem code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;}\n",
    "#artifactSensitivityValue{min-width:34px;text-align:right;color:#d7d7d7;font-size:11px;}\n",
    "#artifactList{display:flex;flex-direction:column;gap:5px;max-height:220px;overflow:auto;}\n",
    ".artifactItem{border:1px solid rgba(255,255,255,.14);border-radius:5px;background:rgba(255,255,255,.045);padding:7px;font-size:11px;line-height:1.3;}\n",
    ".artifactItem.flagged{border-color:rgba(248,113,113,.52);background:rgba(127,29,29,.26);}\n",
    ".artifactTop{display:flex;align-items:center;gap:6px;margin-bottom:4px;}\n",
    ".artifactName{font-weight:650;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".artifactBadge{margin-left:auto;border:1px solid rgba(255,255,255,.18);border-radius:999px;padding:1px 6px;background:rgba(255,255,255,.07);font-size:10px;text-transform:uppercase;letter-spacing:.03em;}\n",
    ".artifactItem.flagged .artifactBadge{color:#fecaca;border-color:rgba(248,113,113,.52);background:rgba(127,29,29,.42);}\n",
    ".artifactMeta{color:#cbd5e1;word-break:break-word;}\n",
    "#jobSummary{font-size:11px;color:#d1d5db;line-height:1.35;margin:0 2px 6px;}\n",
    "#jobList{display:flex;flex-direction:column;gap:6px;max-height:320px;overflow:auto;}\n",
    ".jobItem{border:1px solid rgba(255,255,255,.14);border-left-width:4px;border-radius:5px;background:rgba(255,255,255,.045);padding:8px;}\n",
    ".jobItem.pending{border-left-color:#facc15;}\n",
    ".jobItem.running{border-left-color:#60a5fa;}\n",
    ".jobItem.completed{border-left-color:#2dd4bf;}\n",
    ".jobItem.failed{border-left-color:#f87171;background:rgba(127,29,29,.18);}\n",
    ".jobTop{display:flex;align-items:flex-start;gap:8px;margin-bottom:6px;}\n",
    ".jobName{font-weight:650;line-height:1.2;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".jobStatus{margin-left:auto;font-size:10.5px;text-transform:uppercase;letter-spacing:.04em;color:#cbd5e1;border:1px solid rgba(255,255,255,.16);border-radius:999px;padding:2px 6px;background:rgba(255,255,255,.06);}\n",
    ".jobStatus.running{color:#bfdbfe;border-color:rgba(96,165,250,.42);background:rgba(30,64,175,.34);}\n",
    ".jobStatus.pending,.jobStatus.queued{color:#fde68a;border-color:rgba(250,204,21,.42);background:rgba(113,63,18,.34);}\n",
    ".jobStatus.completed{color:#99f6e4;border-color:rgba(45,212,191,.44);background:rgba(15,118,110,.34);}\n",
    ".jobStatus.failed{color:#fecaca;border-color:rgba(248,113,113,.52);background:rgba(127,29,29,.42);}\n",
    ".jobProgressRow{display:grid;grid-template-columns:minmax(0,1fr) auto;gap:8px;align-items:center;margin-bottom:6px;}\n",
    ".jobProgress{height:7px;border-radius:999px;background:rgba(255,255,255,.12);overflow:hidden;}\n",
    ".jobProgressFill{height:100%;width:0%;background:#5eead4;transition:width .2s ease;}\n",
    ".jobProgressFill.indeterminate{width:42%;animation:wsiJobPulse 1.2s ease-in-out infinite;background:#93c5fd;}\n",
    "@keyframes wsiJobPulse{0%{transform:translateX(-120%)}50%{transform:translateX(80%)}100%{transform:translateX(260%)}}\n",
    ".jobProgressText{font-size:10.5px;color:#e5e7eb;white-space:nowrap;}\n",
    ".jobMessage,.jobMeta,.jobError{font-size:11px;line-height:1.3;word-break:break-word;}\n",
    ".jobMessage{color:#e5e7eb;margin-bottom:3px;}\n",
    ".jobMeta{color:#cbd5e1;}\n",
    ".jobError{color:#fecaca;background:rgba(127,29,29,.34);border:1px solid rgba(248,113,113,.32);border-radius:4px;padding:5px;margin-top:5px;}\n",
    ".jobLogDetails{margin-top:6px;}\n",
    ".jobLogDetails summary{font-size:10.5px;color:#cbd5e1;cursor:pointer;}\n",
    ".jobLog{margin:6px 0 0;max-height:96px;overflow:auto;white-space:pre-wrap;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:10.5px;line-height:1.25;color:#e5e7eb;background:rgba(0,0,0,.28);border-radius:4px;padding:6px;}\n",
    "#miniNavigator{position:fixed;right:12px;bottom:12px;width:220px;z-index:28;pointer-events:auto;padding:7px;}\n",
    "#miniNavigatorCanvas{display:block;width:100%;height:132px;border-radius:4px;background:#0b0b0b;border:1px solid rgba(255,255,255,.18);}\n",
    ".miniNavigatorMeta{display:flex;justify-content:space-between;gap:8px;margin-top:5px;font-size:10.5px;color:#cbd5e1;line-height:1.2;}\n",
    ".miniNavigatorMeta span{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    "@media(max-width:900px){.title{max-width:42vw}.bar{align-items:flex-start}.tools{max-width:56vw}.menuBody{right:auto;left:0}#jobSyncIndicator{flex-basis:118px;width:118px;min-width:118px;max-width:118px;padding:5px 7px;}#jobSyncIndicator .syncDetail{display:none;}.navDock{right:12px;top:auto;bottom:118px;transform:none;}#workspacePanel{left:12px;right:12px;top:72px;width:auto;max-height:calc(100vh - 130px);}#workspaceResizeHandle{display:none;}#selectionCard{left:12px;right:12px;top:auto;bottom:58px;width:auto;}#annotationHistory.maximized{left:12px;right:12px;top:72px;bottom:12px;}#miniNavigator{display:none;}#toastStack{left:12px;right:12px;bottom:104px;align-items:stretch;}#commandPalette,#shortcutHelp{top:8vh;}.quickRecommendationList,.viewerGuideGrid,.viewerGuideTroubleshooting{grid-template-columns:1fr;}#multiViewGrid.layout2,#multiViewGrid.layout4,#multiViewGrid.layout6,#multiViewGrid.layoutCustom{grid-template-columns:1fr;grid-template-rows:repeat(var(--multi-view-count,1),minmax(160px,1fr));}}\n"
  )
}

wsi_viewer_menu <- function(label, title, contents, class = "") {
  paste0(
    "<details class=\"toolMenu ", wsi_html_escape(class), "\">",
    "<summary title=\"", wsi_html_escape(title), "\">", wsi_html_escape(label), "</summary>",
    "<div class=\"menuBody\">", contents, "</div>",
    "</details>"
  )
}

wsi_middle_ellipsis <- function(x, max_chars = 20L, head_chars = 8L, tail_chars = 7L) {
  x <- as.character(x %||% "")
  max_chars <- max(5L, as.integer(max_chars %||% 20L))
  head_chars <- max(1L, as.integer(head_chars %||% 8L))
  tail_chars <- max(1L, as.integer(tail_chars %||% 7L))
  if (head_chars + tail_chars + 3L > max_chars) {
    available <- max(2L, max_chars - 3L)
    head_chars <- ceiling(available / 2)
    tail_chars <- floor(available / 2)
  }
  vapply(x, function(value) {
    if (is.na(value)) {
      return(NA_character_)
    }
    n <- nchar(value, type = "chars", allowNA = TRUE, keepNA = TRUE)
    if (is.na(n) || n <= max_chars) {
      return(value)
    }
    paste0(
      substr(value, 1L, head_chars),
      "...",
      substr(value, n - tail_chars + 1L, n)
    )
  }, character(1))
}

wsi_reduction_display_label <- function(reduction) {
  unname(wsi_middle_ellipsis(wsi_reduction_label(reduction), max_chars = 12L, head_chars = 4L, tail_chars = 4L))
}

wsi_viewer_menu_js <- function() {
  paste0(
    "function closeAllToolMenus(){document.querySelectorAll('.toolMenu[open]').forEach(menu=>{menu.open=false;});}\n",
    "function closeContainingToolMenu(control){const menu=control&&control.closest?control.closest('.toolMenu'):null;if(menu)menu.open=false;}\n",
    "function closeMenuAfterToolAction(control){closeContainingToolMenu(control);setTimeout(closeAllToolMenus,0);}\n",
    "function bindExclusiveMenus(){const menus=Array.from(document.querySelectorAll('.toolMenu'));const closeOtherMenus=active=>menus.forEach(menu=>{if(menu!==active)menu.open=false;});menus.forEach(menu=>{const summary=menu.querySelector('summary');if(summary){summary.addEventListener('pointerdown',()=>closeOtherMenus(menu));summary.addEventListener('click',()=>setTimeout(()=>{if(menu.open)closeOtherMenus(menu);},0));}menu.addEventListener('toggle',()=>{if(menu.open)closeOtherMenus(menu);});});document.addEventListener('click',e=>{if(!e.target.closest('.toolMenu'))closeAllToolMenus();});document.addEventListener('keydown',e=>{if(e.key==='Escape')closeAllToolMenus();});}\n"
  )
}

wsi_viewer_stain_controls <- function(config) {
  stain_enabled <- isTRUE(config$stain$enabled)
  channels <- if (stain_enabled) config$stain$channels else list()
  if (!stain_enabled) {
    return(wsi_viewer_menu(
      label = "Stains",
      title = "Stain deconvolution display options",
      class = "stainMenu",
      contents = paste0(
        "<div class=\"menuTitle\">Base image</div>",
        "<label class=\"control\" title=\"Show or hide the H&E/base image\"><input id=\"baseImageVisible\" type=\"checkbox\" checked><span id=\"baseImageName\">Base image</span></label>",
        "<label class=\"control\" title=\"Base image opacity\">opacity <input id=\"baseImageOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>",
        "<div class=\"menuTitle\">Display</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"stainToggle\" type=\"button\" disabled title=\"Open the viewer with stain = 'ihc' or channel data to enable stain controls\">Stains</button>",
        "<button id=\"stainShowOriginal\" type=\"button\" disabled title=\"Original RGB is already shown\">Original</button>",
        "</div>",
        "<div id=\"stainMessage\" class=\"menuHint\">No stain channels are configured for this viewer.</div>",
        "<div class=\"menuTitle\">Image channels</div>",
        "<div id=\"channelMenuSummary\" class=\"menuHint\"></div>",
        "<div id=\"channelMenuList\"></div>"
      )
    ))
  }
  only_controls <- vapply(seq_along(channels), function(i) {
    channel <- channels[[i]]
    name <- wsi_html_escape(channel$name)
    paste0(
      "<button class=\"stainOnly\" type=\"button\" data-stain-index=\"",
      i - 1L,
      "\" data-stain-id=\"",
      wsi_html_escape(channel$id %||% channel$name %||% as.character(i)),
      "\" title=\"Show only ",
      name,
      "\">",
      name,
      "</button>"
    )
  }, character(1))
  wsi_viewer_menu(
    label = "Stains",
    title = "Stain deconvolution display options",
    class = "stainMenu",
    contents = paste0(
      "<div class=\"menuTitle\">Base image</div>",
      "<label class=\"control\" title=\"Show or hide the H&E/base image\"><input id=\"baseImageVisible\" type=\"checkbox\" checked><span id=\"baseImageName\">Base image</span></label>",
      "<label class=\"control\" title=\"Base image opacity\">opacity <input id=\"baseImageOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>",
      "<div class=\"menuTitle\">Display</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"stainToggle\" class=\"active\" title=\"Toggle stain-channel deconvolution display\">",
      wsi_html_escape(config$stain$button_label %||% "IHC"),
      "</button>",
      "<button id=\"stainShowOriginal\" type=\"button\" title=\"Show the original RGB image\">Original</button>",
      "<button id=\"stainShowAll\" type=\"button\" title=\"Show all stain channels\">All stains</button>",
      "</div>",
      "<div class=\"menuTitle\">Only</div>",
      "<div class=\"menuGrid\">",
      paste(only_controls, collapse = ""),
      "</div>",
      "<div id=\"stainMessage\" class=\"menuHint\"></div>",
      "<div class=\"menuTitle\">Image channels</div>",
      "<div id=\"channelMenuSummary\" class=\"menuHint\"></div>",
      "<div id=\"channelMenuList\"></div>"
    )
  )
}

wsi_viewer_kodama_controls <- function(config) {
  kodama <- config$cellphenotyper$kodama %||% list(enabled = FALSE, geojsons = list())
  geojsons <- kodama$geojsons %||% list()
  geojsons <- if (is.list(geojsons)) geojsons else list()
  plots <- kodama$plots %||% list()
  plots <- if (is.list(plots)) plots else list()
  kodama_enabled <- isTRUE(kodama$enabled)
  has_geojson <- kodama_enabled && length(geojsons) > 0L
  has_plots <- kodama_enabled && length(plots) > 0L
  if (!has_geojson && !has_plots) {
    return("")
  }
  load_buttons <- if (has_geojson) {
    paste(vapply(seq_along(geojsons), function(i) {
      item <- geojsons[[i]]
      count <- item$feature_count %||% 0L
      label <- sprintf(
        "%s (%s ROI%s)",
        item$label %||% sprintf("KODAMA GeoJSON %d", i),
        format(as.integer(count), big.mark = ","),
        if (identical(as.integer(count), 1L)) "" else "s"
      )
      paste0(
        "<button class=\"kodamaLoad\" type=\"button\" data-kodama-index=\"",
        i - 1L,
        "\" title=\"Visualize ",
        wsi_html_escape(item$path %||% ""),
        "\">",
        wsi_html_escape(label),
        "</button>"
      )
    }, character(1)), collapse = "")
  } else {
    "<div class=\"menuHint\">No MedSAM-refined KODAMA GeoJSON was found in this CellPhenotyper project manifest.</div>"
  }
  plot_buttons <- if (has_plots) {
    paste(vapply(seq_along(plots), function(i) {
      item <- plots[[i]]
      label <- item$label %||% sprintf("KODAMA plot %d", i)
      paste0(
        "<button class=\"kodamaPlot\" type=\"button\" data-kodama-plot-index=\"",
        i - 1L,
        "\" title=\"Open ",
        wsi_html_escape(item$path %||% ""),
        "\">",
        wsi_html_escape(label),
        "</button>"
      )
    }, character(1)), collapse = "")
  } else {
    "<div class=\"menuHint\">No KODAMA membership plot PNG was found for this project.</div>"
  }
  wsi_viewer_menu(
    "CellPhenotyper",
    "Visualize CellPhenotyper KODAMA/MedSAM refined GeoJSON annotations and membership plots",
    paste0(
      "<div class=\"menuTitle\">MedSAM refinement GeoJSON</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"kodamaLoadAll\" type=\"button\" title=\"Import all KODAMA refined GeoJSON annotations\"",
      if (has_geojson) "" else " disabled",
      ">Load all</button>",
      "<button id=\"kodamaClear\" type=\"button\" title=\"Remove KODAMA annotations from the viewer\"",
      if (has_geojson) "" else " disabled",
      ">Clear KODAMA</button>",
      "</div>",
      "<div class=\"menuGrid kodamaList\">",
      load_buttons,
      "</div>",
      "<div class=\"menuTitle\">KODAMA plot</div>",
      "<div class=\"menuGrid kodamaPlotList\">",
      plot_buttons,
      "</div>",
      "<div id=\"kodamaSummary\" class=\"menuHint\"></div>"
    )
  )
}

wsi_viewer_artifact_controls <- function(config) {
  cellphenotyper <- config$cellphenotyper %||% list(enabled = FALSE)
  is_cellphenotyper_project <- isTRUE(cellphenotyper$is_project) ||
    identical(as.character(cellphenotyper$project_type %||% ""), "cellphenotyper") ||
    (nzchar(as.character(cellphenotyper$project_root %||% "")) &&
       nzchar(as.character(cellphenotyper$manifest_path %||% "")))
  if (!isTRUE(cellphenotyper$enabled) || !isTRUE(is_cellphenotyper_project)) {
    return("")
  }
  grandqc <- cellphenotyper$grandqc %||% list(enabled = FALSE, geojsons = list())
  geojsons <- grandqc$geojsons %||% list()
  geojsons <- if (is.list(geojsons)) geojsons else list()
  has_geojson <- isTRUE(grandqc$enabled) && length(geojsons) > 0L
  load_buttons <- if (has_geojson) {
    paste(vapply(seq_along(geojsons), function(i) {
      item <- geojsons[[i]]
      count <- item$feature_count %||% 0L
      label <- sprintf(
        "%s (%s ROI%s)",
        item$label %||% sprintf("GrandQC GeoJSON %d", i),
        format(as.integer(count), big.mark = ","),
        if (identical(as.integer(count), 1L)) "" else "s"
      )
      paste0(
        "<button class=\"grandqcLoad\" type=\"button\" data-grandqc-index=\"",
        i - 1L,
        "\" title=\"Visualize ",
        wsi_html_escape(item$path %||% ""),
        "\">",
        wsi_html_escape(label),
        "</button>"
      )
    }, character(1)), collapse = "")
  } else {
    "<div class=\"menuHint\">No GrandQC GeoJSON was found for this project.</div>"
  }
  wsi_viewer_menu(
    "Artifacts",
    "Visualize GrandQC artifact GeoJSON annotations",
    paste0(
      "<div class=\"menuTitle\">GrandQC GeoJSON</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"grandqcLoadAll\" type=\"button\" title=\"Import all GrandQC artifact GeoJSON annotations\"",
      if (has_geojson) "" else " disabled",
      ">Load GrandQC</button>",
      "<button id=\"grandqcClear\" type=\"button\" title=\"Remove GrandQC artifact annotations from the viewer\"",
      if (has_geojson) "" else " disabled",
      ">Clear GrandQC</button>",
      "</div>",
      "<div class=\"menuGrid artifactList\">",
      load_buttons,
      "</div>",
      "<div id=\"artifactSummary\" class=\"menuHint\"></div>"
    )
  )
}

wsi_viewer_cell_controls <- function(config) {
  cellphenotyper <- config$cellphenotyper %||% list(enabled = FALSE)
  segmentation <- config$segmentation %||% list(enabled = FALSE)
  if (!isTRUE(cellphenotyper$enabled) && !isTRUE(segmentation$enabled)) {
    return("")
  }
  engines <- segmentation$engines %||% list()
  engine_options <- if (length(engines)) {
    paste0(
      vapply(engines, function(engine) {
        value <- as.character(engine$engine %||% "")
        label <- as.character(engine$label %||% value)
        paste0(
          "<option value=\"", wsi_html_escape(value), "\"",
          if (identical(value, as.character(segmentation$default_engine %||% ""))) " selected" else "",
          ">",
          wsi_html_escape(label),
          "</option>"
        )
      }, character(1)),
      collapse = ""
    )
  } else {
    "<option value=\"stardist_he\">StarDist H&amp;E</option>"
  }
  cellphenotyper_section <- if (isTRUE(cellphenotyper$enabled)) {
    paste0(
      "<div class=\"menuTitle\">CellPhenotyper cells</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"cellToggle\" title=\"Show or hide CellPhenotyper cell overlays\">Cells</button>",
      "<button id=\"cellZoom\" title=\"Zoom to the CellPhenotyper cell extent\">Zoom cells</button>",
      "</div>",
      "<label class=\"control\" title=\"Cell overlay opacity\">Opacity <input id=\"cellOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"0.75\"></label>",
      "<label class=\"control\" title=\"Cell marker radius in slide pixels\">Cell size <input id=\"cellRadius\" type=\"range\" min=\"1\" max=\"40\" step=\"1\" value=\"6\"><span id=\"cellRadiusValue\">6 px</span></label>",
      "<div id=\"cellSummary\" class=\"menuHint\">No CellPhenotyper cells loaded.</div>"
    )
  } else {
    "<div id=\"cellSummary\" class=\"menuHint\">No CellPhenotyper cell table is attached.</div>"
  }
  segmentation_section <- paste0(
    "<div class=\"menuTitle\">Selected ROI segmentation</div>",
    "<label class=\"control\" title=\"External engine used by the live R endpoint\">Engine <select id=\"segmentationEngine\"",
    if (isTRUE(segmentation$enabled)) "" else " disabled",
    ">",
    engine_options,
    "</select></label>",
    "<div class=\"menuGrid\">",
    "<button id=\"startSegmentation\" title=\"Run segmentation on the selected ROI through the live R endpoint\"",
    if (isTRUE(segmentation$enabled)) "" else " disabled",
    ">Run selected ROI</button>",
    "<button id=\"exportSelectedRoi\" title=\"Export selected ROI GeoJSON for external analysis\">Export ROI</button>",
    "<button id=\"loadSegmentation\" title=\"Load cell polygons from GeoJSON\">Load GeoJSON</button>",
    "<button id=\"loadSegmentationCsv\" title=\"Load cell centroids from CSV or TSV\">Load CSV</button>",
    "<button id=\"loadSegmentationMask\" title=\"Load a browser-readable cell mask image as cell ROIs\">Load mask</button>",
    "<button id=\"clearSegmentation\" title=\"Remove loaded cell overlays\">Clear cells</button>",
    "</div>",
    "<label class=\"control\" title=\"Use selected ROI origin when imported output coordinates are crop-local\"><input id=\"segLocalCoords\" type=\"checkbox\"> crop-local coordinates</label>",
    "<label class=\"control\" title=\"Radius used for centroid tables loaded as cell markers\">Centroid size <input id=\"segCellRadius\" type=\"range\" min=\"1\" max=\"40\" step=\"1\" value=\"8\"><span id=\"segCellRadiusValue\">8 px</span></label>",
    "<input id=\"segmentationFile\" type=\"file\" accept=\".geojson,.json,application/geo+json,application/json\" style=\"display:none\">",
    "<input id=\"segmentationTableFile\" type=\"file\" accept=\".csv,.tsv,.txt,text/csv,text/tab-separated-values,text/plain\" style=\"display:none\">",
    "<input id=\"segmentationMaskFile\" type=\"file\" accept=\"image/png,image/jpeg,image/webp,.png,.jpg,.jpeg,.webp,.tif,.tiff\" style=\"display:none\">",
    "<div id=\"segmentationSummary\" class=\"menuHint\">",
    if (isTRUE(segmentation$enabled)) {
      "Select or draw one ROI, then run StarDist/Mesmer on that crop or load cells from GeoJSON/CSV/mask."
    } else {
      "Open a live viewer with `stardist = TRUE` to run selected-ROI segmentation from R, or load existing cell GeoJSON/CSV/mask outputs."
    },
    "</div>"
  )
  wsi_viewer_menu(
    "Cells",
    "Cell overlays and optional selected-ROI segmentation",
    paste0(
      cellphenotyper_section,
      segmentation_section
    )
  )
}

wsi_viewer_has_spatial_transcriptomics <- function(config) {
  seurat_enabled <- function(x) {
    isTRUE((x %||% list())$enabled)
  }
  if (seurat_enabled(config$seurat)) {
    return(TRUE)
  }
  items <- (config$project %||% list())$items %||% list()
  if (!length(items)) {
    return(FALSE)
  }
  any(vapply(items, function(item) {
    if (seurat_enabled(item$seurat)) {
      return(TRUE)
    }
    sections <- item$sections %||% list()
    length(sections) > 0L && any(vapply(sections, function(section) {
      seurat_enabled(section$seurat)
    }, logical(1)))
  }, logical(1)))
}

wsi_viewer_seurat_controls <- function(config) {
  seurat <- config$seurat %||% list(enabled = FALSE, spot_count = 0L, plots = list())
  source_name <- as.character(seurat$source_name %||% "Seurat")
  menu_label <- wsi_spatial_menu_label(source_name)
  enabled <- isTRUE(seurat$enabled)
  if (!wsi_viewer_has_spatial_transcriptomics(config)) {
    return("")
  }
  plots <- seurat$plots %||% list()
  plot_count <- length(plots)
  spot_count <- as.integer(seurat$displayed_spot_count %||% seurat$spot_count %||% 0L)
  gene_expression <- seurat$gene_expression %||% list(enabled = FALSE, genes = character(), default_gene = NULL)
  genes <- as.character(gene_expression$genes %||% character())
  genes <- genes[nzchar(genes) & !is.na(genes)]
  clusters <- seurat$clusters %||% list(enabled = FALSE, fields = list(), default_field = NULL)
  cluster_fields <- clusters$fields %||% list()
  if (is.data.frame(cluster_fields)) {
    cluster_fields <- lapply(seq_len(nrow(cluster_fields)), function(i) as.list(cluster_fields[i, , drop = FALSE]))
  }
  cluster_fields <- cluster_fields[vapply(cluster_fields, function(x) {
    is.list(x) && nzchar(as.character(x$field %||% ""))
  }, logical(1))]
  has_clusters <- enabled && spot_count > 0L && isTRUE(clusters$enabled) && length(cluster_fields) > 0L
  default_cluster <- as.character(clusters$default_field %||% "")
  cluster_options <- paste0(
    vapply(cluster_fields, function(field) {
      value <- as.character(field$field %||% "")
      label <- as.character(field$label %||% value)
      count <- suppressWarnings(as.integer(field$n_clusters %||% NA_integer_))
      suffix <- if (is.finite(count)) paste0(" (", count, ")") else ""
      paste0(
        "<option value=\"", wsi_html_escape(value), "\"",
        if (identical(tolower(value), tolower(default_cluster))) " selected" else "",
        ">",
        wsi_html_escape(label), wsi_html_escape(suffix),
        "</option>"
      )
    }, character(1)),
    collapse = ""
  )
  has_live_gene_lookup <- !is.null(config$seurat_gene_url) &&
    is.character(config$seurat_gene_url) && length(config$seurat_gene_url) == 1L &&
    !is.na(config$seurat_gene_url) && nzchar(config$seurat_gene_url)
  has_genes <- enabled && (length(genes) > 0L || has_live_gene_lookup)
  default_gene <- as.character(gene_expression$default_gene %||% "")
  gene_options <- paste0(
    vapply(genes, function(gene) {
      paste0("<option value=\"", wsi_html_escape(gene), "\"></option>")
    }, character(1)),
    collapse = ""
  )
  plot_buttons <- if (enabled && plot_count > 0L) {
    paste0(
      vapply(seq_along(plots), function(i) {
        plot <- plots[[i]]
        reduction_label <- wsi_reduction_label(plot$reduction %||% plot$label %||% sprintf("plot_%d", i))
        reduction_display <- wsi_reduction_display_label(reduction_label)
        paste0(
          "<button class=\"seuratPlotOpen\" data-plot-index=\"", i - 1L,
          "\" title=\"Open the ", wsi_html_escape(reduction_label), " reduction plot\">",
          wsi_html_escape(reduction_display),
          "</button>"
        )
      }, character(1)),
      collapse = ""
    )
  } else {
    ""
  }
  spot_opacity_help <- paste(
    "Spot size is fixed by the spatial transcriptomics platform metadata.",
    "Visium spots are 55 microns; this viewer renders the mapped slide-pixel radius without manual resizing."
  )
  spot_tile_help <- paste(
    "Preview tile boxes on the slide before export.",
    "If an annotation is selected, only tiles fully inside that annotation are included."
  )
  wsi_viewer_menu(
    menu_label,
    "Spatial transcriptomics spot overlays and reduction plots",
    paste0(
      "<div class=\"menuTitle\">Spatial spots</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"seuratSpotToggle\" title=\"Show or hide spatial spots\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      ">Spots</button>",
      "</div>",
	      "<label class=\"control\" title=\"Spot overlay opacity\">Opacity <input id=\"seuratSpotOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"0.85\"",
	      if (enabled && spot_count > 0L) "" else " disabled",
	      "></label>",
	      "<div class=\"menuGrid\">",
		      "<button id=\"seuratSpotOpacityHelp\" type=\"button\" aria-label=\"Spot size help\" title=\"",
		      wsi_html_escape(spot_opacity_help),
		      "\">?</button>",
	      "</div>",
	      "<div class=\"menuTitle\">Spot-centered tiles</div>",
	      "<div class=\"menuGrid\">",
	      "<button id=\"seuratTileWindowOpen\" title=\"Preview and export one image tile centered on each spatial spot\"",
	      if (enabled && spot_count > 0L) "" else " disabled",
	      ">Tiles</button>",
	      "</div>",
	      "<div class=\"menuGrid\">",
	      "<button id=\"seuratTileHelp\" type=\"button\" aria-label=\"Spot-centred tile help\" title=\"",
	      wsi_html_escape(spot_tile_help),
	      "\">?</button>",
	      "</div>",
	      "<div class=\"menuTitle\">Annotation association</div>",
	      "<div class=\"menuGrid\">",
	      "<button id=\"seuratAnnotationSpotsCsv\" title=\"Associate each visible spot or cell with the annotation that contains it and save the result as CSV\"",
	      if (enabled && spot_count > 0L) "" else " disabled",
	      ">Save association CSV</button>",
	      "</div>",
	      "<div class=\"menuTitle\">Gene expression</div>",
      "<label class=\"control\" title=\"Colour spatial spots by an extracted gene\">Gene <input id=\"seuratGeneInput\" type=\"text\" list=\"seuratGeneList\" value=\"",
      wsi_html_escape(default_gene),
      "\" placeholder=\"Gene name\"",
      if (has_genes) "" else " disabled",
      "></label>",
      "<datalist id=\"seuratGeneList\">", gene_options, "</datalist>",
      "<div class=\"menuGrid\">",
      "<button id=\"seuratGeneApply\" title=\"Colour spots by this gene\"",
      if (has_genes) "" else " disabled",
      ">Colour by gene</button>",
      "<button id=\"seuratGeneClear\" title=\"Restore the original spot colours\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      ">Reset colour</button>",
      "</div>",
      "<div id=\"seuratGeneSummary\" class=\"menuHint\">",
      if (has_genes) {
        if (has_live_gene_lookup) {
          "Type any gene name; expression values are fetched from R only when selected."
        } else {
          paste0(length(genes), " gene", if (length(genes) == 1L) "" else "s", " available for spot colouring.")
        }
      } else {
        paste0("No live ", wsi_html_escape(source_name), " gene lookup or embedded gene values are available.")
      },
      "</div>",
      "<div class=\"menuTitle\">Clustering</div>",
      "<label class=\"control\" title=\"Colour spatial spots by a clustering or annotation field stored in the R object\">Field <select id=\"seuratClusterSelect\"",
      if (has_clusters) "" else " disabled",
      ">",
      cluster_options,
      "</select></label>",
      "<div class=\"menuGrid\">",
      "<button id=\"seuratClusterApply\" title=\"Colour spots by the selected clustering field\"",
      if (has_clusters) "" else " disabled",
      ">Colour by cluster</button>",
      "<button id=\"seuratClusterClear\" title=\"Restore the original spot colours\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      ">Reset cluster</button>",
      "</div>",
      "<div id=\"seuratClusterSummary\" class=\"menuHint\">",
      if (has_clusters) {
        paste0(length(cluster_fields), " clustering/annotation field", if (length(cluster_fields) == 1L) "" else "s", " detected in the R object.")
      } else {
        paste0("No clustering metadata was detected in this ", wsi_html_escape(source_name), " object.")
      },
      "</div>",
      "<div class=\"menuTitle\">Dimensional reduction</div>",
      "<div class=\"menuGrid\">",
      plot_buttons,
      "<button id=\"seuratClearSelection\" title=\"Clear selected reduction spot highlights\"",
      if (enabled && plot_count > 0L) "" else " disabled",
      ">Clear selection</button>",
      "</div>",
      if (plot_count > 0L) "" else "<div class=\"menuHint\">No dimensional-reduction plots were found in this object.</div>",
      "<div id=\"seuratSummary\" class=\"menuHint\">",
      if (enabled) {
        paste0(
          format(spot_count, big.mark = ","),
          " ", wsi_html_escape(source_name), " spot", if (spot_count == 1L) "" else "s",
          " linked to ", wsi_html_escape(seurat$image_name %||% "spatial image"),
          " | ", wsi_html_escape(toupper(seurat$reduction %||% "PCA"))
        )
      } else {
        paste0("No ", wsi_html_escape(source_name), " object is attached to this viewer.")
      },
      "</div>"
    )
  )
}

wsi_spatial_menu_label <- function(source_name) {
  source_name <- as.character(source_name %||% "")
  source_key <- tolower(source_name)
  if (grepl("spatialexperiment", source_key, fixed = TRUE) ||
      grepl("spatial[[:space:]_-]*experiment", source_key)) {
    return("SpatialExperiment")
  }
  if (grepl("giotto", source_key, fixed = TRUE)) {
    return("Giotto")
  }
  if (grepl("seurat", source_key, fixed = TRUE)) {
    return("Seurat")
  }
  source_name <- trimws(source_name)
  if (nzchar(source_name)) source_name else "Spatial"
}

wsi_viewer_prediction_controls <- function(config) {
  prediction <- config$prediction %||% list(enabled = FALSE, sources = list())
  sources <- prediction$sources %||% list()
  if (!isTRUE(prediction$enabled) || !length(sources)) {
    return("")
  }
  source_options <- paste0(
    vapply(seq_along(sources), function(i) {
      source <- sources[[i]]
      paste0(
        "<option value=\"", wsi_html_escape(source$id %||% sprintf("source_%d", i)), "\">",
        wsi_html_escape(source$label %||% source$id %||% sprintf("Source %d", i)),
        "</option>"
      )
    }, character(1)),
    collapse = ""
  )
  live_hint <- if (!is.null(config$prediction_url) &&
      is.character(config$prediction_url) && length(config$prediction_url) == 1L &&
      !is.na(config$prediction_url) && nzchar(config$prediction_url)) {
    "Prediction runs in the live R session and returns a prediction layer plus `viewer$get_prediction()`."
  } else {
    "Prediction needs a live R session. Reopen with `live = TRUE` or `wsi_viewer_live()`."
  }
  wsi_viewer_menu(
    "Prediction",
    "Predict annotation class from spatial expression/reductions or CellPhenotyper cell features",
    paste0(
      "<div class=\"menuTitle\">PLS-LDA annotation prediction</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"predictionWindowOpen\" type=\"button\" title=\"Open the PLS-LDA prediction window\">Open prediction</button>",
      "<button id=\"predictionLayerClear\" type=\"button\" title=\"Remove the current prediction layer\">Clear layer</button>",
      "</div>",
      "<label class=\"control\" title=\"Feature source used by the prediction model\">Source <select id=\"predictionMenuFeatureSource\">",
      source_options,
      "</select></label>",
      "<div id=\"predictionMenuSummary\" class=\"menuHint\">",
      wsi_html_escape(live_hint),
      "</div>"
    )
  )
}

wsi_viewer_chrome <- function(config, loading_message, tiled = FALSE) {
  class_options <- wsi_viewer_class_options(config$roi_class_presets)
  roi_panel_class <- "panel open"
  managed_analysis_project <- isTRUE(config$managed_analysis_project)
  project_add_controls <- if (managed_analysis_project) {
    ""
  } else {
    paste0(
      "<button id=\"projectOpenImage\" title=\"Add one or more browser-readable images or microscopy/WSI file references as new project items\">Add image</button>",
      "<input id=\"projectImageFile\" type=\"file\" accept=\"image/*,.png,.jpg,.jpeg,.webp,.gif,.bmp,.tif,.tiff,.btf,.ome.tif,.ome.tiff,.qptiff,.svs,.ndpi,.scn,.mrxs,.bif,.czi,.lif,.vsi,.vms,.vmu,.zvi,.lsm,.oir,.isyntax,.jp2,.dicom,.dcm\" multiple style=\"display:none\">"
    )
  }
  project_menu_summary <- if (managed_analysis_project) {
    ""
  } else {
    "Add image accepts browser-readable images plus WSI and microscopy formats such as CZI, SVS, NDPI, BTF, OME-TIFF, QPTIFF, MRXS, SCN, BIF and DICOM. Browser-readable images preview immediately; raw WSI/microscopy files are added as references without loading the whole file into memory and should be opened from R/backends for full-resolution tiles."
  }
  project_menu_summary_markup <- if (nzchar(project_menu_summary)) {
    paste0("<div id=\"projectMenuSummary\" class=\"menuHint\">", wsi_html_escape(project_menu_summary), "</div>")
  } else {
    "<div id=\"projectMenuSummary\" class=\"menuHint\" style=\"display:none\"></div>"
  }
  project_help_item <- if (managed_analysis_project) {
    "<li>Managed Seurat, Giotto, SpatialExperiment, and CellPhenotyper projects keep their image set fixed from R so overlays remain aligned.</li>"
  } else {
    "<li>Use Project > Add image to add ordinary browser-readable images to the current viewer.</li>"
  }
  sync_indicator_markup <- paste0(
    "<span id=\"jobSyncIndicator\" class=\"syncIndicator off\" title=\"Live R synchronization status\" role=\"status\" aria-live=\"polite\">",
    "<span class=\"syncDot\" aria-hidden=\"true\"></span>",
    "<span id=\"jobSyncLabel\" class=\"syncLabel\">Sync off</span>",
    "<span id=\"jobSyncDetail\" class=\"syncDetail\"></span>",
    "</span>"
  )
  viewer_markup <- if (isTRUE(tiled)) {
    "<div id=\"viewer\" class=\"osdViewer\" aria-label=\"Whole-slide image\"></div>\n<canvas id=\"overlay\" aria-label=\"Annotation overlay\"></canvas>\n"
  } else {
    "<canvas id=\"viewer\"></canvas>\n"
  }
  paste0(
    viewer_markup,
    "<div id=\"multiViewGrid\" class=\"multiViewGrid\" aria-label=\"Multi-view tissue display\"></div>\n",
    "<div class=\"bar\">\n",
    "<div class=\"panel\"><div class=\"titleLine\"><div class=\"title\">", wsi_html_escape(config$title), "</div>",
    "<span id=\"annotationDirtyIndicator\" class=\"unsavedIndicator\" role=\"button\" tabindex=\"0\" aria-label=\"Save unsaved project changes\" title=\"Click to save the current project changes\">Unsaved</span></div><div class=\"meta\">",
    wsi_html_escape(config$subtitle), "</div></div>\n",
    "<div class=\"spacer\"></div>\n",
    "<div class=\"panel tools\" role=\"toolbar\" aria-label=\"Viewer tools\">",
    "<button id=\"toolPan\" class=\"navPanButton active\" title=\"Pan mode\" aria-label=\"Pan mode\"><svg class=\"iconMove\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M12 2l3 3h-2v6h6V9l3 3-3 3v-2h-6v6h2l-3 3-3-3h2v-6H5v2l-3-3 3-3v2h6V5H9l3-3z\" fill=\"currentColor\"/></svg></button>",
    wsi_viewer_menu(
      "Project",
      "Open the project panel or add images/file references",
      paste0(
        "<div class=\"menuGrid\">",
        project_add_controls,
        "<button id=\"projectSaveFile\" title=\"Save this viewer project with project images, annotations, and trajectories\">Save project</button>",
        "<button id=\"projectOpenFile\" title=\"Open a saved wsiTools viewer project JSON file\">Open project</button>",
        "</div>",
        "<input id=\"projectFile\" type=\"file\" accept=\"application/json,.json,.wsiproject,.wsiproject.json\" style=\"display:none\">",
        project_menu_summary_markup
      )
    ),
    wsi_viewer_menu(
      "Annotations",
      "Draw, select, import, export, and manage annotations",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"toolDraw\" title=\"Draw a polygon ROI\">Draw ROI</button>",
        "<button id=\"toolBrush\" title=\"Paint a new automatically named annotation; touching the same label merges, and Alt/Command removes from the selected annotation\">Brush</button>",
        "<button id=\"toolEdit\" title=\"Edit selected ROI vertices or redraw a smooth boundary curve\">Edit</button>",
        "</div>",
        "<div class=\"menuTitle\">GeoJSON and display</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"roiToggle\" title=\"Toggle ROI overlays\">ROI</button>",
        "<button id=\"labelsToggle\" title=\"Toggle ROI labels\">Labels</button>",
        "<button id=\"importGeojson\" title=\"Import QuPath or wsiTools GeoJSON annotations\">Import GeoJSON</button>",
        "<button id=\"saveGeojson\" title=\"Open annotation export options\">Save annotations</button>",
        "<button id=\"saveAnnotationSpotsCsv\" title=\"Save the annotation-to-spot association table as CSV\">Save spots CSV</button>",
        "</div>",
        "<label class=\"control\" title=\"ROI opacity\">Opacity <input id=\"roiOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>",
        "<input id=\"geojsonImportFile\" type=\"file\" accept=\".geojson,.json,application/geo+json,application/json\" style=\"display:none\">",
        "<div id=\"geojsonImportSummary\" class=\"menuHint\"></div>",
        "<div class=\"menuTitle\">Undo / redo</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"undoAnnotation\" title=\"Undo the last annotation edit, up to 10 steps\">Undo edit</button>",
        "<button id=\"redoAnnotation\" title=\"Redo the last undone annotation edit, up to 10 steps\">Redo edit</button>",
        "</div>",
        "<div class=\"menuTitle\">Brush refinement</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"fillRoiHoles\" title=\"Fill holes and remove brush-subtraction areas from the selected annotation\">Fill holes</button>",
        "</div>",
        "<button id=\"deleteRoi\" title=\"Delete the selected ROI\">Delete selected</button>"
      )
    ),
    wsi_viewer_cell_controls(config),
    wsi_viewer_seurat_controls(config),
    wsi_viewer_prediction_controls(config),
    wsi_viewer_kodama_controls(config),
    wsi_viewer_artifact_controls(config),
    wsi_viewer_menu(
      "Measure",
      "Distance measurement tools",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"toolMeasure\" title=\"Measure distance between two points\">Distance</button>",
        "<button id=\"clearMeasures\" title=\"Clear distance measurements\">Clear</button>",
        "</div>",
        "<div class=\"menuHint\">Click two points on the slide to measure distance.</div>",
        "<div id=\"measureSummary\" class=\"sideMeta\"></div>",
        "<div id=\"measureList\"></div>"
      )
    ),
    wsi_viewer_menu(
      "Trajectories",
      "Draw smoothed trajectory paths",
      paste0(
        {
          trajectory_help <- paste(
            "Click points on the slide to sketch a trajectory.",
            "Double-click, Enter, or Finish saves a smoothed backbone and returns to pan mode.",
            "Click a saved trajectory path to select it, then press Delete to remove only that trajectory.",
            "Edit border turns the flat-ended trajectory width preview into editable vertices; drag a border dot and nearby dots move smoothly with it.",
            "Update border rebuilds it with the current width."
          )
          paste0(
            "<div class=\"menuGrid\">",
            "<button id=\"trajectoryHelp\" type=\"button\" title=\"",
            wsi_html_escape(trajectory_help),
            "\">Help</button>",
            "</div>"
          )
        },
        "<div class=\"menuGrid\">",
        "<button id=\"toolTrajectory\" title=\"Click control points to sketch a trajectory\">Draw</button>",
        "<button id=\"finishTrajectory\" title=\"Finish and resample the current trajectory\">Finish</button>",
        "<button id=\"undoTrajectoryPoint\" title=\"Remove the last trajectory control point\">Undo point</button>",
        "<button id=\"editTrajectoryArea\" title=\"Edit the trajectory border vertices using the current trajectory width\">Edit border</button>",
        "<button id=\"updateTrajectoryArea\" title=\"Rebuild the selected trajectory border using the current width\">Update border</button>",
        "<button id=\"clearTrajectories\" title=\"Clear all trajectories\">Clear</button>",
        "</div>",
        "<label class=\"control\" title=\"Full width of the annotation corridor created around the trajectory backbone, in slide pixels\">area width <input id=\"trajectoryAreaWidth\" type=\"range\" min=\"16\" max=\"5000\" step=\"16\" value=\"512\"><span id=\"trajectoryAreaWidthValue\">512 px</span></label>",
        "<label class=\"control\" title=\"Preview the annotation corridor around the draft or selected trajectory\"><input id=\"trajectoryAreaPreview\" type=\"checkbox\" checked>preview area</label>",
        "<div class=\"menuTitle\">Gradient profile</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"trajectoryProfileHelp\" type=\"button\" title=\"Show temporary guidance for trajectory gradient profiling\">Profile help</button>",
        "</div>",
        "<label class=\"control wide\" title=\"Visible spot, cell, or point layer sampled along the trajectory\">Points <select id=\"trajectoryProfileSource\"></select></label>",
        "<label class=\"control wide\" title=\"Feature summarised along the trajectory; gene values must first be loaded into the visible spot layer from R\">Feature <select id=\"trajectoryProfileFeature\"></select></label>",
        "<label class=\"control\" title=\"Number of bins along the trajectory backbone\">bins <input id=\"trajectoryProfileBins\" type=\"number\" min=\"2\" max=\"200\" step=\"1\" value=\"20\"></label>",
        "<label class=\"control\" title=\"Full corridor width sampled around the trajectory, in slide pixels\">profile width <input id=\"trajectoryProfileWidth\" type=\"range\" min=\"16\" max=\"5000\" step=\"16\" value=\"512\"><span id=\"trajectoryProfileWidthValue\">512 px</span></label>",
        "<div class=\"menuGrid\">",
        "<button id=\"runTrajectoryProfile\" type=\"button\" title=\"Profile the selected point/cell feature along the trajectory\">Run profile</button>",
        "<button id=\"clearTrajectoryProfile\" type=\"button\" title=\"Clear trajectory profile result overlays\">Clear profile</button>",
        "</div>",
        "<div id=\"trajectoryProfileSummary\" class=\"menuHint\" aria-live=\"polite\"></div>",
        if (isTRUE((config$proximity %||% list())$enabled)) paste0(
          "<div class=\"menuTitle\">Proximity analysis</div>",
          "<div class=\"menuGrid\">",
          "<button id=\"proximityHelp\" type=\"button\" title=\"Show temporary guidance for proximity analysis and statistics\">Proximity help</button>",
          "</div>",
          "<div class=\"proximityControlGrid\">",
          "<label class=\"control proximityWide\" title=\"Spots or cells used for the distance calculation\"><span>Points</span><select id=\"proximityPointSource\"></select></label>",
          "<label class=\"control\" title=\"Annotation containing the spots/cells whose distance will be measured\"><span>Measure inside</span><select id=\"proximityQueryAnnotations\" multiple></select></label>",
          "<label class=\"control\" title=\"Annotation containing the reference spots/cells to measure from\"><span>Distance from</span><select id=\"proximityTargetAnnotations\" multiple></select></label>",
          "</div>",
          "<div class=\"menuGrid\">",
          "<button id=\"runProximityAnalysis\" type=\"button\" title=\"Run nearest-neighbour proximity analysis in the live R session\">Run proximity</button>",
          "<button id=\"clearProximityLayer\" type=\"button\" title=\"Remove the current proximity result layer\">Clear result</button>",
          "</div>",
          "<label class=\"control wide\" title=\"Feature matrix analysed against binned proximity distance\">Statistics source <select id=\"proximityStatsFeatureSource\"></select></label>",
          "<label class=\"control\" title=\"Statistical method used to rank features by distance trend\">Method <select id=\"proximityStatsMethod\"><option value=\"spearman\" selected>Spearman</option><option value=\"pearson\">Pearson</option><option value=\"mine\">MINE</option></select></label>",
          "<label class=\"control\" title=\"Distance quantile step; 0.005 matches 0.5 percent bins\">Quantile step <input id=\"proximityStatsQuantileStep\" type=\"number\" min=\"0.001\" max=\"0.5\" step=\"0.001\" value=\"0.005\"></label>",
          "<label class=\"control\" title=\"Maximum highest-variance features analysed; use 0 for all features\">Max features <input id=\"proximityStatsMaxFeatures\" type=\"number\" min=\"0\" step=\"100\" value=\"5000\"></label>",
          "<div class=\"menuGrid\">",
          "<button id=\"runProximityStats\" type=\"button\" title=\"Bin proximity distances and rank genes/features in the live R session\">Run statistics</button>",
          "<button id=\"showProximityStats\" type=\"button\" title=\"Open the latest proximity statistics table\">Show table</button>",
          "</div>",
          "<div id=\"proximitySummary\" class=\"menuHint\" aria-live=\"polite\"></div>"
        ) else "",
        "<div id=\"trajectorySummary\" class=\"sideMeta\"></div>",
        "<div id=\"trajectoryList\"></div>"
      )
    ),
    wsi_viewer_menu(
      "View",
      "Display aids and coordinates",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"projectPanelToggle\" title=\"Show or hide the left Project panel\">Project panel</button>",
        "<button id=\"annotationPanelToggle\" title=\"Show or hide the left Annotations, ROIs, and Layers panel\">Annotation panel</button>",
        "<button id=\"layerPanelToggle\" title=\"Show or hide the left Layers and ROIs panel\">Layer panel</button>",
        "<button id=\"historyPanelToggle\" title=\"Show or hide the left History panel\">History panel</button>",
        "<button id=\"viewerLogPanelToggle\" title=\"Show troubleshooting logs, warnings, and browser errors\">Logs panel</button>",
        "<button id=\"savePreferences\" title=\"Save brush, annotation, tool, and display preferences in this browser\">Save prefs</button>",
        "<button id=\"resetPreferences\" title=\"Reset persistent viewer preferences saved in this browser\">Reset prefs</button>",
        "</div>",
        "<div class=\"menuTitle\">Magnification</div>",
        "<div class=\"menuGrid magnificationGrid\">",
        "<button id=\"magnification5\" class=\"magnificationPreset\" data-magnification=\"5\" title=\"Zoom to approximately 5x scan-equivalent magnification\">5x</button>",
        "<button id=\"magnification10\" class=\"magnificationPreset\" data-magnification=\"10\" title=\"Zoom to approximately 10x scan-equivalent magnification\">10x</button>",
        "<button id=\"magnification20\" class=\"magnificationPreset\" data-magnification=\"20\" title=\"Zoom to approximately 20x scan-equivalent magnification\">20x</button>",
        "<button id=\"magnification40\" class=\"magnificationPreset\" data-magnification=\"40\" title=\"Zoom to approximately 40x scan-equivalent magnification\">40x</button>",
        "<button id=\"magnificationInitial\" title=\"Return to the initial magnification and starting view\">Initial</button>",
        "</div>",
        "<div id=\"magnificationSummary\" class=\"menuHint\">Magnification unavailable</div>",
        "<div class=\"menuTitle\">Multi-view tissue display</div>",
        "<div class=\"menuGrid multiViewControls\">",
        "<button id=\"multiView1\" class=\"multiViewLayout\" data-layout=\"1\" title=\"Return to one main tissue view\">1 view</button>",
        "<button id=\"multiView2\" class=\"multiViewLayout\" data-layout=\"2\" title=\"Split the workspace into two tissue views\">2 views</button>",
        "<button id=\"multiView4\" class=\"multiViewLayout\" data-layout=\"4\" title=\"Split the workspace into four tissue views\">4 views</button>",
        "<button id=\"multiView6\" class=\"multiViewLayout\" data-layout=\"6\" title=\"Split the workspace into six tissue views\">6 views</button>",
        "</div>",
        "<label class=\"control wide\" title=\"Custom number of multi-view panes, from 1 to 12\">custom <input id=\"multiViewCustomCount\" type=\"number\" min=\"1\" max=\"12\" step=\"1\" value=\"3\"><button id=\"multiViewCustom\" type=\"button\">Apply</button></label>",
        "<label class=\"control\" title=\"Synchronize zoom and pan across all multi-view panes for this viewer session\"><input id=\"multiViewSync\" type=\"checkbox\">link zoom/pan</label>",
        "<div id=\"multiViewSummary\" class=\"menuHint\">Single view. Multi-view panes are independent by default; enable link zoom/pan only when synchronized comparison is needed. Drag project images or sections onto a pane to replace that pane.</div>",
        "<div id=\"syncSummary\" class=\"menuHint\"></div>"
      )
    ),
    wsi_viewer_stain_controls(config),
    wsi_viewer_menu(
      "Help",
      "Viewer guide and shortcuts",
      paste0(
        "<div class=\"menuTitle\">Help</div>",
        "<button id=\"shortcutHelpKeyboard\" title=\"Open keyboard shortcuts first\">Keyboard Shortcuts</button>",
        "<button id=\"shortcutHelpButton\" title=\"Open the full viewer guide\">Full Guide</button>",
        "<div class=\"menuHint\">Press ? anytime to open the organized help dialog. Quick recommendations are included after the guide.</div>"
      )
    ),
    sync_indicator_markup,
    "</div>\n",
    "</div>\n",
    "<div id=\"navDock\" class=\"panel navDock\" aria-label=\"Navigation controls\">",
    "<button id=\"rotateImageLeft\" class=\"imageTransformButton\" title=\"Rotate the displayed image 90 degrees counter-clockwise\" aria-label=\"Rotate image left\"><svg class=\"navIcon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M7 7h7a6 6 0 1 1-4.2 10.2\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"/><path d=\"M7 3v4h4\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg></button>",
    "<button id=\"rotateImageRight\" class=\"imageTransformButton\" title=\"Rotate the displayed image 90 degrees clockwise\" aria-label=\"Rotate image right\"><svg class=\"navIcon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M17 7h-7a6 6 0 1 0 4.2 10.2\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"/><path d=\"M17 3v4h-4\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"/></svg></button>",
    "<button id=\"flipImageVertical\" class=\"imageTransformButton\" title=\"Flip the displayed image vertically\" aria-label=\"Flip image vertically\"><svg class=\"navIcon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M5 12h14\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"/><path d=\"M8 5h8l-4 5z\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linejoin=\"round\"/><path d=\"M8 19h8l-4-5z\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linejoin=\"round\"/></svg></button>",
    "<button id=\"flipImageHorizontal\" class=\"imageTransformButton\" title=\"Flip the displayed image horizontally\" aria-label=\"Flip image horizontally\"><svg class=\"navIcon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M12 5v14\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"/><path d=\"M5 8v8l5-4z\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linejoin=\"round\"/><path d=\"M19 8v8l-5-4z\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linejoin=\"round\"/></svg></button>",
    "<button id=\"screenshotTool\" class=\"screenshotButton\" title=\"Select an area and save a PNG, JPEG, SVG, or PDF screenshot\" aria-label=\"Select screenshot area\"><svg class=\"navIcon\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M4 8h3l1.8-2h6.4L17 8h3v10H4z\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linejoin=\"round\"/><circle cx=\"12\" cy=\"13\" r=\"3.2\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"/><path d=\"M7 18h10\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"/></svg></button>",
    "<button id=\"zoomIn\" title=\"Zoom in\" aria-label=\"Zoom in\">+</button>",
    "<button id=\"zoomOut\" title=\"Zoom out\" aria-label=\"Zoom out\">-</button>",
    "<button id=\"fit\" title=\"Fit slide to window\" aria-label=\"Fit slide to window\">Fit</button>",
    "<button id=\"oneToOne\" title=\"Show image pixels at 1:1\" aria-label=\"Show image pixels at 1:1\">1:1</button>",
    "</div>\n",
    "<div id=\"proximityLegend\" aria-label=\"Proximity distance colour legend\"></div>\n",
    "<div id=\"workspacePanel\" aria-label=\"Project and annotation panels\">",
    "<div id=\"projectPanel\" class=\"panel projectPanel\" aria-label=\"Project images and sections\">",
    "<div id=\"projectPanelHeader\" class=\"projectPanelHeader\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" title=\"Double-click to minimize or restore the project panel\">",
    "<div><div class=\"sideTitle\">Project</div><div class=\"sideMeta\">Images and sections</div></div>",
    "<div class=\"panelHeaderActions\"><div id=\"projectPanelMinimizeState\" class=\"projectPanelMinimizeState\">double-click to minimize</div>",
    "<button id=\"projectPanelClose\" class=\"panelCloseButton\" type=\"button\" title=\"Close Project panel\" aria-label=\"Close Project panel\" onclick=\"event.preventDefault();event.stopPropagation();if(typeof closeProjectPanel==='function'){closeProjectPanel(event);}else{var p=document.getElementById('projectPanel');if(p)p.classList.add('closed');}return false;\">x</button></div>",
    "</div>",
    "<div id=\"projectPanelBody\">",
    "<div id=\"projectSummary\" class=\"sideMeta\"></div>",
    "<div id=\"projectImageList\"></div>",
    "<div id=\"projectSectionList\"></div>",
    "</div>",
    "<div class=\"sidePanelResizeHandle\" data-panel=\"projectPanel\" role=\"separator\" aria-orientation=\"horizontal\" title=\"Drag to resize Project vertically\"></div>",
    "</div>",
    "<div id=\"roiPanel\" class=\"", roi_panel_class, "\" aria-label=\"Annotation manager\">",
    "<div id=\"roiPanelHeader\" class=\"roiPanelHeader\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" title=\"Double-click to minimize or restore the annotation manager\">",
    "<div><div class=\"sideTitle\">Annotations</div><div class=\"sideMeta\">GeoJSON Geometries</div></div>",
    "<div class=\"panelHeaderActions\"><div id=\"roiPanelMinimizeState\" class=\"roiPanelMinimizeState\">double-click to minimize</div>",
    "<button id=\"roiPanelClose\" class=\"panelCloseButton\" type=\"button\" title=\"Close annotation panel\" aria-label=\"Close annotation panel\">x</button></div>",
    "</div><div id=\"roiPanelBody\"><div id=\"roiSummary\" class=\"sideMeta\"></div>",
    "<div id=\"newAnnotationEditor\" class=\"annotationEditor\" aria-label=\"New ROI annotation settings\">",
    "<div class=\"annotationEditorTitle\">New ROI</div>",
    "<div class=\"annotationEditorHint\">Category used for the next brush or polygon annotation.</div>",
    "<label class=\"control\" title=\"Pathology class for the next drawn or painted annotation\">New class <select id=\"panelRoiClassSelect\">",
    class_options,
    "</select></label>",
    "<label class=\"control\" title=\"Optional custom category for the next drawn or painted annotation\">Custom new <input id=\"panelRoiClassCustom\" type=\"text\" maxlength=\"80\" placeholder=\"custom category\"></label>",
    "<button id=\"panelApplyRoiClass\" title=\"Use this class/category for the next drawn or painted annotation\">Set new ROI category</button>",
    "<div class=\"annotationBrushControls\" aria-label=\"Brush controls\">",
    "<label class=\"control\" title=\"Brush screen radius; effective slide-pixel size adapts automatically with zoom\">Brush size <input id=\"brushSize\" type=\"range\" min=\"8\" max=\"240\" step=\"2\" value=\"32\"><span id=\"brushSizeValue\">32 px</span></label>",
    "<div id=\"brushZoomHint\" class=\"sideMeta\">effective 32 slide px</div>",
    "</div>",
    "</div>",
    "<div id=\"selectedAnnotationEditor\" class=\"annotationEditor\" aria-label=\"Selected annotation editor\" hidden>",
    "<div class=\"annotationEditorTitle\">Selected annotation</div>",
    "<div class=\"annotationEditorHint\">Shown only when an annotation is selected. Applying a category also updates the annotation name.</div>",
    "<label class=\"control\" title=\"Pathology class for the selected annotation\">Selected class <select id=\"annotationClassSelect\">",
    class_options,
    "</select></label>",
    "<label class=\"control\" title=\"Optional custom annotation category\">Custom <input id=\"annotationClassCustom\" type=\"text\" maxlength=\"80\" placeholder=\"custom category\"></label>",
    "<label class=\"control\" title=\"Annotation display color\">Color <input id=\"annotationColorInput\" type=\"color\" value=\"#00BFC4\"></label>",
    "<div class=\"annotationActions\">",
    "<button id=\"annotationApply\" title=\"Apply class/category and color to the selected annotation; the annotation name is updated without duplicates\">Apply</button>",
    "<button id=\"annotationDelete\" title=\"Delete the selected annotation\">Delete</button>",
    "</div>",
    "<button id=\"annotationExportSelected\" title=\"Export checked annotations, or the selected annotation when none are checked\">Export selected ROIs</button>",
    "</div>",
    "<div class=\"sideTitle\">Layers</div><div class=\"sideMeta\">R-controlled overlays</div><div id=\"layerSummary\" class=\"sideMeta\"></div><div id=\"layerList\"></div>",
    "<div id=\"channelLegendPanel\" class=\"channelLegendPanel\" aria-label=\"Mask legend\"><div class=\"sideTitle\">Mask legend</div><div id=\"channelLegendSummary\" class=\"sideMeta\"></div><div id=\"channelLegendList\" class=\"channelLegendList\"></div></div>",
    "<div class=\"annotationLabelHighlighter\" aria-label=\"Category highlighting\">",
    "<div class=\"sideTitle\">Highlight labels</div>",
    "<div id=\"annotationLabelHighlightSummary\" class=\"sideMeta\">No categories yet.</div>",
    "<div class=\"annotationListTools\"><button id=\"annotationHighlightAll\" type=\"button\" title=\"Highlight every ROI category\">All labels</button><button id=\"annotationHighlightNone\" type=\"button\" title=\"Clear category highlighting\">Clear</button></div>",
    "<div id=\"annotationLabelHighlightList\" class=\"annotationLabelHighlightList\"></div>",
    "</div>",
    "<div class=\"sideTitle\">ROIs</div>",
    "<div class=\"annotationSearch\" aria-label=\"ROI search and filter controls\">",
    "<input id=\"annotationSearchInput\" type=\"text\" maxlength=\"120\" placeholder=\"search name/category\" title=\"Search annotations by name, category, ID, or source\">",
    "<select id=\"annotationFilter\" title=\"Filter annotation list\">",
    "<option value=\"all\">all</option><option value=\"visible\">visible</option><option value=\"hidden\">hidden</option>",
    "<option value=\"locked\">locked</option><option value=\"unlocked\">unlocked</option><option value=\"selected\">selected</option>",
    "</select>",
    "<select id=\"annotationSort\" title=\"Sort annotation list\">",
    "<option value=\"original\">original</option><option value=\"name\">name</option><option value=\"class\">class</option>",
    "<option value=\"area_desc\">area desc</option><option value=\"area_asc\">area asc</option>",
    "</select>",
    "<button id=\"annotationFilterClear\" title=\"Clear ROI search, filter, and sort\">Clear</button>",
    "</div>",
    "<div class=\"annotationListTools\"><button id=\"prevRoi\" title=\"Previous ROI\">Prev</button><button id=\"nextRoi\" title=\"Next ROI\">Next</button><button id=\"annotationSelectAll\" title=\"Check shown annotations for export\">All</button><button id=\"annotationSelectNone\" title=\"Deselect annotations and clear export checks for shown annotations\">Deselect</button></div>",
    "<div id=\"roiList\"></div>",
    "</div><div class=\"sidePanelResizeHandle\" data-panel=\"roiPanel\" role=\"separator\" aria-orientation=\"horizontal\" title=\"Drag to resize Annotations vertically\"></div></div>",
    "<div id=\"annotationHistory\" class=\"panel historyPanel\" aria-label=\"Viewer history\" tabindex=\"-1\">",
    "<div id=\"historyPanelHeader\" class=\"projectPanelHeader\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" title=\"Double-click to minimize or restore the history panel\">",
    "<div><div class=\"sideTitle\">History</div><div class=\"sideMeta\">Viewer actions</div></div>",
    "<div class=\"panelHeaderActions\"><div id=\"historyPanelMinimizeState\" class=\"roiPanelMinimizeState\">double-click to minimize</div><span class=\"historyActions\"><button id=\"maximizeAnnotationHistory\" title=\"Maximize history in the viewer window\" aria-expanded=\"false\">Maximize</button><button id=\"copyAnnotationHistoryAll\" title=\"Copy viewer history, R sync commands, logs, project state, and current URL for troubleshooting\">Copy all</button><button id=\"clearAnnotationHistory\" title=\"Clear the visible history\">Clear</button></span>",
    "<button id=\"historyPanelClose\" class=\"panelCloseButton\" type=\"button\" title=\"Close history panel\" aria-label=\"Close history panel\">x</button></div>",
    "</div>",
    "<div id=\"annotationHistoryBody\">",
    "<div id=\"annotationHistorySummary\" class=\"sideMeta\">No viewer actions yet.</div>",
    "<div id=\"annotationHistoryList\"></div>",
    "</div>",
    "<div class=\"sidePanelResizeHandle\" data-panel=\"annotationHistory\" role=\"separator\" aria-orientation=\"horizontal\" title=\"Drag to resize History vertically\"></div>",
    "</div>",
    "<div id=\"viewerLogPanel\" class=\"panel projectPanel logPanel closed\" aria-label=\"Troubleshooting logs\" tabindex=\"-1\">",
    "<div id=\"viewerLogPanelHeader\" class=\"projectPanelHeader\" role=\"button\" tabindex=\"0\" aria-expanded=\"false\" title=\"Double-click to minimize or restore the troubleshooting log panel\">",
    "<div><div class=\"sideTitle\">Logs</div><div class=\"sideMeta\">Troubleshooting messages</div></div>",
    "<div class=\"panelHeaderActions\"><div id=\"viewerLogPanelMinimizeState\" class=\"roiPanelMinimizeState\">double-click to minimize</div>",
    "<button id=\"viewerLogPanelClose\" class=\"panelCloseButton\" type=\"button\" title=\"Close logs panel\" aria-label=\"Close logs panel\">x</button></div>",
    "</div>",
    "<div id=\"viewerLogBody\">",
    "<div id=\"viewerLogSummary\" class=\"sideMeta\">No messages yet.</div>",
    "<div class=\"viewerLogActions\"><button id=\"downloadViewerLog\" title=\"Download troubleshooting log as a text file\">Save log</button><button id=\"copyViewerLog\" title=\"Copy troubleshooting log to clipboard\">Copy</button><button id=\"clearViewerLog\" title=\"Clear visible troubleshooting log messages\">Clear</button></div>",
    "<div id=\"viewerLogList\"></div>",
    "</div>",
    "<div class=\"sidePanelResizeHandle\" data-panel=\"viewerLogPanel\" role=\"separator\" aria-orientation=\"horizontal\" title=\"Drag to resize Logs vertically\"></div>",
    "</div>",
    "<div id=\"workspaceResizeHandle\" role=\"separator\" aria-orientation=\"vertical\" title=\"Drag to resize the left panels\"></div>",
    "</div>\n",
    "<div id=\"selectionCard\" class=\"panel\" aria-label=\"Selected ROI summary\" aria-hidden=\"true\">",
    "<div class=\"selectionCardHead\"><span id=\"selectionCardSwatch\" class=\"swatch\"></span>",
    "<div><div id=\"selectionCardName\" class=\"selectionCardTitle\">No ROI selected</div>",
    "<div id=\"selectionCardClass\" class=\"selectionCardClass\"></div></div></div>",
    "<div class=\"selectionCardStats\">",
    "<span>Area</span><span id=\"selectionCardArea\">NA</span>",
    "<span>Cells</span><span id=\"selectionCardCells\">NA</span>",
    "<span>Density</span><span id=\"selectionCardDensity\">NA</span>",
    "</div>",
    "<div class=\"selectionCardActions\">",
    "<button id=\"selectionZoom\" title=\"Zoom to the selected ROI\">Zoom</button>",
    "<button id=\"selectionEdit\" title=\"Switch to ROI edit mode for vertices or smooth boundary curves\">Edit</button>",
    "<button id=\"selectionDelete\" title=\"Delete the selected ROI\">Delete</button>",
    "<button id=\"selectionClose\" title=\"Hide the selected ROI summary\">Close</button>",
    "</div></div>\n",
    "<div id=\"miniNavigator\" class=\"panel\" aria-label=\"Mini navigator\">",
    "<canvas id=\"miniNavigatorCanvas\"></canvas>",
    "<div class=\"miniNavigatorMeta\"><span id=\"miniNavigatorViewport\">viewport</span><span id=\"miniNavigatorDensity\">density</span></div>",
    "</div>\n",
    "<div id=\"screenshotDialogBackdrop\" aria-hidden=\"true\"></div>\n",
    "<div id=\"screenshotDialog\" class=\"panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"screenshotDialogTitle\" aria-hidden=\"true\">",
    "<div class=\"screenshotDialogHead\"><div><div id=\"screenshotDialogTitle\">Save screenshot</div>",
    "<div id=\"screenshotDialogSubtitle\">Choose format, filename, and visible content.</div></div>",
    "<button id=\"screenshotDialogClose\" type=\"button\" title=\"Cancel screenshot export\">X</button></div>",
    "<div class=\"screenshotDialogGrid\">",
    "<label><span>Format</span><select id=\"screenshotDialogFormat\" aria-label=\"Screenshot format\"><option value=\"png\" selected>PNG</option><option value=\"jpeg\">JPEG</option><option value=\"svg\">SVG</option><option value=\"pdf\">PDF</option></select></label>",
    "<label><span>Filename</span><input id=\"screenshotFileName\" type=\"text\" autocomplete=\"off\" spellcheck=\"false\" aria-label=\"Screenshot filename\"></label>",
    "</div>",
    "<div id=\"screenshotSaveLocation\" class=\"screenshotSaveLocation\">Choose location opens a native desktop or browser Save As dialog when supported.</div>",
    "<div class=\"menuTitle\">Include in image</div>",
    "<div class=\"screenshotOptionList\">",
    "<label><input id=\"screenshotIncludeTissue\" type=\"checkbox\" checked> Tissue image</label>",
    "<label><input id=\"screenshotIncludeLayers\" type=\"checkbox\" checked> Spots/layers</label>",
    "<label><input id=\"screenshotIncludeAnnotations\" type=\"checkbox\" checked> Annotations</label>",
    "<label><input id=\"screenshotIncludeLabels\" type=\"checkbox\" checked> Annotation labels</label>",
    "<label><input id=\"screenshotIncludeMeasurements\" type=\"checkbox\" checked> Measurements</label>",
    "<label><input id=\"screenshotIncludeTrajectories\" type=\"checkbox\" checked> Trajectories</label>",
    "<label><input id=\"screenshotIncludeTileGrid\" type=\"checkbox\" checked> Tile grid</label>",
    "<label><input id=\"screenshotIncludeArtifacts\" type=\"checkbox\" checked> Artifacts/QC</label>",
    "</div>",
    "<div id=\"screenshotDialogSummary\" class=\"menuHint\"></div>",
    "<div class=\"screenshotDialogActions\"><button id=\"screenshotDialogCancel\" type=\"button\">Cancel</button><button id=\"screenshotDialogSave\" type=\"button\">Save screenshot</button></div>",
    "</div>\n",
    "<div id=\"annotationExportDialogBackdrop\" aria-hidden=\"true\"></div>\n",
    "<div id=\"annotationExportDialog\" class=\"panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"annotationExportDialogTitle\" aria-hidden=\"true\">",
    "<div class=\"screenshotDialogHead\"><div><div id=\"annotationExportDialogTitle\">Save annotations</div>",
    "<div id=\"annotationExportDialogSubtitle\">Choose export format, scope, filename, and save location.</div></div>",
    "<button id=\"annotationExportDialogClose\" type=\"button\" title=\"Cancel annotation export\">X</button></div>",
    "<div class=\"screenshotDialogGrid\">",
    "<label><span>Format</span><select id=\"annotationExportFormat\" aria-label=\"Annotation export format\"><option value=\"geojson\" selected>GeoJSON</option><option value=\"json\">JSON</option><option value=\"csv\">CSV summary</option></select></label>",
    "<label><span>Scope</span><select id=\"annotationExportScope\" aria-label=\"Annotation export scope\"><option value=\"all\" selected>All exportable annotations</option><option value=\"selected\">Selected or checked annotations</option></select></label>",
    "<label><span>Filename</span><input id=\"annotationExportFileName\" type=\"text\" autocomplete=\"off\" spellcheck=\"false\" value=\"wsiTools_annotations.geojson\" aria-label=\"Annotation export filename\"></label>",
    "</div>",
    "<div id=\"annotationExportSaveLocation\" class=\"screenshotSaveLocation\">Choose location opens a native desktop or browser Save As dialog when supported. Download keeps the browser download fallback.</div>",
    "<div id=\"annotationExportDialogSummary\" class=\"menuHint\"></div>",
    "<div class=\"screenshotDialogActions\"><button id=\"annotationExportDialogCancel\" type=\"button\">Cancel</button><button id=\"annotationExportDialogDownload\" type=\"button\" aria-label=\"Download annotations using the browser download folder\" title=\"Download using the browser download folder\">Download</button><button id=\"annotationExportDialogSave\" type=\"button\" aria-label=\"Choose annotation save location\" title=\"Choose the annotation export file path\">Choose location and save</button></div>",
    "</div>\n",
    "<div id=\"commandPaletteBackdrop\" aria-hidden=\"true\"></div>\n",
    "<div id=\"commandPalette\" class=\"panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"commandPaletteTitle\" aria-hidden=\"true\">",
    "<div class=\"commandPaletteHead\"><div><div id=\"commandPaletteTitle\" class=\"commandPaletteTitle\">Command Palette</div>",
    "<div class=\"commandPaletteHint\">Type an action or use arrow keys</div></div><span class=\"commandKbd\">Ctrl+K</span></div>",
    "<input id=\"commandPaletteSearch\" type=\"text\" autocomplete=\"off\" spellcheck=\"false\" placeholder=\"Search commands\">",
    "<div id=\"commandPaletteList\" class=\"commandPaletteList\" role=\"listbox\" aria-label=\"Viewer commands\"></div>",
    "</div>\n",
    "<div id=\"kodamaPlotWindow\" class=\"panel\" role=\"dialog\" aria-modal=\"false\" aria-labelledby=\"kodamaPlotTitle\" aria-hidden=\"true\">",
    "<div class=\"kodamaPlotHead\"><div><div id=\"kodamaPlotTitle\">KODAMA plot</div>",
    "<div id=\"kodamaPlotSubtitle\"></div></div><button id=\"kodamaPlotClose\" type=\"button\" title=\"Close KODAMA plot window\">X</button></div>",
    "<div class=\"kodamaPlotTools\">",
    "<button id=\"kodamaPlotAnnotation\" type=\"button\" title=\"Draw KODAMA points with the same class colors used for annotations\">Annotation colours</button>",
    "<button id=\"kodamaClearSelection\" type=\"button\" title=\"Clear KODAMA-selected cell highlights\">Clear selection</button>",
    "</div>",
    "<div id=\"kodamaPlotSelectionStatus\" class=\"menuHint\">Draw a lasso around KODAMA points to highlight matching cells on the slide.</div>",
    "<div id=\"kodamaPlotViewport\"><canvas id=\"kodamaPlotCanvas\"></canvas></div>",
    "<div id=\"kodamaPlotLegend\"></div>",
    "</div>\n",
    "<div id=\"seuratPlotWindow\" class=\"panel\" role=\"dialog\" aria-modal=\"false\" aria-labelledby=\"seuratPlotTitle\" aria-hidden=\"true\">",
    "<div class=\"seuratPlotHead\"><div><div id=\"seuratPlotTitle\">Spatial reduction plot</div>",
    "<div id=\"seuratPlotSubtitle\"></div></div><button id=\"seuratPlotClose\" type=\"button\" title=\"Close reduction plot window\">X</button></div>",
    "<div class=\"seuratPlotTools\">",
    "<button id=\"seuratPlotReset\" type=\"button\" title=\"Redraw the reduction plot\">Redraw</button>",
    "<button id=\"seuratPlotClearSelection\" type=\"button\" title=\"Clear selected spots\">Clear selection</button>",
    "<div class=\"seuratPlotScope\" role=\"group\" aria-label=\"Dimensionality reduction tissue scope\">",
    "<button id=\"seuratPlotScopeCurrent\" type=\"button\" title=\"Show spots from the currently selected tissue only\">Current tissue</button>",
    "<button id=\"seuratPlotScopeAll\" type=\"button\" title=\"Show spots from all tissues in the project together\">All tissues</button>",
    "</div>",
    "</div>",
	    "<div id=\"seuratPlotSelectionStatus\" class=\"menuHint\">Draw a lasso around reduction points to highlight matching spots on the slide.</div>",
	    "<div id=\"seuratPlotViewport\"><canvas id=\"seuratPlotCanvas\"></canvas></div>",
	    "<div id=\"seuratPlotLegend\"></div>",
	    "</div>\n",
	    "<div id=\"proximityStatsWindow\" class=\"panel\" role=\"dialog\" aria-modal=\"false\" aria-labelledby=\"proximityStatsTitle\" aria-hidden=\"true\">",
	    "<div class=\"proximityStatsHead\"><div><div id=\"proximityStatsTitle\">Proximity statistics</div>",
	    "<div id=\"proximityStatsSubtitle\">No statistics have been run.</div></div><button id=\"proximityStatsClose\" type=\"button\" title=\"Close proximity statistics window\">X</button></div>",
	    "<div class=\"proximityStatsTools\">",
	    "<button id=\"proximityStatsDownloadCsv\" type=\"button\" title=\"Download the ranked proximity statistics table as CSV\">Save CSV</button>",
	    "<button id=\"proximityStatsClear\" type=\"button\" title=\"Clear the current proximity statistics table\">Clear</button>",
	    "</div>",
	    "<div id=\"proximityStatsWindowSummary\" class=\"menuHint\">Run proximity statistics from the Trajectories menu.</div>",
	    "<div class=\"proximityStatsTableWrap\"><table id=\"proximityStatsTable\" class=\"proximityStatsTable\"><thead><tr><th>rank</th><th>feature</th><th>method</th><th>correlation</th><th>MIC</th><th>p</th><th>bins</th><th>points</th></tr></thead><tbody></tbody></table></div>",
	    "</div>\n",
	    "<div id=\"predictionWindow\" class=\"panel\" role=\"dialog\" aria-modal=\"false\" aria-labelledby=\"predictionTitle\" aria-hidden=\"true\">",
	    "<div class=\"predictionHead\"><div><div id=\"predictionTitle\">PLS-LDA prediction</div>",
	    "<div id=\"predictionSubtitle\"></div></div><button id=\"predictionClose\" type=\"button\" title=\"Close prediction window\">X</button></div>",
	    "<div class=\"predictionForm\">",
	    "<label class=\"control wide\" title=\"Predictor matrix used by fastPLS\">Source <select id=\"predictionFeatureSource\"></select></label>",
	    "<label class=\"control wide\" title=\"Annotations used as the labelled training set\">Training annotations <select id=\"predictionTrainAnnotations\" multiple></select></label>",
	    "<label class=\"control wide\" title=\"Annotations to predict, or all non-training spots/cells\">Test set <select id=\"predictionTestAnnotations\" multiple></select></label>",
	    "<label class=\"control\" title=\"Number of PLS components\">Components <input id=\"predictionNcomp\" type=\"number\" min=\"1\" step=\"1\" value=\"2\"></label>",
	    "<label id=\"predictionReductionDimsControl\" class=\"control\" title=\"For PCA/UMAP/t-SNE/KODAMA sources, use the first N dimensions from the live R reduction matrix\">Reduction dims <input id=\"predictionReductionDims\" type=\"number\" min=\"1\" step=\"1\" value=\"10\"></label>",
	    "<label id=\"predictionRefineSvmControl\" class=\"control\" title=\"Optionally refine PLS-LDA labels in R using an internal SVM refinement step; requires the suggested e1071 package\"><input id=\"predictionRefineSvm\" type=\"checkbox\"> Refine SVM</label>",
	    "<label class=\"control\" title=\"Feature scaling used before fitting\">Scaling <select id=\"predictionScaling\"><option value=\"autoscaling\">autoscaling</option><option value=\"centering\">centering</option><option value=\"none\">none</option></select></label>",
	    "<label class=\"control\" title=\"PLS method passed to fastPLS\">Method <select id=\"predictionMethod\"><option value=\"simpls\">simpls</option><option value=\"plssvd\">plssvd</option><option value=\"opls\">opls</option><option value=\"kernelpls\">kernelpls</option></select></label>",
	    "<label class=\"control\" title=\"Maximum highest-variance features used from raw expression; 0 means all features\">Max features <input id=\"predictionMaxFeatures\" type=\"number\" min=\"0\" step=\"100\" value=\"5000\"></label>",
	    "</div>",
	    "<div class=\"predictionTools\">",
	    "<button id=\"predictionRefreshAnnotations\" type=\"button\" title=\"Refresh the annotation choices from the current viewer ROIs\">Refresh ROIs</button>",
	    "<button id=\"predictionRun\" type=\"button\" title=\"Run fastPLS PLS-LDA in R\">Run PLS-LDA</button>",
	    "<button id=\"predictionClear\" type=\"button\" title=\"Remove the current prediction layer\">Clear layer</button>",
	    "</div>",
	    "<div id=\"predictionSummary\" class=\"menuHint\">Select training annotations from the current ROI list.</div>",
	    "</div>\n",
	    "<div id=\"spatialTileWindow\" class=\"panel\" role=\"dialog\" aria-modal=\"false\" aria-labelledby=\"spatialTileTitle\" aria-hidden=\"true\">",
	    "<div class=\"spatialTileHead\"><div><div id=\"spatialTileTitle\">Spot-centered tiles</div>",
	    "<div id=\"spatialTileSubtitle\"></div></div><button id=\"spatialTileClose\" type=\"button\" title=\"Close spot tile window\">X</button></div>",
	    "<div class=\"spatialTileForm\">",
	    "<label class=\"control\" title=\"Tile width and height\">Size <input id=\"spatialTileSize\" type=\"number\" min=\"1\" step=\"1\" value=\"512\"></label>",
	    "<label class=\"control\" title=\"Interpret tile size as pixels or microns\">Units <select id=\"spatialTileUnits\"><option value=\"px\">pixels</option><option value=\"um\">microns</option></select></label>",
	    "<label class=\"control\" title=\"Image format for saved tiles\">Format <select id=\"spatialTileFormat\"><option value=\"png\">png</option><option value=\"jpeg\">jpg</option><option value=\"tiff\">tiff</option></select></label>",
	    "<label class=\"control\" title=\"Allow existing tile files to be replaced\"><span>Overwrite</span> <input id=\"spatialTileOverwrite\" type=\"checkbox\"></label>",
	    "<label class=\"control wide\" title=\"Output folder written by the live R session\">Output folder <input id=\"spatialTileOutputDir\" type=\"text\" value=\"spot_tiles\"></label>",
	    "</div>",
	    "<div class=\"spatialTileTools\">",
	    "<button id=\"spatialTilePreview\" type=\"button\" title=\"Show tile boxes centered on spatial spots\">Preview</button>",
	    "<button id=\"spatialTileClear\" type=\"button\" title=\"Remove the spot tile preview layer\">Clear preview</button>",
	    "<button id=\"spatialTileSave\" type=\"button\" title=\"Save the previewed spot-centered tiles through the live R session\">Save tiles</button>",
	    "</div>",
	    "<div id=\"spatialTileSummary\" class=\"menuHint\">Choose a tile size, preview boxes, then save from a live viewer session.</div>",
	    "</div>\n",
	    "<div id=\"shortcutHelpBackdrop\" aria-hidden=\"true\"></div>\n",
    "<div id=\"shortcutHelp\" class=\"panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"shortcutHelpTitle\" aria-hidden=\"true\">",
    "<div class=\"shortcutHelpHead\"><div><div id=\"shortcutHelpTitle\" class=\"shortcutHelpTitle\">Viewer Help</div>",
    "<div class=\"shortcutHelpHint\">Keyboard shortcuts, full guide, and quick recommendations. Press ? to open or close this guide.</div></div><span class=\"commandKbd\">?</span></div>",
    "<div class=\"shortcutHelpTabs\" role=\"tablist\" aria-label=\"Help sections\">",
    "<button id=\"shortcutHelpTabKeyboard\" class=\"shortcutHelpTab active\" type=\"button\" role=\"tab\" aria-controls=\"helpKeyboardShortcuts\" aria-selected=\"true\">Keyboard Shortcuts</button>",
    "<button id=\"shortcutHelpTabFull\" class=\"shortcutHelpTab\" type=\"button\" role=\"tab\" aria-controls=\"helpFullGuide\" aria-selected=\"false\">Full Guide</button>",
    "</div>",
    "<section id=\"helpKeyboardShortcuts\" class=\"helpPart\"><h2 class=\"helpPartTitle\">Keyboard Shortcuts</h2>",
    "<dl class=\"shortcutList\">",
    "<dt><span class=\"commandKbd\">Space</span> / <span class=\"commandKbd\">P</span></dt><dd>Pan mode</dd>",
    "<dt><span class=\"commandKbd\">Arrow keys</span></dt><dd>Pan the image; hold Shift for a larger step</dd>",
    "<dt><span class=\"commandKbd\">D</span></dt><dd>Draw polygon ROI</dd>",
    "<dt><span class=\"commandKbd\">B</span></dt><dd>Brush annotation editing; hold Alt on Windows/Linux or Command on Mac to subtract</dd>",
    "<dt><span class=\"commandKbd\">N</span></dt><dd>Deselect the current annotation and start a new ROI</dd>",
    "<dt><span class=\"commandKbd\">E</span></dt><dd>Edit ROI vertices or redraw smooth boundary curves</dd>",
    "<dt><span class=\"commandKbd\">M</span></dt><dd>Measure distance between two points</dd>",
    "<dt><span class=\"commandKbd\">T</span></dt><dd>Draw trajectory control points; Enter or double-click finishes and returns to pan</dd>",
    "<dt><span class=\"commandKbd\">Delete</span> / <span class=\"commandKbd\">Backspace</span></dt><dd>Delete the selected annotation, trajectory, marker, layer object, or measurement. ROI and trajectory edits can be undone with Ctrl+Z.</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+Z</span></dt><dd>Undo annotation, trajectory, or closed-image edit</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+Shift+Z</span> / <span class=\"commandKbd\">Ctrl+Y</span></dt><dd>Redo annotation, trajectory, or closed-image edit</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+S</span></dt><dd>Open annotation export options</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+I</span></dt><dd>Import GeoJSON annotations</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+E</span></dt><dd>Export selected ROIs</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+K</span></dt><dd>Open command palette</dd>",
    "<dt><span class=\"commandKbd\">Esc</span></dt><dd>Close help, command palette, or return to pan mode</dd>",
    "</dl>",
    "</section>",
    "<section id=\"helpFullGuide\" class=\"helpPart\"><h2 class=\"helpPartTitle\">Full Guide</h2>",
    "<div class=\"viewerGuideGrid\">",
    "<section class=\"viewerGuideSection\"><h3>Open images</h3><ul class=\"viewerGuideList\">",
    project_help_item,
    "<li>Open WSI, CZI, SVS, OME-TIFF, and tiled sources from R when possible, so wsiTools can prepare tiles without loading the full image into memory.</li>",
    "<li>The Project panel lists the current images and tissue sections. Select a row to switch the visible sample.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Zoom and pan</h3><ul class=\"viewerGuideList\">",
    "<li>Use the four-arrow Pan control, mouse drag, trackpad, or scroll wheel to move through the slide.</li>",
    "<li>The right-side controls provide zoom in, zoom out, fit, and 1:1 view.</li>",
    "<li>Use the camera button on the right side to drag-select an area, then choose PNG, JPEG, SVG, or PDF and whether to include tissue, spots/layers, annotations, measurements, trajectories, tile grid, and artifacts.</li>",
    "<li>Use View > Save prefs to remember brush size, next annotation category, preferred tool, display toggles, screenshot format, and other common annotation settings in this browser.</li>",
    "<li>Use View for approximate 5x, 10x, 20x, or 40x magnification. The viewer uses slide MPP/objective metadata when available and otherwise assumes 40x at full resolution.</li>",
    "<li>Use View > Multi-view tissue display to split tiled slides into 2, 4, or 6 side-by-side panes. Link views for synchronized zoom/pan or leave them independent to compare separate regions.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Side panels</h3><ul class=\"viewerGuideList\">",
    "<li>The left Project, Annotations, and History panels can be resized. Double-click a panel header to minimize it.</li>",
    "<li>Use the X button to close a panel and the Project or Annotations menu to reopen it.</li>",
    "<li>The Project panel is for images and sections; Annotations is for ROI objects and GeoJSON content.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>ROIs and annotations</h3><ul class=\"viewerGuideList\">",
    "<li>Choose the active class before drawing. New brush annotations use that class and get an automatic name.</li>",
    "<li>Same-class brush regions merge when they touch. Different classes remain separate and are clipped to avoid overlap.</li>",
    "<li>Double-click an ROI to select it. Use Command on Mac or Alt on Windows/Linux while brushing to subtract from the selected ROI.</li>",
    "<li>Import or export QuPath-style GeoJSON from the Annotations menu.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Measurements and trajectories</h3><ul class=\"viewerGuideList\">",
    "<li>Use Measure for distances. Values are shown in pixels and microns when slide metadata contains pixel size.</li>",
    "<li>Use Trajectories to click control points, then finish with Enter or double-click.</li>",
    "<li>Only one annotation, trajectory, marker, layer object, or measurement is selected at a time.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Stains, channels, and overlays</h3><ul class=\"viewerGuideList\">",
    "<li>The Stains menu shows only channels available for the current image.</li>",
    "<li>H&E deconvolution, IHC channels, and mIHC overlays are display layers and may be slower than the base tiled image.</li>",
    "<li>mIHC overlays are tied to their matching H&E image and are hidden when another tissue section is selected.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Projects and saved outputs</h3><ul class=\"viewerGuideList\">",
    "<li>Use Project to save or reopen viewer state, including images, annotations, trajectories, measurements, layers, and viewport position when supported.</li>",
    "<li>Screenshot saving uses the desktop or browser Save As dialog when supported, so you can choose a local folder. Browsers without that file-system API show a warning instead of silently choosing a folder.</li>",
    "<li>Live viewers can sync selected viewer objects back to R; static HTML viewers do not automatically update R objects.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Analysis tools</h3><ul class=\"viewerGuideList\">",
    "<li>Artifacts imports GrandQC GeoJSON QC regions as editable annotations when a CellPhenotyper project provides them.</li>",
    "<li>Cell segmentation is expected to come from CellPhenotyper outputs and appears through the Cells menu when present.</li>",
    "<li>Tile grids can be previewed before extraction to confirm which regions will be exported.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection viewerGuideWide\"><h3>Troubleshooting</h3><div class=\"viewerGuideTroubleshooting\">",
    "<div class=\"viewerGuideIssue\"><strong>Images or tiles do not load</strong>Check that the file path exists, the backend is available with wsi_backends(), and the viewer is opened through http://127.0.0.1 or localhost when canvas pixel access is needed.</div>",
    "<div class=\"viewerGuideIssue\"><strong>Black or missing tiles</strong>Try refreshing the viewer, use prebuilt Deep Zoom tiles when available, and confirm OpenSlide/libvips can read the image. Dynamic tiles are a fallback and may be slower.</div>",
    "<div class=\"viewerGuideIssue\"><strong>Slow rendering</strong>Prefer static prebuilt tiles for SVS/TIFF/OME-TIFF, reduce visible overlays, and avoid enabling several deconvolved or mIHC channels at once on very large slides.</div>",
    "<div class=\"viewerGuideIssue\"><strong>Saved outputs are hard to find</strong>Browser-triggered exports usually appear in Downloads. R-side exports are written to the output path passed to the R function or project save call.</div>",
    "</div></section>",
    "</div></section>",
    "<section id=\"helpQuickRecommendations\" class=\"helpPart\"><h2 class=\"helpPartTitle\">Quick Recommendations</h2>",
    "<ul class=\"viewerGuideList quickRecommendationList\">",
    "<li>Open large WSI, CZI, SVS, OME-TIFF, and tiled images from R so wsiTools can use tiled backends instead of loading full images into memory.</li>",
    "<li>Use the Project panel to switch images or tissue sections, and save a project before opening a different case.</li>",
    "<li>Choose the active annotation class before drawing or brushing; same-label brush strokes merge, different labels stay separate.</li>",
    "<li>Use prebuilt Deep Zoom tiles when possible for the smoothest full-resolution zooming.</li>",
    "<li>Watch the sync status pill for running tasks, failed jobs, or live R synchronization status.</li>",
    "<li>Export important ROIs as GeoJSON and save the viewer project after annotation or analysis steps.</li>",
    "</ul></section>",
    "<div class=\"shortcutHelpActions\"><button id=\"shortcutHelpClose\" title=\"Close viewer guide\">Close</button></div>",
    "</div>\n",
    "<div id=\"annotationSectionBackdrop\" aria-hidden=\"true\"></div>\n",
    "<div id=\"toastStack\" aria-live=\"polite\" aria-atomic=\"false\"></div>\n",
    "<div id=\"scaleBar\" class=\"unavailable\" aria-label=\"Micron scale bar\"><div id=\"scaleBarLine\"></div><div id=\"scaleBarLabel\">scale unavailable</div></div>\n",
    "<div id=\"status\">", wsi_html_escape(loading_message), "</div>\n"
  )
}

wsi_viewer_toast_js <- function() {
  paste0(
    "let annotationsDirty=false,annotationDirtyReason='',projectDirty=false,projectDirtyReason='';\n",
    "let annotationHistory=[],annotationHistorySeq=0;\n",
    "let viewerLog=[],viewerLogSeq=0;\n",
    "function annotationHistoryDetailText(detail={}){if(!detail||typeof detail!=='object')return '';const parts=[];['message','name','old_name','class','operation','file','id','source','source_id','count','added','type','job_id'].forEach(k=>{if(detail[k]!==null&&typeof detail[k]!=='undefined'&&String(detail[k]).length)parts.push(k.replace('_',' ')+': '+String(detail[k]));});if(Array.isArray(detail.selected_indices)&&detail.selected_indices.length)parts.push('selected: '+detail.selected_indices.length);return parts.join(' | ');}\n",
    "function annotationHistoryActionLabel(action,detail={}){const a=String(action||'annotation_changed');if(a==='roi_added')return 'Created ROI';if(a==='geojson_imported')return 'Imported GeoJSON';if(a==='roi_renamed')return 'Updated '+(detail.name||detail.id||'ROI');if(a==='roi_metadata_updated'||a==='roi_updated')return 'Updated '+(detail.name||detail.id||'ROI');if(a==='roi_brush_extend')return 'Brush add';if(a==='roi_brush_subtract')return 'Brush subtract';if(a==='roi_deleted')return 'Deleted '+(detail.name||detail.id||'ROI');if(a==='roi_duplicated')return 'Duplicated ROI';if(a==='roi_color_updated')return 'Changed ROI color';if(a==='roi_visibility_updated')return detail.visible?'Showed ROI':'Hid ROI';if(a==='roi_lock_updated')return detail.locked?'Locked ROI':'Unlocked ROI';if(a==='roi_smoothed')return 'Smoothed ROI';if(a==='roi_simplified')return 'Simplified ROI';if(a==='roi_holes_filled')return 'Filled ROI holes';if(a==='rois_merged')return 'Merged ROIs';if(a==='roi_split')return 'Split ROI';if(a==='annotation_undo')return 'Undo annotation edit';if(a==='annotation_redo')return 'Redo annotation edit';if(a==='segmentation_imported')return 'Imported cell segmentation';if(a==='measurement_added')return 'Measured distance';if(a==='trajectory_added')return 'Created trajectory';if(a==='trajectory_deleted')return 'Deleted '+(detail.name||detail.id||'trajectory');if(a==='trajectory_area_created')return 'Created trajectory area';if(a==='trajectory_area_updated')return 'Updated trajectory area';if(a==='trajectories_cleared')return 'Cleared trajectories';if(a==='artifact_detected')return 'Detected artifacts';if(a==='artifact_flagged')return 'Flagged artifact';if(a==='artifacts_cleared')return 'Cleared artifacts';if(a==='grandqc_loaded')return 'Loaded GrandQC';if(a==='grandqc_cleared')return 'Cleared GrandQC';if(a==='viewer_warning')return 'Viewer warning';if(a==='viewer_error')return 'Viewer error';if(a==='annotation_history_cleared')return 'Cleared history';return a.replace(/_/g,' ').replace(/^./,c=>c.toUpperCase());}\n",
    "function renderAnnotationHistory(){const summary=el('annotationHistorySummary'),list=el('annotationHistoryList');if(!summary||!list)return;list.innerHTML='';if(!annotationHistory.length){summary.textContent='No viewer actions yet.';return;}summary.textContent=annotationHistory.length+' viewer action'+(annotationHistory.length===1?'':'s')+' in this session.';annotationHistory.slice(0,30).forEach(entry=>{const item=document.createElement('div');item.className='historyItem';const action=document.createElement('div');action.className='historyAction';action.textContent=entry.label||annotationHistoryActionLabel(entry.action,entry.detail);const meta=document.createElement('div');meta.className='historyMeta';const when=entry.time?new Date(entry.time):null,time=when&&!Number.isNaN(when.getTime())?when.toLocaleTimeString():'';const details=annotationHistoryDetailText(entry.detail||{});meta.textContent=[time,details].filter(Boolean).join(' | ');item.append(action,meta);list.appendChild(item);});}\n",
    "function annotationHistoryPayload(){return annotationHistory.slice().reverse().map(entry=>({id:entry.id,time:entry.time,action:entry.action,label:entry.label,detail:entry.detail||{}}));}\n",
    "function recordAnnotationHistory(action,detail={},sync=false){const entry={id:'history_'+(++annotationHistorySeq),time:new Date().toISOString(),action:String(action||'annotation_changed'),label:annotationHistoryActionLabel(action,detail||{}),detail:detail||{}};annotationHistory.unshift(entry);if(annotationHistory.length>120)annotationHistory.length=120;renderAnnotationHistory();if(sync&&typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('annotation_history_updated',{action:entry.action,label:entry.label});return entry;}\n",
    "function historyTextValue(value){if(value===null||typeof value==='undefined')return '';if(typeof value==='string')return value;try{return JSON.stringify(value);}catch(e){return String(value);}}\n",
    "function historyDetailLine(detail={}){if(!detail||typeof detail!=='object')return '';const keys=Object.keys(detail);if(!keys.length)return '';return keys.slice(0,24).map(k=>k+': '+historyTextValue(detail[k])).join(' | ');}\n",
    "function historyDiagnosticText(){const lines=['wsiTools viewer history and R sync report','title: '+String(cfg.title||''),'url: '+String(location.href||''),'generated: '+new Date().toISOString(),'live sync: '+String((el('syncSummary')&&el('syncSummary').textContent)||''),''];if(typeof projectStatePayload==='function'){try{const p=projectStatePayload();lines.push('Project');lines.push('active key: '+String(p.active_key||''));lines.push('active image: '+String((p.active&&p.active.label)||''));lines.push('active section: '+String((p.section&&p.section.label)||''));lines.push('image count: '+String(p.count||0));lines.push('');}catch(e){lines.push('Project: unavailable ('+e.message+')','');}}lines.push('Viewer history: '+annotationHistory.length);annotationHistoryPayload().forEach(entry=>{lines.push('['+entry.time+'] '+String(entry.label||entry.action||''));const d=historyDetailLine(entry.detail||{});if(d)lines.push('  '+d);});lines.push('');const syncRows=(typeof viewerSyncHistory!=='undefined'&&Array.isArray(viewerSyncHistory))?viewerSyncHistory.slice():[];lines.push('R/live sync commands and events: '+syncRows.length);syncRows.forEach(entry=>{lines.push('['+entry.time+'] '+String(entry.direction||'sync').toUpperCase()+' '+String(entry.event||entry.type||''));const d=historyDetailLine(entry.detail||{});if(d)lines.push('  '+d);});lines.push('');lines.push('Troubleshooting logs: '+viewerLog.length);viewerLogPayload().forEach(entry=>{lines.push('['+entry.time+'] '+String(entry.level||'info').toUpperCase()+' '+String(entry.message||''));const d=historyDetailLine(entry.detail||{});if(d)lines.push('  '+d);});lines.push('');lines.push('Current state summary');lines.push('rois: '+String((rois||[]).length));lines.push('trajectories: '+String((typeof trajectories!=='undefined'&&Array.isArray(trajectories))?trajectories.length:0));lines.push('layers: '+String((layers||[]).length));lines.push('selected roi index: '+String(selectedRoi));lines.push('mode: '+String(mode));lines.push('');return lines.join('\\n')+'\\n';}\n",
    "function copyAnnotationHistoryAll(){const text=historyDiagnosticText(),fallback=()=>new Promise((resolve,reject)=>{try{const area=document.createElement('textarea');area.value=text;area.setAttribute('readonly','');area.style.position='fixed';area.style.left='-9999px';document.body.appendChild(area);area.select();const ok=document.execCommand('copy');area.remove();ok?resolve():reject(new Error('copy failed'));}catch(e){reject(e);}}),p=(navigator.clipboard&&navigator.clipboard.writeText)?navigator.clipboard.writeText(text):fallback();p.then(()=>{recordAnnotationHistory('history_copied',{history_count:annotationHistory.length,log_count:viewerLog.length,sync_count:(typeof viewerSyncHistory!=='undefined'&&Array.isArray(viewerSyncHistory))?viewerSyncHistory.length:0},false);if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('annotation_history_updated',{action:'history_copied',copied:true});notify('History and R sync report copied','success',2600);}).catch(e=>notify('Could not copy history report: '+e.message,'warning',4200));}\n",
    "function clearAnnotationHistory(){annotationHistory=[];renderAnnotationHistory();if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('annotation_history_cleared',{});notify('History cleared','success');}\n",
    "function viewerLogLevel(type){const x=String(type||'info').toLowerCase();return ['error','warning','success','info'].includes(x)?x:'info';}\n",
    "function viewerLogString(value){if(value instanceof Error)return value.message||String(value);if(typeof value==='string')return value;try{return JSON.stringify(value);}catch(e){return String(value);}}\n",
    "function viewerLogDetailText(detail={}){if(!detail||typeof detail!=='object')return '';const parts=[];Object.keys(detail).slice(0,12).forEach(k=>{const v=detail[k];if(v!==null&&typeof v!=='undefined'&&String(v).length)parts.push(k+': '+viewerLogString(v));});return parts.join(' | ');}\n",
    "function benignViewerConsoleWarning(message){const text=String(message||'');return text.indexOf('Ignoring tile')>=0&&text.indexOf('loaded before reset')>=0;}\n",
    "function renderViewerLog(){const summary=el('viewerLogSummary'),list=el('viewerLogList'),download=el('downloadViewerLog'),copy=el('copyViewerLog'),clear=el('clearViewerLog');if(!summary||!list)return;list.innerHTML='';const counts={error:0,warning:0,success:0,info:0};viewerLog.forEach(entry=>{counts[entry.level]=(counts[entry.level]||0)+1;});if(!viewerLog.length){summary.textContent='No messages yet.';}else{summary.textContent=viewerLog.length+' log message'+(viewerLog.length===1?'':'s')+' in this viewer session'+(counts.error?(' | errors '+counts.error):'')+(counts.warning?(' | warnings '+counts.warning):'')+'.';}viewerLog.slice(0,80).forEach(entry=>{const item=document.createElement('div');item.className='viewerLogItem '+entry.level;const head=document.createElement('div');head.className='viewerLogHead';const level=document.createElement('span');level.className='viewerLogLevel';level.textContent=entry.level;const when=document.createElement('span');const t=entry.time?new Date(entry.time):null;when.textContent=t&&!Number.isNaN(t.getTime())?t.toLocaleTimeString():'';head.append(level,when);const msg=document.createElement('div');msg.className='viewerLogMessage';msg.textContent=entry.message||'';item.append(head,msg);const details=viewerLogDetailText(entry.detail||{});if(details){const d=document.createElement('div');d.className='viewerLogDetail';d.textContent=details;item.appendChild(d);}list.appendChild(item);});[download,copy,clear].forEach(button=>{if(button)button.disabled=!viewerLog.length;});}\n",
    "function viewerLogPayload(){return viewerLog.slice().reverse().map(entry=>({id:entry.id,time:entry.time,level:entry.level,message:entry.message,source:entry.source||'viewer',detail:entry.detail||{}}));}\n",
    "function recordViewerLog(message,type='info',detail={},source='viewer'){const msg=String(message||'').trim();if(!msg)return null;const level=viewerLogLevel(type),src=String(source||'viewer');const entry={id:'log_'+(++viewerLogSeq),time:new Date().toISOString(),level:level,message:msg,source:src,detail:detail||{}};viewerLog.unshift(entry);if(viewerLog.length>500)viewerLog.length=500;renderViewerLog();if(level==='warning'||level==='error'){if(typeof recordAnnotationHistory==='function')recordAnnotationHistory('viewer_'+level,Object.assign({message:msg,source:src},detail||{}),false);if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('viewer_log_updated',{level:level,message:msg});}return entry;}\n",
    "function viewerLogFileName(){const base=String(cfg.title||'wsiTools_viewer').replace(/\\.[^.]+$/,'').replace(/[^A-Za-z0-9_-]+/g,'_').replace(/^_+|_+$/g,'')||'wsiTools_viewer';const stamp=new Date().toISOString().replace(/[:.]/g,'-');return base+'_troubleshooting_log_'+stamp+'.txt';}\n",
    "function viewerLogText(){const lines=['wsiTools viewer troubleshooting log','title: '+String(cfg.title||''),'url: '+String(location.href||''),'saved: '+new Date().toISOString(),'messages: '+viewerLog.length,''];viewerLogPayload().forEach(entry=>{lines.push('['+entry.time+'] '+String(entry.level||'info').toUpperCase()+' '+String(entry.message||''));const details=viewerLogDetailText(entry.detail||{});if(details)lines.push('  '+details);});return lines.join('\\n')+'\\n';}\n",
    "function downloadViewerLogFile(){if(!viewerLog.length){notify('No troubleshooting logs to save','info',1800);return;}const blob=new Blob([viewerLogText()],{type:'text/plain;charset=utf-8'}),a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=viewerLogFileName();document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('viewer_log_exported',{count:viewerLog.length});notify('Troubleshooting log saved','success',2200);}\n",
    "function copyViewerLogText(){if(!viewerLog.length){notify('No troubleshooting logs to copy','info',1800);return;}const text=viewerLogText();const fallback=()=>new Promise((resolve,reject)=>{try{const area=document.createElement('textarea');area.value=text;area.setAttribute('readonly','');area.style.position='fixed';area.style.left='-9999px';document.body.appendChild(area);area.select();const ok=document.execCommand('copy');area.remove();ok?resolve():reject(new Error('copy failed'));}catch(e){reject(e);}});const p=(navigator.clipboard&&navigator.clipboard.writeText)?navigator.clipboard.writeText(text):fallback();p.then(()=>notify('Troubleshooting log copied','success',2200)).catch(e=>notify('Could not copy troubleshooting log: '+e.message,'warning',3600));}\n",
    "function clearViewerLog(){viewerLog=[];renderViewerLog();if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('viewer_log_cleared',{});notify('Troubleshooting log cleared','success',1800);}\n",
    "function viewerLogPanelIsClosed(){const panel=el('viewerLogPanel');return !!(panel&&(panel.classList.contains('closed')||panel.style.display==='none'));}\n",
    "function updateViewerLogPanelToggle(){const button=el('viewerLogPanelToggle');if(button)button.classList.toggle('active',!viewerLogPanelIsClosed());}\n",
    "function ensureViewerLogWorkspaceVisible(){const workspace=el('workspacePanel'),panel=el('viewerLogPanel');if(workspace){workspace.style.visibility='visible';workspace.style.opacity='1';workspace.style.pointerEvents='auto';workspace.removeAttribute('aria-hidden');const rect=workspace.getBoundingClientRect(),safeTop=(typeof workspacePanelSafeTop==='function')?workspacePanelSafeTop():72;if(rect.width<8||rect.height<8||rect.right<24||rect.bottom<72||rect.left>innerWidth-24||rect.top<safeTop-1||rect.top>innerHeight-24){workspace.style.left='12px';workspace.style.top=safeTop+'px';workspace.style.right='auto';}}if(panel){panel.style.display='';panel.classList.remove('closed','minimized');const header=el('viewerLogPanelHeader'),state=el('viewerLogPanelMinimizeState');if(header)header.setAttribute('aria-expanded','true');if(state)state.textContent='double-click to minimize';}}\n",
    "function setViewerLogPanelClosed(closed){const panel=el('viewerLogPanel'),header=el('viewerLogPanelHeader');if(!panel)return;closed=!!closed;panel.classList.toggle('closed',closed);panel.style.display=closed?'none':'';if(closed)panel.classList.remove('minimized');if(header)header.setAttribute('aria-expanded',closed?'false':'true');if(!closed)ensureViewerLogWorkspaceVisible();updateViewerLogPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function setViewerLogPanelMinimized(minimized){const panel=el('viewerLogPanel'),header=el('viewerLogPanelHeader'),state=el('viewerLogPanelMinimizeState');if(!panel)return;if(minimized&&viewerLogPanelIsClosed())setViewerLogPanelClosed(false);panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(state)state.textContent=minimized?'double-click to expand':'double-click to minimize';if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleViewerLogPanelMinimized(){const panel=el('viewerLogPanel');if(!panel)return;if(viewerLogPanelIsClosed())setViewerLogPanelClosed(false);setViewerLogPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function openViewerLogPanel(){setViewerLogPanelClosed(false);renderViewerLog();updateViewerLogPanelToggle();notify('Logs panel opened','success',1400);}\n",
    "function closeViewerLogPanel(e){if(e){e.preventDefault();e.stopPropagation();}setViewerLogPanelClosed(true);notify('Logs panel closed','info',1600);return false;}\n",
    "function toggleViewerLogPanelClosed(){if(viewerLogPanelIsClosed())openViewerLogPanel();else closeViewerLogPanel();}\n",
    "function bindViewerLogControls(){const header=el('viewerLogPanelHeader'),close=el('viewerLogPanelClose'),toggle=el('viewerLogPanelToggle'),download=el('downloadViewerLog'),copy=el('copyViewerLog'),clear=el('clearViewerLog');if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.ondblclick=e=>{e.preventDefault();toggleViewerLogPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleViewerLogPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';['pointerdown','mousedown','dblclick'].forEach(name=>close.addEventListener(name,e=>{e.stopPropagation();}));close.onclick=e=>closeViewerLogPanel(e);}if(toggle&&toggle.dataset.bound!=='1'){toggle.dataset.bound='1';toggle.onclick=()=>toggleViewerLogPanelClosed();}if(download)download.onclick=downloadViewerLogFile;if(copy)copy.onclick=copyViewerLogText;if(clear)clear.onclick=clearViewerLog;updateViewerLogPanelToggle();renderViewerLog();}\n",
    "function bindViewerLogCapture(){if(window.__wsiToolsLogCaptureBound)return;window.__wsiToolsLogCaptureBound=true;window.addEventListener('error',e=>recordViewerLog(e.message||'Browser error','error',{file:e.filename||'',line:e.lineno||'',column:e.colno||''},'browser'));window.addEventListener('unhandledrejection',e=>recordViewerLog((e.reason&&e.reason.message)||viewerLogString(e.reason)||'Unhandled promise rejection','error',{type:'unhandledrejection'},'browser'));if(window.console){const warn=console.warn?console.warn.bind(console):null,error=console.error?console.error.bind(console):null;if(warn)console.warn=function(...args){warn(...args);const msg=args.map(viewerLogString).join(' ');if(benignViewerConsoleWarning(msg))return;recordViewerLog(msg,'warning',{source:'console'},'console');};if(error)console.error=function(...args){error(...args);recordViewerLog(args.map(viewerLogString).join(' '),'error',{source:'console'},'console');};}}\n",
    "function historyPanelIsClosed(){const panel=el('annotationHistory');return !!(panel&&(panel.classList.contains('closed')||panel.style.display==='none'));}\n",
    "function updateHistoryPanelToggle(){const button=el('historyPanelToggle');if(button)button.classList.toggle('active',!historyPanelIsClosed());}\n",
    "function setHistoryPanelMinimized(minimized){const panel=el('annotationHistory'),header=el('historyPanelHeader'),state=el('historyPanelMinimizeState');if(!panel)return;if(minimized&&historyPanelIsClosed())setHistoryPanelClosed(false);panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(state)state.textContent=minimized?'double-click to expand':'double-click to minimize';if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleHistoryPanelMinimized(){const panel=el('annotationHistory');if(!panel)return;if(historyPanelIsClosed())setHistoryPanelClosed(false);setHistoryPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function ensureHistoryWorkspaceVisible(){const workspace=el('workspacePanel'),panel=el('annotationHistory');if(workspace){workspace.style.visibility='visible';workspace.style.opacity='1';workspace.style.pointerEvents='auto';workspace.removeAttribute('aria-hidden');const rect=workspace.getBoundingClientRect(),safeTop=(typeof workspacePanelSafeTop==='function')?workspacePanelSafeTop():72;if(rect.width<8||rect.height<8||rect.right<24||rect.bottom<72||rect.left>innerWidth-24||rect.top<safeTop-1||rect.top>innerHeight-24){workspace.style.left='12px';workspace.style.top=safeTop+'px';workspace.style.right='auto';}}if(panel){panel.style.display='';panel.classList.remove('closed','minimized');const header=el('historyPanelHeader'),state=el('historyPanelMinimizeState');if(header)header.setAttribute('aria-expanded','true');if(state)state.textContent='double-click to minimize';}}\n",
    "function setHistoryPanelClosed(closed){const panel=el('annotationHistory'),header=el('historyPanelHeader');if(!panel)return;closed=!!closed;if(closed&&maximizedAnnotationSection==='annotationHistory'&&typeof closeMaximizedAnnotationSection==='function')closeMaximizedAnnotationSection();panel.classList.toggle('closed',closed);panel.style.display=closed?'none':'';if(closed)panel.classList.remove('minimized');if(header)header.setAttribute('aria-expanded',closed?'false':'true');if(!closed)ensureHistoryWorkspaceVisible();updateHistoryPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function openHistoryPanel(){setHistoryPanelClosed(false);renderAnnotationHistory();updateHistoryPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();notify('History panel opened','success',1400);}\n",
    "function closeHistoryPanel(e){if(e){e.preventDefault();e.stopPropagation();}setHistoryPanelClosed(true);notify('History panel closed','info',1600);return false;}\n",
    "function toggleHistoryPanelClosed(){if(historyPanelIsClosed())openHistoryPanel();else closeHistoryPanel();}\n",
    "function bindAnnotationHistoryControls(){const clear=el('clearAnnotationHistory'),copyAll=el('copyAnnotationHistoryAll'),header=el('historyPanelHeader'),close=el('historyPanelClose'),toggle=el('historyPanelToggle');if(clear)clear.onclick=clearAnnotationHistory;if(copyAll)copyAll.onclick=copyAnnotationHistoryAll;if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.ondblclick=e=>{e.preventDefault();toggleHistoryPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleHistoryPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';['pointerdown','mousedown','dblclick'].forEach(name=>close.addEventListener(name,e=>{e.stopPropagation();}));close.onclick=e=>closeHistoryPanel(e);}if(toggle&&toggle.dataset.bound!=='1'){toggle.dataset.bound='1';toggle.onclick=()=>toggleHistoryPanelClosed();}updateHistoryPanelToggle();renderAnnotationHistory();}\n",
    "let maximizedAnnotationSection=null;\n",
    "function annotationSectionButton(sectionId){return sectionId==='annotationHistory'?el('maximizeAnnotationHistory'):null;}\n",
    "function annotationSectionTitle(sectionId){return sectionId==='annotationHistory'?'History':'';}\n",
    "function setAnnotationSectionMaximized(sectionId,maximize=true){const ids=['annotationHistory'];if(!ids.includes(sectionId))return;const active=maximize?sectionId:null;if(active==='annotationHistory'){if(typeof setHistoryPanelClosed==='function')setHistoryPanelClosed(false);if(typeof setHistoryPanelMinimized==='function')setHistoryPanelMinimized(false);}ids.forEach(id=>{const section=el(id),button=annotationSectionButton(id),on=active===id;if(section)section.classList.toggle('maximized',on);if(button){button.textContent=on?'Restore':'Maximize';button.setAttribute('aria-expanded',on?'true':'false');}});const backdrop=el('annotationSectionBackdrop');if(backdrop)backdrop.classList.toggle('open',!!active);maximizedAnnotationSection=active;if(active){const section=el(active);if(section){section.scrollTop=0;setTimeout(()=>section.focus&&section.focus(),0);}notify(annotationSectionTitle(active)+' maximized','info',1800);}}\n",
    "function closeMaximizedAnnotationSection(){if(maximizedAnnotationSection)setAnnotationSectionMaximized(maximizedAnnotationSection,false);}\n",
    "function toggleAnnotationSectionMaximized(sectionId){setAnnotationSectionMaximized(sectionId,maximizedAnnotationSection!==sectionId);}\n",
    "function bindAnnotationSectionMaximizeControls(){const history=el('maximizeAnnotationHistory'),backdrop=el('annotationSectionBackdrop');if(history)history.onclick=()=>toggleAnnotationSectionMaximized('annotationHistory');if(backdrop)backdrop.onclick=closeMaximizedAnnotationSection;window.addEventListener('keydown',e=>{if(e.key==='Escape'&&maximizedAnnotationSection){e.preventDefault();e.stopPropagation();closeMaximizedAnnotationSection();}},true);}\n",
    "function dismissToast(toast){if(!toast)return;toast.classList.remove('visible');toast.classList.add('leaving');setTimeout(()=>toast.remove(),220);}\n",
    "function toastDisplayMessage(message,type='info'){const msg=String(message||''),level=viewerLogLevel(type),limit=(level==='warning'||level==='error')?190:260;if(msg.length<=limit)return msg;return msg.slice(0,limit).replace(/\\s+$/,'')+'... Full message saved in History and Logs.';}\n",
    "function showToast(message,type='info',timeout=2600,action=null){if(message)recordViewerLog(message,type,{timeout:timeout,action:action&&action.label?action.label:null},'toast');const stack=el('toastStack');if(!stack||!message)return null;const safeType=String(type||'info').replace(/[^A-Za-z0-9_-]+/g,'').toLowerCase()||'info',displayMessage=toastDisplayMessage(message,safeType);if((safeType==='warning'||safeType==='error')&&!action&&String(message||'').length>190&&typeof openViewerLogPanel==='function')action={label:'Logs',run:openViewerLogPanel};const toast=document.createElement('div');toast.className='toast toast-'+safeType;toast.title=String(message);toast.setAttribute('role',safeType==='error'?'alert':'status');const text=document.createElement('span');text.className='toastMessage';text.textContent=displayMessage;toast.appendChild(text);if(action&&action.label&&typeof action.run==='function'){toast.classList.add('toast-actionable');const button=document.createElement('button');button.type='button';button.className='toastAction';button.textContent=String(action.label);button.onclick=e=>{e.stopPropagation();dismissToast(toast);action.run();};toast.appendChild(button);}toast.onclick=e=>{if(e.target===toast||e.target===text)dismissToast(toast);};stack.appendChild(toast);requestAnimationFrame(()=>toast.classList.add('visible'));const ttl=Number(timeout);if(!Number.isFinite(ttl)||ttl>0)setTimeout(()=>dismissToast(toast),Number.isFinite(ttl)?ttl:2600);return toast;}\n",
    "function notify(message,type='info',timeout=2600){return showToast(message,type,timeout);}\n",
    "function notifyAction(message,actionLabel,actionRun,type='info',timeout=6000){return showToast(message,type,timeout,{label:actionLabel,run:actionRun});}\n",
    "function countText(value){return Number(value).toLocaleString();}\n",
    "function updateAnnotationDirtyIndicator(){const node=el('annotationDirtyIndicator');if(!node)return;const dirty=!!(annotationsDirty||projectDirty);node.classList.toggle('dirty',dirty);node.setAttribute('aria-hidden',dirty?'false':'true');node.title=dirty?'Click to save the current project changes':'Project is saved';node.setAttribute('aria-label',dirty?'Save unsaved project changes':'Project is saved');}\n",
    "function setAnnotationsDirty(value=true,reason='annotation_changed',sync=false){annotationsDirty=!!value;annotationDirtyReason=reason||'';if(typeof invalidateAnnotationSpotAssociations==='function')invalidateAnnotationSpotAssociations();updateAnnotationDirtyIndicator();if(sync&&typeof scheduleViewerStateSync==='function')scheduleViewerStateSync(annotationsDirty?'annotations_dirty':'annotations_saved',{dirty:annotationsDirty,reason:annotationDirtyReason});}\n",
    "function markAnnotationsDirty(reason='annotation_changed'){setAnnotationsDirty(true,reason,false);}\n",
    "function markAnnotationsSaved(reason='annotations_saved'){setAnnotationsDirty(false,reason,true);}\n",
    "function hasUnsavedViewerChanges(){return !!(annotationsDirty||projectDirty);}\n",
    "function unsavedRefreshMessage(){return 'You have unsaved annotations or project changes. Save the project or export annotations before refreshing or closing this page.';}\n",
    "function bindUnsavedRefreshGuard(){if(window.__wsiToolsUnsavedRefreshGuardBound)return;window.__wsiToolsUnsavedRefreshGuardBound=true;window.addEventListener('beforeunload',event=>{if(!hasUnsavedViewerChanges())return;const message=unsavedRefreshMessage();try{recordViewerLog(message,'warning',{reason:annotationDirtyReason||projectDirtyReason||'unsaved_changes'},'browser');}catch(e){}event.preventDefault();event.returnValue=message;return message;});}\n"
  )
}

wsi_viewer_preferences_js <- function() {
  paste0(
    "const viewerPreferenceKey=cfg.preference_key||'wsiTools.viewer.preferences.v1';\n",
    "let viewerPreferencesReady=false,viewerPreferencesCache=null,roiPanelDragState=null,workspaceResizeState=null,sidePanelResizeState=null,selectionCardVisible=false,proximityLegendDragState=null;\n",
    "function loadViewerPreferences(){if(viewerPreferencesCache)return viewerPreferencesCache;try{const raw=window.localStorage&&localStorage.getItem(viewerPreferenceKey);viewerPreferencesCache=raw?JSON.parse(raw):{};}catch(e){viewerPreferencesCache={};}return viewerPreferencesCache||{};}\n",
    "function saveViewerPreferences(patch={}){if(!viewerPreferencesReady&&!patch.__initial)return;try{const current=Object.assign({},loadViewerPreferences());delete patch.__initial;const next=Object.assign(current,patch,{viewer_mode:cfg.viewer_mode||current.viewer_mode||'',updated_at:new Date().toISOString()});viewerPreferencesCache=next;if(window.localStorage)localStorage.setItem(viewerPreferenceKey,JSON.stringify(next));return next;}catch(e){return loadViewerPreferences();}}\n",
    "function validToolMode(value){const modeName=String(value||'');return ['pan','select','draw','brush','edit','measure','trajectory'].includes(modeName)?modeName:null;}\n",
    "function preferenceNumber(value,min,max){const n=Number(value);return Number.isFinite(n)?clamp(n,min,max):NaN;}\n",
    "function setPreferenceInput(id,value){const input=el(id);if(input&&value!==null&&typeof value!=='undefined'&&!Number.isNaN(value))input.value=String(value);}\n",
    "function setPreferenceChecked(id,value){const input=el(id);if(input)input.checked=!!value;}\n",
    "function safeInputValue(id){const input=el(id);return input?String(input.value||'').trim():'';}\n",
    "function leftWorkspacePanel(){return el('workspacePanel')||el('roiPanel');}\n",
    "function applyPanelPreferences(prefs){const panel=leftWorkspacePanel(),roi=el('roiPanel');if(!panel||!roi)return;const panelPrefs=prefs.panel||{};if(Number.isFinite(Number(panelPrefs.width)))setWorkspacePanelWidth(Number(panelPrefs.width),false);if(Number.isFinite(Number(panelPrefs.left))&&Number.isFinite(Number(panelPrefs.top)))setRoiPanelPosition(Number(panelPrefs.left),Number(panelPrefs.top),false);if(typeof panelPrefs.open==='boolean'&&typeof setRoiPanelOpen==='function')setRoiPanelOpen(panelPrefs.open,{save:false});if(typeof panelPrefs.minimized==='boolean')setRoiPanelMinimized(panelPrefs.minimized,false);if(typeof ensureProjectWorkspaceVisible==='function')ensureProjectWorkspaceVisible({preserve_state:true});if(typeof panelPrefs.project_closed==='boolean'&&typeof setProjectPanelClosed==='function')setProjectPanelClosed(panelPrefs.project_closed,false);if(typeof panelPrefs.project_minimized==='boolean'&&typeof setProjectPanelMinimized==='function'&&!panelPrefs.project_closed)setProjectPanelMinimized(panelPrefs.project_minimized,false);if(Number.isFinite(Number(panelPrefs.project_height)))setSidePanelHeight('projectPanel',Number(panelPrefs.project_height),false);if(Number.isFinite(Number(panelPrefs.roi_height)))setSidePanelHeight('roiPanel',Number(panelPrefs.roi_height),false);if(Number.isFinite(Number(panelPrefs.history_height)))setSidePanelHeight('annotationHistory',Number(panelPrefs.history_height),false);if(Number.isFinite(Number(panelPrefs.log_height)))setSidePanelHeight('viewerLogPanel',Number(panelPrefs.log_height),false);if(typeof panelPrefs.history_minimized==='boolean'&&typeof setHistoryPanelMinimized==='function')setHistoryPanelMinimized(panelPrefs.history_minimized);if(typeof panelPrefs.history_closed==='boolean'&&typeof setHistoryPanelClosed==='function')setHistoryPanelClosed(panelPrefs.history_closed);if(typeof panelPrefs.log_minimized==='boolean'&&typeof setViewerLogPanelMinimized==='function')setViewerLogPanelMinimized(panelPrefs.log_minimized);if(typeof panelPrefs.log_closed==='boolean'&&typeof setViewerLogPanelClosed==='function')setViewerLogPanelClosed(panelPrefs.log_closed);if(typeof applyProximityLegendPreferences==='function')applyProximityLegendPreferences(prefs);}\n",
    "function applyStainPreferences(prefs){if(!stainEnabled||!prefs.stain)return;stainOn=typeof prefs.stain.enabled==='boolean'?prefs.stain.enabled:stainOn;const saved=Array.isArray(prefs.stain.channels)?prefs.stain.channels:[];stainChannels.forEach((ch,i)=>{const pref=saved.find(s=>String(s.id||'')===String(ch.id))||saved[i]||{};if(!stainState[i])stainState[i]={visible:true,color:'#666666',strength:1,opacity:1,contrast_min:0,contrast_max:1};if(typeof pref.visible==='boolean')stainState[i].visible=pref.visible;if(pref.color)stainState[i].color=pref.color;const strength=Number(pref.strength??pref.gain),opacity=Number(pref.opacity),cmin=Number(pref.contrast_min),cmax=Number(pref.contrast_max);if(Number.isFinite(strength))stainState[i].strength=strength;if(Number.isFinite(opacity))stainState[i].opacity=opacity;if(Number.isFinite(cmin))stainState[i].contrast_min=cmin;if(Number.isFinite(cmax))stainState[i].contrast_max=cmax;const vis=el('stainVisible_'+ch.id),color=el('stainColor_'+ch.id),gain=el('stainStrength_'+ch.id),op=el('stainOpacity_'+ch.id),lo=el('stainContrastMin_'+ch.id),hi=el('stainContrastMax_'+ch.id);if(vis)vis.checked=!!stainState[i].visible;if(color)color.value=stainState[i].color;if(gain)gain.value=String(stainState[i].strength);if(op)op.value=String(stainState[i].opacity);if(lo)lo.value=String(stainState[i].contrast_min);if(hi)hi.value=String(stainState[i].contrast_max);});if(typeof updateStainControls==='function')updateStainControls();if(typeof syncTiledStainChannels==='function'&&syncTiledStainChannels())return;if(typeof invalidateBaseImage==='function')invalidateBaseImage();}\n",
    "function applyDisplayPreferences(prefs){if(typeof prefs.show_rois==='boolean')showRois=prefs.show_rois;if(typeof prefs.show_labels==='boolean')showLabels=prefs.show_labels;showCrosshair=false;const screenshot=String(prefs.screenshot_format||'').toLowerCase(),screenshotFmt=screenshot==='jpg'?'jpeg':screenshot;if(['png','jpeg','svg','pdf'].includes(screenshotFmt)){setPreferenceInput('screenshotFormat',screenshotFmt);setPreferenceInput('screenshotDialogFormat',screenshotFmt);}if(typeof prefs.image_export_dir==='string')setPreferenceInput('imageExportDir',prefs.image_export_dir);if(typeof multiViewSync!=='undefined')multiViewSync=false;setPreferenceChecked('multiViewSync',false);}\n",
    "function applyAnnotationPreferences(prefs){const brush=preferenceNumber(prefs.brush_size,8,240);if(Number.isFinite(brush)){brushScreenRadius=brush;setPreferenceInput('brushSize',brush);}const opacity=preferenceNumber(prefs.roi_opacity,0,1);if(Number.isFinite(opacity)){roiOpacity=opacity;setPreferenceInput('roiOpacity',opacity);}const width=preferenceNumber(prefs.trajectory_area_width,16,5000);if(Number.isFinite(width))setPreferenceInput('trajectoryAreaWidth',Math.round(width));if(typeof prefs.trajectory_area_preview==='boolean')setPreferenceChecked('trajectoryAreaPreview',prefs.trajectory_area_preview);const cls=String(prefs.selected_class||'').trim();if(cls){nextRoiClass=cls;activeRoiClass=cls;if(typeof ensureRoiClassOption==='function')ensureRoiClassOption(cls);setPreferenceInput('panelRoiClassSelect',cls);}if(prefs.custom_class)setPreferenceInput('panelRoiClassCustom',prefs.custom_class);}\n",
    "function applyViewerPreferences(){const prefs=loadViewerPreferences();applyAnnotationPreferences(prefs);applyDisplayPreferences(prefs);if(typeof updateBrushControls==='function')updateBrushControls();if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();if(typeof updateMultiViewControls==='function')updateMultiViewControls();applyStainPreferences(prefs);applyPanelPreferences(prefs);viewerPreferencesReady=true;saveViewerPreferences({__initial:true,viewer_mode:cfg.viewer_mode||'',last_opened_at:new Date().toISOString()});return validToolMode(prefs.tool_mode)||'pan';}\n",
    "function saveToolPreference(){const preferred=validToolMode(mode);if(preferred)saveViewerPreferences({tool_mode:preferred});}\n",
    "function saveBrushPreference(){saveViewerPreferences({brush_size:brushScreenRadius});}\n",
    "function saveRoiOpacityPreference(){saveViewerPreferences({roi_opacity:roiOpacity});}\n",
    "function currentClassPreference(){const customValue=typeof customCategoryValue==='function'?customCategoryValue():safeInputValue('panelRoiClassCustom');return {selected_class:nextRoiClass||activeRoiClass||currentRoiClass(),custom_class:customValue};}\n",
    "function saveRoiClassPreference(){saveViewerPreferences(currentClassPreference());}\n",
    "function currentAnnotationPreferencePatch(){const width=Number(safeInputValue('trajectoryAreaWidth')),preview=el('trajectoryAreaPreview');return Object.assign({brush_size:brushScreenRadius,roi_opacity:roiOpacity,trajectory_area_width:Number.isFinite(width)?width:512,trajectory_area_preview:preview?!!preview.checked:true},currentClassPreference());}\n",
    "function currentDisplayPreferencePatch(){const raw=String((typeof screenshotFormat==='function'?screenshotFormat():safeInputValue('screenshotFormat'))||'png').toLowerCase(),screenshot=raw==='jpg'?'jpeg':raw;const out={tool_mode:validToolMode(mode)||'pan',show_rois:!!showRois,show_labels:!!showLabels,show_crosshair:!!showCrosshair,screenshot_format:['png','jpeg','svg','pdf'].includes(screenshot)?screenshot:'png',image_export_dir:safeInputValue('imageExportDir')};if(typeof baseImagePayload==='function')out.base_layer=baseImagePayload();return out;}\n",
    "function currentViewerPreferencePatch(){const out=Object.assign({},currentAnnotationPreferencePatch(),currentDisplayPreferencePatch());const stain=(typeof currentStainPreferences==='function')?currentStainPreferences():null;if(stain)out.stain=stain;const panel=roiPanelPosition();if(panel)out.panel=panel;return out;}\n",
    "function saveCurrentViewerPreferences(showMessage=true){const saved=saveViewerPreferences(currentViewerPreferencePatch());if(showMessage)notify('Viewer preferences saved in this browser','success',2200);return saved;}\n",
    "function saveDisplayPreference(){saveViewerPreferences(currentDisplayPreferencePatch());}\n",
    "function saveAnnotationPreference(){saveViewerPreferences(currentAnnotationPreferencePatch());}\n",
    "function currentStainPreferences(){if(!stainEnabled)return null;syncStainStateFromControls();return {enabled:stainOn,channels:stainChannels.map((ch,i)=>({id:ch.id,name:ch.name,visible:!!(stainState[i]&&stainState[i].visible),color:stainState[i]?stainState[i].color:ch.colour,strength:stainState[i]?stainState[i].strength:ch.strength,gain:stainState[i]?stainState[i].strength:ch.strength,opacity:stainState[i]?stainState[i].opacity:(ch.opacity??1),contrast_min:stainState[i]?stainState[i].contrast_min:(ch.contrast_min??0),contrast_max:stainState[i]?stainState[i].contrast_max:(ch.contrast_max??1)}))};}\n",
    "function saveStainPreferences(){const stain=currentStainPreferences();if(stain)saveViewerPreferences({stain:stain});}\n",
    "function sidePanelIds(){return ['projectPanel','roiPanel','annotationHistory','viewerLogPanel'];}\n",
    "function sidePanelHeightForPrefs(id){const panel=el(id);if(!panel||(!panel.classList.contains('resized')&&!panel.style.height))return null;const rect=panel.getBoundingClientRect(),h=Math.round(rect.height);return Number.isFinite(h)&&h>0?h:null;}\n",
    "function roiPanelPosition(){const panel=leftWorkspacePanel(),roi=el('roiPanel'),project=el('projectPanel'),history=el('annotationHistory'),logs=el('viewerLogPanel');if(!panel||!roi)return null;const rect=panel.getBoundingClientRect(),out={left:Math.round(rect.left),top:Math.round(rect.top),width:Math.round(rect.width),open:roi.classList.contains('open'),minimized:roi.classList.contains('minimized'),project_minimized:!!(project&&project.classList.contains('minimized')),project_closed:!!(project&&project.classList.contains('closed')),history_minimized:!!(history&&history.classList.contains('minimized')),history_closed:!!(history&&history.classList.contains('closed')),log_minimized:!!(logs&&logs.classList.contains('minimized')),log_closed:!!(logs&&logs.classList.contains('closed'))};const ph=sidePanelHeightForPrefs('projectPanel'),rh=sidePanelHeightForPrefs('roiPanel'),hh=sidePanelHeightForPrefs('annotationHistory'),lh=sidePanelHeightForPrefs('viewerLogPanel');if(ph!==null)out.project_height=ph;if(rh!==null)out.roi_height=rh;if(hh!==null)out.history_height=hh;if(lh!==null)out.log_height=lh;return out;}\n",
    "function savePanelPreferences(){const panel=roiPanelPosition();if(panel)saveViewerPreferences({panel:panel});}\n",
    "function workspacePanelBaseTop(){return 72;}\n",
    "function workspacePanelSafeTop(){const base=workspacePanelBaseTop(),bar=document.querySelector('.bar'),rect=bar&&bar.getBoundingClientRect?bar.getBoundingClientRect():null,barBottom=rect&&Number.isFinite(rect.bottom)?Math.ceil(rect.bottom+8):0;return Math.max(base,barBottom);}\n",
    "function setRoiPanelPosition(left,top,save=true){const panel=leftWorkspacePanel();if(!panel)return;const rect=panel.getBoundingClientRect(),minTop=workspacePanelSafeTop(),maxLeft=Math.max(0,innerWidth-Math.min(rect.width||420,innerWidth)),maxTop=Math.max(minTop,innerHeight-Math.min(rect.height||80,innerHeight));panel.style.left=Math.round(clamp(left,0,maxLeft))+'px';panel.style.top=Math.round(clamp(top,minTop,maxTop))+'px';panel.style.right='auto';if(save)savePanelPreferences();}\n",
    "function workspacePanelWidthLimits(){return {min:280,max:Math.min(Math.max(320,innerWidth-48),760)};}\n",
    "function setWorkspacePanelWidth(width,save=true){const panel=leftWorkspacePanel();if(!panel)return;if(innerWidth<=900){panel.style.width='auto';if(save)savePanelPreferences();return;}const rect=panel.getBoundingClientRect(),limits=workspacePanelWidthLimits(),maxByViewport=Math.max(limits.min,innerWidth-Math.max(12,rect.left)-24),maxWidth=Math.max(limits.min,Math.min(limits.max,maxByViewport)),next=Math.round(clamp(Number(width)||rect.width||420,limits.min,maxWidth));panel.style.width=next+'px';if(save)savePanelPreferences();}\n",
    "function clampWorkspacePanelWidth(save=false){const panel=leftWorkspacePanel();if(!panel)return;if(innerWidth<=900){panel.style.width='auto';return;}const rect=panel.getBoundingClientRect();setWorkspacePanelWidth(rect.width||420,save);}\n",
    "function startWorkspacePanelResize(e){const panel=leftWorkspacePanel();if(!panel||innerWidth<=900||e.button!==0)return;e.preventDefault();e.stopPropagation();const rect=panel.getBoundingClientRect();workspaceResizeState={startX:e.clientX,width:rect.width,moved:false};panel.classList.add('resizing');if(document.body)document.body.classList.add('workspacePanelResizing');window.addEventListener('mousemove',moveWorkspacePanelResize);window.addEventListener('mouseup',finishWorkspacePanelResize,{once:true});}\n",
    "function moveWorkspacePanelResize(e){if(!workspaceResizeState)return;const dx=e.clientX-workspaceResizeState.startX;if(Math.abs(dx)>2)workspaceResizeState.moved=true;e.preventDefault();setWorkspacePanelWidth(workspaceResizeState.width+dx,false);}\n",
    "function finishWorkspacePanelResize(){window.removeEventListener('mousemove',moveWorkspacePanelResize);const panel=leftWorkspacePanel();if(panel)panel.classList.remove('resizing');if(document.body)document.body.classList.remove('workspacePanelResizing');if(workspaceResizeState&&workspaceResizeState.moved)savePanelPreferences();workspaceResizeState=null;}\n",
    "function sidePanelVisible(panel){if(!panel||panel.classList.contains('closed'))return false;const style=window.getComputedStyle(panel);return style.display!=='none'&&style.visibility!=='hidden';}\n",
    "function sidePanelHeightLimits(panel){const workspace=leftWorkspacePanel(),id=panel&&panel.id;if(!workspace||!panel)return {min:96,max:480};const min=id==='roiPanel'?180:96,workspaceRect=workspace.getBoundingClientRect(),visible=sidePanelIds().map(el).filter(sidePanelVisible),gap=Math.max(0,visible.length-1)*8,other=visible.filter(p=>p!==panel).reduce((sum,p)=>sum+p.getBoundingClientRect().height,0)+gap,maxByViewport=innerHeight-workspaceRect.top-24-other,max=Math.max(min,Math.min(720,maxByViewport));return {min:min,max:max};}\n",
    "function setSidePanelHeight(id,height,save=true){const panel=el(id);if(!panel||panel.classList.contains('closed')||panel.classList.contains('minimized')||panel.classList.contains('maximized'))return;const rect=panel.getBoundingClientRect(),limits=sidePanelHeightLimits(panel),next=Math.round(clamp(Number(height)||rect.height||limits.min,limits.min,limits.max));panel.style.height=next+'px';panel.style.maxHeight='none';panel.style.flex='0 0 auto';panel.classList.add('resized');if(save)savePanelPreferences();}\n",
    "function clampSidePanelHeights(save=false){sidePanelIds().forEach(id=>{const panel=el(id);if(panel&&panel.classList.contains('resized'))setSidePanelHeight(id,panel.getBoundingClientRect().height,save);});}\n",
    "function startSidePanelResize(e){const handle=e.currentTarget,panel=el(handle&&handle.dataset?handle.dataset.panel:'');if(!panel||panel.classList.contains('minimized')||panel.classList.contains('closed')||e.button!==0)return;e.preventDefault();e.stopPropagation();const rect=panel.getBoundingClientRect();sidePanelResizeState={id:panel.id,startY:e.clientY,height:rect.height,moved:false};panel.classList.add('resizingVertical');if(document.body)document.body.classList.add('sidePanelResizing');window.addEventListener('mousemove',moveSidePanelResize);window.addEventListener('mouseup',finishSidePanelResize,{once:true});}\n",
    "function moveSidePanelResize(e){if(!sidePanelResizeState)return;const dy=e.clientY-sidePanelResizeState.startY;if(Math.abs(dy)>2)sidePanelResizeState.moved=true;e.preventDefault();setSidePanelHeight(sidePanelResizeState.id,sidePanelResizeState.height+dy,false);}\n",
    "function finishSidePanelResize(){window.removeEventListener('mousemove',moveSidePanelResize);const id=sidePanelResizeState&&sidePanelResizeState.id,panel=id?el(id):null;if(panel)panel.classList.remove('resizingVertical');if(document.body)document.body.classList.remove('sidePanelResizing');if(sidePanelResizeState&&sidePanelResizeState.moved)savePanelPreferences();sidePanelResizeState=null;}\n",
    "function bindSidePanelResizeControls(){document.querySelectorAll('.sidePanelResizeHandle').forEach(handle=>{if(handle.dataset.bound==='1')return;handle.dataset.bound='1';handle.addEventListener('mousedown',startSidePanelResize);});window.addEventListener('resize',()=>clampSidePanelHeights(false));}\n",
    "function bindWorkspaceResizeControls(){const handle=el('workspaceResizeHandle');if(handle&&handle.dataset.bound!=='1'){handle.dataset.bound='1';handle.addEventListener('mousedown',startWorkspacePanelResize);window.addEventListener('resize',()=>clampWorkspacePanelWidth(false));}bindSidePanelResizeControls();}\n",
    "function startRoiPanelDrag(e){const panel=leftWorkspacePanel(),roi=el('roiPanel');if(!panel||!roi||e.button!==0||e.detail>1||e.target.closest('input,select,textarea,button,a'))return;const rect=panel.getBoundingClientRect();roiPanelDragState={startX:e.clientX,startY:e.clientY,left:rect.left,top:rect.top,moved:false};window.addEventListener('mousemove',moveRoiPanelDrag);window.addEventListener('mouseup',finishRoiPanelDrag,{once:true});}\n",
    "function moveRoiPanelDrag(e){if(!roiPanelDragState)return;const dx=e.clientX-roiPanelDragState.startX,dy=e.clientY-roiPanelDragState.startY;if(Math.abs(dx)+Math.abs(dy)>3)roiPanelDragState.moved=true;if(roiPanelDragState.moved){e.preventDefault();setRoiPanelPosition(roiPanelDragState.left+dx,roiPanelDragState.top+dy,false);}}\n",
    "function finishRoiPanelDrag(){window.removeEventListener('mousemove',moveRoiPanelDrag);if(roiPanelDragState&&roiPanelDragState.moved)savePanelPreferences();roiPanelDragState=null;}\n",
    "function bindPreferenceControls(){bindWorkspaceResizeControls();bindSidePanelResizeControls();if(typeof bindScreenshotDialogControls==='function')bindScreenshotDialogControls();if(typeof bindAnnotationExportDialogControls==='function')bindAnnotationExportDialogControls();const save=el('savePreferences'),reset=el('resetPreferences'),screenshot=el('screenshotDialogFormat')||el('screenshotFormat'),dir=el('imageExportDir'),trajectoryWidth=el('trajectoryAreaWidth'),trajectoryPreview=el('trajectoryAreaPreview');if(save)save.onclick=()=>saveCurrentViewerPreferences(true);if(reset)reset.onclick=()=>{try{if(window.localStorage)localStorage.removeItem(viewerPreferenceKey);}catch(e){}viewerPreferencesCache={};notify('Viewer preferences reset','success');};if(screenshot)screenshot.addEventListener('change',saveDisplayPreference);if(dir)dir.addEventListener('change',saveDisplayPreference);if(trajectoryWidth)trajectoryWidth.addEventListener('input',saveAnnotationPreference);if(trajectoryPreview)trajectoryPreview.addEventListener('change',saveAnnotationPreference);}\n"
  )
}

wsi_viewer_jobs_js <- function() {
  paste0(
    "const viewerJobs=new Map();\n",
    "let jobSyncMessage='';\n",
    "function jobStatusLabel(status){const s=String(status||'pending').toLowerCase();if(['queued','queue','created','waiting'].includes(s))return 'pending';if(s==='finished'||s==='complete'||s==='success')return 'completed';if(s==='error')return 'failed';return s;}\n",
    "function jobLogLines(job){const log=job&&job.log;if(Array.isArray(log))return log.map(String).filter(Boolean);if(typeof log==='string'&&log.length)return log.split(/\\r?\\n/).filter(Boolean);return [];}\n",
    "function jobErrorText(job,lines=[]){const direct=job&&(job.error||job.error_message||job.stderr);if(direct)return String(direct);if(job&&job.status==='failed'&&job.message)return String(job.message);if(job&&job.status==='failed'&&lines.length)return String(lines[lines.length-1]);return '';}\n",
    "function normaliseViewerJob(job){if(!job)return null;const id=String(job.id||job.job_id||'');if(!id)return null;const status=jobStatusLabel(job.status||job.display_status||job.raw_status);const progress=Number(job.progress);const lines=jobLogLines(job);return {id:id,name:String(job.name||job.job_name||'wsiTools job'),status:status,progress:Number.isFinite(progress)?Math.max(0,Math.min(100,progress)):NaN,progress_available:!!job.progress_available&&Number.isFinite(progress),message:String(job.message||''),error:String(job.error||job.error_message||job.stderr||''),log:lines,updated:String(job.updated||new Date().toISOString()),started:String(job.started||''),finished:String(job.finished||'')};}\n",
    "function jobProgressText(job){if(Number.isFinite(job.progress))return Math.round(job.progress)+'%';if(job.status==='pending')return 'pending';if(job.status==='running')return 'running';if(job.status==='completed')return '100%';if(job.status==='failed')return 'failed';return job.status;}\n",
    "function jobTimeText(job){const parts=[];if(job.started)parts.push('started '+job.started);if(job.finished)parts.push('finished '+job.finished);if(job.updated)parts.push('updated '+job.updated);return parts.join(' | ');}\n",
    "function jobCountsObject(jobs){return jobs.reduce((acc,j)=>{acc[j.status]=(acc[j.status]||0)+1;return acc;},{});}\n",
    "function jobCountsText(jobs){const counts=jobCountsObject(jobs),order=['pending','running','completed','failed'],parts=[];order.forEach(k=>{if(counts[k])parts.push(k+' '+counts[k]);});Object.keys(counts).sort().forEach(k=>{if(!order.includes(k))parts.push(k+' '+counts[k]);});return parts.join(', ');}\n",
    "function jobPrimaryStatus(jobs){const counts=jobCountsObject(jobs);if(counts.failed)return 'failed';if(counts.running)return 'running';if(counts.pending)return 'pending';if(jobs.length&&counts.completed===jobs.length)return 'completed';return 'idle';}\n",
    "function jobLatestByStatus(jobs,status){const matches=jobs.filter(j=>j.status===status);return matches.length?matches[0]:null;}\n",
    "function jobSyncMessageLabel(message){const msg=String(message||'').trim();if(!msg)return '';return msg.replace(/^R sync:\\s*/i,'').replace(/^R command:\\s*/i,'').replace(/^Autosave\\s*/i,'Autosave ');}\n",
    "function updateJobSyncIndicator(jobs=null,message=null){if(message!==null)jobSyncMessage=String(message||'');const node=el('jobSyncIndicator'),label=el('jobSyncLabel'),detail=el('jobSyncDetail');if(!node||!label)return;const all=jobs||Array.from(viewerJobs.values()).sort((a,b)=>String(b.updated||'').localeCompare(String(a.updated||''))),counts=jobCountsObject(all);let cls=jobPrimaryStatus(all),text='Synced',small='',title='Live R synchronization status';if(cls==='failed'){const job=jobLatestByStatus(all,'failed');text='Failed';small=(counts.failed||1)+'/'+all.length;title=(job&&job.name?job.name+' failed':'A background job failed')+(job&&job.message?': '+job.message:'');}else if(cls==='running'){const job=jobLatestByStatus(all,'running');text='Running';small=job?jobProgressText(job):((counts.running||1)+' running');title=(job&&job.name?job.name:'Background job')+' is running'+(job&&job.message?': '+job.message:'');}else if(cls==='pending'){text='Pending';small=(counts.pending||1)+' queued';title='Background job waiting to run';}else if(cls==='completed'&&all.length){text='Completed';small=all.length+' done';title='All background jobs completed';}else{const msg=jobSyncMessageLabel(jobSyncMessage),live=(typeof liveSyncAvailable==='function')&&liveSyncAvailable();if(jobSyncMessage&&/fail|error/i.test(jobSyncMessage)){cls='failed';text='Sync failed';small='';title=jobSyncMessage;}else if(!live){cls='off';text='Sync off';small='static';title='Static viewer: no live R synchronization endpoint is configured.';}else{cls='idle';text=msg&&/websocket/i.test(msg)?'WebSocket':(msg&&/autosav/i.test(msg)?'Autosaved':'Synced');small=msg&&text!==msg?msg:'';title=jobSyncMessage||'R synchronization is ready.';}}node.className='syncIndicator '+cls.replace(/[^A-Za-z0-9_-]+/g,'');label.textContent=text;if(detail)detail.textContent=small;node.title=title;}\n",
    "function renderJobList(){const summary=el('jobSummary'),list=el('jobList'),jobs=Array.from(viewerJobs.values()).sort((a,b)=>String(b.updated||'').localeCompare(String(a.updated||'')));updateJobSyncIndicator(jobs);if(!summary||!list)return;list.innerHTML='';if(!jobs.length){summary.textContent='No background jobs yet. Start tile extraction, conversion, pyramid generation, or project sync to see progress here.';return;}summary.textContent=jobs.length+' background job'+(jobs.length===1?'':'s')+' | '+jobCountsText(jobs);jobs.slice(0,24).forEach(job=>{const item=document.createElement('div'),statusClass=job.status.replace(/[^A-Za-z0-9_-]+/g,'');item.className='jobItem '+statusClass;const top=document.createElement('div');top.className='jobTop';const name=document.createElement('div');name.className='jobName';name.textContent=job.name;const status=document.createElement('span');status.className='jobStatus '+statusClass;status.textContent=job.status;top.append(name,status);const row=document.createElement('div');row.className='jobProgressRow';const bar=document.createElement('div');bar.className='jobProgress';const fill=document.createElement('div');fill.className='jobProgressFill';if(Number.isFinite(job.progress)){fill.style.width=job.progress+'%';}else if(job.status==='running'||job.status==='pending'){fill.classList.add('indeterminate');}else if(job.status==='completed'){fill.style.width='100%';}else{fill.style.width='0%';}bar.appendChild(fill);const progressText=document.createElement('div');progressText.className='jobProgressText';progressText.textContent=jobProgressText(job);row.append(bar,progressText);item.append(top,row);if(job.message){const message=document.createElement('div');message.className='jobMessage';message.textContent=job.message;item.appendChild(message);}const metaText=jobTimeText(job);if(metaText){const meta=document.createElement('div');meta.className='jobMeta';meta.textContent=metaText;item.appendChild(meta);}const lines=job.log||[],errorText=job.error||jobErrorText(job,lines);if(errorText){const error=document.createElement('div');error.className='jobError';error.textContent='Error: '+errorText;item.appendChild(error);}if(lines.length){const details=document.createElement('details');details.className='jobLogDetails';const summaryNode=document.createElement('summary');summaryNode.textContent='Log ('+lines.length+' line'+(lines.length===1?'':'s')+')';const pre=document.createElement('pre');pre.className='jobLog';pre.textContent=lines.slice(-12).join('\\n');details.append(summaryNode,pre);item.appendChild(details);}list.appendChild(item);});}\n",
    "function upsertViewerJob(job){const rec=normaliseViewerJob(job);if(!rec)return;const old=viewerJobs.get(rec.id),merged=Object.assign({},old||{},rec);if(!rec.error)merged.error=jobErrorText(Object.assign({},job,merged),merged.log||[]);viewerJobs.set(rec.id,merged);renderJobList();if(old&&old.status!==merged.status){if(merged.status==='completed')notify(merged.name+' completed','success',3200);else if(merged.status==='failed')notify(merged.name+' failed','error',4800);}}\n",
    "function updateViewerJob(id,patch={}){const old=viewerJobs.get(id)||{id:id,name:patch.name||'wsiTools job',status:'pending'};upsertViewerJob(Object.assign({},old,patch,{id:id,updated:new Date().toISOString()}));}\n",
    "function handleViewerJobs(body){const jobs=(body&&body.jobs)||[];if(Array.isArray(jobs))jobs.forEach(upsertViewerJob);}\n",
    "function bindJobControls(){renderJobList();}\n"
  )
}

wsi_viewer_stain_js <- function() {
  paste0(
    "const stainEnabled=!!(cfg.stain&&cfg.stain.enabled);\n",
    "const stainChannels=stainEnabled?(cfg.stain.channels||[]):[];\n",
    "let stainOn=stainEnabled;\n",
    "let stainState=stainChannels.map(ch=>({visible:ch.visible!==false,color:ch.colour||'#666666',strength:Number(ch.strength||ch.gain||1),opacity:Number(ch.opacity??1),contrast_min:Number(ch.contrast_min??0),contrast_max:Number(ch.contrast_max??1)}));\n",
    "const stainDefaultVisible=stainChannels.map(ch=>ch.visible!==false);\n",
    "let stainDisplayMode=stainEnabled?'default':'original';\n",
    "let stainInv=null,stainBasis=[],heGramInv=null;\n",
    "let stainError='';\n",
    "function stainIsHE(){return /^H&E$/i.test(String((cfg.stain||{}).type||''));}\n",
    "function setStainInputState(i,visible){const ch=stainChannels[i];if(!ch)return;if(!stainState[i])stainState[i]={visible:true,color:ch.colour||'#666666',strength:Number(ch.strength||ch.gain||1),opacity:Number(ch.opacity??1),contrast_min:Number(ch.contrast_min??0),contrast_max:Number(ch.contrast_max??1)};stainState[i].visible=!!visible;const input=el('stainVisible_'+ch.id);if(input)input.checked=!!visible;}\n",
    "function stainChannelIndex(value){const raw=String(value||'');const numeric=Number(raw);if(Number.isInteger(numeric)&&numeric>=0&&numeric<stainChannels.length)return numeric;const wanted=raw.trim().toLowerCase();return stainChannels.findIndex(ch=>String(ch.id||ch.name||'').trim().toLowerCase()===wanted);}\n",
    "function stainVisibleFlags(){return stainState.map(s=>!!(s&&s.visible));}\n",
    "function sameStainFlags(a,b){return Array.isArray(a)&&Array.isArray(b)&&a.length===b.length&&a.every((v,i)=>!!v===!!b[i]);}\n",
    "function inferStainDisplayMode(){if(!stainOn)return 'original';const flags=stainVisibleFlags();if(flags.length&&flags.every(Boolean))return 'all';if(sameStainFlags(flags,stainDefaultVisible))return 'default';return 'only';}\n",
    "function rgbHex(hex){const h=String(hex||'#000000').replace('#','');const s=h.length===3?h.split('').map(c=>c+c).join(''):h;const n=parseInt(s,16);return {r:(n>>16)&255,g:(n>>8)&255,b:n&255};}\n",
    "function norm3(v){const n=Math.hypot(Number(v[0]),Number(v[1]),Number(v[2]));return n>0?[Number(v[0])/n,Number(v[1])/n,Number(v[2])/n]:[0,0,0];}\n",
    "function cross3(a,b){return [a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]];}\n",
    "function inv3(m){const a=m[0][0],b=m[0][1],c=m[0][2],d=m[1][0],e=m[1][1],f=m[1][2],g=m[2][0],h=m[2][1],i=m[2][2];const A=e*i-f*h,B=-(d*i-f*g),C=d*h-e*g,D=-(b*i-c*h),E=a*i-c*g,F=-(a*h-b*g),G=b*f-c*e,H=-(a*f-c*d),I=a*e-b*d;const det=a*A+b*B+c*C;if(Math.abs(det)<1e-8)return null;return [[A/det,D/det,G/det],[B/det,E/det,H/det],[C/det,F/det,I/det]];}\n",
    "function initStain(){if(!stainEnabled)return;const b=(cfg.stain.basis||[]).map(norm3);stainBasis=b;heGramInv=null;if(b.length!==3){notify('IHC stain basis incomplete','error',4200);return;}stainInv=inv3([[b[0][0],b[1][0],b[2][0]],[b[0][1],b[1][1],b[2][1]],[b[0][2],b[1][2],b[2][2]]]);if(/^H&E$/i.test(String(cfg.stain.type||''))&&b.length>=2){const h=b[0],e=b[1],hh=h[0]*h[0]+h[1]*h[1]+h[2]*h[2],he=h[0]*e[0]+h[1]*e[1]+h[2]*e[2],ee=e[0]*e[0]+e[1]*e[1]+e[2]*e[2],det=hh*ee-he*he;if(Math.abs(det)>1e-8)heGramInv=[[ee/det,-he/det],[-he/det,hh/det]];}if(!stainInv)notify('IHC stain vectors invalid','error',4200);}\n",
    "function syncStainStateFromControls(){if(!stainEnabled)return;stainChannels.forEach((ch,i)=>{if(!stainState[i])stainState[i]={visible:true,color:'#666666',strength:1,opacity:1,contrast_min:0,contrast_max:1};const vis=el('stainVisible_'+ch.id),color=el('stainColor_'+ch.id),strength=el('stainStrength_'+ch.id),opacity=el('stainOpacity_'+ch.id),cmin=el('stainContrastMin_'+ch.id),cmax=el('stainContrastMax_'+ch.id);if(vis)stainState[i].visible=!!vis.checked;if(color)stainState[i].color=color.value;if(strength)stainState[i].strength=Number(strength.value);if(opacity)stainState[i].opacity=Number(opacity.value);if(cmin)stainState[i].contrast_min=Number(cmin.value);if(cmax)stainState[i].contrast_max=Number(cmax.value);});}\n",
    "function activeStainNames(){syncStainStateFromControls();return stainChannels.filter((ch,i)=>stainState[i]&&stainState[i].visible).map(ch=>ch.name||ch.id).join(', ');}\n",
    "function stainChannelSourceFor(ch){if(typeof channelSources==='undefined'||!Array.isArray(channelSources)||!ch)return null;const wanted=String(ch.id||'');return channelSources.find(src=>{const meta=(src&&src.metadata)||{};return (String(meta.kind||'')==='stain_channel'||String(meta.source_type||'')==='stain_deconvolution')&&String(meta.stain_channel_id||src.stain_channel_id||'')===wanted;})||null;}\n",
    "function hasTiledStainChannels(){return stainEnabled&&stainChannels.length&&stainChannels.some(ch=>!!stainChannelSourceFor(ch));}\n",
    "function setBaseImageVisibleForStain(visible){if(typeof baseImageState!=='undefined'){baseImageState.visible=!!visible;if(typeof applyBaseImageDisplay==='function')applyBaseImageDisplay();return;}if(typeof setBaseImageVisible==='function')setBaseImageVisible(visible);}\n",
    "function tiledStainSettings(src,state,ch,visible){return {visible:!!visible,colour:state.color||ch.colour||src.colour,gain:Number(state.strength??ch.strength??src.gain??1),opacity:Number(state.opacity??ch.opacity??src.opacity??.9),contrast_min:Number(state.contrast_min??ch.contrast_min??src.contrast_min??0),contrast_max:Number(state.contrast_max??ch.contrast_max??src.contrast_max??1)};}\n",
    "function applyTiledStainSourceVisibility(src,settings){if(!src)return;if(!settings.visible){Object.assign(src,settings);if(typeof removeChannelItem==='function')removeChannelItem(src.id);else if(typeof setChannelSettings==='function')setChannelSettings(src.id,settings);return;}if(typeof setChannelSettings==='function')setChannelSettings(src.id,settings);else{Object.assign(src,settings);if(typeof upsertChannelSource==='function')upsertChannelSource(src);}}\n",
    "function syncTiledStainChannels(){if(!hasTiledStainChannels())return false;syncStainStateFromControls();const active=[];stainState.forEach((s,i)=>{if(stainOn&&s&&s.visible)active.push(i);});const activeNameList=active.map(i=>stainChannels[i]&&(stainChannels[i].name||stainChannels[i].id)).filter(Boolean).join(', ');stainChannels.forEach((ch,i)=>{const src=stainChannelSourceFor(ch),state=stainState[i]||{};if(!src)return;const visible=!!(stainOn&&state.visible),settings=tiledStainSettings(src,state,ch,visible);applyTiledStainSourceVisibility(src,settings);});setBaseImageVisibleForStain(!(stainOn&&active.length));if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();if(typeof buildChannelList==='function')buildChannelList();if(!stainOn)setStainMessage('Showing original RGB image.');else if(stainDisplayMode==='all')setStainMessage(activeNameList?('Showing all tiled stain channels: '+activeNameList+'.'):'No stain channel visible.');else if(stainDisplayMode==='default')setStainMessage(activeNameList?('Showing tiled '+((cfg.stain&&cfg.stain.button_label)||cfg.stain.label||'stain')+' display: '+activeNameList+'.'):'No stain channel visible.');else setStainMessage(activeNameList?('Showing tiled '+activeNameList+' channel layer'+(activeNameList.includes(',')?'s':'')):('No stain channel visible.'));return true;}\n",
    "function setStainMessage(msg){const box=el('stainMessage');if(box)box.textContent=msg||'';}\n",
    "function stainStatus(){if(!stainEnabled)return '';if(stainError)return ' | stains unavailable: '+stainError;if(!stainOn)return ' | original RGB';const active=activeStainNames();return ' | '+(cfg.stain.label||'IHC channels')+(active?' '+active:' no channels');}\n",
    "function stainInputImageData(targetCtx,targetCanvas,sourceCanvas=null){if(!sourceCanvas)return targetCtx.getImageData(0,0,targetCanvas.width,targetCanvas.height);let readCanvas=sourceCanvas,readCtx=sourceCanvas.getContext('2d',{willReadFrequently:true});if(readCanvas.width!==targetCanvas.width||readCanvas.height!==targetCanvas.height){const tmp=document.createElement('canvas');tmp.width=targetCanvas.width;tmp.height=targetCanvas.height;const tctx=tmp.getContext('2d',{willReadFrequently:true});tctx.drawImage(readCanvas,0,0,tmp.width,tmp.height);readCanvas=tmp;readCtx=tctx;}return readCtx.getImageData(0,0,targetCanvas.width,targetCanvas.height);}\n",
    "function stainConcentrations(odR,odG,odB){if(heGramInv&&stainBasis.length>=2){const h=stainBasis[0],e=stainBasis[1],dh=h[0]*odR+h[1]*odG+h[2]*odB,de=e[0]*odR+e[1]*odG+e[2]*odB,ch=Math.max(0,heGramInv[0][0]*dh+heGramInv[0][1]*de),ce=Math.max(0,heGramInv[1][0]*dh+heGramInv[1][1]*de),rr=odR-h[0]*ch-e[0]*ce,gg=odG-h[1]*ch-e[1]*ce,bb=odB-h[2]*ch-e[2]*ce;return [ch,ce,Math.max(0,Math.hypot(rr,gg,bb))];}return [Math.max(0,stainInv[0][0]*odR+stainInv[0][1]*odG+stainInv[0][2]*odB),Math.max(0,stainInv[1][0]*odR+stainInv[1][1]*odG+stainInv[1][2]*odB),Math.max(0,stainInv[2][0]*odR+stainInv[2][1]*odG+stainInv[2][2]*odB)];}\n",
    "function activeStainIndices(){syncStainStateFromControls();const idx=[];stainState.forEach((s,i)=>{if(s&&s.visible)idx.push(i);});return idx;}\n",
    "function stainAutoScales(data,active){const out=stainChannels.map(()=>1);if(!active.length)return out;const samples=active.map(()=>[]),pixels=data.length/4,step=Math.max(1,Math.floor(pixels/45000));for(let px=0;px<pixels;px+=step){const p=px*4,r=data[p],g=data[p+1],b=data[p+2];if(data[p+3]===0||(r>246&&g>246&&b>246)||(r<28&&g<28&&b<28))continue;const c=stainConcentrations(-Math.log((r+1)/256),-Math.log((g+1)/256),-Math.log((b+1)/256));active.forEach((idx,j)=>{const v=Number(c[idx]);if(Number.isFinite(v)&&v>0)samples[j].push(v);});}active.forEach((idx,j)=>{const s=samples[j];if(!s.length){out[idx]=1;return;}s.sort((a,b)=>a-b);out[idx]=Math.max(0.08,s[Math.max(0,Math.floor(s.length*0.985)-1)]||s[s.length-1]||1);});return out;}\n",
    "function stainIntensity(c,state,scale){const lo=Number.isFinite(state.contrast_min)?state.contrast_min:0,manual=!!(state&&state.manual_contrast),hi=Math.max(lo+1e-6,manual&&Number.isFinite(state.contrast_max)?state.contrast_max:scale),ci=clamp((c-lo)/(hi-lo),0,1),strength=Number.isFinite(state.strength)?state.strength:1,opacity=Number.isFinite(state.opacity)?state.opacity:1;return clamp((1-Math.exp(-ci*Math.max(1.2,strength*2.4)))*opacity,0,1);}\n",
    "function applyStainToCanvas(targetCtx=ctx,targetCanvas=canvas,sourceCanvas=null){if(!stainEnabled||!stainOn||!stainInv||!stainChannels.length)return false;const active=activeStainIndices();if(!active.length)return false;let img;try{img=stainInputImageData(targetCtx,targetCanvas,sourceCanvas);stainError='';setStainMessage(active.length===1?'Showing '+(stainChannels[active[0]].name||stainChannels[active[0]].id)+' deconvolution channel.':'Showing deconvolved stain composite.');}catch(e){stainError=(location.protocol==='file:')?'open the viewer through localhost/http, not file://':'canvas pixel access blocked';setStainMessage('Stain selection needs readable canvas pixels. Use wsi_viewer_live(..., dynamic_tiles = TRUE), or serve the viewer from http://127.0.0.1:<port>/ instead of opening it as file://.');return false;}const data=img.data,colors=stainState.map(s=>rgbHex(s.color)),scales=stainAutoScales(data,active);for(let p=0;p<data.length;p+=4){const r=data[p],g=data[p+1],b=data[p+2];if(data[p+3]===0||(r<28&&g<28&&b<28))continue;const c=stainConcentrations(-Math.log((r+1)/256),-Math.log((g+1)/256),-Math.log((b+1)/256));let rr=255,gg=255,bb=255;for(const i of active){if(i>=c.length)continue;const state=stainState[i]||{},t=stainIntensity(c[i],state,scales[i]),col=colors[i]||{r:102,g:102,b:102};rr=rr*(1-t)+col.r*t;gg=gg*(1-t)+col.g*t;bb=bb*(1-t)+col.b*t;}data[p]=rr;data[p+1]=gg;data[p+2]=bb;}targetCtx.putImageData(img,0,0);return true;}\n",
    "function stainDisplayChanged(event='stain_updated'){updateStainControls();saveStainPreferences();if(syncTiledStainChannels()){scheduleViewerStateSync(event,{tiled_stain_channels:true});if(typeof requestDraw==='function')requestDraw();else draw();return;}scheduleViewerStateSync(event,{});if(typeof invalidateBaseImage==='function')invalidateBaseImage();else if(typeof requestDraw==='function')requestDraw();else draw();}\n",
    "function setStainVisible(indices,mode='only'){const keep=new Set((Array.isArray(indices)?indices:[indices]).map(stainChannelIndex).filter(i=>Number.isInteger(i)&&i>=0));stainOn=true;stainDisplayMode=mode;stainChannels.forEach((ch,i)=>setStainInputState(i,keep.has(i)));stainDisplayChanged('stain_updated');}\n",
    "function showDefaultStains(){stainOn=true;stainDisplayMode='default';stainChannels.forEach((ch,i)=>setStainInputState(i,!!stainDefaultVisible[i]));stainDisplayChanged('stain_updated');}\n",
    "function showAllStains(){setStainVisible(stainChannels.map((_,i)=>i),'all');}\n",
    "function showOriginalStain(){stainOn=false;stainDisplayMode='original';stainDisplayChanged('stain_updated');}\n",
    "function updateStainControls(){if(!stainEnabled)return;stainDisplayMode=inferStainDisplayMode();const active=stainVisibleFlags().reduce((out,v,i)=>{if(v)out.push(i);return out;},[]);const toggle=el('stainToggle');if(toggle)toggle.classList.toggle('active',stainDisplayMode==='default');const original=el('stainShowOriginal'),all=el('stainShowAll');if(original)original.classList.toggle('active',stainDisplayMode==='original');if(all)all.classList.toggle('active',stainDisplayMode==='all');document.querySelectorAll('.stainOnly').forEach(button=>{const idx=Number(button.dataset.stainIndex);button.classList.toggle('active',stainDisplayMode==='only'&&active.length===1&&active[0]===idx);});stainChannels.forEach(ch=>['Visible_','Color_','Strength_','Opacity_','ContrastMin_','ContrastMax_'].forEach(prefix=>{const input=el('stain'+prefix+ch.id);if(input)input.disabled=!stainOn;}));}\n",
    "function bindStainControls(){if(!stainEnabled)return;initStain();syncStainStateFromControls();stainDisplayMode=inferStainDisplayMode();const toggle=el('stainToggle');if(toggle)toggle.onclick=showDefaultStains;const original=el('stainShowOriginal'),all=el('stainShowAll');if(original)original.onclick=showOriginalStain;if(all)all.onclick=showAllStains;document.querySelectorAll('.stainOnly').forEach(button=>{button.onclick=()=>setStainVisible([button.dataset.stainId||button.dataset.stainIndex],'only');});const redraw=()=>{stainOn=true;syncStainStateFromControls();stainDisplayMode=inferStainDisplayMode();stainDisplayChanged('stain_updated');};stainChannels.forEach(ch=>{['Visible_','Color_','Strength_','Opacity_','ContrastMin_','ContrastMax_'].forEach(prefix=>{const input=el('stain'+prefix+ch.id);if(input){input.addEventListener('input',redraw);input.addEventListener('change',redraw);}});});updateStainControls();syncTiledStainChannels();}\n"
  )
}

wsi_viewer_base_image_js <- function() {
  paste0(
    "let baseImageState={visible:!((cfg.base_layer||{}).visible===false),opacity:clamp(Number((cfg.base_layer||{}).opacity??1),0,1),name:String((cfg.base_layer||{}).name||'Base image')};\n",
    "function baseImageOpacityValue(){return baseImageState.visible?clamp(Number(baseImageState.opacity??1),0,1):0;}\n",
    "function baseImagePayload(){return {id:'base_image',name:baseImageState.name,type:'base',visible:!!baseImageState.visible,opacity:clamp(Number(baseImageState.opacity??1),0,1)};}\n",
    "function applyBaseImageDisplay(){const opacity=baseImageOpacityValue();const item=(typeof osdItem==='function')?osdItem():null;if(item&&typeof item.setOpacity==='function')item.setOpacity(opacity);const vis=el('baseImageVisible'),op=el('baseImageOpacity'),name=el('baseImageName');if(vis)vis.checked=!!baseImageState.visible;if(op)op.value=String(clamp(Number(baseImageState.opacity??1),0,1));if(name)name.textContent=baseImageState.name;}\n",
    "function setBaseImageVisible(visible){baseImageState.visible=!!visible;applyBaseImageDisplay();saveViewerPreferences({base_layer:baseImagePayload()});scheduleViewerStateSync('layer_visibility_updated',{id:'base_image',name:baseImageState.name,visible:baseImageState.visible});if(typeof requestDraw==='function')requestDraw();else if(typeof draw==='function')draw();}\n",
    "function setBaseImageOpacity(opacity){const value=clamp(Number(opacity),0,1);if(Number.isFinite(value))baseImageState.opacity=value;applyBaseImageDisplay();saveViewerPreferences({base_layer:baseImagePayload()});scheduleViewerStateSync('layer_opacity_updated',{id:'base_image',name:baseImageState.name,opacity:baseImageState.opacity});if(typeof requestDraw==='function')requestDraw();else if(typeof draw==='function')draw();}\n",
    "function bindBaseImageControls(){const prefs=loadViewerPreferences();if(prefs&&prefs.base_layer){if(typeof prefs.base_layer.visible==='boolean')baseImageState.visible=prefs.base_layer.visible;const op=Number(prefs.base_layer.opacity);if(Number.isFinite(op))baseImageState.opacity=clamp(op,0,1);}const vis=el('baseImageVisible'),op=el('baseImageOpacity');if(vis)vis.onchange=e=>setBaseImageVisible(e.target.checked);if(op)op.oninput=e=>setBaseImageOpacity(e.target.value);applyBaseImageDisplay();}\n"
  )
}

wsi_viewer_channel_js <- function() {
  paste0(
    "let channelSources=Array.isArray(cfg.channel_sources)?cfg.channel_sources.slice():[];\n",
    "const channelItems=new Map();\n",
    "const channelPendingItems=new Set();\n",
    "const channelMaskFilterState=new Map();\n",
    "const channelMaskTileCache=new Map();\n",
    "const channelMaskProcessedCache=new Map();\n",
    "let channelMaskFilterWarningShown=false;\n",
    "function normaliseChannelSource(src){if(!src)return null;const meta=src.metadata||{},id=String(src.id||src.name||'channel');return Object.assign({id:id,name:String(src.name||id),type:String(src.type||'deepzoom'),visible:src.visible!==false,opacity:Number(src.opacity??1),colour:String(src.colour||src.color||'#ffffff'),gain:Number(src.gain??src.strength??1),contrast_min:Number(src.contrast_min??0),contrast_max:Number(src.contrast_max??1),tile_url_style:String(src.tile_url_style||'deepzoom'),metadata:meta},src,{id:id,metadata:meta});}\n",
    "function channelTileUrl(src,level,x,y){let url=tileUrlFromParts(String(src.tile_url_base||''),String(src.tile_url_template||''),String(src.tile_url_style||'deepzoom'),String(src.tile_format||cfg.tile_format||'png'),level,x,y);const meta=src.metadata||{};if(src.type==='dynamic'&&meta.server_colourized!==false){const params=new URLSearchParams();params.set('colour',String(src.colour||src.color||'#ffffff'));params.set('gain',String(Number(src.gain??src.strength??1)));params.set('contrast_min',String(Number(src.contrast_min??0)));params.set('contrast_max',String(Number(src.contrast_max??1)));if(src.cache_key||meta.cache_key||src.created)params.set('v',String(src.cache_key||meta.cache_key||src.created));url+=(url.indexOf('?')>=0?'&':'?')+params.toString();}return url;}\n",
    "function channelTileSource(src){if(!src||(src.type==='stain'))return null;if(!(src.tile_url_base||src.tile_url_template))return null;const out={width:Number(src.width||cfg.slide_width),height:Number(src.height||cfg.slide_height),tileSize:Number(src.tile_size||cfg.tile_size||512),tileOverlap:Number(src.tile_overlap||0),minLevel:Number(src.min_level||0),maxLevel:Number(src.max_level||cfg.max_level||0),getTileUrl:(level,x,y)=>channelTileUrl(src,level,x,y)};return withTileCors(out,src.tile_url_base,src.tile_url_template);}\n",
    "function channelNormValue(value){return String(value||'').trim().toLowerCase();}\n",
    "function channelBasename(value){const s=channelNormValue(value);return s.split(/[\\\\/]+/).filter(Boolean).pop()||s;}\n",
    "function channelPushIdentity(out,value){const v=channelNormValue(value);if(!v)return;out.add(v);const base=channelBasename(v);if(base)out.add(base);}\n",
    "function channelIdentitySetFrom(obj){const out=new Set();if(!obj)return out;['id','label','name','path','file_name','source_path','tile_source_id','tile_url_base','tile_url_template'].forEach(key=>channelPushIdentity(out,obj[key]));const meta=obj.metadata||{};['id','label','name','path','source_path','target_path','base_path','project_item_id','project_image_id','tile_source_id'].forEach(key=>channelPushIdentity(out,meta[key]));return out;}\n",
    "function activeChannelIdentitySet(){const out=new Set();if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||!projectItems.length){channelPushIdentity(out,'active_project_image');return out;}const item=projectItems[activeProjectIndex]||null,section=(typeof activeProjectSection==='function')?activeProjectSection():null,display=(typeof projectDisplaySource==='function')?projectDisplaySource(item,section):(section||item);[item,section,display].forEach(obj=>channelIdentitySetFrom(obj).forEach(v=>out.add(v)));if(item&&item.active)channelPushIdentity(out,'active_project_image');return out;}\n",
    "function channelExplicitTargets(src){const out=new Set(),meta=(src&&src.metadata)||{};['target_id','target_project_id','target_project_item_id','target_project_image_id','target_item_id','target_path','target_source_path','target_tile_source_id','base_id','base_path','base_slide_path','slide_path','project_item_id','project_image_id','item_id','tile_source_id'].forEach(key=>{channelPushIdentity(out,src&&src[key]);channelPushIdentity(out,meta[key]);});return out;}\n",
    "function channelSourceMatchesActive(src){if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||projectItems.length<=1)return true;src=normaliseChannelSource(src);const active=activeChannelIdentitySet(),targets=channelExplicitTargets(src);if(targets.size)return Array.from(targets).some(v=>active.has(v));const meta=(src&&src.metadata)||{},sourceIds=channelIdentitySetFrom(src);['source_path','path','tile_source_id'].forEach(key=>channelPushIdentity(sourceIds,meta[key]));if(Array.from(sourceIds).some(v=>active.has(v)))return true;return false;}\n",
    "function visibleChannelSources(){return channelSources.map(normaliseChannelSource).filter(Boolean).filter(channelSourceMatchesActive);}\n",
    "function channelSourceById(id){id=String(id||'');return channelSources.find(x=>String(x&&x.id)===id)||null;}\n",
    "function setChannelItemSettings(src){src=normaliseChannelSource(channelSourceById(src&&src.id)||src);const item=src&&channelItems.get(src.id);if(item&&typeof item.setOpacity==='function'){const active=channelSourceMatchesActive(src),canvasFilter=channelMaskCanvasFilterActive(src);item.setOpacity((!active||src.visible===false||canvasFilter)?0:clamp(Number(src.opacity??1),0,1));}}\n",
    "function channelPlacementOptions(src){const meta=src.metadata||{},extent=meta.extent||src.extent||null,out={};if(extent&&Number.isFinite(Number(extent.x))&&Number.isFinite(Number(extent.width))){out.x=Number(extent.x)/Math.max(Number(cfg.slide_width)||1,1);out.y=Number(extent.y||0)/Math.max(Number(cfg.slide_width)||1,1);out.width=Number(extent.width)/Math.max(Number(cfg.slide_width)||1,1);}else{out.x=0;out.y=0;out.width=1;}return out;}\n",
    "function upsertChannelSource(src){src=normaliseChannelSource(src);if(!src)return;const idx=channelSources.findIndex(x=>String(x.id)===src.id);if(idx>=0)channelSources[idx]=Object.assign({},channelSources[idx],src);else channelSources.push(src);src=channelSourceById(src.id)||src;const existing=channelItems.get(src.id);if(existing){setChannelItemSettings(src);if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();return;}if(channelPendingItems.has(src.id)){if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();return;}const tileSource=channelTileSource(src);if(!tileSource||!osdViewer||typeof osdViewer.addTiledImage!=='function'){if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();return;}channelPendingItems.add(src.id);const canvasFilter=channelMaskCanvasFilterActive(src);const opts=Object.assign({tileSource:tileSource,opacity:(!channelSourceMatchesActive(src)||src.visible===false||canvasFilter)?0:clamp(Number(src.opacity??1),0,1),success:event=>{const pending=channelPendingItems.has(src.id);channelPendingItems.delete(src.id);const latest=normaliseChannelSource(channelSourceById(src.id)||src);if(!pending||!latest||latest.visible===false||!channelSourceMatchesActive(latest)){try{if(osdViewer&&osdViewer.world&&typeof osdViewer.world.removeItem==='function')osdViewer.world.removeItem(event.item);}catch(e){}buildChannelList();return;}channelItems.set(src.id,event.item);setChannelItemSettings(latest);if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();},error:()=>{channelPendingItems.delete(src.id);notify('Channel '+(src.name||src.id)+' failed to load','warning',3600);}},channelPlacementOptions(src));osdViewer.addTiledImage(opts);if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();}\n",
    "function removeChannelItem(id){id=String(id||'');channelPendingItems.delete(id);const item=channelItems.get(id);if(item&&osdViewer&&osdViewer.world&&typeof osdViewer.world.removeItem==='function'){try{osdViewer.world.removeItem(item);}catch(e){}}channelItems.delete(id);}\n",
    "function clearChannelItems(){Array.from(channelItems.keys()).forEach(removeChannelItem);channelPendingItems.clear();}\n",
    "function removeChannelSource(id){id=String(id||'');removeChannelItem(id);channelSources=channelSources.filter(src=>String(src.id)!==id);if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();scheduleViewerStateSync('channel_source_removed',{id:id});}\n",
    "function channelNeedsReload(src,settings){if(!src||!settings)return false;const nextColour=settings.colour||settings.color;if(nextColour&&String(nextColour)!==String(src.colour||src.color||'#ffffff'))return true;const numericChanged=(key,current)=>typeof settings[key]!=='undefined'&&Number(settings[key])!==Number(current);return numericChanged('gain',src.gain??src.strength??1)||numericChanged('contrast_min',src.contrast_min??0)||numericChanged('contrast_max',src.contrast_max??1);}\n",
    "function reloadChannelSource(src){const id=String(src.id||'');removeChannelItem(id);upsertChannelSource(src);}\n",
    "function setChannelSettings(id,settings={}){id=String(id||'');const src=channelSourceById(id);if(!src)return;const reloadDynamic=src.type==='dynamic'&&channelNeedsReload(src,settings);Object.assign(src,settings);if(typeof settings.visible==='boolean')src.visible=settings.visible;if(typeof settings.opacity!=='undefined')src.opacity=Number(settings.opacity);if(settings.colour||settings.color)src.colour=String(settings.colour||settings.color);if(typeof settings.gain!=='undefined')src.gain=Number(settings.gain);if(typeof settings.contrast_min!=='undefined')src.contrast_min=Number(settings.contrast_min);if(typeof settings.contrast_max!=='undefined')src.contrast_max=Number(settings.contrast_max);if(Array.isArray(settings.selected_values))setChannelMaskSelectedValues(src,settings.selected_values,false);if(reloadDynamic)reloadChannelSource(src);else if(src.visible!==false&&!channelItems.has(src.id)&&!channelPendingItems.has(src.id))upsertChannelSource(src);else setChannelItemSettings(src);if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();if(src.type==='stain'&&stainEnabled){const idx=stainChannels.findIndex(ch=>String(ch.id)===id);if(idx>=0){if(typeof src.visible==='boolean')stainState[idx].visible=src.visible;if(src.colour)stainState[idx].color=src.colour;if(Number.isFinite(src.gain))stainState[idx].strength=src.gain;if(Number.isFinite(src.opacity))stainState[idx].opacity=src.opacity;if(Number.isFinite(src.contrast_min))stainState[idx].contrast_min=src.contrast_min;if(Number.isFinite(src.contrast_max))stainState[idx].contrast_max=src.contrast_max;applyStainPreferences({stain:{enabled:stainOn,channels:stainState.map((s,i)=>Object.assign({id:stainChannels[i].id,name:stainChannels[i].name},s,{color:s.color,strength:s.strength,gain:s.strength}))}});invalidateBaseImage();}}buildChannelList();scheduleViewerStateSync('channel_updated',{id:id,selected_values:channelMaskSelectedValues(src)});requestDraw();}\n",
    "function syncChannelSourcesForActiveImage(){const activeIds=new Set();channelSources.map(normaliseChannelSource).filter(Boolean).forEach(src=>{const active=channelSourceMatchesActive(src);if(active){activeIds.add(src.id);if(channelItems.has(src.id))setChannelItemSettings(src);else upsertChannelSource(src);}else{removeChannelItem(src.id);}});Array.from(channelItems.keys()).forEach(id=>{if(!activeIds.has(id))removeChannelItem(id);});if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();buildChannelList();}\n",
    "function installInitialChannelSources(){clearChannelItems();syncChannelSourcesForActiveImage();}\n",
    "function currentChannelSettingsPayload(){const tileSettings=channelSources.map(src=>({id:String(src.id||''),name:String(src.name||src.id||''),type:String(src.type||'deepzoom'),visible:src.visible!==false,opacity:Number(src.opacity??1),colour:String(src.colour||src.color||'#ffffff'),gain:Number(src.gain??src.strength??1),contrast_min:Number(src.contrast_min??0),contrast_max:Number(src.contrast_max??1),selected_values:channelMaskSelectedValues(src),legend_count:channelLegendEntries(src).length}));if(typeof currentStainPayload==='function'){const stain=currentStainPayload();if(stain&&Array.isArray(stain.channels))stain.channels.forEach(ch=>{if(!tileSettings.some(x=>x.id===String(ch.id)))tileSettings.push({id:String(ch.id),name:String(ch.name||ch.id),type:'stain',visible:ch.visible!==false,opacity:Number(ch.opacity??1),colour:String(ch.colour||ch.color||'#ffffff'),gain:Number(ch.gain??ch.strength??1),contrast_min:Number(ch.contrast_min??0),contrast_max:Number(ch.contrast_max??1)});});}return tileSettings;}\n",
    "function channelSourceLabel(src){return String(src.name||src.id||'channel');}\n",
    "function channelLegendEntries(src){const meta=(src&&src.metadata)||{},legend=meta.legend||meta.labels||src.legend||src.labels||[];if(Array.isArray(legend))return legend.map((entry,i)=>({label:String(entry.label||entry.class_name||entry.class||entry.key||entry.name||('Label '+(i+1))),value:String(entry.value??entry.class_id??entry.id??(i+1)),colour:normaliseHexColour(entry.colour||entry.color||entry.fill||'','#ffffff'),count:entry.count??entry.cell_count??entry.n??null})).filter(x=>x.label&&x.colour);return [];}\n",
    "function channelLegendValueKey(entry){return String(entry&&entry.value!=null?entry.value:(entry&&entry.label)||'');}\n",
    "function channelMaskFilterKey(src){return String(src&&src.id||'');}\n",
    "function channelMaskFilterStateFor(src){src=normaliseChannelSource(src);const key=channelMaskFilterKey(src),entries=channelLegendEntries(src),valid=new Set(entries.map(channelLegendValueKey));let state=channelMaskFilterState.get(key);if(!state){let initial=Array.isArray(src.selected_values)?src.selected_values:(Array.isArray((src.metadata||{}).selected_values)?src.metadata.selected_values:null);const selected=initial?new Set(initial.map(String).filter(v=>valid.has(v))):new Set(valid);state={selected:selected};channelMaskFilterState.set(key,state);}state.selected=new Set(Array.from(state.selected||[]).filter(v=>valid.has(v)));if(valid.size&&!state.selected.size&&src.selected_values==null)state.selected=new Set(valid);src.selected_values=Array.from(state.selected);return state;}\n",
    "function channelMaskSelectedValues(src){if(!src)return[];return Array.from(channelMaskFilterStateFor(src).selected||[]);}\n",
    "function channelMaskCanvasFilterActive(src){const entries=channelLegendEntries(src),meta=(src&&src.metadata)||{};if(!entries.length||!src||src.visible===false)return false;if(meta.kind==='mask'&&meta.transparent_background!==false)return true;const selected=channelMaskFilterStateFor(src).selected;return selected.size<entries.length;}\n",
    "function channelMaskClearProcessedCache(src){const id=channelMaskFilterKey(src);Array.from(channelMaskProcessedCache.keys()).forEach(key=>{if(String(key).indexOf(id+'|')===0)channelMaskProcessedCache.delete(key);});}\n",
    "function setChannelMaskSelectedValues(src,values,sync=true){src=normaliseChannelSource(src);if(!src)return;const entries=channelLegendEntries(src),valid=new Set(entries.map(channelLegendValueKey)),state=channelMaskFilterStateFor(src);state.selected=new Set((Array.isArray(values)?values:[]).map(String).filter(v=>valid.has(v)));src.selected_values=Array.from(state.selected);channelMaskClearProcessedCache(src);setChannelItemSettings(src);if(typeof syncMultiViewChannelSources==='function')syncMultiViewChannelSources();renderChannelLegend();if(typeof requestDraw==='function')requestDraw();else if(typeof draw==='function')draw();if(sync)scheduleViewerStateSync('channel_updated',{id:src.id,selected_values:src.selected_values,legend_count:entries.length});}\n",
    "function setChannelMaskLegendValue(src,value,selected){const state=channelMaskFilterStateFor(src),next=new Set(state.selected||[]);if(selected)next.add(String(value));else next.delete(String(value));setChannelMaskSelectedValues(src,Array.from(next),true);}\n",
    "function setChannelMaskLegendAll(src,selected){const values=selected?channelLegendEntries(src).map(channelLegendValueKey):[];setChannelMaskSelectedValues(src,values,true);}\n",
    "function channelMaskSelectedColours(src){const selected=channelMaskFilterStateFor(src).selected;return channelLegendEntries(src).filter(entry=>selected.has(channelLegendValueKey(entry))).map(entry=>{const h=normaliseHexColour(entry.colour,'#000000').replace('#',''),n=parseInt(h,16);return {hex:'#'+h.toUpperCase(),r:(n>>16)&255,g:(n>>8)&255,b:n&255};});}\n",
    "function channelMaskSelectionSummary(src){const entries=channelLegendEntries(src),selected=channelMaskSelectedValues(src);if(!entries.length)return '';return selected.length+'/'+entries.length+' visible';}\n",
    "function renderChannelLegend(){const panel=el('channelLegendPanel'),list=el('channelLegendList'),summary=el('channelLegendSummary');if(!panel||!list)return;const sources=visibleChannelSources().filter(src=>src.visible!==false&&channelLegendEntries(src).length);list.innerHTML='';if(!sources.length){panel.classList.remove('open');if(summary)summary.textContent='';return;}panel.classList.add('open');const total=sources.reduce((n,src)=>n+channelLegendEntries(src).length,0);if(summary)summary.textContent=total+' label'+(total===1?'':'s')+' from '+sources.length+' mask layer'+(sources.length===1?'':'s');sources.forEach(src=>{const header=document.createElement('div');header.className='sideMeta';header.textContent=channelSourceLabel(src)+' | '+channelMaskSelectionSummary(src);list.appendChild(header);const tools=document.createElement('div');tools.className='channelLegendTools';const all=document.createElement('button');all.type='button';all.textContent='All';all.onclick=()=>setChannelMaskLegendAll(src,true);const none=document.createElement('button');none.type='button';none.textContent='None';none.onclick=()=>setChannelMaskLegendAll(src,false);tools.append(all,none);list.appendChild(tools);const selected=channelMaskFilterStateFor(src).selected,entries=channelLegendEntries(src);entries.slice(0,160).forEach(entry=>{const valueKey=channelLegendValueKey(entry),checked=selected.has(valueKey),row=document.createElement('label');row.className='channelLegendItem'+(checked?'':' disabled');row.title=channelSourceLabel(src)+' | '+entry.label;const box=document.createElement('input');box.type='checkbox';box.checked=checked;box.onchange=e=>setChannelMaskLegendValue(src,valueKey,!!e.target.checked);const sw=document.createElement('span');sw.className='swatch';sw.style.background=entry.colour;const label=document.createElement('span');label.className='legendLabel';label.textContent=entry.label;const value=document.createElement('span');value.className='legendValue';value.textContent=entry.count!=null?Number(entry.count).toLocaleString():entry.value;row.append(box,sw,label,value);list.appendChild(row);});if(entries.length>160){const more=document.createElement('div');more.className='sideMeta';more.textContent='+'+(entries.length-160)+' more labels';list.appendChild(more);}});}\n",
    "function channelControlRow(src,compact=false){src=normaliseChannelSource(src);const row=document.createElement('div');row.className='layerItem channelItem';if(src.visible===false)row.classList.add('hidden');const top=document.createElement('div');top.className='layerTop';const box=document.createElement('input');box.type='checkbox';box.checked=src.visible!==false;box.title='Toggle channel visibility';box.onchange=e=>setChannelSettings(src.id,{visible:!!e.target.checked});const sw=document.createElement('input');sw.type='color';sw.value=src.colour||src.color||'#ffffff';sw.title='Channel colour';sw.onchange=e=>setChannelSettings(src.id,{colour:e.target.value});const nm=document.createElement('span');nm.className='roiName';nm.textContent=channelSourceLabel(src);const meta=document.createElement('span');meta.className='roiClass';meta.textContent=src.type||'channel';top.append(box,sw,nm,meta);const controls=document.createElement('div');controls.className='layerControls';const op=document.createElement('input');op.type='range';op.min='0';op.max='1';op.step='0.05';op.value=String(Number(src.opacity??1));op.title='Channel opacity';op.oninput=e=>setChannelSettings(src.id,{opacity:Number(e.target.value)});controls.append(document.createTextNode('opacity'),op);if(!compact){const gain=document.createElement('input');gain.type='range';gain.min='0';gain.max='5';gain.step='0.05';gain.value=String(Number(src.gain??1));gain.title='Channel gain';gain.onchange=e=>setChannelSettings(src.id,{gain:Number(e.target.value)});const cmin=document.createElement('input');cmin.type='range';cmin.min='0';cmin.max='1';cmin.step='0.01';cmin.value=String(Number(src.contrast_min??0));cmin.title='Contrast minimum';cmin.onchange=e=>setChannelSettings(src.id,{contrast_min:Number(e.target.value)});const cmax=document.createElement('input');cmax.type='range';cmax.min='0.01';cmax.max='1';cmax.step='0.01';cmax.value=String(Number(src.contrast_max??1));cmax.title='Contrast maximum';cmax.onchange=e=>setChannelSettings(src.id,{contrast_max:Number(e.target.value)});controls.append(document.createTextNode(' gain'),gain,document.createTextNode(' min'),cmin,document.createTextNode(' max'),cmax);}row.append(top,controls);return row;}\n",
    "function buildChannelList(){const list=el('channelList'),summary=el('channelSummary'),menuList=el('channelMenuList'),menuSummary=el('channelMenuSummary'),sources=visibleChannelSources();const count=sources.length,visible=sources.filter(s=>s.visible!==false).length,total=channelSources.length,hidden=total-count;if(summary)summary.textContent=count?(visible+'/'+count+' channel overlays visible'):(hidden?'No image channels for the current image.':'No image channel overlays.');if(menuSummary)menuSummary.textContent=count?(visible+'/'+count+' overlay channels visible'+(hidden?(' | '+hidden+' hidden for this image'):'')):(hidden?'No image channels for the current image.':'No tiled mIHC/image channels configured.');if(list){list.innerHTML='';sources.forEach(src=>list.appendChild(channelControlRow(src,false)));}if(menuList){menuList.innerHTML='';sources.forEach(src=>menuList.appendChild(channelControlRow(src,true)));}renderChannelLegend();}\n"
  )
}

wsi_viewer_sync_js <- function() {
  paste0(
    "let stateSyncTimer=null,stateSyncEvent='viewer_state',stateSyncDetail={},stateSyncSeq=0,lastSyncedSelectedRoi=-2;\n",
    "let viewerSyncHistory=[];\n",
    "let stateSocket=null,stateSocketReady=false,stateSocketReconnectTimer=null,stateSocketFallbackNotified=false;\n",
    "function liveSyncAvailable(){return !!(cfg.viewer_state_url||cfg.viewer_state_ws_url||'');}\n",
    "function syncMessage(msg){const text=msg||(liveSyncAvailable()?'R sync ready':'R sync off');const box=el('syncSummary');if(box)box.textContent=text;if(typeof updateJobSyncIndicator==='function')updateJobSyncIndicator(null,text);}\n",
    "function recordViewerSyncHistory(direction,event,detail={}){const entry={time:new Date().toISOString(),direction:String(direction||'sync'),event:String(event||''),detail:detail||{}};viewerSyncHistory.unshift(entry);if(viewerSyncHistory.length>300)viewerSyncHistory.length=300;return entry;}\n",
    "function roiGeojsonObject(filterFn=null){const features=[];rois.forEach((roi,i)=>{if(filterFn&&!filterFn(roi,i))return;const feature=roiFeature(roi,i);if(feature)features.push(feature);});return {type:'FeatureCollection',features:features};}\n",
    "function selectedRoiFeatureObject(){if(selectedRoi<0||!rois[selectedRoi])return null;return roiFeature(rois[selectedRoi],selectedRoi);}\n",
    "function selectedRoisGeojsonObject(){const features=[];const indices=(typeof roiExportIndices==='function')?roiExportIndices():[];indices.forEach(i=>{if(i>=0&&rois[i]){const feature=roiFeature(rois[i],i);if(feature)features.push(feature);}});return {type:'FeatureCollection',features:features};}\n",
    "function segmentationGeojsonObject(){return roiGeojsonObject(roi=>{const source=String(roi.source||'').toLowerCase(),cls=String(roi.class||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells';});}\n",
    "function currentStainPayload(){if(!stainEnabled)return null;syncStainStateFromControls();return {enabled:stainOn,channels:stainChannels.map((ch,i)=>({id:ch.id,name:ch.name,type:'stain',visible:!!(stainState[i]&&stainState[i].visible),color:stainState[i]?stainState[i].color:ch.colour,colour:stainState[i]?stainState[i].color:ch.colour,strength:stainState[i]?stainState[i].strength:ch.strength,gain:stainState[i]?stainState[i].strength:ch.strength,opacity:stainState[i]?stainState[i].opacity:(ch.opacity??1),contrast_min:stainState[i]?stainState[i].contrast_min:(ch.contrast_min??0),contrast_max:stainState[i]?stainState[i].contrast_max:(ch.contrast_max??1)}))};}\n",
    "function viewerStatePayload(event,detail={}){return {event:event||'viewer_state',time:new Date().toISOString(),sequence:++stateSyncSeq,slide:{title:cfg.title,width:cfg.slide_width,height:cfg.slide_height},project:(typeof projectStatePayload==='function'?projectStatePayload():null),selected_index:selectedRoi,selected_object:(typeof selectedObjectPayload==='function'?selectedObjectPayload():null),selected_roi:selectedRoiFeatureObject(),selected_rois:selectedRoisGeojsonObject(),rois:roiGeojsonObject(),segmentation:segmentationGeojsonObject(),layers:layerStatePayload(),measurements:measures,trajectories:(typeof trajectoryPayload==='function'?trajectoryPayload():[]),artifacts:(typeof artifactPayload==='function'?artifactPayload():[]),view:{mode:mode,scale:scale,offset_x:offsetX,offset_y:offsetY,roi_opacity:roiOpacity,show_rois:showRois,show_labels:showLabels,image_transform:(typeof imageTransformPayload==='function'?imageTransformPayload():null),base_layer:baseImagePayload()},annotations:{dirty:!!(annotationsDirty||projectDirty),dirty_reason:annotationDirtyReason||projectDirtyReason,annotation_dirty:annotationsDirty,project_dirty:projectDirty},history:annotationHistoryPayload(),logs:(typeof viewerLogPayload==='function'?viewerLogPayload():[]),stain:currentStainPayload(),channel_sources:channelSources,channel_settings:(typeof currentChannelSettingsPayload==='function'?currentChannelSettingsPayload():[]),tile_sources:cfg.tile_sources||[],kodama_selection:(typeof kodamaSelectionPayload==='function'?kodamaSelectionPayload():null),seurat_selection:(typeof seuratSelectionPayload==='function'?seuratSelectionPayload():null),annotation_spots:(typeof annotationSpotAssociationPayload==='function'?annotationSpotAssociationPayload():[]),detail:detail};}\n",
    "let stateCommandPollTimer=null,stateCommandSeen=new Set(),viewerAutosaveTimer=null,viewerAutosaveLastError='';\n",
    "function handleViewerCommand(command){if(!command||!command.id||stateCommandSeen.has(command.id))return;stateCommandSeen.add(command.id);const payload=command.payload||{},geojson=payload.geojson||payload;recordViewerSyncHistory('from_R',command.type||'command',{id:command.id,name:payload.name||payload.id||'',count:payload.count||payload.added||null});if(command.type==='job_update'){if(typeof upsertViewerJob==='function')upsertViewerJob(payload.job||payload);syncMessage('R command: job update');return;}if(command.type==='add_rois'){if(typeof addImportedGeojson==='function')addImportedGeojson(geojson,payload.name||'R session');syncMessage('R command: added ROIs');return;}if(command.type==='add_segmentation'){if(typeof addSegmentationGeojson==='function')addSegmentationGeojson(geojson,{local:false,detail:{source:payload.name||'R session'}});syncMessage('R command: added segmentation');return;}if(command.type==='add_layer'){if(typeof upsertViewerLayer==='function')upsertViewerLayer(payload.layer||payload);syncMessage('R command: added layer');return;}if(command.type==='set_layer_visible'){if(typeof setViewerLayerVisible==='function')setViewerLayerVisible(payload.id||payload.name,payload.visible);syncMessage('R command: layer visibility');return;}if(command.type==='remove_layer'){if(typeof removeViewerLayer==='function')removeViewerLayer(payload.id||payload.name);syncMessage('R command: removed layer');return;}if(command.type==='add_channel_source'){if(typeof upsertChannelSource==='function')upsertChannelSource(payload.source||payload);syncMessage('R command: added channel');return;}if(command.type==='remove_channel_source'){if(typeof removeChannelSource==='function')removeChannelSource(payload.id);syncMessage('R command: removed channel');return;}if(command.type==='set_channel_settings'){if(typeof setChannelSettings==='function')setChannelSettings(payload.id,payload.settings||payload);syncMessage('R command: channel settings');return;}if(command.type==='colour_spots_by_gene'){if(typeof applySeuratGeneColour==='function'){applySeuratGeneColour(payload.gene||payload.name||'',true);syncMessage('R command: colour spots by gene');}else{notify('No spatial spot gene colouring is available in this viewer','warning',4200);}return;}if(command.type==='restore_project_state'){if(payload.rois&&typeof addImportedGeojson==='function')addImportedGeojson(payload.rois,'restored project');if(Array.isArray(payload.trajectories)){trajectories.splice(0,trajectories.length);payload.trajectories.forEach(t=>trajectories.push(t));selectedTrajectory=trajectories.length?0:-1;if(typeof updateTrajectoryList==='function')updateTrajectoryList();}if(payload.segmentation&&typeof addSegmentationGeojson==='function')addSegmentationGeojson(payload.segmentation,{local:false,detail:{source:'restored project'}});if(Array.isArray(payload.channel_sources)&&typeof upsertChannelSource==='function')payload.channel_sources.forEach(upsertChannelSource);if(Array.isArray(payload.channel_settings)&&typeof setChannelSettings==='function')payload.channel_settings.forEach(s=>setChannelSettings(s.id,s));if(payload.stain&&typeof applyStainPreferences==='function')applyStainPreferences({stain:payload.stain});syncMessage('R command: project restored');draw();return;}if(command.type==='annotations_saved'){markAnnotationsSaved(payload.reason||'project_saved');syncMessage('R command: annotations saved');return;}console.warn('Unknown wsiTools viewer command',command.type);}\n",
    "function handleViewerCommands(body){if(typeof handleViewerJobs==='function')handleViewerJobs(body);const commands=(body&&body.commands)||[];if(Array.isArray(commands))commands.forEach(handleViewerCommand);}\n",
    "function viewerAutosaveMessage(body){const autosave=body&&body.autosave;if(!autosave||!autosave.enabled)return '';if(autosave.last_error){const msg='Autosave failed: '+autosave.last_error;if(viewerAutosaveLastError!==autosave.last_error){viewerAutosaveLastError=autosave.last_error;notify(msg,'error',5200);}return msg;}viewerAutosaveLastError='';if(autosave.last_save&&autosave.count>0){const t=new Date(autosave.last_save);const stamp=Number.isNaN(t.getTime())?autosave.last_save:t.toLocaleTimeString();return 'Autosaved '+stamp;}return 'Autosave ready';}\n",
    "function viewerSocketSend(event='viewer_state',detail={}){if(!stateSocketReady||!stateSocket||stateSocket.readyState!==WebSocket.OPEN)return false;try{stateSocket.send(JSON.stringify(viewerStatePayload(event,detail)));recordViewerSyncHistory('to_R',event,Object.assign({transport:'websocket'},detail||{}));syncMessage('R sync: '+event+' via WebSocket');return true;}catch(e){stateSocketReady=false;recordViewerSyncHistory('error',event,{transport:'websocket',message:e.message});return false;}}\n",
    "function scheduleViewerSocketReconnect(){const url=cfg.viewer_state_ws_url||'';if(!url||stateSocketReconnectTimer)return;stateSocketReconnectTimer=setTimeout(()=>{stateSocketReconnectTimer=null;startViewerStateSocket();},2000);}\n",
    "function startViewerStateSocket(){const url=cfg.viewer_state_ws_url||'';if(!url||typeof WebSocket==='undefined'||stateSocket||stateSocketReconnectTimer)return false;try{stateSocket=new WebSocket(url);}catch(e){stateSocket=null;recordViewerSyncHistory('error','websocket_open',{message:e.message});startViewerCommandPolling();return false;}stateSocket.onopen=()=>{stateSocketReady=true;stateSocketFallbackNotified=false;recordViewerSyncHistory('status','websocket_connected',{});syncMessage('R sync: WebSocket connected');};stateSocket.onmessage=event=>{try{const body=JSON.parse(event.data);if(body&&body.ok===false&&body.error){recordViewerSyncHistory('error','websocket_message',{message:body.error});syncMessage('R sync failed: '+body.error);notify('R sync failed: '+body.error,'error',4200);return;}handleViewerCommands(body);syncMessage(viewerAutosaveMessage(body)||'R sync: WebSocket');}catch(e){recordViewerSyncHistory('error','websocket_parse',{message:e.message});console.warn('Could not parse wsiTools WebSocket message',e);}};stateSocket.onclose=()=>{stateSocketReady=false;stateSocket=null;recordViewerSyncHistory('status','websocket_closed',{});if(!stateSocketFallbackNotified){stateSocketFallbackNotified=true;syncMessage('R sync: WebSocket unavailable; polling fallback active');}startViewerCommandPolling();scheduleViewerSocketReconnect();};stateSocket.onerror=()=>{stateSocketReady=false;recordViewerSyncHistory('error','websocket_error',{});try{if(stateSocket)stateSocket.close();}catch(e){}};return true;}\n",
    "async function syncViewerState(event='viewer_state',detail={}){if(viewerSocketSend(event,detail))return true;const url=cfg.viewer_state_url||'';if(!url)return false;try{recordViewerSyncHistory('to_R',event,Object.assign({transport:'polling'},detail||{}));const response=await fetch(url,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(viewerStatePayload(event,detail))});if(!response.ok){const text=await response.text();throw new Error(text||('HTTP '+response.status));}let body=null;try{body=await response.json();}catch(e){}handleViewerCommands(body);syncMessage(viewerAutosaveMessage(body)||('R sync: '+event));return true;}catch(e){recordViewerSyncHistory('error',event,{transport:'polling',message:e.message});syncMessage('R sync failed: '+e.message);return false;}}\n",
    "async function pollViewerCommands(){if(stateSocketReady)return;const url=cfg.viewer_state_url||'';if(!url)return;try{const response=await fetch(url,{method:'GET',headers:{'Accept':'application/json'}});if(response.ok)handleViewerCommands(await response.json());}catch(e){}}\n",
    "function startViewerCommandPolling(){if(!(cfg.viewer_state_url||'')||stateCommandPollTimer)return;stateCommandPollTimer=setInterval(pollViewerCommands,1000);}\n",
    "function startViewerAutosave(){if(!liveSyncAvailable()||!cfg.autosave_enabled||viewerAutosaveTimer)return;const interval=Math.max(1000,Number(cfg.autosave_interval_ms||5000));viewerAutosaveTimer=setInterval(()=>syncViewerState('autosave_tick',{autosave:true,path:cfg.autosave_path||null}),interval);window.addEventListener('beforeunload',()=>{try{const payload=JSON.stringify(viewerStatePayload('autosave_unload',{autosave:true,path:cfg.autosave_path||null}));const url=cfg.viewer_state_url||'';if(url&&navigator.sendBeacon){navigator.sendBeacon(url,new Blob([payload],{type:'application/json'}));}else if(url){fetch(url,{method:'POST',headers:{'Content-Type':'application/json'},body:payload,keepalive:true});}else if(stateSocketReady&&stateSocket){stateSocket.send(payload);}}catch(e){}});syncMessage('Autosave ready');}\n",
    "function syncRoiSelection(reason='selection'){if(!liveSyncAvailable())return;const current=Number.isFinite(Number(selectedRoi))?Number(selectedRoi):-1;if(current===lastSyncedSelectedRoi)return;lastSyncedSelectedRoi=current;if(current>=0&&rois[current])scheduleViewerStateSync('roi_selected',{index:current,id:rois[current].id||null,reason:reason});}\n",
    "function scheduleViewerStateSync(event='viewer_state',detail={}){if(!liveSyncAvailable()){syncMessage('');return;}stateSyncEvent=event;stateSyncDetail=detail||{};clearTimeout(stateSyncTimer);stateSyncTimer=setTimeout(()=>syncViewerState(stateSyncEvent,stateSyncDetail),250);}\n"
  )
}

wsi_viewer_shortcuts_js <- function() {
  paste0(
    "let shortcutHelpOpen=false;\n",
    "function shortcutTypingTarget(target){return !!(target&&['INPUT','TEXTAREA','SELECT'].includes(target.tagName));}\n",
    "function shortcutHelpTitleFor(sectionId){return sectionId==='helpFullGuide'?'Full Guide':'Keyboard Shortcuts';}\n",
    "function setShortcutHelpSection(sectionId='helpKeyboardShortcuts'){sectionId=sectionId==='helpFullGuide'?'helpFullGuide':'helpKeyboardShortcuts';const parts=['helpKeyboardShortcuts','helpFullGuide','helpQuickRecommendations'];parts.forEach(id=>{const part=el(id);if(!part)return;const show=id===sectionId||(sectionId==='helpFullGuide'&&id==='helpQuickRecommendations');part.hidden=!show;part.setAttribute('aria-hidden',show?'false':'true');});const keyboard=el('shortcutHelpTabKeyboard'),full=el('shortcutHelpTabFull');if(keyboard){keyboard.classList.toggle('active',sectionId==='helpKeyboardShortcuts');keyboard.setAttribute('aria-selected',sectionId==='helpKeyboardShortcuts'?'true':'false');}if(full){full.classList.toggle('active',sectionId==='helpFullGuide');full.setAttribute('aria-selected',sectionId==='helpFullGuide'?'true':'false');}const title=el('shortcutHelpTitle'),hint=document.querySelector('#shortcutHelp .shortcutHelpHint');if(title)title.textContent=shortcutHelpTitleFor(sectionId);if(hint)hint.textContent=sectionId==='helpFullGuide'?'Full viewer guide and quick recommendations. Use the tabs to switch help sections.':'Keyboard shortcuts only. Use the tabs to switch help sections.';const help=el('shortcutHelp');if(help)help.scrollTop=0;return sectionId;}\n",
    "function openShortcutHelp(sectionId='helpKeyboardShortcuts'){if(typeof closeCommandPalette==='function')closeCommandPalette();document.querySelectorAll('.toolMenu').forEach(menu=>menu.open=false);const help=el('shortcutHelp'),backdrop=el('shortcutHelpBackdrop');if(!help)return;shortcutHelpOpen=true;setShortcutHelpSection(sectionId);help.classList.add('open');help.setAttribute('aria-hidden','false');if(backdrop)backdrop.classList.add('open');}\n",
    "function closeShortcutHelp(){const help=el('shortcutHelp'),backdrop=el('shortcutHelpBackdrop');shortcutHelpOpen=false;if(help){help.classList.remove('open');help.setAttribute('aria-hidden','true');}if(backdrop)backdrop.classList.remove('open');}\n",
    "function toggleShortcutHelp(){if(shortcutHelpOpen)closeShortcutHelp();else openShortcutHelp('helpKeyboardShortcuts');}\n",
    "function shortcutImportGeojson(){const b=el('importGeojson'),f=el('geojsonImportFile');if(b)b.click();else if(f){f.value='';f.click();}}\n",
    "function bindShortcutHelp(){const button=el('shortcutHelpButton'),keyboard=el('shortcutHelpKeyboard'),close=el('shortcutHelpClose'),backdrop=el('shortcutHelpBackdrop'),tabKeyboard=el('shortcutHelpTabKeyboard'),tabFull=el('shortcutHelpTabFull');if(keyboard)keyboard.onclick=()=>openShortcutHelp('helpKeyboardShortcuts');if(button)button.onclick=()=>openShortcutHelp('helpFullGuide');if(tabKeyboard)tabKeyboard.onclick=()=>setShortcutHelpSection('helpKeyboardShortcuts');if(tabFull)tabFull.onclick=()=>setShortcutHelpSection('helpFullGuide');if(close)close.onclick=closeShortcutHelp;if(backdrop)backdrop.onclick=closeShortcutHelp;setShortcutHelpSection('helpKeyboardShortcuts');document.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase(),typing=shortcutTypingTarget(e.target),modifier=e.ctrlKey||e.metaKey;if(shortcutHelpOpen){if(e.key==='Escape'||key==='?'){e.preventDefault();e.stopPropagation();closeShortcutHelp();}return;}if(typing)return;if((e.key==='?'||(e.shiftKey&&key==='/'))&&!modifier&&!e.altKey){e.preventDefault();e.stopPropagation();openShortcutHelp('helpKeyboardShortcuts');return;}if(modifier&&!e.shiftKey&&key==='s'){e.preventDefault();e.stopPropagation();if(typeof saveGeojson==='function')saveGeojson();return;}if(modifier&&!e.shiftKey&&key==='i'){e.preventDefault();e.stopPropagation();shortcutImportGeojson();return;}if(modifier&&!e.shiftKey&&key==='e'){e.preventDefault();e.stopPropagation();if(typeof exportSelectedAnnotations==='function')exportSelectedAnnotations();return;}if(typeof commandPaletteOpen!=='undefined'&&commandPaletteOpen)return;if(!modifier&&!e.altKey&&!e.shiftKey&&(key==='p'||e.code==='Space')){e.preventDefault();e.stopPropagation();setMode('pan');return;}},true);}\n"
  )
}

wsi_viewer_command_palette_js <- function() {
  paste0(
    "let commandPaletteOpen=false,commandPaletteActive=0,tileGridVisible=false;\n",
    "function tileGridSize(){return Math.max(1,Number(cfg.tile_size||512));}\n",
    "function canvasToSlidePoint(x,y){let out;if(typeof osdReady!=='undefined'&&osdReady&&typeof osdItem==='function'&&osdItem()&&typeof OpenSeadragon!=='undefined'){const p=(typeof overlayPixelForOsdDisplay==='function')?overlayPixelForOsdDisplay({x:x,y:y}):{x:x,y:y},vp=osdViewer.viewport.pointFromPixel(new OpenSeadragon.Point(p.x,p.y),true),img=osdItem().viewportToImageCoordinates(vp);out={x:img.x,y:img.y};}else if(typeof image!=='undefined'&&image.naturalWidth&&typeof imageToSlide==='function'){const p={x:(x-offsetX)/scale,y:(y-offsetY)/scale};out=imageToSlide((typeof viewToImagePoint==='function')?viewToImagePoint(p):p);}else out={x:(x-offsetX)/scale,y:(y-offsetY)/scale};return (typeof normaliseSlidePoint==='function')?normaliseSlidePoint(out):out;}\n",
    "function visibleSlideBounds(){const pts=[canvasToSlidePoint(0,0),canvasToSlidePoint(innerWidth,0),canvasToSlidePoint(0,innerHeight),canvasToSlidePoint(innerWidth,innerHeight)].filter(p=>p&&Number.isFinite(p.x)&&Number.isFinite(p.y));if(!pts.length)return {xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};return {xmin:clamp(Math.min(...pts.map(p=>p.x)),0,cfg.slide_width),ymin:clamp(Math.min(...pts.map(p=>p.y)),0,cfg.slide_height),xmax:clamp(Math.max(...pts.map(p=>p.x)),0,cfg.slide_width),ymax:clamp(Math.max(...pts.map(p=>p.y)),0,cfg.slide_height)};}\n",
    "function drawTileGrid(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('tile_grid'))return;if(!tileGridVisible)return;const b=visibleSlideBounds();let step=tileGridSize(),base=step,a=slideToCanvas({x:0,y:0}),c=slideToCanvas({x:step,y:0}),spacing=Math.abs(c.x-a.x);while(Number.isFinite(spacing)&&spacing>0&&spacing<22&&step<base*64){step*=2;spacing*=2;}const x0=Math.floor(b.xmin/step)*step,y0=Math.floor(b.ymin/step)*step;ctx.save();ctx.strokeStyle='rgba(250,204,21,.55)';ctx.lineWidth=1;ctx.setLineDash([4,5]);ctx.beginPath();for(let x=x0;x<=b.xmax+step;x+=step){const p0=slideToCanvas({x:x,y:b.ymin}),p1=slideToCanvas({x:x,y:b.ymax});ctx.moveTo(p0.x,p0.y);ctx.lineTo(p1.x,p1.y);}for(let y=y0;y<=b.ymax+step;y+=step){const p0=slideToCanvas({x:b.xmin,y:y}),p1=slideToCanvas({x:b.xmax,y:y});ctx.moveTo(p0.x,p0.y);ctx.lineTo(p1.x,p1.y);}ctx.stroke();ctx.setLineDash([]);ctx.fillStyle='rgba(250,204,21,.9)';ctx.font='11px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.fillText('tile grid '+Math.round(tileGridSize())+' px',12,innerHeight-18);ctx.restore();}\n",
    "function toggleTileGrid(){tileGridVisible=!tileGridVisible;scheduleViewerStateSync('tile_grid_toggled',{visible:tileGridVisible,tile_size:tileGridSize()});notify(tileGridVisible?'Tile grid shown':'Tile grid hidden','success');draw();}\n",
    "async function requestProjectSave(){if(!liveSyncAvailable()){notify('Live R sync is off; use Save project in the Project menu to download a project file, or use viewer$save_project(...) in R.','warning',5200);return false;}const snapshot=(typeof projectBrowserSnapshot==='function')?projectBrowserSnapshot(false):null;const ok=await syncViewerState('project_save_requested',{dirty:annotationsDirty,reason:annotationDirtyReason,project_snapshot:snapshot});notify(ok?'Project save requested in R':'Project save request failed',ok?'success':'error',3600);return !!ok;}\n",
    "async function saveFromUnsavedIndicator(event){if(event){event.preventDefault();event.stopPropagation();}if(!(annotationsDirty||projectDirty)){notify('Project is already saved','info',1600);return false;}if(liveSyncAvailable())return requestProjectSave();if(typeof saveProjectFile==='function')return saveProjectFile();notify('Project saving is not available in this viewer','warning',3600);return false;}\n",
    "function bindUnsavedIndicator(){const node=el('annotationDirtyIndicator');if(!node||node.dataset.bound==='1')return;node.dataset.bound='1';node.onclick=saveFromUnsavedIndicator;node.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){saveFromUnsavedIndicator(e);}};}\n",
    "function commandPaletteDefinitions(){return [{id:'open_project_panel',label:'Open project panel',hint:'Show the left Project panel',kbd:'Project',enabled:()=>typeof openProjectPanel==='function',run:()=>openProjectPanel()},{id:'add_project_image',label:'Add project image',hint:'Add browser-readable images or WSI/microscopy file references to this viewer project',kbd:'file',enabled:()=>!!el('projectImageFile'),run:()=>{const b=el('projectOpenImage'),f=el('projectImageFile');if(b)b.click();else if(f)f.click();}},{id:'new_roi',label:'New ROI',hint:'Deselect current annotation and start painting a separate ROI',kbd:'N',enabled:()=>true,run:()=>startNewAnnotation('brush')},{id:'draw_trajectory',label:'Draw trajectory',hint:'Click control points to create a smoothed path synced to R',kbd:'T',enabled:()=>true,run:()=>setMode('trajectory')},{id:'import_geojson',label:'Import GeoJSON',hint:'Load QuPath or wsiTools annotations',kbd:'file',enabled:()=>!!el('geojsonImportFile'),run:()=>{const b=el('importGeojson'),f=el('geojsonImportFile');if(b)b.click();else if(f)f.click();}},{id:'export_selected_rois',label:'Export selected ROIs',hint:'Download checked or selected ROI annotations',kbd:'GeoJSON',enabled:()=>typeof roiExportIndices==='function'&&roiExportIndices().length>0,run:()=>exportSelectedAnnotations()},{id:'load_grandqc_artifacts',label:'Load GrandQC artifacts',hint:'Import GrandQC artifact GeoJSON annotations from the CellPhenotyper project',kbd:'QC',enabled:()=>typeof loadAllGrandqcGeojsons==='function'&&grandqcItems().length>0,run:()=>loadAllGrandqcGeojsons()},{id:'show_tile_grid',label:(tileGridVisible?'Hide tile grid':'Show tile grid'),hint:'Overlay a coordinate-only tile grid; no pixels are read',kbd:Math.round(tileGridSize())+' px',enabled:()=>true,run:()=>toggleTileGrid()},{id:'save_project',label:'Save project',hint:'Sync viewer state and request project saving in R',kbd:'R',enabled:()=>liveSyncAvailable(),run:()=>requestProjectSave()}];}\n",
    "function commandPaletteQuery(){const input=el('commandPaletteSearch');return String(input&&input.value||'').trim().toLowerCase();}\n",
    "function commandPaletteItems(){const q=commandPaletteQuery();return commandPaletteDefinitions().filter(item=>!cfg.managed_analysis_project||item.id!=='add_project_image').filter(item=>!q||[item.label,item.hint,item.id].join(' ').toLowerCase().includes(q));}\n",
    "function updateCommandPaletteActive(){document.querySelectorAll('.commandItem').forEach((item,i)=>item.classList.toggle('active',i===commandPaletteActive));}\n",
    "function runCommandPaletteItem(index=commandPaletteActive){const items=commandPaletteItems(),item=items[index];if(!item)return;if(!item.enabled()){notify(item.label+' is not available right now','warning',3600);return;}closeCommandPalette();item.run();}\n",
    "function renderCommandPalette(){const list=el('commandPaletteList');if(!list)return;const items=commandPaletteItems();commandPaletteActive=clamp(commandPaletteActive,0,Math.max(0,items.length-1));list.innerHTML='';if(!items.length){const empty=document.createElement('div');empty.className='commandItem';empty.textContent='No commands found';list.appendChild(empty);return;}items.forEach((item,i)=>{const button=document.createElement('button');button.type='button';button.className='commandItem';button.disabled=!item.enabled();button.setAttribute('role','option');button.setAttribute('aria-selected',i===commandPaletteActive?'true':'false');const label=document.createElement('span');label.className='commandLabel';label.textContent=item.label;const kbd=document.createElement('span');kbd.className='commandKbd';kbd.textContent=item.kbd||'';const meta=document.createElement('span');meta.className='commandMeta';meta.textContent=item.hint||'';button.append(label,kbd,meta);button.onmouseenter=()=>{commandPaletteActive=i;updateCommandPaletteActive();};button.onclick=()=>runCommandPaletteItem(i);list.appendChild(button);});updateCommandPaletteActive();}\n",
    "function openCommandPalette(){if(typeof closeShortcutHelp==='function')closeShortcutHelp();document.querySelectorAll('.toolMenu').forEach(menu=>menu.open=false);const palette=el('commandPalette'),backdrop=el('commandPaletteBackdrop'),input=el('commandPaletteSearch');if(!palette)return;commandPaletteOpen=true;commandPaletteActive=0;palette.classList.add('open');palette.setAttribute('aria-hidden','false');if(backdrop)backdrop.classList.add('open');if(input){input.value='';setTimeout(()=>input.focus(),0);}renderCommandPalette();}\n",
    "function closeCommandPalette(){const palette=el('commandPalette'),backdrop=el('commandPaletteBackdrop');commandPaletteOpen=false;if(palette){palette.classList.remove('open');palette.setAttribute('aria-hidden','true');}if(backdrop)backdrop.classList.remove('open');}\n",
    "function toggleCommandPalette(){if(commandPaletteOpen)closeCommandPalette();else openCommandPalette();}\n",
    "function bindCommandPalette(){const input=el('commandPaletteSearch'),backdrop=el('commandPaletteBackdrop');if(input){input.addEventListener('input',()=>{commandPaletteActive=0;renderCommandPalette();});input.addEventListener('keydown',e=>e.stopPropagation());}if(backdrop)backdrop.onclick=closeCommandPalette;document.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase();if((e.ctrlKey||e.metaKey)&&key==='k'){e.preventDefault();e.stopPropagation();toggleCommandPalette();return;}if(!commandPaletteOpen)return;if(e.key==='Escape'){e.preventDefault();e.stopPropagation();closeCommandPalette();return;}if(e.key==='ArrowDown'){e.preventDefault();e.stopPropagation();const n=commandPaletteItems().length;commandPaletteActive=n?(commandPaletteActive+1)%n:0;updateCommandPaletteActive();return;}if(e.key==='ArrowUp'){e.preventDefault();e.stopPropagation();const n=commandPaletteItems().length;commandPaletteActive=n?(commandPaletteActive+n-1)%n:0;updateCommandPaletteActive();return;}if(e.key==='Enter'){e.preventDefault();e.stopPropagation();runCommandPaletteItem();}},true);}\n"
  )
}

wsi_viewer_artifact_js <- function() {
  paste0(
    "function grandqcConfig(){const cp=(typeof cellphenotyperConfig==='function')?cellphenotyperConfig():(cfg.cellphenotyper||{});return cp.grandqc||{enabled:false,geojsons:[]};}\n",
    "function grandqcItems(){const items=grandqcConfig().geojsons||[];return Array.isArray(items)?items:[];}\n",
    "function grandqcSource(item){return 'GrandQC: '+String((item&&(item.label||item.id))||'artifact GeoJSON');}\n",
    "function artifactStatus(msg){const box=el('artifactSummary');if(box)box.textContent=msg||'';}\n",
    "function isGrandqcRoi(roi,id=null){if(!roi)return false;const props=roi.properties||{},source=String(roi.source||'');if(id&&String(props.grandqc_id||'')===String(id))return true;return roi.grandqc===true||source.indexOf('GrandQC:')===0||props.source_menu==='GrandQC';}\n",
    "function artifactPayload(){return rois.filter(r=>isGrandqcRoi(r)).map((roi,i)=>({id:roi.id||('grandqc_'+(i+1)),name:roi.name||roi.label||'',class:roi.class||'',source:'grandqc',bbox:(typeof roiBounds==='function')?roiBounds(roi):null}));}\n",
    "function clearGrandqcRois(redraw=true){let removed=0;for(let i=rois.length-1;i>=0;i--){if(isGrandqcRoi(rois[i])){if(!removed)pushAnnotationUndo('grandqc_cleared');rois.splice(i,1);removed++;}}if(selectedRoi>=rois.length)selectedRoi=rois.length-1;if(removed){recordAnnotationHistory('grandqc_cleared',{removed:removed});scheduleViewerStateSync('grandqc_cleared',{removed:removed});markAnnotationsDirty('grandqc_cleared');}if(redraw){buildRoiList();updateButtons();draw();artifactStatus(removed?('Removed '+removed+' GrandQC ROI'+(removed===1?'':'s')+'.'):'No GrandQC annotations were loaded.');}return removed;}\n",
    "function tagGrandqcRois(start,item){const source=grandqcSource(item);let tagged=0;for(let i=start;i<rois.length;i++){const roi=rois[i];if(!roi)continue;const props=roi.properties||{},cls=importedFeatureClass(props);roi.grandqc=true;roi.source=source;roi.class=cls&&cls!=='annotation'?cls:'artefact';roi.properties=props;roi.properties.source_menu='GrandQC';roi.properties.grandqc_id=item&&item.id||null;roi.properties.grandqc_path=item&&item.path||null;if(!roi.colour){roi.colour=classColour(roi.class,'#ef4444');roi.fill=hexToRgba(roi.colour,.18);}tagged++;}return tagged;}\n",
    "function appendGrandqcGeojson(item){if(!item||!item.geojson){notify('No GrandQC GeoJSON is available for this entry','warning');return 0;}const before=rois.length;addImportedGeojson(item.geojson,grandqcSource(item));return tagGrandqcRois(before,item);}\n",
    "function loadGrandqcGeojson(index){const item=grandqcItems()[Number(index)];if(!item){artifactStatus('GrandQC entry not found.');return;}clearGrandqcRois(false);const added=appendGrandqcGeojson(item);buildRoiList();updateButtons();draw();artifactStatus('Loaded '+added+' ROI'+(added===1?'':'s')+' from '+(item.label||item.id||'GrandQC')+'.');recordAnnotationHistory('grandqc_loaded',{added:added,label:item.label||item.id||'GrandQC'});scheduleViewerStateSync('grandqc_loaded',{added:added,id:item.id||null,label:item.label||null});notify('GrandQC GeoJSON loaded','success');}\n",
    "function loadAllGrandqcGeojsons(){const items=grandqcItems();if(!items.length){artifactStatus('No GrandQC GeoJSON was found in this project.');notify('No GrandQC GeoJSON found','warning');return;}clearGrandqcRois(false);let added=0;items.forEach(item=>{added+=appendGrandqcGeojson(item);});buildRoiList();updateButtons();draw();artifactStatus('Loaded '+added+' GrandQC ROI'+(added===1?'':'s')+' from '+items.length+' file'+(items.length===1?'':'s')+'.');recordAnnotationHistory('grandqc_loaded',{added:added,files:items.length});scheduleViewerStateSync('grandqc_loaded',{added:added,files:items.length});notify('GrandQC artifact annotations loaded','success');}\n",
    "function drawArtifactOverlays(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('artifacts'))return;}\n",
    "function bindArtifactControls(){const items=grandqcItems(),loadAll=el('grandqcLoadAll'),clear=el('grandqcClear');document.querySelectorAll('.grandqcLoad').forEach(button=>{button.onclick=()=>loadGrandqcGeojson(button.dataset.grandqcIndex);button.disabled=!items.length;});if(loadAll){loadAll.onclick=loadAllGrandqcGeojsons;loadAll.disabled=!items.length;}if(clear){clear.onclick=()=>clearGrandqcRois(true);clear.disabled=!items.length;}artifactStatus(items.length?(items.length+' GrandQC GeoJSON file'+(items.length===1?'':'s')+' available.'):'No GrandQC GeoJSON was found for this project.');}\n"
  )
}

wsi_viewer_image_transform_js <- function() {
  paste0(
    "let imageRotation=0,imageFlipX=false,imageFlipY=false;\n",
    "function normalizedImageRotation(value=imageRotation){let r=Number(value)||0;r=((Math.round(r/90)*90)%360+360)%360;return r;}\n",
    "function imageTransformHasDisplayTransform(){return normalizedImageRotation()!==0||!!imageFlipX||!!imageFlipY;}\n",
    "function imageTransformPayload(){return {rotation:normalizedImageRotation(),flip_x:!!imageFlipX,flip_y:!!imageFlipY};}\n",
    "function imageTransformLabel(){const parts=[];const r=normalizedImageRotation();if(r)parts.push('rot '+r+' deg');if(imageFlipX)parts.push('flip H');if(imageFlipY)parts.push('flip V');return parts.length?parts.join(' + '):'original orientation';}\n",
    "function imageTransformStatus(){const label=imageTransformLabel();return label==='original orientation'?'':' | image '+label;}\n",
    "function updateImageTransformSummary(){const box=el('imageTransformSummary');if(box)box.textContent=imageTransformLabel();const reset=el('resetImageTransform');if(reset)reset.disabled=normalizedImageRotation()===0&&!imageFlipX&&!imageFlipY;}\n",
    "function sourceImageSize(){if(typeof image!=='undefined'&&image&&image.naturalWidth)return {width:image.naturalWidth,height:image.naturalHeight};return {width:Number(cfg.slide_width)||1,height:Number(cfg.slide_height)||1};}\n",
    "function viewImageSize(){const size=sourceImageSize(),r=normalizedImageRotation();return r===90||r===270?{width:size.height,height:size.width}:size;}\n",
    "function imageToViewPoint(p){const size=sourceImageSize(),w=size.width,h=size.height,r=normalizedImageRotation();let x=Number(p.x),y=Number(p.y);if(imageFlipX)x=w-x;if(imageFlipY)y=h-y;if(r===90)return {x:h-y,y:x};if(r===180)return {x:w-x,y:h-y};if(r===270)return {x:y,y:w-x};return {x:x,y:y};}\n",
    "function viewToImagePoint(p){const size=sourceImageSize(),w=size.width,h=size.height,r=normalizedImageRotation();let x=Number(p.x),y=Number(p.y),q;if(r===90)q={x:y,y:h-x};else if(r===180)q={x:w-x,y:h-y};else if(r===270)q={x:w-y,y:x};else q={x:x,y:y};if(imageFlipX)q.x=w-q.x;if(imageFlipY)q.y=h-q.y;return q;}\n",
    "function slideToImage(p){if(typeof image!=='undefined'&&image&&image.naturalWidth)return {x:Number(p.x)/Number(cfg.slide_width||image.naturalWidth)*image.naturalWidth,y:Number(p.y)/Number(cfg.slide_height||image.naturalHeight)*image.naturalHeight};return {x:Number(p.x),y:Number(p.y)};}\n",
    "function imageToSlide(p){if(typeof image!=='undefined'&&image&&image.naturalWidth)return {x:Number(p.x)/image.naturalWidth*Number(cfg.slide_width||image.naturalWidth),y:Number(p.y)/image.naturalHeight*Number(cfg.slide_height||image.naturalHeight)};return {x:Number(p.x),y:Number(p.y)};}\n",
    "function slideToViewImagePoint(p){return imageToViewPoint(slideToImage(p));}\n",
    "function applyCanvasImageTransform(img){const w=img.naturalWidth,h=img.naturalHeight,r=normalizedImageRotation();if(r===90){ctx.translate(h,0);ctx.rotate(Math.PI/2);}else if(r===180){ctx.translate(w,h);ctx.rotate(Math.PI);}else if(r===270){ctx.translate(0,w);ctx.rotate(-Math.PI/2);}if(imageFlipX||imageFlipY){ctx.translate(imageFlipX?w:0,imageFlipY?h:0);ctx.scale(imageFlipX?-1:1,imageFlipY?-1:1);}}\n",
    "function drawTransformedImage(img){ctx.save();ctx.globalAlpha=baseImageOpacityValue();ctx.translate(offsetX,offsetY);ctx.scale(scale,scale);applyCanvasImageTransform(img);if(baseImageOpacityValue()>0)ctx.drawImage(img,0,0);ctx.restore();}\n",
    "function effectiveOpenSeadragonTransform(){let rotation=normalizedImageRotation(),flip=!!imageFlipX;if(imageFlipY){rotation=(rotation+180)%360;flip=!flip;}return {rotation:rotation,flip:flip};}\n",
    "function osdDisplayPixelSize(){const node=(typeof viewerEl!=='undefined'&&viewerEl)||canvas,rect=node&&node.getBoundingClientRect?node.getBoundingClientRect():null;return {width:rect&&rect.width?rect.width:innerWidth,height:rect&&rect.height?rect.height:innerHeight};}\n",
    "function osdDisplayPixelForOverlayInRect(p,width,height){const q={x:Number(p.x),y:Number(p.y)},t=effectiveOpenSeadragonTransform();if(t.flip){const w=Number(width);q.x=(Number.isFinite(w)&&w>0?w:osdDisplayPixelSize().width)-q.x;}return q;}\n",
    "function osdDisplayPixelForOverlay(p){const size=osdDisplayPixelSize();return osdDisplayPixelForOverlayInRect(p,size.width,size.height);}\n",
    "function overlayPixelForOsdDisplay(p){return osdDisplayPixelForOverlay(p);}\n",
    "function refreshProgressivePreviewForImageTransform(){if(typeof viewerEl==='undefined'||!viewerEl)return;if(typeof progressivePreviewEnabled!=='function'||!progressivePreviewEnabled()){viewerEl.style.backgroundImage='';viewerEl.classList.remove('progressivePreview');return;}if(imageTransformHasDisplayTransform()){viewerEl.style.backgroundImage='';viewerEl.classList.remove('progressivePreview');return;}if(typeof installProgressivePreviewBackground==='function')installProgressivePreviewBackground();}\n",
    "function setImageTransformRefreshing(on){try{if(typeof viewerEl!=='undefined'&&viewerEl)viewerEl.classList.toggle('imageTransformRefreshing',!!on);}catch(e){}}\n",
    "function clearOpenSeadragonTransformArtifacts(){try{if(typeof viewerEl!=='undefined'&&viewerEl){viewerEl.style.backgroundImage='';viewerEl.classList.remove('progressivePreview');if(viewerEl.querySelectorAll){viewerEl.querySelectorAll('canvas').forEach(c=>{try{const cctx=c.getContext&&c.getContext('2d');if(cctx)cctx.clearRect(0,0,c.width||0,c.height||0);}catch(e){}});viewerEl.querySelectorAll('.openseadragon-canvas canvas').forEach(c=>{try{const cctx=c.getContext&&c.getContext('2d');if(cctx)cctx.clearRect(0,0,c.width||0,c.height||0);}catch(e){}});}}}catch(e){}try{const drawer=osdViewer&&osdViewer.drawer;if(drawer&&typeof drawer.clear==='function')drawer.clear();}catch(e){}try{const item=(typeof osdItem==='function')?osdItem():null;if(item&&typeof item.reset==='function')item.reset();}catch(e){}try{if(typeof stainOverlayCanvas!=='undefined'&&stainOverlayCanvas){const sctx=stainOverlayCanvas.getContext&&stainOverlayCanvas.getContext('2d');if(sctx)sctx.clearRect(0,0,stainOverlayCanvas.width||0,stainOverlayCanvas.height||0);}}catch(e){}if(typeof stainOverlayKey!=='undefined')stainOverlayKey='';if(typeof baseImageDirty!=='undefined')baseImageDirty=true;}\n",
    "function scheduleOpenSeadragonTransformCleanup(){if(typeof requestAnimationFrame!=='function'){setImageTransformRefreshing(false);return;}requestAnimationFrame(()=>{clearOpenSeadragonTransformArtifacts();if(typeof osdViewer!=='undefined'&&osdViewer&&typeof osdViewer.forceRedraw==='function')osdViewer.forceRedraw();if(typeof requestDraw==='function')requestDraw();requestAnimationFrame(()=>setImageTransformRefreshing(false));});}\n",
    "function applyOpenSeadragonImageTransform(){if(typeof osdViewer==='undefined'||!osdViewer||!osdViewer.viewport)return false;const t=effectiveOpenSeadragonTransform();setImageTransformRefreshing(true);if(typeof refreshProgressivePreviewForImageTransform==='function')refreshProgressivePreviewForImageTransform();if(typeof markBaseImageDirty==='function')markBaseImageDirty();if(typeof stainOverlayKey!=='undefined')stainOverlayKey='';try{if(typeof osdViewer.viewport.stop==='function')osdViewer.viewport.stop();}catch(e){}clearOpenSeadragonTransformArtifacts();if(typeof osdViewer.viewport.setRotation==='function')osdViewer.viewport.setRotation(t.rotation,true);if(typeof osdViewer.viewport.setFlip==='function')osdViewer.viewport.setFlip(t.flip);clearOpenSeadragonTransformArtifacts();if(typeof osdViewer.forceRedraw==='function')osdViewer.forceRedraw();scheduleOpenSeadragonTransformCleanup();return true;}\n",
    "function applyImageTransform(refit=false){updateImageTransformSummary();if(typeof osdViewer!=='undefined'&&osdViewer){applyOpenSeadragonImageTransform();if(typeof applyMultiViewImageTransform==='function')applyMultiViewImageTransform();syncViewState();prefetchNeighborTiles();draw();}else if(refit&&typeof fitView==='function'){fitView();}else if(typeof draw==='function'){draw();}scheduleViewerStateSync('image_transform_updated',imageTransformPayload());}\n",
    "function rotateImageDisplay(delta){const before=viewImageSize();imageRotation=normalizedImageRotation(imageRotation+delta);const after=viewImageSize();const refit=before.width!==after.width||before.height!==after.height;applyImageTransform(refit);notify('Image '+imageTransformLabel(),'success',1800);}\n",
    "function flipImageDisplay(axis){if(axis==='x')imageFlipX=!imageFlipX;else imageFlipY=!imageFlipY;applyImageTransform(false);notify('Image '+imageTransformLabel(),'success',1800);}\n",
    "function resetImageDisplayTransform(){imageRotation=0;imageFlipX=false;imageFlipY=false;applyImageTransform(true);notify('Image orientation reset','success',1800);}\n",
    "function bindImageTransformControls(){const left=el('rotateImageLeft'),right=el('rotateImageRight'),flipH=el('flipImageHorizontal'),flipV=el('flipImageVertical'),reset=el('resetImageTransform');if(left)left.onclick=()=>rotateImageDisplay(-90);if(right)right.onclick=()=>rotateImageDisplay(90);if(flipH)flipH.onclick=()=>flipImageDisplay('x');if(flipV)flipV.onclick=()=>flipImageDisplay('y');if(reset)reset.onclick=resetImageDisplayTransform;updateImageTransformSummary();}\n"
  )
}

wsi_viewer_navigator_js <- function() {
  paste0(
    "let miniNavigatorDragging=false,navigatorImageToken=0,navigatorImageSourceKey='',navigatorDisplayImage=null;\n",
    "function miniNavigatorCanvas(){return el('miniNavigatorCanvas');}\n",
    "function miniNavigatorLayout(){const canvas=miniNavigatorCanvas();if(!canvas)return null;const rect=canvas.getBoundingClientRect(),dpr=window.devicePixelRatio||1,w=Math.max(1,Math.floor(rect.width*dpr)),h=Math.max(1,Math.floor(rect.height*dpr));if(canvas.width!==w||canvas.height!==h){canvas.width=w;canvas.height=h;}const nctx=canvas.getContext('2d');nctx.setTransform(dpr,0,0,dpr,0,0);const pad=8,cw=Math.max(1,rect.width),ch=Math.max(1,rect.height),sw=Math.max(1,Number(cfg.slide_width)||1),sh=Math.max(1,Number(cfg.slide_height)||1),s=Math.min((cw-pad*2)/sw,(ch-pad*2)/sh),rw=sw*s,rh=sh*s,ox=(cw-rw)/2,oy=(ch-rh)/2;return {canvas:canvas,ctx:nctx,width:cw,height:ch,scale:s,ox:ox,oy:oy,sw:sw,sh:sh};}\n",
    "function miniNavigatorPoint(p,l){return {x:l.ox+Number(p.x)*l.scale,y:l.oy+Number(p.y)*l.scale};}\n",
    "function miniNavigatorBounds(b,l){if(!b)return null;const p0=miniNavigatorPoint({x:b.xmin,y:b.ymin},l),p1=miniNavigatorPoint({x:b.xmax,y:b.ymax},l);return {x:p0.x,y:p0.y,w:p1.x-p0.x,h:p1.y-p0.y};}\n",
    "function miniNavigatorSlidePoint(clientX,clientY,l){const rect=l.canvas.getBoundingClientRect(),x=clamp((clientX-rect.left-l.ox)/Math.max(l.scale,1e-9),0,l.sw),y=clamp((clientY-rect.top-l.oy)/Math.max(l.scale,1e-9),0,l.sh);return {x:x,y:y};}\n",
    "function navigatorCacheBustedSource(source,key=''){const raw=String(source||'');if(!raw||raw.startsWith('data:')||raw.startsWith('blob:'))return raw;const sep=raw.includes('?')?'&':'?';return raw+sep+'_wsi_nav='+encodeURIComponent(String(key||Date.now()));}\n",
    "function setNavigatorImageSource(source,key=''){if(typeof navigatorImage==='undefined')return;const raw=String(source||''),sourceKey=String(key||raw||'empty');cfg.navigator_image_data_uri=raw;navigatorImageSourceKey=sourceKey;const token=++navigatorImageToken;if(!raw){navigatorDisplayImage=null;try{navigatorImage.removeAttribute('src');}catch(e){}drawMiniNavigator();return;}const next=navigatorCacheBustedSource(raw,sourceKey+'_'+token),loader=new Image();if(/^https?:\\/\\//i.test(next))loader.crossOrigin='anonymous';loader.onload=()=>{if(token!==navigatorImageToken)return;navigatorDisplayImage=loader;try{navigatorImage.src=loader.src;}catch(e){}drawMiniNavigator();};loader.onerror=()=>{if(token!==navigatorImageToken)return;if(navigatorDisplayImage&&navigatorDisplayImage.naturalWidth){drawMiniNavigator();return;}try{navigatorImage.removeAttribute('src');}catch(e){}drawMiniNavigator();};loader.src=next;if(loader.complete&&loader.naturalWidth){navigatorDisplayImage=loader;try{navigatorImage.src=loader.src;}catch(e){}drawMiniNavigator();}else{drawMiniNavigator();}}\n",
    "function miniNavigatorOverviewImage(){if(navigatorDisplayImage&&navigatorDisplayImage.complete&&navigatorDisplayImage.naturalWidth)return navigatorDisplayImage;if(typeof navigatorImage!=='undefined'&&navigatorImage.complete&&navigatorImage.naturalWidth)return navigatorImage;if(typeof image!=='undefined'&&image.complete&&image.naturalWidth)return image;return null;}\n",
    "function drawMiniNavigatorOverview(nctx,l){const img=miniNavigatorOverviewImage(),w=l.sw*l.scale,h=l.sh*l.scale;nctx.save();nctx.beginPath();nctx.rect(l.ox,l.oy,w,h);nctx.clip();if(img){nctx.imageSmoothingEnabled=true;nctx.drawImage(img,l.ox,l.oy,w,h);nctx.restore();return true;}nctx.fillStyle='rgba(15,23,42,.96)';nctx.fillRect(l.ox,l.oy,w,h);nctx.restore();return false;}\n",
    "function navigatorRoiBounds(roi){try{return typeof roiBounds==='function'?roiBounds(roi):null;}catch(e){return null;}}\n",
    "function navigatorCellLikeRoi(roi){const source=String((roi&&roi.source)||'').toLowerCase(),cls=String((roi&&roi.class)||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells'||cls==='detection';}\n",
    "function navigatorTissueLikeRoi(roi){const cls=String((roi&&roi.class)||'').toLowerCase();return !navigatorCellLikeRoi(roi)&&cls!=='exclusion'&&cls!=='artefact';}\n",
    "function drawMiniNavigatorLayerDensity(nctx,l){return false;}\n",
    "function drawMiniNavigatorRoiDensity(nctx,l){let visible=0,tissue=0;(rois||[]).forEach(roi=>{if(typeof visibleRoi==='function'&&!visibleRoi(roi))return;if(typeof isDrawable==='function'&&!isDrawable(roi))return;visible++;if(navigatorTissueLikeRoi(roi))tissue++;});return {annotation:visible,tissue:tissue};}\n",
    "function drawMiniNavigatorMarkers(nctx,l){const marked=new Set();function mark(i,primary=false){if(i<0||!rois[i]||marked.has(i))return;marked.add(i);const b=navigatorRoiBounds(rois[i]);if(!b)return;const cx=(b.xmin+b.xmax)/2,cy=(b.ymin+b.ymax)/2,p=miniNavigatorPoint({x:cx,y:cy},l),r=primary?4.5:3;const rect=miniNavigatorBounds(b,l);nctx.save();if(rect){nctx.strokeStyle=primary?'#ffffff':'#5eead4';nctx.globalAlpha=primary ? .95 : .55;nctx.lineWidth=primary?1.5:1;nctx.strokeRect(rect.x,rect.y,Math.max(2,rect.w),Math.max(2,rect.h));}nctx.globalAlpha=1;nctx.beginPath();nctx.arc(p.x,p.y,r,0,Math.PI*2);nctx.fillStyle=primary?'#ffffff':'#5eead4';nctx.strokeStyle='#0b0b0b';nctx.lineWidth=1.5;nctx.fill();nctx.stroke();nctx.restore();}\n",
    "if(selectedRoi>=0)mark(selectedRoi,true);rois.forEach((roi,i)=>{if(roi&&roi.export_selected)mark(i,false);});}\n",
    "function drawMiniNavigatorViewport(nctx,l){if(typeof visibleSlideBounds!=='function')return null;const b=visibleSlideBounds(),r=miniNavigatorBounds(b,l);if(!r)return null;nctx.save();nctx.strokeStyle='#67e8f9';nctx.lineWidth=2;nctx.shadowColor='rgba(103,232,249,.45)';nctx.shadowBlur=5;nctx.strokeRect(r.x,r.y,Math.max(2,r.w),Math.max(2,r.h));nctx.shadowBlur=0;nctx.strokeStyle='rgba(255,255,255,.88)';nctx.lineWidth=1;nctx.strokeRect(r.x+.5,r.y+.5,Math.max(1,r.w-1),Math.max(1,r.h-1));nctx.restore();return b;}\n",
    "function navigatorDensitySummary(roiDensity,layerDensity){const visible=(rois||[]).filter(roi=>!(typeof visibleRoi==='function')||visibleRoi(roi)).length,selected=(selectedRoi>=0?1:0)+(rois||[]).filter(roi=>roi&&roi.export_selected).length;let text=(visible?visible+' ROI'+(visible===1?'':'s'):'no ROIs');if(selected)text+=' | '+selected+' marked';return text;}\n",
    "function drawMiniNavigator(){const l=miniNavigatorLayout();if(!l)return;const nctx=l.ctx;nctx.clearRect(0,0,l.width,l.height);nctx.save();nctx.fillStyle='rgba(3,7,18,.82)';nctx.fillRect(0,0,l.width,l.height);drawMiniNavigatorOverview(nctx,l);nctx.strokeStyle='rgba(255,255,255,.3)';nctx.lineWidth=1;nctx.strokeRect(l.ox,l.oy,l.sw*l.scale,l.sh*l.scale);const layerDensity=drawMiniNavigatorLayerDensity(nctx,l),roiDensity=drawMiniNavigatorRoiDensity(nctx,l);drawMiniNavigatorMarkers(nctx,l);const b=drawMiniNavigatorViewport(nctx,l);nctx.restore();const viewport=el('miniNavigatorViewport'),density=el('miniNavigatorDensity');if(viewport&&b){viewport.textContent='x '+Math.round(b.xmin)+'-'+Math.round(b.xmax)+' y '+Math.round(b.ymin)+'-'+Math.round(b.ymax);}if(density)density.textContent=navigatorDensitySummary(roiDensity,layerDensity);}\n",
    "function panMiniNavigatorTo(clientX,clientY){const l=miniNavigatorLayout();if(!l)return;const p=miniNavigatorSlidePoint(clientX,clientY,l);if(typeof osdReady!=='undefined'&&osdReady&&typeof osdItem==='function'&&osdItem()&&typeof OpenSeadragon!=='undefined'){const vp=osdItem().imageToViewportCoordinates(p.x,p.y);osdViewer.viewport.panTo(vp,true);osdViewer.viewport.applyConstraints(true);if(typeof syncViewState==='function')syncViewState();if(typeof prefetchNeighborTiles==='function')prefetchNeighborTiles();draw();return;}if(typeof image!=='undefined'&&image.naturalWidth&&typeof slideToImage==='function'){const q=(typeof slideToViewImagePoint==='function')?slideToViewImagePoint(p):slideToImage(p);offsetX=innerWidth/2-q.x*scale;offsetY=innerHeight/2-q.y*scale;draw();}}\n",
    "function bindMiniNavigator(){const canvas=miniNavigatorCanvas();if(!canvas)return;if(typeof navigatorImage!=='undefined'&&cfg.navigator_image_data_uri&&!navigatorImage.src)setNavigatorImageSource(cfg.navigator_image_data_uri,'initial');canvas.addEventListener('mousedown',e=>{miniNavigatorDragging=true;panMiniNavigatorTo(e.clientX,e.clientY);});window.addEventListener('mousemove',e=>{if(miniNavigatorDragging)panMiniNavigatorTo(e.clientX,e.clientY);});window.addEventListener('mouseup',()=>{miniNavigatorDragging=false;});canvas.addEventListener('dblclick',e=>{e.preventDefault();panMiniNavigatorTo(e.clientX,e.clientY);if(typeof fitView==='function'&&e.altKey)fitView();});drawMiniNavigator();}\n"
  )
}

wsi_viewer_geometry_js <- function() {
  paste0(
    "function fmt(v,d=0){return Number.isFinite(Number(v))?Number(v).toFixed(d):'NA';}\n",
    "function visibleRoi(roi){return !(roi&&roi.visible===false);}\n",
    "function lockedRoi(roi){return !!(roi&&(roi.locked===true||roi.isLocked===true||roi.is_locked===true));}\n",
    "function editableRoi(roi){return !!(roi&&!lockedRoi(roi));}\n",
    "function normaliseHexColour(value,fallback='#00BFC4'){let h=String(value||'').trim();if(/^#[0-9A-Fa-f]{6}$/.test(h))return h.toUpperCase();if(/^[0-9A-Fa-f]{6}$/.test(h))return ('#'+h).toUpperCase();return fallback;}\n",
    "const roiClassPresetDefaults=JSON.parse(JSON.stringify(cfg.roi_class_presets||[]));let roiClassPresets=[];\n",
    "function classPresetKey(value){return String(value||'').trim().toLowerCase();}\n",
    "function roiClassName(roi){return String((roi&&roi.class)||'annotation').trim()||'annotation';}\n",
    "function roiClassKey(roi){return classPresetKey(roiClassName(roi));}\n",
    "function roisSameClass(a,b){return roiClassKey(a)===roiClassKey(b);}\n",
    "function annotationIndicesShareClass(indices){const keys=new Set((indices||[]).map(i=>roiClassKey(rois[i])).filter(Boolean));return keys.size<=1;}\n",
    "function normaliseClassPreset(preset){const cls=String((preset&&preset.class)||'').trim();if(!cls)return null;const label=String((preset&&preset.label)||cls).trim()||cls,color=normaliseHexColour((preset&&preset.color)||(preset&&preset.colour)||'#00BFC4','#00BFC4'),rule=String((preset&&preset.export_rule)||((preset&&preset.export)===false?'exclude':'include')).toLowerCase()==='exclude'?'exclude':'include';return {class:cls,label:label,color:color,export:rule!=='exclude'&&(preset&&preset.export)!==false,export_rule:rule};}\n",
    "function normaliseClassPresets(presets){const seen=new Set(),out=[];(presets||[]).forEach(p=>{const preset=normaliseClassPreset(p);if(!preset)return;const key=classPresetKey(preset.class);if(seen.has(key))return;seen.add(key);out.push(preset);});return out;}\n",
    "function classPreset(value){const key=classPresetKey(value);return roiClassPresets.find(p=>classPresetKey(p.class)===key)||null;}\n",
    "function classPresetColour(value,fallback=''){const preset=classPreset(value);return preset?normaliseHexColour(preset.color,fallback):fallback;}\n",
    "function stableClassColour(value,fallback='#00BFC4'){const key=classPresetKey(value);if(!key)return fallback;const palette=['#E41A1C','#377EB8','#4DAF4A','#984EA3','#FF7F00','#A65628','#F781BF','#999999','#1B9E77','#D95F02','#7570B3','#E7298A','#66A61E','#E6AB02','#A6761D','#666666','#8DD3C7','#FB8072','#80B1D3','#B3DE69'];const numeric=key.match(/(?:^|[_ -])(\\d+)$/);if(numeric){const n=Number(numeric[1]);if(Number.isFinite(n)&&n>0)return palette[(Math.round(n)-1)%palette.length]||fallback;}let hash=0;for(let i=0;i<key.length;i++)hash=(hash+key.charCodeAt(i)*(i+1))%1000003;return palette[hash%palette.length]||fallback;}\n",
    "function ensureClassPreset(value,seedColour=''){const cls=String(value||'').trim()||'annotation';let preset=classPreset(cls);if(preset)return preset;const colour=normaliseHexColour(seedColour,'')||stableClassColour(cls);preset={class:cls,label:cls,color:colour,export:true,export_rule:'include'};roiClassPresets.push(preset);return preset;}\n",
    "function classColour(value,seedColour=''){const preset=ensureClassPreset(value,seedColour);return preset?normaliseHexColour(preset.color,'#00BFC4'):'#00BFC4';}\n",
    "function annotationHighlightKeys(){if(typeof highlightedRoiClassKeys==='undefined')return new Set();return highlightedRoiClassKeys;}\n",
    "function annotationHighlightActive(){return !!(typeof highlightAllRoiClasses!=='undefined'&&highlightAllRoiClasses)||annotationHighlightKeys().size>0;}\n",
    "function roiClassHighlighted(roi){if(!annotationHighlightActive())return false;if(typeof highlightAllRoiClasses!=='undefined'&&highlightAllRoiClasses)return true;return annotationHighlightKeys().has(roiClassKey(roi));}\n",
    "function annotationClassEntries(){const map=new Map();(rois||[]).forEach(roi=>{const cls=roiClassName(roi),key=classPresetKey(cls),colour=classColour(cls,roi&&roi.colour||'');if(!key)return;const old=map.get(key)||{key:key,label:cls,colour:colour,count:0};old.count++;if(!old.colour&&colour)old.colour=colour;map.set(key,old);});return Array.from(map.values()).sort((a,b)=>a.label.localeCompare(b.label));}\n",
    "function setAnnotationLabelHighlightAll(value=true){highlightAllRoiClasses=!!value;if(value)annotationHighlightKeys().clear();renderAnnotationLabelHighlights();draw();}\n",
    "function clearAnnotationLabelHighlights(){highlightAllRoiClasses=false;annotationHighlightKeys().clear();renderAnnotationLabelHighlights();draw();}\n",
    "function setAnnotationLabelHighlighted(key,value){highlightAllRoiClasses=false;key=String(key||'');if(!key)return;if(value)annotationHighlightKeys().add(key);else annotationHighlightKeys().delete(key);renderAnnotationLabelHighlights();draw();}\n",
    "function renderAnnotationLabelHighlights(){const list=el('annotationLabelHighlightList'),summary=el('annotationLabelHighlightSummary'),all=el('annotationHighlightAll'),none=el('annotationHighlightNone');if(!list)return;const entries=annotationClassEntries(),keys=annotationHighlightKeys(),active=annotationHighlightActive();list.innerHTML='';if(summary){if(!entries.length)summary.textContent='No categories yet.';else if(highlightAllRoiClasses)summary.textContent='Highlighting all '+entries.length+' label categor'+(entries.length===1?'y':'ies')+'.';else if(keys.size)summary.textContent='Highlighting '+keys.size+' of '+entries.length+' label categor'+(entries.length===1?'y':'ies')+'.';else summary.textContent=entries.length+' label categor'+(entries.length===1?'y':'ies')+' available.';}if(all)all.classList.toggle('active',!!highlightAllRoiClasses);if(none)none.disabled=!active;entries.forEach(entry=>{const checked=!!highlightAllRoiClasses||keys.has(entry.key),row=document.createElement('label');row.className='annotationLabelHighlightItem'+(checked?' active':'');row.title='Highlight '+entry.label;const box=document.createElement('input');box.type='checkbox';box.checked=checked;box.onchange=e=>setAnnotationLabelHighlighted(entry.key,!!e.target.checked);const sw=document.createElement('span');sw.className='swatch';sw.style.background=entry.colour||'#cccccc';const name=document.createElement('span');name.className='labelName';name.textContent=entry.label;const count=document.createElement('span');count.className='labelCount';count.textContent=String(entry.count);row.append(box,sw,name,count);list.appendChild(row);});}\n",
    "function classPresetExportable(value){const preset=classPreset(value);return !preset||preset.export!==false&&preset.export_rule!=='exclude';}\n",
    "function classPresetExportRule(value){const preset=classPreset(value);return preset?(preset.export_rule||'include'):'include';}\n",
    "function respectClassExportRules(){const input=el('respectClassExportRules');return !!(input&&input.checked);}\n",
    "function roiAllowedByExportRules(roi){return !respectClassExportRules()||classPresetExportable(roi&&roi.class);}\n",
    "roiClassPresets=normaliseClassPresets(roiClassPresetDefaults);\n",
    "function setRoiColour(roi,colour,markEdited=true){if(!roi)return;const c=normaliseHexColour(colour,roi.colour||'#00BFC4');roi.colour=c;roi.fill=hexToRgba(c,0.18);if(markEdited!==false)roi.edited=true;}\n",
    "function isDrawable(roi){return roi&&roi.drawable!==false&&((roi.rings&&roi.rings.length>0)||(roi.add_rings&&roi.add_rings.length>0));}\n",
    "function positiveRingGroups(roi){const groups=[];if(roi&&roi.rings&&roi.rings.length)groups.push(roi.rings);(roi&&roi.add_groups?roi.add_groups:[]).forEach(g=>{const rings=(g||[]).filter(r=>r&&r.length>=4);if(rings.length)groups.push(rings);});(roi&&roi.add_rings?roi.add_rings:[]).forEach(r=>{if(r&&r.length>=4)groups.push([r]);});return groups;}\n",
    "function subtractRings(roi){return (roi&&roi.subtract_rings?roi.subtract_rings:[]).filter(r=>r&&r.length>=4);}\n",
    "function allRoiRings(roi,includeSubtract=false){let rings=[];positiveRingGroups(roi).forEach(g=>{rings=rings.concat(g);});if(includeSubtract)rings=rings.concat(subtractRings(roi));return rings;}\n",
    "function ringCenter(ring){const pts=(ring||[]).slice(0,Math.max(0,(ring||[]).length-1));if(!pts.length)return null;return {x:pts.reduce((s,p)=>s+p.x,0)/pts.length,y:pts.reduce((s,p)=>s+p.y,0)/pts.length};}\n",
    "function pointInRingsEvenOdd(p,rings){let inside=false;(rings||[]).forEach(r=>{if(pointInRing(p,r))inside=!inside;});return inside;}\n",
    "function clippedHoleForGroup(hole,group){if(!hole||hole.length<4)return null;const c=ringCenter(hole);if(!(c&&pointInRingsEvenOdd(c,group)))return null;if(hole.every(p=>pointInRingsEvenOdd(p,group)))return hole;const inside=hole.filter(p=>pointInRingsEvenOdd(p,group));return inside.length>=3?closedRing(inside):null;}\n",
    "function roiDrawGroups(roi){const holes=subtractRings(roi);return positiveRingGroups(roi).map(group=>({rings:group,holes:holes.map(h=>clippedHoleForGroup(h,group)).filter(Boolean)}));}\n",
    "function roiContainsPoint(roi,p){if(!p)return false;let inside=false;roiDrawGroups(roi).forEach(g=>{if(pointInRingsEvenOdd(p,g.rings)&&!pointInRingsEvenOdd(p,g.holes))inside=true;});return inside;}\n",
    "function hasDrawable(){return rois.some(isDrawable);}\n",
    "function boundsFromRing(ring){const xs=ring.map(p=>p.x),ys=ring.map(p=>p.y);return {xmin:Math.min(...xs),ymin:Math.min(...ys),xmax:Math.max(...xs),ymax:Math.max(...ys)};}\n",
    "function ringArea(ring){if(!ring||ring.length<3)return NaN;let a=0;for(let i=0,j=ring.length-1;i<ring.length;j=i++){a+=(ring[j].x*ring[i].y-ring[i].x*ring[j].y);}return Math.abs(a/2);}\n",
    "function polygonArea(rings){if(!rings||!rings.length)return NaN;const outer=ringArea(rings[0]);let holes=0;for(let i=1;i<rings.length;i++)holes+=ringArea(rings[i]);return Math.max(0,outer-holes);}\n",
    "function roiBounds(roi){if(roi&&roi.bbox&&Number.isFinite(Number(roi.bbox.xmin)))return roi.bbox;if(isDrawable(roi)){let xs=[],ys=[];allRoiRings(roi,false).forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return xs.length?{xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)}:null;}return null;}\n",
    "function geometryType(roi){return roi.geometry_type||roi.geometryType||'Geometry';}\n",
    "function pointCount(roi){if(Number.isFinite(Number(roi.point_count)))return Number(roi.point_count);let n=0;allRoiRings(roi,true).forEach(r=>{n+=r.length;});return n;}\n",
    "function formatBounds(b){return b?('x '+fmt(b.xmin)+'-'+fmt(b.xmax)+' | y '+fmt(b.ymin)+'-'+fmt(b.ymax)):'NA';}\n",
    "function geometrySummary(){if(!rois.length)return 'No GeoJSON geometries loaded.';const counts={};rois.forEach(r=>{const t=geometryType(r);counts[t]=(counts[t]||0)+1;});return rois.length+' geometr'+(rois.length===1?'y':'ies')+' | '+Object.keys(counts).map(k=>k+' '+counts[k]).join(', ');}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(visibleRoi(rois[i])&&isDrawable(rois[i])&&roiContainsPoint(rois[i],p))return i;}return -1;}\n",
    "function cursorTargets(){const targets=[canvas];if(typeof viewerEl!=='undefined'&&viewerEl)targets.push(viewerEl);if(typeof multiViewPanes!=='undefined'&&Array.isArray(multiViewPanes))multiViewPanes.forEach(p=>{if(p&&p.overlay)targets.push(p.overlay);});return targets;}\n",
    "function brushBlockedAt(p){const hit=roiAt(p);return hit>=0&&lockedRoi(rois[hit])?hit:-1;}\n",
    "function cursorBlocked(){return (mode==='brush'||mode==='edit')&&lastPointer&&pointInsideSlide(lastPointer)&&brushBlockedAt(lastPointer)>=0;}\n",
    "function viewerIsMac(){const platform=String(navigator.platform||navigator.userAgent||'');return /Mac|iPhone|iPad|iPod/i.test(platform);}\n",
    "function brushSubtractModifier(e={}){return !!(e&&(viewerIsMac()?e.metaKey:e.altKey));}\n",
    "function brushSubtractKeyEvent(e={}){return viewerIsMac()?e.key==='Meta':e.key==='Alt';}\n",
    "function brushCursorState(){const blocked=cursorBlocked(),subtract=mode==='brush'&&!blocked&&(brushing?brushOperation==='subtract':brushAltDown),add=mode==='brush'&&!blocked&&!subtract;return {blocked:blocked,subtract:subtract,add:add};}\n",
    "function updateCursorFeedback(e={}){if(e&&(typeof e.altKey==='boolean'||typeof e.metaKey==='boolean')){brushAltDown=brushSubtractModifier(e);if(mode==='brush'&&brushing&&brushTargetRoi>=0)brushOperation=brushAltDown?'subtract':'new';}const state=brushCursorState();cursorTargets().forEach(target=>{target.classList.toggle('brush-add',state.add);target.classList.toggle('brush-subtract',state.subtract);target.classList.toggle('cursor-blocked',state.blocked);});return state;}\n",
    "function clearSelectedTrajectory(refresh=true){if(typeof selectedTrajectory==='undefined'||selectedTrajectory<0)return false;selectedTrajectory=-1;if(refresh){if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();}return true;}\n",
    "function clearSelectedLayerObject(refresh=true){const changed=Number.isFinite(Number(selectedLayerIndex))&&selectedLayerIndex>=0&&Number.isFinite(Number(selectedLayerItemIndex))&&selectedLayerItemIndex>=0;selectedLayerIndex=-1;selectedLayerItemIndex=-1;if(refresh){if(typeof buildLayerList==='function')buildLayerList();if(typeof updateButtons==='function')updateButtons();}return changed;}\n",
    "function clearSelectedMeasure(refresh=true){const changed=Number.isFinite(Number(selectedMeasure))&&selectedMeasure>=0;selectedMeasure=-1;if(refresh&&typeof updateMeasureList==='function')updateMeasureList();return changed;}\n",
    "function clearSelectedAnnotation(refresh=true){const changed=!(typeof selectedRoi==='undefined'||selectedRoi<0);selectedRoi=-1;activeVertex=null;draggingVertex=null;brushTargetRoi=-1;if(typeof selectionCardVisible!=='undefined')selectionCardVisible=false;if(refresh){if(typeof updateRoiList==='function')updateRoiList();else if(typeof buildRoiList==='function')buildRoiList();}return changed;}\n",
    "function selectAnnotation(index,refresh=true){const idx=Number(index);selectedRoi=Number.isFinite(idx)?idx:-1;if(selectedRoi>=0){clearSelectedTrajectory(false);clearSelectedLayerObject(false);clearSelectedMeasure(false);}activeVertex=null;draggingVertex=null;if(refresh){if(typeof updateRoiList==='function')updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateButtons==='function')updateButtons();}return selectedRoi;}\n",
    "function selectTrajectory(index,refresh=true){const idx=Number(index);selectedTrajectory=Number.isFinite(idx)?idx:-1;if(selectedTrajectory>=0){clearSelectedAnnotation(false);clearSelectedLayerObject(false);clearSelectedMeasure(false);}if(refresh){if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof updateRoiList==='function')updateRoiList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateButtons==='function')updateButtons();}return selectedTrajectory;}\n",
    "function selectLayerObject(layerIndex,itemIndex,refresh=true){selectedLayerIndex=Number(layerIndex);selectedLayerItemIndex=Number(itemIndex);if(selectedLayerIndex>=0&&selectedLayerItemIndex>=0){clearSelectedAnnotation(false);clearSelectedTrajectory(false);clearSelectedMeasure(false);}if(refresh){if(typeof buildLayerList==='function')buildLayerList();if(typeof updateRoiList==='function')updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof updateButtons==='function')updateButtons();}return selectedLayerIndex>=0&&selectedLayerItemIndex>=0;}\n",
    "function selectMeasure(index,refresh=true){selectedMeasure=Number(index);if(selectedMeasure>=0){clearSelectedAnnotation(false);clearSelectedTrajectory(false);clearSelectedLayerObject(false);}if(refresh){if(typeof updateMeasureList==='function')updateMeasureList();if(typeof updateRoiList==='function')updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateButtons==='function')updateButtons();}return selectedMeasure;}\n",
    "function selectedLayerObject(){const layer=layers&&layers[selectedLayerIndex],items=layer&&Array.isArray(layer.items)?layer.items:[],item=items[selectedLayerItemIndex];return item?{layer:layer,item:item,layerIndex:selectedLayerIndex,itemIndex:selectedLayerItemIndex}:null;}\n",
    "function layerObjectLabel(rec=selectedLayerObject()){if(!rec)return 'layer object';const item=rec.item||{},layer=rec.layer||{};return String(item.name||item.label||item.id||layer.name||layer.id||('Layer object '+(Number(rec.itemIndex)+1)));}\n",
    "function selectedObjectPayload(){if(selectedRoi>=0&&rois[selectedRoi])return {type:'annotation',index:selectedRoi,id:rois[selectedRoi].id||null,name:roiLabelText(rois[selectedRoi],selectedRoi)};if(typeof selectedTrajectory!=='undefined'&&selectedTrajectory>=0&&typeof trajectories!=='undefined'&&trajectories[selectedTrajectory]){const t=trajectories[selectedTrajectory];return {type:'trajectory',index:selectedTrajectory,id:t.id||null,name:t.name||('Trajectory '+(selectedTrajectory+1))};}const rec=selectedLayerObject();if(rec)return {type:'layer_object',layer_index:rec.layerIndex,item_index:rec.itemIndex,layer_id:rec.layer.id||null,layer_name:rec.layer.name||null,item_id:rec.item.id||null,name:layerObjectLabel(rec)};if(selectedMeasure>=0&&measures[selectedMeasure])return {type:'measurement',index:selectedMeasure,id:measures[selectedMeasure].id||null,name:'Distance '+(selectedMeasure+1)};return null;}\n",
    "function enforceSingleObjectSelection(prefer='annotation'){const hasRoi=Number.isFinite(Number(selectedRoi))&&selectedRoi>=0&&!!rois[selectedRoi],hasTrajectory=typeof selectedTrajectory!=='undefined'&&Number.isFinite(Number(selectedTrajectory))&&selectedTrajectory>=0&&typeof trajectories!=='undefined'&&!!trajectories[selectedTrajectory],hasLayer=!!selectedLayerObject(),hasMeasure=Number.isFinite(Number(selectedMeasure))&&selectedMeasure>=0&&!!measures[selectedMeasure],count=[hasRoi,hasTrajectory,hasLayer,hasMeasure].filter(Boolean).length;if(count<2)return false;if(prefer!=='annotation')selectedRoi=-1;if(prefer!=='trajectory')selectedTrajectory=-1;if(prefer!=='layer'){selectedLayerIndex=-1;selectedLayerItemIndex=-1;}if(prefer!=='measure')selectedMeasure=-1;return true;}\n",
    "function clearSelectionAndPan(){clearSelectedAnnotation(false);clearSelectedTrajectory(false);clearSelectedLayerObject(false);clearSelectedMeasure(false);setMode('pan');if(typeof updateRoiList==='function')updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof updateButtons==='function')updateButtons();if(typeof draw==='function')draw();}\n",
    "function selectObjectAtPoint(p,prefer='trajectory'){const tryTrajectory=()=>{if(typeof trajectoryAt!=='function')return false;const t=trajectoryAt(p);if(t>=0){selectTrajectory(t,true);setMode('select');notify('Selected '+((trajectories[t]&&trajectories[t].name)||('Trajectory '+(t+1))),'info',1400);draw();return true;}return false;},tryRoi=()=>{const hit=roiAt(p);if(hit>=0){selectAnnotation(hit,true);setMode('select');notify('Selected '+(rois[hit].name||rois[hit].id||'ROI'),'info',1400);draw();return true;}return false;},tryMeasure=()=>{if(typeof measurementAt!=='function')return false;const m=measurementAt(p);if(m>=0){selectMeasure(m,true);setMode('select');notify('Selected distance measurement '+(m+1),'info',1400);draw();return true;}return false;},tryLayer=()=>{if(typeof layerObjectAt!=='function')return false;const hit=layerObjectAt(p);if(hit){selectLayerObject(hit.layerIndex,hit.itemIndex,true);setMode('select');notify('Selected '+layerObjectLabel(hit),'info',1400);draw();return true;}return false;};if(prefer==='annotation')return tryRoi()||tryTrajectory()||tryMeasure()||tryLayer();if(prefer==='layer')return tryLayer()||tryRoi()||tryTrajectory()||tryMeasure();if(prefer==='measure')return tryMeasure()||tryTrajectory()||tryRoi()||tryLayer();return tryTrajectory()||tryMeasure()||tryRoi()||tryLayer();}\n",
    "function centerRoi(i){if(!hasDrawable())return;let idx=-1;for(let k=0;k<rois.length;k++){const candidate=(i+rois.length+k)%rois.length;if(isDrawable(rois[candidate])){idx=candidate;break;}}if(idx<0)return;selectAnnotation(idx,false);const b=roiBounds(rois[selectedRoi]);if(!b){notify('No drawable bounds','warning');updateRoiList();draw();return;}const pad=1.35;if(typeof zoomToSlideBounds==='function'){zoomToSlideBounds(b,pad);updateRoiList();draw();return;}let viewW=b.xmax-b.xmin,viewH=b.ymax-b.ymin,centerX=(b.xmin+b.xmax)/2,centerY=(b.ymin+b.ymax)/2;if(typeof slideToViewImagePoint==='function'){const corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(slideToViewImagePoint),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys);viewW=xmax-xmin;viewH=ymax-ymin;centerX=(xmin+xmax)/2;centerY=(ymin+ymax)/2;}else if(typeof slideToImage==='function'){const p0=slideToImage({x:b.xmin,y:b.ymin}),p1=slideToImage({x:b.xmax,y:b.ymax});viewW=p1.x-p0.x;viewH=p1.y-p0.y;centerX=(p0.x+p1.x)/2;centerY=(p0.y+p1.y)/2;}const maxScale=(typeof image!=='undefined')?40:4;scale=clamp(Math.min(innerWidth/Math.max(1,viewW*pad),innerHeight/Math.max(1,viewH*pad)),minScale*0.8,maxScale);offsetX=innerWidth/2-centerX*scale;offsetY=innerHeight/2-centerY*scale;updateRoiList();draw();}\n",
    "function roiLabelText(roi,i){return roi.label||roi.name||roi.id||('geometry '+(i+1));}\n",
    "function roiLabelPoint(roi){const b=roiBounds(roi);if(b)return slideToCanvas({x:(b.xmin+b.xmax)/2,y:(b.ymin+b.ymax)/2});if(isDrawable(roi)&&roi.rings[0]&&roi.rings[0][0])return slideToCanvas(roi.rings[0][0]);return null;}\n",
    "function labelRectOverlaps(a,b,pad=10){return !(a.x+a.w+pad<b.x||b.x+b.w+pad<a.x||a.y+a.h+pad<b.y||b.y+b.h+pad<a.y);}\n",
    "function roiLabelCandidates(anchor,w,h){const gap=14,near=h+gap,far=h*2+gap,offsets=[[0,-h/2],[0,-near],[0,gap],[w/2+gap,-h/2],[-w/2-gap,-h/2],[w/2+gap,gap],[-w/2-gap,gap],[w/2+gap,-near],[-w/2-gap,-near],[0,-far],[0,h+gap],[w+gap,-h/2],[-w-gap,-h/2],[w+gap,gap],[-w-gap,gap],[w+gap,-near],[-w-gap,-near],[w/2+gap,-far],[-w/2-gap,-far],[w/2+gap,h+gap],[-w/2-gap,h+gap]];const seen=new Set();return offsets.map((o,rank)=>{const x=clamp(anchor.x+o[0]-w/2,6,Math.max(6,innerWidth-w-6)),y=clamp(anchor.y+o[1],6,Math.max(6,innerHeight-h-6)),key=Math.round(x)+'|'+Math.round(y);if(seen.has(key))return null;seen.add(key);return {x:x,y:y,w:w,h:h,rank:rank};}).filter(Boolean);}\n",
    "function labelAnchorDistanceScore(anchor,c){const cx=c.x+c.w/2,cy=c.y+c.h/2;return Math.hypot(cx-anchor.x,cy-anchor.y)+(c.rank||0)*4;}\n",
    "function placeRoiLabel(anchor,w,h,occupied){const candidates=roiLabelCandidates(anchor,w,h).sort((a,b)=>labelAnchorDistanceScore(anchor,a)-labelAnchorDistanceScore(anchor,b));for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,12)))return c;}for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,2)))return c;}return null;}\n",
    "function crispLabelRect(rect){return {x:Math.round(rect.x),y:Math.round(rect.y),w:Math.ceil(rect.w),h:Math.ceil(rect.h)};}\n",
    "function labelLeaderTarget(anchor,r){return {x:clamp(anchor.x,r.x,r.x+r.w),y:clamp(anchor.y,r.y,r.y+r.h)};}\n",
    "function drawLabelLeader(item,r,colour){if(!item.anchor)return;const t=labelLeaderTarget(item.anchor,r),dist=Math.hypot(t.x-item.anchor.x,t.y-item.anchor.y);if(dist<8)return;ctx.save();ctx.strokeStyle='rgba(0,0,0,.84)';ctx.lineWidth=3.5;ctx.beginPath();ctx.moveTo(item.anchor.x,item.anchor.y);ctx.lineTo(t.x,t.y);ctx.stroke();ctx.strokeStyle=colour;ctx.lineWidth=1.8;ctx.beginPath();ctx.moveTo(item.anchor.x,item.anchor.y);ctx.lineTo(t.x,t.y);ctx.stroke();ctx.fillStyle=colour;ctx.strokeStyle='rgba(0,0,0,.9)';ctx.lineWidth=1.2;ctx.beginPath();ctx.arc(item.anchor.x,item.anchor.y,3.2,0,Math.PI*2);ctx.fill();ctx.stroke();ctx.restore();}\n",
    "function drawPlacedRoiLabel(item,rect){const r=crispLabelRect(rect),colour=item.colour||'#5eead4';ctx.save();drawLabelLeader(item,r,colour);ctx.font='700 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='middle';ctx.shadowColor='rgba(0,0,0,.55)';ctx.shadowBlur=5;ctx.fillStyle='rgba(0,0,0,.92)';ctx.fillRect(r.x,r.y,r.w,r.h);ctx.shadowBlur=0;ctx.strokeStyle='rgba(255,255,255,.86)';ctx.lineWidth=2.5;ctx.strokeRect(r.x+.5,r.y+.5,Math.max(1,r.w-1),Math.max(1,r.h-1));ctx.strokeStyle=colour;ctx.lineWidth=1.6;ctx.strokeRect(r.x+2.5,r.y+2.5,Math.max(1,r.w-5),Math.max(1,r.h-5));ctx.fillStyle=colour;ctx.fillRect(r.x+1,r.y+1,6,Math.max(1,r.h-2));ctx.fillStyle='#ffffff';ctx.fillText(item.text,r.x+13,r.y+r.h/2);ctx.restore();}\n",
    "function drawRoiLabels(items){if(!items.length)return;const occupied=[];items.sort((a,b)=>(b.priority||0)-(a.priority||0));items.forEach(item=>{const rect=placeRoiLabel(item.anchor,item.w,item.h,occupied);if(!rect)return;occupied.push(rect);drawPlacedRoiLabel(item,rect);});}\n",
    "function drawPathRings(rings){(rings||[]).forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});}\n",
    "function strokeRoiBorderGroup(item){const group=item.group,roi=item.roi,selected=item.selected,highlighted=item.highlighted,dimmed=item.dimmed,locked=lockedRoi(roi),colour=selected?'#ffffff':(locked?'#facc15':(roi.colour||'#5eead4')),drawPath=()=>{ctx.beginPath();drawPathRings(group.rings);drawPathRings(group.holes);};ctx.save();ctx.lineJoin='round';ctx.lineCap='round';ctx.globalAlpha=dimmed?0.32:1;ctx.setLineDash([]);drawPath();ctx.strokeStyle=highlighted?'rgba(255,255,255,.96)':'rgba(0,0,0,.72)';ctx.lineWidth=highlighted?10:(selected?7:(locked?6:5));ctx.stroke();drawPath();ctx.strokeStyle=colour;ctx.lineWidth=highlighted?5:(selected?4:(locked?3:2));if(highlighted){ctx.shadowColor=colour;ctx.shadowBlur=7;}else if(!selected){ctx.setLineDash([7,4]);ctx.lineDashOffset=-(item.index%9)*2;}ctx.stroke();ctx.setLineDash([]);ctx.restore();}\n",
    "function roiCanvasMetrics(b){if(!b)return {w:0,h:0,cx:0,cy:0};const p0=slideToCanvas({x:b.xmin,y:b.ymin}),p1=slideToCanvas({x:b.xmax,y:b.ymax});return {x:Math.min(p0.x,p1.x),y:Math.min(p0.y,p1.y),w:Math.abs(p1.x-p0.x),h:Math.abs(p1.y-p0.y),cx:(p0.x+p1.x)/2,cy:(p0.y+p1.y)/2};}\n",
    "function roiVisibleSlideBounds(padFraction=.08){const b=(typeof visibleSlideBounds==='function')?visibleSlideBounds():{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height},pad=Math.max(b.xmax-b.xmin,b.ymax-b.ymin)*padFraction;return {xmin:Math.max(0,b.xmin-pad),ymin:Math.max(0,b.ymin-pad),xmax:Math.min(cfg.slide_width,b.xmax+pad),ymax:Math.min(cfg.slide_height,b.ymax+pad)};}\n",
    "function roiIntersectsViewport(roi,bounds){const b=roiBounds(roi);return !!(b&&bounds&&b.xmin<=bounds.xmax&&b.xmax>=bounds.xmin&&b.ymin<=bounds.ymax&&b.ymax>=bounds.ymin);}\n",
    "function roiLodMode(roi,i,b,metrics,highlighted){const selected=(i===selectedRoi)||!!(roi&&roi.export_selected);if(selected||highlighted||rois.length<350)return 'detail';const px=typeof slideUnitScale==='function'?slideUnitScale():scale,maxDim=Math.max(metrics.w,metrics.h),pts=Number(pointCount(roi)||0);if(px>.22&&maxDim>12)return 'detail';if(rois.length<1500&&px>.12&&pts<800&&maxDim>28)return 'detail';if(maxDim<5)return 'centroid';return 'bbox';}\n",
    "function drawRoiLodMarker(roi,i,b,metrics,mode,dimmed,highlighted,selected){const locked=lockedRoi(roi),colour=selected?'#ffffff':(locked?'#facc15':(roi.colour||'#5eead4'));ctx.save();ctx.globalAlpha=dimmed?0.28:Math.max(.42,Math.min(1,roiOpacity));ctx.strokeStyle=highlighted?'#ffffff':colour;ctx.fillStyle=hexToRgba(colour,mode==='centroid'?.72:.12);ctx.lineWidth=highlighted?2.4:(selected?2.2:1.2);ctx.setLineDash(selected?[]:[4,3]);if(mode==='centroid'){const r=highlighted||selected?4:2.5;ctx.beginPath();ctx.arc(metrics.cx,metrics.cy,r,0,Math.PI*2);ctx.fill();ctx.stroke();}else{const x=Math.round(metrics.x)+.5,y=Math.round(metrics.y)+.5,w=Math.max(2,Math.round(metrics.w)),h=Math.max(2,Math.round(metrics.h));ctx.fillRect(x,y,w,h);ctx.strokeRect(x,y,w,h);}ctx.setLineDash([]);ctx.restore();}\n",
    "function drawRois(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('annotations'))return;if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='600 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';const labelItems=[],borderItems=[],highlightActive=annotationHighlightActive(),viewBounds=roiVisibleSlideBounds(.10);let detailed=0,lod=0,culled=0;rois.forEach((roi,i)=>{if(!visibleRoi(roi)||!isDrawable(roi)){culled++;return;}const b=roiBounds(roi);if(!b||!roiIntersectsViewport(roi,viewBounds)){culled++;return;}const selected=(i===selectedRoi)||!!roi.export_selected,highlighted=roiClassHighlighted(roi),dimmed=highlightActive&&!highlighted,metrics=roiCanvasMetrics(b),mode=roiLodMode(roi,i,b,metrics,highlighted);if(mode==='detail'){const groups=roiDrawGroups(roi);groups.forEach(group=>{ctx.beginPath();drawPathRings(group.rings);drawPathRings(group.holes);ctx.globalAlpha=dimmed?Math.min(.12,roiOpacity*.35):roiOpacity;ctx.fillStyle=roi.fill;ctx.fill('evenodd');ctx.globalAlpha=1;borderItems.push({group:group,roi:roi,index:i,selected:selected,highlighted:highlighted,dimmed:dimmed});});detailed++;}else{drawRoiLodMarker(roi,i,b,metrics,mode,dimmed,highlighted,selected);lod++;}if(showLabels&&(mode==='detail'||highlighted||selected)){const label=roiLabelPoint(roi),text=roiLabelText(roi,i);if(label&&text&&!dimmed){labelItems.push({anchor:label,text:text,w:ctx.measureText(text).width+18,h:22,colour:roi.colour||'#5eead4',priority:highlighted?20:(selected?10:0)});}}});borderItems.forEach(strokeRoiBorderGroup);if(showLabels)drawRoiLabels(labelItems);if(lod>0&&typeof recordViewerLog==='function'&&Date.now()-(drawRois._lastLodLog||0)>8000){drawRois._lastLodLog=Date.now();recordViewerLog('Annotation level-of-detail active: '+lod+' simplified, '+detailed+' detailed, '+culled+' outside view.','info',{simplified:lod,detailed:detailed,culled:culled},'annotations');}ctx.restore();}\n",
    "function annotationSnapshot(){return {rois:JSON.parse(JSON.stringify(rois)),selectedRoi:selectedRoi,newRoiCount:newRoiCount,trajectories:JSON.parse(JSON.stringify(typeof trajectories==='undefined'?[]:trajectories)),selectedTrajectory:typeof selectedTrajectory==='undefined'?-1:selectedTrajectory,trajectorySeq:typeof trajectorySeq==='undefined'?0:trajectorySeq};}\n",
    "function pushHistory(stack,state){stack.push(state);if(stack.length>10)stack.shift();}\n",
    "function pushAnnotationUndo(action='annotation_edit'){try{pushHistory(annotationUndo,annotationSnapshot());annotationRedo=[];markAnnotationsDirty(action);updateButtons();}catch(e){console.warn('Could not record annotation undo state',e);}}\n",
    "function restoreAnnotationState(state,eventName){if(state&&state.project&&typeof restoreProjectUndoSnapshot==='function'){restoreProjectUndoSnapshot(state.project,eventName||'annotation_history');return;}rois.splice(0,rois.length);(state.rois||[]).forEach(roi=>rois.push(roi));selectedRoi=Math.min(Math.max(Number(state.selectedRoi),-1),rois.length-1);if(!Number.isFinite(selectedRoi))selectedRoi=-1;if(Number.isFinite(Number(state.newRoiCount)))newRoiCount=Number(state.newRoiCount);if(Object.prototype.hasOwnProperty.call(state,'trajectories')&&typeof trajectories!=='undefined'){trajectories.splice(0,trajectories.length);(state.trajectories||[]).forEach(t=>trajectories.push(t));selectedTrajectory=Math.min(Math.max(Number(state.selectedTrajectory),-1),trajectories.length-1);if(!Number.isFinite(selectedTrajectory))selectedTrajectory=-1;if(Number.isFinite(Number(state.trajectorySeq)))trajectorySeq=Number(state.trajectorySeq);enforceSingleObjectSelection('annotation');if(typeof renderTrajectoryList==='function')renderTrajectoryList();}activeVertex=null;draggingVertex=null;brushing=false;brushPoints=[];brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();draft=[];if(typeof trajectoryDraft!=='undefined')trajectoryDraft=[];markAnnotationsDirty(eventName||'annotation_history');buildRoiList();updateButtons();draw();scheduleViewerStateSync(eventName||'annotation_history',{undo:annotationUndo.length,redo:annotationRedo.length,dirty:annotationsDirty,trajectory_count:typeof trajectories==='undefined'?0:trajectories.length});}\n",
    "function restoreAnnotationUndo(){if(!annotationUndo.length){notify('Nothing to undo','warning');return false;}const state=annotationUndo.pop();const current=annotationSnapshot();if(state&&state.project&&typeof projectUndoSnapshot==='function')current.project=projectUndoSnapshot('project_redo');pushHistory(annotationRedo,current);restoreAnnotationState(state,'annotation_undo');recordAnnotationHistory('annotation_undo',{undo:annotationUndo.length,redo:annotationRedo.length});notify('Undo applied','success');return true;}\n",
    "function restoreAnnotationRedo(){if(!annotationRedo.length){notify('Nothing to redo','warning');return false;}const state=annotationRedo.pop();const current=annotationSnapshot();if(state&&state.project&&typeof projectUndoSnapshot==='function')current.project=projectUndoSnapshot('project_undo');pushHistory(annotationUndo,current);restoreAnnotationState(state,'annotation_redo');recordAnnotationHistory('annotation_redo',{undo:annotationUndo.length,redo:annotationRedo.length});notify('Redo applied','success');return true;}\n",
    "function annotationLabelValue(){return '';}\n",
    "function clearNextAnnotationName(){activeRoiName='';nextRoiNameDirty=false;}\n",
    "function nextRoiClassSelects(){return ['panelRoiClassSelect'].map(id=>el(id)).filter(Boolean);}\n",
    "function nextRoiCustomInputs(){return ['panelRoiClassCustom'].map(id=>el(id)).filter(Boolean);}\n",
    "function syncNextRoiCustomInputs(value='',source=null){nextRoiCustomInputs().forEach(input=>{if(input!==source)input.value=value;});}\n",
    "function customCategoryValue(){for(const input of nextRoiCustomInputs()){const value=input.value.trim();if(value)return value;}return '';}\n",
    "function currentRoiClass(){const custom=customCategoryValue();if(custom)return custom;const select=nextRoiClassSelects().find(s=>s&&s.value);return select&&select.value?select.value:(nextRoiClass||activeRoiClass||'annotation');}\n",
    "function setNextRoiClass(cls){const value=String(cls||'annotation').trim()||'annotation';nextRoiClass=value;activeRoiClass=value;ensureRoiClassOption(value);nextRoiClassSelects().forEach(select=>setSelectValue(select,value));const panel=el('annotationClassSelect');if(panel&&selectedRoi<0)setSelectValue(panel,value);return value;}\n",
    "function escapeRegExp(value){return String(value||'').replace(/[.*+?^${}()|[\\]\\\\]/g,'\\\\$&');}\n",
    "function automaticAnnotationName(className,labelPrefix='ROI',excludeIndex=-1){const raw=String(className||'').trim()||String(labelPrefix||'ROI').trim()||'ROI';return raw.replace(/\\s+/g,' ');}\n",
    "function ensureRoiClassOption(value){if(!value)return;const selects=(typeof classSelects==='function')?classSelects():[el('panelRoiClassSelect')].filter(Boolean);selects.forEach(select=>addSelectOption(select,value));}\n",
    "function roiPanelUserHidden(){const panel=el('roiPanel');return !!(panel&&(!panel.classList.contains('open')||panel.classList.contains('minimized')));}\n",
    "function setRoiPanelOpen(open,options={}){const panel=el('roiPanel'),automatic=!!(options&&options.automatic),save=!(options&&options.save===false);if(open&&automatic&&roiPanelUserHidden())return false;if(panel){panel.classList.toggle('open',!!open);panel.style.display=open?'':'';if(open)panel.classList.remove('minimized');}const isOpen=!!(panel&&panel.classList.contains('open'));['layersToggle','annotationPanelToggle','layerPanelToggle'].forEach(id=>{const button=el(id);if(button)button.classList.toggle('active',isOpen);});if(save&&typeof savePanelPreferences==='function')savePanelPreferences();return isOpen;}\n",
    "function openRoiPanel(){setRoiPanelOpen(true);updateButtons();notify('Annotation panel opened','success',1400);}\n",
    "function toggleRoiPanel(){const panel=el('roiPanel');if(!(panel&&panel.classList.contains('open')))openRoiPanel();else setRoiPanelOpen(false);}\n",
    "function setRoiPanelMinimized(minimized,save=true){const panel=el('roiPanel'),header=el('roiPanelHeader'),state=el('roiPanelMinimizeState');if(!panel)return;panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(state)state.textContent=minimized?'double-click to expand':'double-click to minimize';if(save&&typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleRoiPanelMinimized(){const panel=el('roiPanel');if(!panel)return;if(!panel.classList.contains('open'))setRoiPanelOpen(true);setRoiPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function bindRoiPanelControls(){const header=el('roiPanelHeader'),close=el('roiPanelClose');if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.addEventListener('mousedown',startRoiPanelDrag);header.ondblclick=e=>{e.preventDefault();toggleRoiPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleRoiPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';close.onclick=e=>{e.preventDefault();e.stopPropagation();setRoiPanelOpen(false);notify('Annotation panel closed','info',1600);};}}\n",
    "function setSelectionText(id,value){const node=el(id);if(node)node.textContent=value;}\n",
    "function selectedRoiAreaValue(roi){if(!roi)return NaN;if(Number.isFinite(Number(roi.area)))return Number(roi.area);if(isDrawable(roi))return roiDrawGroups(roi).reduce((sum,g)=>sum+polygonArea(g.rings)-g.holes.reduce((s,h)=>s+ringArea(h),0),0);return NaN;}\n",
    "function formatSelectionArea(area){if(!Number.isFinite(area)||area<=0)return 'NA';const px=(typeof measurePixelSize==='function')?measurePixelSize():null;if(px){const mm2=area*px.x*px.y/1e6;return fmt(mm2,3)+' mm2';}return fmt(area,0)+' px2';}\n",
    "function roiIsCellLike(roi){if(!roi)return false;const source=String(roi.source||'').toLowerCase(),cls=String(roi.class||'').toLowerCase(),props=roi.properties||{},obj=String(props.objectType||props.object_type||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells'||obj==='detection';}\n",
    "function roiCentroidPoint(roi){if(!roi)return null;const c=roi.centroid||roi.center;if(c){const x=Number(c.x!=null?c.x:c[0]),y=Number(c.y!=null?c.y:c[1]);if(Number.isFinite(x)&&Number.isFinite(y))return {x:x,y:y};}const b=roiBounds(roi);return b?{x:(b.xmin+b.xmax)/2,y:(b.ymin+b.ymax)/2}:null;}\n",
    "function selectionCellCount(roi,index){if(!roi||!isDrawable(roi))return NaN;let n=0;rois.forEach((candidate,i)=>{if(i===index||!visibleRoi(candidate)||!roiIsCellLike(candidate))return;const p=roiCentroidPoint(candidate);if(p&&roiContainsPoint(roi,p))n++;});return n;}\n",
    "function formatSelectionDensity(cells,area){if(!Number.isFinite(cells)||!Number.isFinite(area)||area<=0)return 'NA';const px=(typeof measurePixelSize==='function')?measurePixelSize():null;if(px){const mm2=area*px.x*px.y/1e6;return mm2>0?(fmt(cells/mm2,1)+' cells/mm2'):'NA';}return fmt(cells/area*1e6,1)+' cells/Mpx';}\n",
    "function setSelectionCardVisible(show){const card=el('selectionCard');selectionCardVisible=!!(show&&selectedRoi>=0&&rois[selectedRoi]);if(card){card.classList.toggle('open',selectionCardVisible);card.setAttribute('aria-hidden',selectionCardVisible?'false':'true');}}\n",
    "function toggleSelectionCard(){if(selectedRoi<0||!rois[selectedRoi]){setSelectionCardVisible(false);notify('Select an ROI','warning');return;}setSelectionCardVisible(!selectionCardVisible);updateSelectionCard();}\n",
    "function updateSelectionCard(){const card=el('selectionCard'),roi=selectedRoi>=0?rois[selectedRoi]:null;if(!card)return;if(!roi){setSelectionCardVisible(false);return;}const area=selectedRoiAreaValue(roi),cells=selectionCellCount(roi,selectedRoi),sw=el('selectionCardSwatch'),del=el('selectionDelete'),edit=el('selectionEdit');if(sw)sw.style.background=roi.colour||'#cccccc';setSelectionText('selectionCardName',roiLabelText(roi,selectedRoi));setSelectionText('selectionCardClass',lockedRoi(roi)?((roi.class||'annotation')+' | locked'):(roi.class||'annotation'));setSelectionText('selectionCardArea',formatSelectionArea(area));setSelectionText('selectionCardCells',Number.isFinite(cells)?String(cells):'NA');setSelectionText('selectionCardDensity',formatSelectionDensity(cells,area));if(del)del.disabled=!editableRoi(roi);if(edit)edit.disabled=!editableRoi(roi)||!isDrawable(roi);setSelectionCardVisible(selectionCardVisible);}\n",
    "function bindSelectionCardControls(){const zoom=el('selectionZoom'),edit=el('selectionEdit'),del=el('selectionDelete'),close=el('selectionClose');if(zoom)zoom.onclick=()=>{if(selectedRoi>=0)centerRoi(selectedRoi);};if(edit)edit.onclick=()=>{if(selectedRoi>=0&&isDrawable(rois[selectedRoi])&&editableRoi(rois[selectedRoi]))setMode('edit');};if(del)del.onclick=()=>deleteSelectedRoi();if(close)close.onclick=()=>setSelectionCardVisible(false);}\n",
    "function classSelects(){return [...nextRoiClassSelects(),el('annotationClassSelect')].filter(Boolean);}\n",
    "function roiClassSelects(){return classSelects();}\n",
    "function addSelectOption(select,value){if(!select)return;if(value&&!Array.from(select.options).some(o=>o.value===value)){const opt=document.createElement('option');opt.value=value;opt.textContent=value;select.appendChild(opt);}}\n",
    "function setSelectValue(select,value){if(!select)return;addSelectOption(select,value);if(value)select.value=value;}\n",
    "function populateRoiClassSelects(){classSelects().forEach(select=>{const current=select.value||activeRoiClass||'';select.innerHTML='';roiClassPresets.forEach(p=>{const opt=document.createElement('option');opt.value=p.class;opt.textContent=p.label||p.class;select.appendChild(opt);});if(current)setSelectValue(select,current);else if(roiClassPresets.length)select.value=roiClassPresets[0].class;});}\n",
    "function recolourRoisForClass(cls,colour,markEdited=true){const key=classPresetKey(cls),c=normaliseHexColour(colour,'');if(!key||!c)return;rois.forEach(roi=>{if(classPresetKey(roi.class)===key)setRoiColour(roi,c,markEdited);});}\n",
    "function setClassColour(cls,colour,markEdited=true){const preset=ensureClassPreset(cls,colour);const c=normaliseHexColour(colour,preset.color||stableClassColour(cls));preset.color=c;recolourRoisForClass(preset.class,c,markEdited);return c;}\n",
    "function applyClassPresetColoursToRois(markEdited=false){rois.forEach(roi=>{if(!roi)return;const cls=roi.class||'annotation',seed=normaliseHexColour(roi.colour||roi.original_colour||'','');const colour=classColour(cls,seed);setRoiColour(roi,colour,markEdited);if(markEdited===false)roi.original_colour=colour;});}\n",
    "function setRoiControlsFromSelection(){const roi=selectedRoi>=0?rois[selectedRoi]:null;const has=!!roi,selectedEditor=el('selectedAnnotationEditor');if(selectedEditor)selectedEditor.hidden=!has;const panelClass=el('annotationClassSelect'),panelCustom=el('annotationClassCustom'),panelColor=el('annotationColorInput');if(!has){if(panelCustom)panelCustom.value='';if(panelClass&&(mode==='brush'||brushing))setSelectValue(panelClass,brushClass||nextRoiClass||activeRoiClass||'annotation');return;}const cls=roi.class||'annotation';if(panelCustom)panelCustom.value='';if(panelClass)setSelectValue(panelClass,cls);if(panelColor)panelColor.value=normaliseHexColour(roi.colour||classColour(cls)||'#00BFC4');}\n",
    "function updateRoiList(){if(typeof enforceSingleObjectSelection==='function')enforceSingleObjectSelection('annotation');document.querySelectorAll('.roiItem').forEach(b=>{const i=Number(b.dataset.index),roi=rois[i],selected=roi&&!!roi.export_selected;b.classList.toggle('active',i===selectedRoi||selected);b.classList.toggle('hidden',roi&&roi.visible===false);b.classList.toggle('locked',lockedRoi(roi));b.classList.toggle('highlighted',roi&&roiClassHighlighted(roi));const box=b.querySelector('.roiSelect');if(box)box.checked=!!selected;});setRoiControlsFromSelection();updateSelectionCard();renderAnnotationLabelHighlights();if(typeof syncProximityAnnotations==='function')syncProximityAnnotations(false);updateButtons();syncRoiSelection();}\n",
    "function addDetail(parent,label,value,asCode=false){const l=document.createElement('span');l.textContent=label;const v=document.createElement(asCode?'code':'span');v.textContent=value;parent.append(l,v);}\n",
    "function roiExportIndices(){const marked=[];rois.forEach((roi,i)=>{if(roi.export_selected)marked.push(i);});let out=marked.length?marked:((selectedRoi>=0&&rois[selectedRoi])?[selectedRoi]:[]);if(respectClassExportRules())out=out.filter(i=>roiAllowedByExportRules(rois[i]));return out;}\n",
    "function annotationSearchQuery(){const input=el('annotationSearchInput');return String(input&&input.value||'').trim().toLowerCase();}\n",
    "function annotationFilterValue(){const input=el('annotationFilter');return input&&input.value?input.value:'all';}\n",
    "function annotationSortValue(){const input=el('annotationSort');return input&&input.value?input.value:'original';}\n",
    "function roiListArea(roi){const area=selectedRoiAreaValue(roi);return Number.isFinite(area)?area:-Infinity;}\n",
    "function roiListSearchText(roi,i){return [roiLabelText(roi,i),roi.class||'',roi.id||'',roi.source||'',geometryType(roi)].join(' ').toLowerCase();}\n",
    "function roiMatchesAnnotationSearch(roi,i){const q=annotationSearchQuery();return !q||roiListSearchText(roi,i).includes(q);}\n",
    "function roiMatchesAnnotationFilter(roi,i){const f=annotationFilterValue();if(f==='visible')return visibleRoi(roi);if(f==='hidden')return !visibleRoi(roi);if(f==='locked')return lockedRoi(roi);if(f==='unlocked')return !lockedRoi(roi);if(f==='selected')return i===selectedRoi||!!roi.export_selected;return true;}\n",
    "function annotationListActive(){return !!(annotationSearchQuery()||annotationFilterValue()!=='all'||annotationSortValue()!=='original');}\n",
    "function currentRoiListEntries(){const entries=rois.map((roi,i)=>({roi:roi,index:i,area:roiListArea(roi)})).filter(entry=>roiMatchesAnnotationSearch(entry.roi,entry.index)&&roiMatchesAnnotationFilter(entry.roi,entry.index));const sort=annotationSortValue();entries.sort((a,b)=>{if(sort==='name')return roiLabelText(a.roi,a.index).localeCompare(roiLabelText(b.roi,b.index))||a.index-b.index;if(sort==='class')return String(a.roi.class||'').localeCompare(String(b.roi.class||''))||roiLabelText(a.roi,a.index).localeCompare(roiLabelText(b.roi,b.index))||a.index-b.index;if(sort==='area_desc')return (b.area-a.area)||a.index-b.index;if(sort==='area_asc')return (a.area-b.area)||a.index-b.index;return a.index-b.index;});return entries;}\n",
    "function annotationListSummary(entries){let text=geometrySummary();if(annotationListActive())text+=' | showing '+entries.length+'/'+rois.length;return text;}\n",
    "function clearAnnotationListFilters(){const search=el('annotationSearchInput'),filter=el('annotationFilter'),sort=el('annotationSort');if(search)search.value='';if(filter)filter.value='all';if(sort)sort.value='original';buildRoiList();}\n",
    "function bindAnnotationListControls(){const search=el('annotationSearchInput'),filter=el('annotationFilter'),sort=el('annotationSort'),clear=el('annotationFilterClear'),all=el('annotationHighlightAll'),none=el('annotationHighlightNone');const rebuild=()=>buildRoiList();if(search){search.oninput=rebuild;search.onkeydown=e=>{if(e.key==='Escape'){e.preventDefault();clearAnnotationListFilters();}};}if(filter)filter.onchange=rebuild;if(sort)sort.onchange=rebuild;if(clear)clear.onclick=clearAnnotationListFilters;if(all)all.onclick=()=>setAnnotationLabelHighlightAll(true);if(none)none.onclick=clearAnnotationLabelHighlights;renderAnnotationLabelHighlights();}\n",
    "function focusRoiCategoryEditor(index){const i=Number(index);if(!Number.isFinite(i)||!rois[i]){notify('Select an ROI','warning');return false;}selectAnnotation(i,false);setRoiControlsFromSelection();updateRoiList();const editor=el('selectedAnnotationEditor'),select=el('annotationClassSelect');if(editor){editor.hidden=false;editor.scrollIntoView({block:'nearest'});}if(select)setTimeout(()=>select.focus(),0);notify('Choose a category and click Apply','info',2200);draw();return true;}\n",
    "function buildRoiList(){const list=el('roiList'),summary=el('roiSummary');if(!list||!summary)return;list.innerHTML='';const entries=currentRoiListEntries();summary.textContent=annotationListSummary(entries);entries.forEach(entry=>{const roi=entry.roi,i=entry.index;const item=document.createElement('div');item.className='roiItem';item.dataset.index=String(i);item.style.setProperty('--wsi-highlight-accent',roi.colour||classColour(roi.class||'annotation'));const top=document.createElement('div');top.className='roiTop';const exportBox=document.createElement('input');exportBox.type='checkbox';exportBox.className='roiSelect';exportBox.title='Select this annotation for export';exportBox.checked=!!roi.export_selected;exportBox.onclick=e=>e.stopPropagation();exportBox.onchange=e=>{roi.export_selected=!!e.target.checked;updateButtons();scheduleViewerStateSync('roi_export_selection_updated',{id:roi.id||null,selected:roi.export_selected});};const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour||'#cccccc';const nm=document.createElement('span');nm.className='roiName';nm.textContent=roiLabelText(roi,i);const cl=document.createElement('span');cl.className='roiClass';cl.textContent=(lockedRoi(roi)?'locked ':'')+(roi.class||'');top.append(exportBox,sw,nm,cl);const details=document.createElement('div');details.className='roiDetails';const bb=roiBounds(roi);addDetail(details,'Geometry',geometryType(roi));addDetail(details,'Bounds',formatBounds(bb),true);addDetail(details,'Points',String(pointCount(roi)));const area=entry.area;addDetail(details,'Area',Number.isFinite(area)?fmt(area,1):'NA');addDetail(details,'Source',roi.source||'geojson');addDetail(details,'ID',String(roi.id||i+1),true);if(!isDrawable(roi)){const badge=document.createElement('span');badge.className='roiBadge';badge.textContent='listed only';details.append(document.createElement('span'),badge);}const controls=document.createElement('div');controls.className='roiControls';const vis=document.createElement('button');vis.type='button';vis.textContent=visibleRoi(roi)?'Hide':'Show';vis.title='Toggle annotation visibility';vis.onclick=e=>{e.stopPropagation();toggleRoiVisibility(i);};const lock=document.createElement('button');lock.type='button';lock.textContent=lockedRoi(roi)?'Unlock':'Lock';lock.title='Lock or unlock annotation editing';lock.onclick=e=>{e.stopPropagation();toggleRoiLock(i);};const colour=document.createElement('input');colour.type='color';colour.value=normaliseHexColour(roi.colour||'#00BFC4');colour.title='Annotation color';colour.onclick=e=>e.stopPropagation();colour.onchange=e=>{e.stopPropagation();updateRoiColor(i,e.target.value);};const category=document.createElement('button');category.type='button';category.textContent='Category';category.title='Modify annotation category';category.onclick=e=>{e.stopPropagation();focusRoiCategoryEditor(i);};const zoom=document.createElement('button');zoom.type='button';zoom.textContent='Zoom';zoom.title='Zoom to ROI';zoom.onclick=e=>{e.stopPropagation();centerRoi(i);};const dup=document.createElement('button');dup.type='button';dup.textContent='Dup';dup.title='Duplicate ROI';dup.onclick=e=>{e.stopPropagation();duplicateRoi(i);};const del=document.createElement('button');del.type='button';del.textContent='Del';del.title='Delete ROI';del.onclick=e=>{e.stopPropagation();deleteRoi(i);};controls.append(vis,lock,colour,category,zoom,dup,del);item.append(top,details,controls);item.onclick=()=>{selectAnnotation(i,false);if(isDrawable(roi)){updateRoiList();draw();}else{updateRoiList();draw();notify('Geometry listed only','warning');}};list.appendChild(item);});if(!entries.length){const empty=document.createElement('div');empty.className='roiListEmpty';empty.textContent=rois.length?'No annotations match the current search or filter.':'No annotations yet.';list.appendChild(empty);}if(rois.length)setRoiPanelOpen(true,{automatic:true});updateRoiList();}\n",
    "function geojsonImportStatus(msg){const box=el('geojsonImportSummary');if(box)box.textContent=msg||'';}\n",
    "function clonePlain(x){return x&&typeof x==='object'?JSON.parse(JSON.stringify(x)):{};}\n",
    "function geojsonFeatures(obj){if(!obj)return [];if(obj.type==='FeatureCollection')return obj.features||[];if(obj.type==='Feature')return [obj];if(obj.geometry)return [{type:'Feature',geometry:obj.geometry,properties:obj.properties||{}}];return [];}\n",
    "function collectGeojsonPoints(coords,out=[]){if(Array.isArray(coords)&&coords.length>=2&&typeof coords[0]==='number'&&typeof coords[1]==='number'){const x=Number(coords[0]),y=Number(coords[1]);if(Number.isFinite(x)&&Number.isFinite(y))out.push({x:x,y:y});return out;}if(Array.isArray(coords))coords.forEach(c=>collectGeojsonPoints(c,out));return out;}\n",
    "function boundsFromPoints(points){if(!points||!points.length)return null;const xs=points.map(p=>p.x),ys=points.map(p=>p.y);return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointFromGeojsonCoord(coord){if(!coord||coord.length<2)return null;const x=Number(coord[0]),y=Number(coord[1]);if(!Number.isFinite(x)||!Number.isFinite(y))return null;return {x:x,y:y};}\n",
    "function ringFromGeojsonCoords(coords){const ring=(coords||[]).map(pointFromGeojsonCoord).filter(Boolean);return closedRing(ring);}\n",
    "function polygonRingsFromGeojsonCoords(coords){return (coords||[]).map(ringFromGeojsonCoords).filter(r=>r.length>=4);}\n",
    "function geojsonGeometryParts(geometry){const type=String((geometry&&geometry.type)||'Geometry'),lower=type.toLowerCase(),coords=(geometry&&geometry.coordinates)||[];if(lower==='polygon'){const rings=polygonRingsFromGeojsonCoords(coords);return rings.length?[{geometry_type:'Polygon',rings:rings,add_groups:[],coordinates:coords}]:[];}if(lower==='multipolygon'){const groups=(coords||[]).map(poly=>polygonRingsFromGeojsonCoords(poly)).filter(g=>g.length);return groups.length?[{geometry_type:'MultiPolygon',rings:groups[0],add_groups:groups.slice(1),coordinates:coords}]:[];}return [{geometry_type:type,rings:[],add_groups:[],coordinates:coords}];}\n",
    "function importedFeatureClass(properties){const cls=properties&&properties.classification;if(cls&&typeof cls==='object'&&cls.name)return String(cls.name);if(typeof cls==='string')return cls;if(properties&&properties.class)return String(properties.class);if(properties&&properties.objectType)return String(properties.objectType);return 'annotation';}\n",
    "function featureColorValue(value){if(value===null||typeof value==='undefined')return null;if(typeof value==='number'&&Number.isFinite(value)){const rgb=((value%16777216)+16777216)%16777216;return '#'+rgb.toString(16).padStart(6,'0').toUpperCase();}if(typeof value==='string'){const c=normaliseHexColour(value,'');return c||null;}if(typeof value==='object'){const r=Number(value.r??value.red),g=Number(value.g??value.green),b=Number(value.b??value.blue);if([r,g,b].every(Number.isFinite)){const rr=Math.round(r<=1?r*255:r),gg=Math.round(g<=1?g*255:g),bb=Math.round(b<=1?b*255:b);return '#'+[rr,gg,bb].map(v=>Math.round(clamp(v,0,255)).toString(16).padStart(2,'0')).join('').toUpperCase();}}return null;}\n",
    "function importedFeatureColor(properties,fallback){const cls=properties&&properties.classification;const candidates=[properties&&properties.color,properties&&properties.colour,properties&&properties.colorRGB,cls&&typeof cls==='object'&&cls.color,cls&&typeof cls==='object'&&cls.colour,cls&&typeof cls==='object'&&cls.colorRGB];for(const candidate of candidates){const c=featureColorValue(candidate);if(c)return c;}return fallback;}\n",
    "function importedFeatureName(feature,properties,i){const cls=importedFeatureClass(properties);return String((properties&&(properties.name||properties.label||properties.objectName))||feature.id||cls||('Imported ROI '+(i+1)));}\n",
    "function importedRoiFromFeature(feature,part,i,fileName,partCount){const props=clonePlain(feature.properties||{}),featureCopy=clonePlain(feature||{}),rings=part.rings||[],addGroups=part.add_groups||[],groups=[rings].concat(addGroups).filter(g=>g&&g.length),points=collectGeojsonPoints(part.coordinates||((feature.geometry||{}).coordinates)||[]),ringPoints=[];groups.forEach(g=>g.forEach(r=>r.forEach(p=>ringPoints.push(p))));const importedClass=importedFeatureClass(props),importedColour=importedFeatureColor(props,''),colour=classColour(importedClass,importedColour||paletteColour(rois.length+i)),baseName=importedFeatureName(feature,props,i),suffix=partCount>1?(' part '+part.part):'',name=baseName+suffix,id=String(feature.id||props.id||props.objectId||('imported_roi_'+Date.now()+'_'+i))+(partCount>1?('_'+part.part):''),bbox=ringPoints.length?boundsFromPoints(ringPoints):boundsFromPoints(points);const locked=props.isLocked===true||props.locked===true;featureCopy.type='Feature';featureCopy.id=id;featureCopy.properties=props;featureCopy.geometry=clonePlain(feature.geometry||{});const roi={id:id,name:name,label:name,class:importedClass,visible:props.visible!==false,locked:locked,isLocked:locked,geometry_type:part.geometry_type||((feature.geometry||{}).type||'Geometry'),source:fileName?('imported: '+fileName):'imported geojson',drawable:groups.length>0,point_count:points.length,area:groups.length?groups.reduce((s,g)=>s+polygonArea(g),0):NaN,bbox:bbox,coordinates:part.coordinates,colour:colour,original_colour:colour,fill:hexToRgba(colour,0.18),rings:rings,add_groups:addGroups,measurements:props.measurements||null,centroid:props.centroid||props.center||null,properties:props,geometry:clonePlain(feature.geometry||{}),feature:featureCopy,imported:true};if(roi.drawable)refreshRoiGeometry(roi);return roi;}\n",
    "function geojsonMaskUrl(){return String(cfg.geojson_mask_url||'');}\n",
    "function geojsonCoordinateCount(value){if(!Array.isArray(value))return 0;if(value.length>=2&&typeof value[0]==='number'&&typeof value[1]==='number')return 1;return value.reduce((n,v)=>n+geojsonCoordinateCount(v),0);}\n",
    "function geojsonImportLooksLikeCells(obj,fileName){const features=geojsonFeatures(obj),name=String(fileName||'');if(/cell|segmentation|mask|medsam|stardist|mesmer|cellphenotyper/i.test(name))return true;return features.some(f=>{const p=f.properties||{},cls=p.classification;if(/cell|nucleus|segmentation|mesmer|stardist/i.test(String(p.objectType||p.type||p.name||p.label||'')))return true;if(cls&&/cell|nucleus|segmentation/i.test(String(typeof cls==='object'?cls.name:cls)))return true;return false;});}\n",
    "function geojsonShouldImportAsMask(obj,fileName){const features=geojsonFeatures(obj);if(!features.length)return false;const pointCount=features.reduce((n,f)=>n+geojsonCoordinateCount((f.geometry||{}).coordinates||[]),0);return geojsonImportLooksLikeCells(obj,fileName)||features.length>=1000||pointCount>=50000;}\n",
    "async function addImportedGeojsonAsMask(obj,fileName){const url=geojsonMaskUrl();if(!url){geojsonImportStatus('Dense/cell GeoJSON needs a live R viewer for mask conversion. Open with wsi_viewer_live().');notify('Dense GeoJSON was not imported as vector geometry. Reopen with live R sync to convert it to a tiled mask overlay.','warning',5600);return;}geojsonImportStatus('Converting GeoJSON to a tiled mask overlay in R...');try{const response=await fetch(url,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({geojson:obj,file:fileName||'imported.geojson',name:'Mask: '+(fileName||'imported GeoJSON'),id:'mask_'+String(fileName||Date.now()).replace(/[^A-Za-z0-9_]+/g,'_').replace(/^_+|_+$/g,''),downsample:4,label_by:'class',opacity:0.45,visible:true,smooth:true,smooth_iterations:1,smooth_max_vertices:4000})});let payload=null;try{payload=await response.json();}catch(e){payload={error:await response.text()};}if(!response.ok||!payload||payload.ok===false)throw new Error((payload&&payload.error)||('GeoJSON mask conversion failed with HTTP '+response.status));const overlay=payload.geojson_mask_overlay||payload;if(overlay&&overlay.source&&typeof upsertChannelSource==='function')upsertChannelSource(overlay.source);if(payload&&Array.isArray(payload.commands))handleViewerCommands(payload);if(typeof buildChannelList==='function')buildChannelList();if(typeof buildLayerList==='function')buildLayerList();draw();recordAnnotationHistory('geojson_mask_overlay_created',{file:fileName||null,id:overlay&&overlay.source?overlay.source.id:null});geojsonImportStatus('Converted GeoJSON to tiled mask overlay.');notify('GeoJSON displayed as a mask overlay','success',3200);}catch(e){geojsonImportStatus('Could not convert GeoJSON to mask overlay: '+e.message);notify('Could not convert GeoJSON to mask overlay: '+e.message,'error',6200);}}\n",
    "function importGeojsonObject(obj,fileName){if(geojsonShouldImportAsMask(obj,fileName))return addImportedGeojsonAsMask(obj,fileName);return addImportedGeojson(obj,fileName);}\n",
    "function addImportedGeojson(obj,fileName){const features=geojsonFeatures(obj);if(!features.length){geojsonImportStatus('No GeoJSON features found.');return;}pushAnnotationUndo('geojson_imported');let added=0,listed=0;features.forEach((feature,fi)=>{const parts=geojsonGeometryParts(feature.geometry||{});parts.forEach((part,pi)=>{const roi=importedRoiFromFeature(feature,part,added,fileName,parts.length);if(!roi.drawable&&(!roi.bbox||!Number.isFinite(Number(roi.bbox.xmin))))return;rois.push(roi);if(roi.drawable)added++;else listed++;});});if(!added&&!listed){geojsonImportStatus('No supported GeoJSON geometries found.');return;}selectAnnotation(rois.length-1,false);showRois=true;buildRoiList();updateButtons();draw();recordAnnotationHistory('geojson_imported',{file:fileName||null,added:added,listed:listed});scheduleViewerStateSync('geojson_imported',{file:fileName||null,added:added,listed:listed});geojsonImportStatus('Imported '+added+' drawable ROI'+(added===1?'':'s')+(listed?(' and listed '+listed+' other geometr'+(listed===1?'y':'ies')):'' )+'.');}\n",
    "function bindGeojsonImportControls(){const button=el('importGeojson'),file=el('geojsonImportFile');if(button&&file)button.onclick=()=>{file.value='';file.click();};if(file){file.onchange=()=>{const picked=file.files&&file.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{Promise.resolve(importGeojsonObject(JSON.parse(reader.result),picked.name)).catch(e=>{geojsonImportStatus('Could not import GeoJSON: '+e.message);notify('Could not import GeoJSON: '+e.message,'error',5200);});}catch(e){geojsonImportStatus('Could not import GeoJSON: '+e.message);}};reader.readAsText(picked);};}geojsonImportStatus('');}\n",
    "function paletteColour(i){const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];return palette[i%palette.length];}\n",
    "function refreshRoiGeometry(roi){if(!isDrawable(roi))return;let xs=[],ys=[],area=0;positiveRingGroups(roi).forEach(group=>{group.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));area+=polygonArea(group);});subtractRings(roi).forEach(h=>{area-=ringArea(h);});roi.drawable=true;roi.bbox=xs.length?{xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)}:null;roi.area=Math.max(0,area);roi.point_count=allRoiRings(roi,true).reduce((n,r)=>n+r.length,0);roi.geometry_type=(positiveRingGroups(roi).length>1)?'MultiPolygon':'Polygon';roi.edited=true;}\n",
    "function closedRing(points){const ring=points.map(p=>({x:Math.round(clamp(p.x,0,cfg.slide_width)),y:Math.round(clamp(p.y,0,cfg.slide_height))}));if(ring.length<3)return ring;const f=ring[0],l=ring[ring.length-1];if(!l||f.x!==l.x||f.y!==l.y)ring.push({x:f.x,y:f.y});return ring;}\n",
    "function clonePoint(p){return {x:Number(p.x),y:Number(p.y)};}\n",
    "function cloneRing(ring){return (ring||[]).map(clonePoint);}\n",
    "function ringOpenPoints(ring){const pts=cloneRing(ring);if(pts.length>1){const f=pts[0],l=pts[pts.length-1];if(f.x===l.x&&f.y===l.y)pts.pop();}return pts;}\n",
    "function setRoiPositiveGroups(roi,groups){const clean=(groups||[]).map(g=>(g||[]).filter(r=>r&&r.length>=4)).filter(g=>g.length);roi.rings=clean.length?clean[0]:[];roi.add_groups=clean.slice(1);roi.add_rings=[];refreshRoiGeometry(roi);}\n",
    "function transformRoiRings(roi,fn){const groups=positiveRingGroups(roi).map(group=>group.map(ring=>closedRing(fn(ringOpenPoints(ring)))).filter(r=>r.length>=4)).filter(group=>group.length);const sub=subtractRings(roi).map(ring=>closedRing(fn(ringOpenPoints(ring)))).filter(r=>r.length>=4);setRoiPositiveGroups(roi,groups);roi.subtract_rings=sub;refreshRoiGeometry(roi);}\n",
    "function chaikinSmoothPoints(points,iterations=2){let pts=points.map(clonePoint);if(pts.length<3)return pts;for(let it=0;it<iterations;it++){const out=[];for(let i=0;i<pts.length;i++){const p=pts[i],q=pts[(i+1)%pts.length];out.push({x:p.x*.75+q.x*.25,y:p.y*.75+q.y*.25});out.push({x:p.x*.25+q.x*.75,y:p.y*.25+q.y*.75});}pts=out;}return pts;}\n",
    "function pointLineDistance(p,a,b){const dx=b.x-a.x,dy=b.y-a.y,len2=dx*dx+dy*dy;if(!len2)return Math.hypot(p.x-a.x,p.y-a.y);let t=((p.x-a.x)*dx+(p.y-a.y)*dy)/len2;t=clamp(t,0,1);return Math.hypot(p.x-(a.x+t*dx),p.y-(a.y+t*dy));}\n",
    "function rdpSimplify(points,tolerance){if(points.length<=3)return points.map(clonePoint);let best=0,maxD=-1;const first=points[0],last=points[points.length-1];for(let i=1;i<points.length-1;i++){const d=pointLineDistance(points[i],first,last);if(d>maxD){maxD=d;best=i;}}if(maxD>tolerance){const left=rdpSimplify(points.slice(0,best+1),tolerance),right=rdpSimplify(points.slice(best),tolerance);return left.slice(0,-1).concat(right);}return [clonePoint(first),clonePoint(last)];}\n",
    "function simplifyClosedPoints(points,tolerance){if(points.length<=3)return points.map(clonePoint);const pts=points.map(clonePoint),anchor=pts[0];let far=1,maxD=-1;for(let i=1;i<pts.length;i++){const d=Math.hypot(pts[i].x-anchor.x,pts[i].y-anchor.y);if(d>maxD){maxD=d;far=i;}}const rotated=pts.slice(far).concat(pts.slice(0,far+1));let out=rdpSimplify(rotated,Math.max(0,Number(tolerance)||0));if(out.length>1)out.pop();if(out.length<3)return pts;return out;}\n",
    "function checkedAnnotationIndices(){const out=[];rois.forEach((roi,i)=>{if(roi.export_selected)out.push(i);});return out;}\n",
    "function selectedEditableDrawableRoi(){return selectedRoi>=0&&rois[selectedRoi]&&isDrawable(rois[selectedRoi])&&editableRoi(rois[selectedRoi])?rois[selectedRoi]:null;}\n",
    "function smoothSelectedRoi(){const roi=selectedEditableDrawableRoi();if(!roi){notify('Select unlocked ROI','warning');return;}pushAnnotationUndo('roi_smoothed');transformRoiRings(roi,pts=>chaikinSmoothPoints(pts,2));enforceRoiNonOverlap(selectedRoi);buildRoiList();draw();recordAnnotationHistory('roi_smoothed',{id:roi.id||null,name:roiLabelText(roi,selectedRoi),non_overlapping:true});scheduleViewerStateSync('roi_smoothed',{id:roi.id||null,non_overlapping:true});notify('ROI smoothed','success');}\n",
    "function fillSelectedRoiHoles(){const roi=selectedEditableDrawableRoi();if(!roi){notify('Select unlocked ROI','warning');return;}pushAnnotationUndo('roi_holes_filled');const groups=positiveRingGroups(roi).map(group=>group&&group[0]?[cloneRing(group[0])]:[]).filter(group=>group.length);setRoiPositiveGroups(roi,groups);roi.subtract_rings=[];roi.filled_holes=true;refreshRoiGeometry(roi);enforceRoiNonOverlap(selectedRoi);buildRoiList();draw();recordAnnotationHistory('roi_holes_filled',{id:roi.id||null,name:roiLabelText(roi,selectedRoi),non_overlapping:true});scheduleViewerStateSync('roi_holes_filled',{id:roi.id||null,non_overlapping:true});notify('ROI holes filled','success');}\n",
    "function mergeCandidateIndices(){const indices=checkedAnnotationIndices().filter(i=>rois[i]&&isDrawable(rois[i]));return annotationIndicesShareClass(indices)?indices:[];}\n",
    "function mergeSelectedAnnotations(){const checked=checkedAnnotationIndices().filter(i=>rois[i]&&isDrawable(rois[i]));if(checked.length<2){notify('Select at least two ROIs','warning');return;}if(!annotationIndicesShareClass(checked)){notify('Only annotations with the same class can be merged','warning');return;}const indices=checked,locked=indices.filter(i=>!editableRoi(rois[i]));if(locked.length){notify('Unlock ROIs before merging','warning');return;}pushAnnotationUndo('rois_merged');const baseIndex=indices[0],base=rois[baseIndex],groups=[],sub=[];indices.forEach(i=>{positiveRingGroups(rois[i]).forEach(group=>groups.push(group.map(cloneRing)));subtractRings(rois[i]).forEach(r=>sub.push(cloneRing(r)));});setRoiPositiveGroups(base,groups);base.subtract_rings=sub;base.name=(base.name||base.label||base.id||'Merged ROI')+' merged';base.label=base.name;base.merged=true;base.export_selected=false;refreshRoiGeometry(base);indices.slice(1).sort((a,b)=>b-a).forEach(i=>rois.splice(i,1));selectAnnotation(baseIndex,false);buildRoiList();draw();recordAnnotationHistory('rois_merged',{count:indices.length,id:base.id||null,name:base.name||null,class:roiClassName(base)});scheduleViewerStateSync('rois_merged',{count:indices.length,id:base.id||null,class:roiClassName(base)});notify('ROIs merged','success');}\n",
    "function splitSelectedAnnotation(){const roi=selectedEditableDrawableRoi();if(!roi){notify('Select unlocked ROI','warning');return;}const groups=roiDrawGroups(roi).map(g=>g.rings.concat(g.holes)).filter(g=>g.length);if(groups.length<2){notify('Only one ROI part','warning');return;}pushAnnotationUndo('roi_split');const baseName=roi.name||roi.label||roi.id||'ROI',baseId=roi.id||('roi_'+(selectedRoi+1)),clones=groups.map((group,j)=>{const clone=JSON.parse(JSON.stringify(roi));clone.id=String(baseId)+'_part_'+(j+1);clone.name=baseName+' part '+(j+1);clone.label=clone.name;clone.rings=group.map(cloneRing);clone.add_groups=[];clone.add_rings=[];clone.subtract_rings=[];clone.export_selected=false;clone.split_from=baseId;clone.edited=true;refreshRoiGeometry(clone);return clone;});rois.splice(selectedRoi,1,...clones);buildRoiList();draw();recordAnnotationHistory('roi_split',{id:baseId,name:baseName,count:clones.length});scheduleViewerStateSync('roi_split',{id:baseId,count:clones.length});notify('ROI split into '+countText(clones.length),'success');}\n",
    "function addRoiFromRing(ring,source,labelPrefix,className=null){return addRoiFromBrushRings([ring],source,labelPrefix,className);}\n",
    "function looksLikeBrushPoint(value){return !!(value&&Number.isFinite(Number(value.x))&&Number.isFinite(Number(value.y)));}\n",
    "function looksLikeBrushRing(value){return Array.isArray(value)&&value.length&&looksLikeBrushPoint(value[0]);}\n",
    "function looksLikeBrushRingList(value){return Array.isArray(value)&&value.length&&looksLikeBrushRing(value[0]);}\n",
    "function normaliseBrushRings(rings){const raw=looksLikeBrushRing(rings)?[rings]:(Array.isArray(rings)?rings:[]);return raw.filter(looksLikeBrushRing).map(r=>closedRing(r||[])).filter(r=>r&&r.length>=4&&ringArea(r)>0);}\n",
    "function ringsToBrushGroups(rings){return normaliseBrushRings(rings).map(r=>[r]);}\n",
    "function normaliseBrushGroups(groups){if(looksLikeBrushRing(groups))return [[closedRing(groups)]].filter(g=>g[0]&&g[0].length>=4);if(looksLikeBrushRingList(groups))return [normaliseBrushRings(groups)].filter(g=>g.length);if(!Array.isArray(groups))return [];return groups.map(g=>normaliseBrushRings(g)).filter(g=>g.length);}\n",
    "function brushGroupRings(groups){let rings=[];(groups||[]).forEach(g=>{rings=rings.concat(g||[]);});return rings;}\n",
    "function addRoiFromBrushGroups(groups,source,labelPrefix,className=null){activeRoiClass=String(className||currentRoiClass()||'annotation').trim()||'annotation';nextRoiClass=activeRoiClass;activeRoiName=source==='brush'?'':annotationLabelValue();const clipped=clipBrushGroupsAgainstAnnotations(groups,-1,activeRoiClass),merged=unionBrushGroupsWithSameLabel(clipped.groups,activeRoiClass,-1),clean=merged.groups,all=brushGroupRings(clean);if(!clean.length||!all.length){notify('Annotation overlaps a different label; no new area to add','warning');return null;}const colour=classColour(activeRoiClass);pushAnnotationUndo(merged.merged?'roi_same_label_merged':'roi_added');if(merged.merged&&merged.merged_indices.length){const target=rois[merged.merged_indices[0]];setRoiPositiveGroups(target,clean);target.subtract_rings=[];target.class=activeRoiClass;if(activeRoiName){target.name=activeRoiName;target.label=activeRoiName;}else if(!(target.name||target.label)){const autoName=automaticAnnotationName(activeRoiClass,labelPrefix);target.name=autoName;target.label=autoName;}target.category_label=target.name||target.label||activeRoiClass;setRoiColour(target,colour,true);target.same_label_merged=true;target.non_overlapping=true;target.overlap_clipped=!!clipped.clipped;refreshRoiGeometry(target);removeMergedSameLabelAnnotations(merged.merged_indices,target);showRois=true;buildRoiList();updateButtons();recordAnnotationHistory('roi_same_label_merged',{source:source,id:target.id||null,name:roiLabelText(target,selectedRoi),class:target.class,color:colour,merged_count:merged.merged_indices.length,overlap_clipped:!!clipped.clipped,non_overlapping:true,consistent_category_label:true});scheduleViewerStateSync('roi_same_label_merged',{id:target.id||null,class:target.class,merged_count:merged.merged_indices.length,overlap_clipped:!!clipped.clipped,non_overlapping:true,consistent_category_label:true});notify(clipped.clipped?'ROI merged with same label; different-label overlap clipped':'ROI merged with same label','success');draw();return target;}newRoiCount++;const roiName=activeRoiName||automaticAnnotationName(activeRoiClass,labelPrefix);const roi={id:source+'_roi_'+newRoiCount,name:roiName,label:roiName,class:activeRoiClass,category_label:roiName,visible:true,locked:false,isLocked:false,geometry_type:clean.length>1?'MultiPolygon':'Polygon',source:source,drawable:true,point_count:all.reduce((n,r)=>n+r.length-1,0),area:clean.reduce((s,g)=>s+polygonArea(g),0),bbox:boundsFromRings(all),colour:colour,original_colour:colour,fill:hexToRgba(colour,0.18),rings:clean[0],add_groups:clean.slice(1),add_rings:[],subtract_rings:[],drawn:source==='drawn',brushed:source==='brush',brush_mask_contour:true,brush_ring_count:all.length,non_overlapping:true,overlap_clipped:!!clipped.clipped,automatic_name:!activeRoiName,consistent_category_label:!activeRoiName};rois.push(roi);selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();recordAnnotationHistory('roi_added',{source:source,id:roi.id,name:roi.name,class:roi.class,color:colour,brush_ring_count:all.length,brush_mask_contour:true,non_overlapping:true,overlap_clipped:!!clipped.clipped,automatic_name:!activeRoiName,consistent_category_label:!activeRoiName});scheduleViewerStateSync('roi_added',{source:source,id:roi.id,class:roi.class,name:roi.name,color:colour,brush_ring_count:all.length,brush_mask_contour:true,non_overlapping:true,overlap_clipped:!!clipped.clipped,automatic_name:!activeRoiName,consistent_category_label:!activeRoiName});notify(clipped.clipped?'ROI saved; different-label overlap clipped':'ROI saved','success');return roi;}\n",
    "function addRoiFromBrushRings(rings,source,labelPrefix,className=null){return addRoiFromBrushGroups(ringsToBrushGroups(rings),source,labelPrefix,className);}\n",
    "function finishDraft(){if(draft.length<3){notify('Add at least 3 points','warning');return;}const ring=closedRing(draft);addRoiFromRing(ring,'drawn','Drawn ROI',currentRoiClass());draft=[];setMode('select');if(typeof closeAllToolMenus==='function')closeAllToolMenus();draw();}\n",
    "function brushPointSpacing(radius){return Math.max(.5,Math.min(4,radius*.04));}\n",
    "function densifyBrushPoints(points,radius){const raw=points.filter(pointInsideSlide);if(raw.length<2)return raw;const spacing=brushPointSpacing(radius),out=[raw[0]];for(let i=1;i<raw.length;i++){const from=out[out.length-1],to=raw[i],dx=to.x-from.x,dy=to.y-from.y,dist=Math.hypot(dx,dy);if(!dist)continue;const steps=Math.max(1,Math.ceil(dist/spacing));for(let s=1;s<=steps;s++){out.push({x:from.x+dx*s/steps,y:from.y+dy*s/steps});}}return out;}\n",
    "function brushCircleRing(p,radius,steps=192){const ring=[];for(let i=0;i<steps;i++){const a=i/steps*Math.PI*2;ring.push({x:p.x+Math.cos(a)*radius,y:p.y+Math.sin(a)*radius});}return closedRing(ring);}\n",
    "function brushRingFromPoints(points,radius){const pts=densifyBrushPoints(points,radius);if(!pts.length)return [];if(pts.length===1)return brushCircleRing(pts[0],radius);const left=[],right=[];for(let i=0;i<pts.length;i++){const prev=pts[Math.max(0,i-1)],next=pts[Math.min(pts.length-1,i+1)],dx=next.x-prev.x,dy=next.y-prev.y,len=Math.hypot(dx,dy)||1,nx=-dy/len,ny=dx/len;left.push({x:pts[i].x+nx*radius,y:pts[i].y+ny*radius});right.push({x:pts[i].x-nx*radius,y:pts[i].y-ny*radius});}return closedRing(left.concat(right.reverse()));}\n",
    "function brushGeometryPathPoints(points,radius){const raw=(points||[]).filter(pointInsideSlide);if(raw.length<2)return raw;const spacing=Math.max(2,Math.min(32,Number(radius||0)*.25)),out=[raw[0]];for(let i=1;i<raw.length;i++){const last=out[out.length-1],p=raw[i];if(Math.hypot(p.x-last.x,p.y-last.y)>=spacing)out.push(p);}const final=raw[raw.length-1],last=out[out.length-1];if(final&&last&&Math.hypot(final.x-last.x,final.y-last.y)>1e-6)out.push(final);return out;}\n",
    "function brushCapsuleRing(a,b,radius,steps=32){const dx=b.x-a.x,dy=b.y-a.y,len=Math.hypot(dx,dy);if(!Number.isFinite(len)||len<1e-6)return brushCircleRing(a,radius);const theta=Math.atan2(dy,dx),ring=[];ring.push({x:a.x+Math.cos(theta+Math.PI/2)*radius,y:a.y+Math.sin(theta+Math.PI/2)*radius});for(let i=0;i<=steps;i++){const ang=theta+Math.PI/2-i*Math.PI/steps;ring.push({x:b.x+Math.cos(ang)*radius,y:b.y+Math.sin(ang)*radius});}ring.push({x:a.x+Math.cos(theta-Math.PI/2)*radius,y:a.y+Math.sin(theta-Math.PI/2)*radius});for(let i=0;i<=steps;i++){const ang=theta-Math.PI/2-i*Math.PI/steps;ring.push({x:a.x+Math.cos(ang)*radius,y:a.y+Math.sin(ang)*radius});}return closedRing(ring);}\n",
    "function additiveBrushRingsFromPoints(points,radius){const geom=brushMaskGeometry(points,radius,null,'new');return geom?geom.groups.map(g=>g[0]).filter(Boolean):[];}\n",
    "function brushSlideUnitScale(){try{if(typeof multiViewLayout!=='undefined'&&multiViewLayout>1&&typeof multiViewPointerPane!=='undefined'&&multiViewPointerPane&&typeof multiViewCanvasUnitScale==='function'){const mv=multiViewCanvasUnitScale(multiViewPointerPane);if(Number.isFinite(mv)&&mv>.00011)return mv;}const px=(typeof slideUnitScale==='function')?slideUnitScale():1;return Number.isFinite(px)&&px>.00011?px:1;}catch(e){return 1;}}\n",
    "function brushEffectiveRadius(screenRadius=brushScreenRadius){const base=Number(screenRadius);const size=Number.isFinite(base)?base:80;return clamp(size/brushSlideUnitScale(),1,Math.max(cfg.slide_width,cfg.slide_height));}\n",
    "function syncBrushRadiusToZoom(){brushScreenRadius=clamp(Number(brushScreenRadius)||80,8,240);brushRadius=brushEffectiveRadius(brushScreenRadius);const label=el('brushSizeValue'),hint=el('brushZoomHint');if(label)label.textContent=Math.round(brushScreenRadius)+' px';if(hint)hint.textContent='effective '+Math.round(brushRadius)+' slide px at current zoom';return brushRadius;}\n",
    "function brushRadiusValue(){return syncBrushRadiusToZoom();}\n",
    "function updateBrushControls(){const input=el('brushSize');if(input)brushScreenRadius=Number(input.value||80);syncBrushRadiusToZoom();}\n",
    "function brushSelectionIsAdditive(e={}){return !!(e&&(e.shiftKey||e.ctrlKey));}\n",
    "function clearBrushSelection(){rois.forEach(roi=>{roi.export_selected=false;});}\n",
    "function selectRoiForBrush(i){if(i<0||!rois[i])return false;rois[i].export_selected=true;if(brushTargetRoi<0||selectedRoi<0)selectedRoi=i;return true;}\n",
    "function deselectAnnotation(reason='manual'){selectedRoi=-1;activeVertex=null;draggingVertex=null;brushTargetRoi=-1;brushClass='';brushOperation='new';brushTouchedSelection=new Set();clearBrushSelection();setRoiControlsFromSelection();buildRoiList();updateButtons();updateCursorFeedback();scheduleViewerStateSync('roi_deselected',{reason:reason});draw();}\n",
    "function startNewAnnotation(preferredMode=null){if(brushing)finishBrush();draft=[];brushPoints=[];brushing=false;brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();deselectAnnotation('new_roi');const next=preferredMode||((mode==='draw'||mode==='brush')?mode:'brush');setMode(next);notify('Ready for new ROI','info');}\n",
    "function pointNearRing(p,ring,radius){return ringSegments(ring).some(seg=>pointLineDistance(p,seg[0],seg[1])<=radius);}\n",
    "function pointNearRoi(p,roi,radius){if(!p||!roi||!isDrawable(roi))return false;const b=expandedBounds(roiBounds(roi),radius);if(!boundsOverlap({xmin:p.x,ymin:p.y,xmax:p.x,ymax:p.y},b))return false;const groups=roiDrawGroups(roi);if(groups.some(g=>pointInRingsEvenOdd(p,g.rings)&&!pointInRingsEvenOdd(p,g.holes)))return true;return groups.some(g=>(g.rings||[]).concat(g.holes||[]).some(r=>pointNearRing(p,r,radius)));}\n",
    "function brushEditableTarget(p){if(selectedRoi>=0&&rois[selectedRoi]&&isDrawable(rois[selectedRoi])&&editableRoi(rois[selectedRoi]))return selectedRoi;const hit=roiAt(p);if(hit>=0&&rois[hit]&&isDrawable(rois[hit])&&editableRoi(rois[hit])){selectAnnotation(hit,false);return hit;}return -1;}\n",
    "function boundsOverlap(a,b){return !!(a&&b&&a.xmin<=b.xmax&&a.xmax>=b.xmin&&a.ymin<=b.ymax&&a.ymax>=b.ymin);}\n",
    "function boundsFromRings(rings){const pts=[];(rings||[]).forEach(r=>(r||[]).forEach(p=>pts.push(p)));return pts.length?boundsFromPoints(pts):null;}\n",
    "function orient(a,b,c){return (b.x-a.x)*(c.y-a.y)-(b.y-a.y)*(c.x-a.x);}\n",
    "function pointOnSegment(p,a,b,eps=1e-7){return Math.abs(orient(a,b,p))<=eps&&p.x>=Math.min(a.x,b.x)-eps&&p.x<=Math.max(a.x,b.x)+eps&&p.y>=Math.min(a.y,b.y)-eps&&p.y<=Math.max(a.y,b.y)+eps;}\n",
    "function segmentsIntersect(a,b,c,d){const o1=orient(a,b,c),o2=orient(a,b,d),o3=orient(c,d,a),o4=orient(c,d,b);if(pointOnSegment(c,a,b)||pointOnSegment(d,a,b)||pointOnSegment(a,c,d)||pointOnSegment(b,c,d))return true;return ((o1>0)!==(o2>0))&&((o3>0)!==(o4>0));}\n",
    "function ringSegments(ring){const segments=[];if(!ring||ring.length<2)return segments;const closed=ringClosed(ring),limit=closed?ring.length-1:ring.length;for(let i=0;i<limit;i++){const a=ring[i],b=closed?ring[i+1]:ring[(i+1)%ring.length];if(a&&b)segments.push([a,b]);}return segments;}\n",
    "function ringsHaveSegmentIntersection(a,b){const as=ringSegments(a),bs=ringSegments(b);for(const sa of as){for(const sb of bs){if(segmentsIntersect(sa[0],sa[1],sb[0],sb[1]))return true;}}return false;}\n",
    "function signedRingArea(ring){if(!ring||ring.length<3)return 0;let a=0;for(let i=0,j=ring.length-1;i<ring.length;j=i++)a+=(ring[j].x*ring[i].y-ring[i].x*ring[j].y);return a/2;}\n",
    "function unionBounds(a,b){if(!a)return b;if(!b)return a;return {xmin:Math.min(a.xmin,b.xmin),ymin:Math.min(a.ymin,b.ymin),xmax:Math.max(a.xmax,b.xmax),ymax:Math.max(a.ymax,b.ymax)};}\n",
    "function expandedBounds(b,pad){if(!b)return null;const p=Math.max(0,Number(pad)||0);return {xmin:clamp(Math.floor(b.xmin-p),0,cfg.slide_width),ymin:clamp(Math.floor(b.ymin-p),0,cfg.slide_height),xmax:clamp(Math.ceil(b.xmax+p),0,cfg.slide_width),ymax:clamp(Math.ceil(b.ymax+p),0,cfg.slide_height)};}\n",
    "function brushStrokeBounds(points,radius){const pts=(points||[]).filter(pointInsideSlide);if(!pts.length)return null;return expandedBounds(boundsFromPoints(pts),Math.max(2,Number(radius)||1)+4);}\n",
    "function brushMaskBounds(points,radius,baseRoi=null){let b=brushStrokeBounds(points,radius);if(baseRoi&&isDrawable(baseRoi))b=unionBounds(b,expandedBounds(roiBounds(baseRoi),Math.max(2,Number(radius||0)*.05+4)));return b;}\n",
    "function brushMaskPixelSize(bounds,radius){if(!bounds)return 1;const maxDim=Math.max(1,bounds.xmax-bounds.xmin,bounds.ymax-bounds.ymin);let px=Math.max(1,Math.ceil(maxDim/3600));if(maxDim<16000&&Number(radius)>0)px=Math.min(px,Math.max(1,Math.ceil(Number(radius)/24)));return Math.max(1,Math.min(128,px));}\n",
    "function brushMaskFrame(bounds,radius){let px=brushMaskPixelSize(bounds,radius),origin,xmax,ymax,w,h;for(let guard=0;guard<8;guard++){origin={x:Math.max(0,Math.floor(bounds.xmin)-px),y:Math.max(0,Math.floor(bounds.ymin)-px)};xmax=Math.min(cfg.slide_width,Math.ceil(bounds.xmax)+px);ymax=Math.min(cfg.slide_height,Math.ceil(bounds.ymax)+px);w=Math.max(2,Math.ceil((xmax-origin.x)/px)+2);h=Math.max(2,Math.ceil((ymax-origin.y)/px)+2);if(w*h<=9000000||px>=128)break;px*=2;}return {origin:origin,pixel:px,width:w,height:h};}\n",
    "function maskCanvas(frame){const c=document.createElement('canvas');c.width=frame.width;c.height=frame.height;return c;}\n",
    "function maskLocalPoint(p,frame){return {x:(p.x-frame.origin.x)/frame.pixel,y:(p.y-frame.origin.y)/frame.pixel};}\n",
    "function drawMaskPathRings(mctx,rings,frame){(rings||[]).forEach(ring=>{(ring||[]).forEach((p,j)=>{const q=maskLocalPoint(p,frame);if(j===0)mctx.moveTo(q.x,q.y);else mctx.lineTo(q.x,q.y);});mctx.closePath();});}\n",
    "function drawRoiOnMask(mctx,roi,frame,composite='source-over'){if(!roi||!isDrawable(roi))return;mctx.save();mctx.globalCompositeOperation=composite;mctx.fillStyle='#fff';roiDrawGroups(roi).forEach(group=>{mctx.beginPath();drawMaskPathRings(mctx,group.rings,frame);drawMaskPathRings(mctx,group.holes,frame);mctx.fill('evenodd');});mctx.restore();}\n",
    "function annotationCandidateRois(targetIndex=-1){return rois.filter((roi,i)=>i!==targetIndex&&isDrawable(roi)&&!(typeof roiIsCellLike==='function'&&roiIsCellLike(roi)));}\n",
    "function annotationProtectionRois(targetIndex=-1,className=null){const candidates=annotationCandidateRois(targetIndex);if(className===null||typeof className==='undefined')return candidates;const key=classPresetKey(className||'annotation');return candidates.filter(roi=>roiClassKey(roi)!==key);}\n",
    "function sameLabelAnnotationEntries(className,targetIndex=-1){const key=classPresetKey(className||'annotation');return rois.map((roi,i)=>({roi:roi,index:i})).filter(entry=>entry.index!==targetIndex&&entry.roi&&isDrawable(entry.roi)&&!(typeof roiIsCellLike==='function'&&roiIsCellLike(entry.roi))&&roiClassKey(entry.roi)===key);}\n",
    "function brushGroupsGeometry(groups){const clean=normaliseBrushGroups(groups),rings=brushGroupRings(clean);return rings.length?{groups:clean,rings:rings,ring:rings[0],bbox:boundsFromRings(rings)}:null;}\n",
    "function brushGroupsTouchRoi(groups,roi){const geom=brushGroupsGeometry(groups);if(!geom||!geom.bbox||!roi||!isDrawable(roi))return false;const rb=roiBounds(roi);if(!rb||!boundsOverlap(expandedBounds(geom.bbox,2),expandedBounds(rb,2)))return false;return roiDrawGroups(roi).some(group=>geom.rings.some(ring=>brushRingIntersectsRoiGroup(ring,group)));}\n",
    "function sameLabelTouchedAnnotationEntries(groups,className,targetIndex=-1){return sameLabelAnnotationEntries(className,targetIndex).filter(entry=>brushGroupsTouchRoi(groups,entry.roi));}\n",
    "function drawBrushGroupsOnMask(mctx,groups,frame,composite='source-over'){const clean=normaliseBrushGroups(groups);mctx.save();mctx.globalCompositeOperation=composite;mctx.fillStyle='#fff';clean.forEach(group=>{mctx.beginPath();drawMaskPathRings(mctx,group,frame);mctx.fill('evenodd');});mctx.restore();}\n",
    "function brushGroupsArea(groups){return (groups||[]).reduce((sum,group)=>sum+polygonArea(group),0);}\n",
    "function clipBrushGroupsAgainstAnnotations(groups,targetIndex=-1,className=null){const clean=normaliseBrushGroups(groups),rings=brushGroupRings(clean),protectedRois=annotationProtectionRois(targetIndex,className);if(!clean.length||!rings.length||!protectedRois.length)return {groups:clean,clipped:false,protected_count:protectedRois.length};const bounds=expandedBounds(boundsFromRings(rings),4);if(!bounds)return {groups:clean,clipped:false,protected_count:protectedRois.length};const frame=brushMaskFrame(bounds,1),canvas=maskCanvas(frame),mctx=canvas.getContext('2d',{willReadFrequently:true});drawBrushGroupsOnMask(mctx,clean,frame,'source-over');drawProtectedRoisOnMask(mctx,frame,protectedRois);const out=maskContoursFromCanvas(canvas,frame),before=brushGroupsArea(clean),after=brushGroupsArea(out),clipped=Math.abs(before-after)>Math.max(8,frame.pixel*frame.pixel*2)||out.length!==clean.length;return {groups:out,clipped:clipped,protected_count:protectedRois.length};}\n",
    "function unionBrushGroupsWithSameLabel(groups,className,targetIndex=-1){const clean=normaliseBrushGroups(groups),rings=brushGroupRings(clean),same=sameLabelTouchedAnnotationEntries(clean,className,targetIndex);if(!clean.length||!rings.length||!same.length)return {groups:clean,merged_indices:[],merged:false};let bounds=expandedBounds(boundsFromRings(rings),4);same.forEach(entry=>{bounds=unionBounds(bounds,expandedBounds(roiBounds(entry.roi),4));});if(!bounds)return {groups:clean,merged_indices:[],merged:false};const frame=brushMaskFrame(bounds,1),canvas=maskCanvas(frame),mctx=canvas.getContext('2d',{willReadFrequently:true});drawBrushGroupsOnMask(mctx,clean,frame,'source-over');same.forEach(entry=>drawRoiOnMask(mctx,entry.roi,frame,'source-over'));drawProtectedRoisOnMask(mctx,frame,annotationProtectionRois(targetIndex,className));const out=maskContoursFromCanvas(canvas,frame);return {groups:out,merged_indices:same.map(entry=>entry.index),merged:same.length>0};}\n",
    "function removeMergedSameLabelAnnotations(indices,targetRoi){(indices||[]).filter(i=>rois[i]&&rois[i]!==targetRoi).sort((a,b)=>b-a).forEach(i=>rois.splice(i,1));const idx=rois.indexOf(targetRoi);if(idx>=0)selectedRoi=idx;return idx;}\n",
    "function enforceRoiNonOverlap(index=selectedRoi){const roi=rois[index];if(!roi||!isDrawable(roi))return {clipped:false,merged:false};const cls=roiClassName(roi),clipped=clipBrushGroupsAgainstAnnotations(positiveRingGroups(roi),index,cls);if(!clipped.groups.length){notify('ROI would overlap existing annotations; edit reverted by undo if needed','warning');return {clipped:false,merged:false,empty:true};}const merged=unionBrushGroupsWithSameLabel(clipped.groups,cls,index);if(!merged.groups.length){notify('ROI would overlap existing annotations; edit reverted by undo if needed','warning');return {clipped:false,merged:false,empty:true};}setRoiPositiveGroups(roi,merged.groups);roi.subtract_rings=[];roi.non_overlapping=true;roi.same_label_merged=merged.merged;refreshRoiGeometry(roi);removeMergedSameLabelAnnotations(merged.merged_indices,roi);if(merged.merged)notify('Same-label annotations merged','success',2200);else if(clipped.clipped)notify('Overlapping annotation area clipped','info',2200);return {clipped:clipped.clipped,merged:merged.merged,merged_indices:merged.merged_indices};}\n",
    "function drawBrushStrokeOnMask(mctx,points,frame,radius,composite='source-over'){const pts=(points||[]).filter(pointInsideSlide);if(!pts.length)return;mctx.save();mctx.globalCompositeOperation=composite;mctx.fillStyle='#fff';mctx.strokeStyle='#fff';mctx.lineCap='round';mctx.lineJoin='round';mctx.lineWidth=Math.max(1,Number(radius||1)*2/frame.pixel);if(pts.length===1){const q=maskLocalPoint(pts[0],frame);mctx.beginPath();mctx.arc(q.x,q.y,Math.max(.5,Number(radius||1)/frame.pixel),0,Math.PI*2);mctx.fill();}else{mctx.beginPath();pts.forEach((p,i)=>{const q=maskLocalPoint(p,frame);if(i===0)mctx.moveTo(q.x,q.y);else mctx.lineTo(q.x,q.y);});mctx.stroke();}mctx.restore();}\n",
    "function maskFilled(data,w,h,x,y){return x>=0&&y>=0&&x<w&&y<h&&data[(y*w+x)*4+3]>0;}\n",
    "function maskEdgeKey(x,y){return x+','+y;}\n",
    "function maskEdgeAngle(e){return Math.atan2(e.y2-e.y1,e.x2-e.x1);}\n",
    "function maskTurn(prev,next){let a=maskEdgeAngle(next)-maskEdgeAngle(prev);while(a<=-Math.PI)a+=Math.PI*2;while(a>Math.PI)a-=Math.PI*2;return a;}\n",
    "function chooseMaskEdge(prev,candidates){let best=candidates[0],bestScore=Infinity;candidates.forEach(e=>{const score=Math.abs(maskTurn(prev,e));if(score<bestScore){best=e;bestScore=score;}});return best;}\n",
    "function rawMaskBoundaryRings(canvas){const w=canvas.width,h=canvas.height,data=canvas.getContext('2d',{willReadFrequently:true}).getImageData(0,0,w,h).data,edges=[];function add(x1,y1,x2,y2){edges.push({x1:x1,y1:y1,x2:x2,y2:y2,used:false});}for(let y=0;y<h;y++){for(let x=0;x<w;x++){if(!maskFilled(data,w,h,x,y))continue;if(!maskFilled(data,w,h,x,y-1))add(x,y,x+1,y);if(!maskFilled(data,w,h,x+1,y))add(x+1,y,x+1,y+1);if(!maskFilled(data,w,h,x,y+1))add(x+1,y+1,x,y+1);if(!maskFilled(data,w,h,x-1,y))add(x,y+1,x,y);}}const starts=new Map();edges.forEach(e=>{const key=maskEdgeKey(e.x1,e.y1);if(!starts.has(key))starts.set(key,[]);starts.get(key).push(e);});const rings=[];edges.forEach(first=>{if(first.used)return;let e=first,ring=[{x:e.x1,y:e.y1}],guard=0;while(e&&!e.used&&guard++<edges.length+4){e.used=true;ring.push({x:e.x2,y:e.y2});if(e.x2===first.x1&&e.y2===first.y1)break;const candidates=(starts.get(maskEdgeKey(e.x2,e.y2))||[]).filter(next=>!next.used);e=candidates.length>1?chooseMaskEdge(e,candidates):candidates[0];}const last=ring[ring.length-1];if(ring.length>=4&&last&&last.x===ring[0].x&&last.y===ring[0].y)rings.push(ring);});return rings;}\n",
    "function removeCollinearRingPoints(ring){const pts=ringOpenPoints(ring);if(pts.length<3)return pts;const out=[];for(let i=0;i<pts.length;i++){const a=pts[(i+pts.length-1)%pts.length],b=pts[i],c=pts[(i+1)%pts.length];if(Math.abs(orient(a,b,c))>1e-9)out.push(b);}return out.length>=3?out:pts;}\n",
    "function maskRingToSlide(ring,frame){const pts=ring.map(p=>({x:frame.origin.x+p.x*frame.pixel,y:frame.origin.y+p.y*frame.pixel}));let clean=removeCollinearRingPoints(closedRing(pts));const tol=Math.max(.15,Math.min(2,frame.pixel*.35));clean=simplifyClosedPoints(clean,tol);if(clean.length<1200)clean=chaikinSmoothPoints(clean,1);return closedRing(clean);}\n",
    "function maskContoursFromCanvas(canvas,frame){let rings=rawMaskBoundaryRings(canvas).map(r=>maskRingToSlide(r,frame)).filter(r=>r.length>=4&&ringArea(r)>Math.max(4,frame.pixel*frame.pixel));if(!rings.length)return [];rings.sort((a,b)=>ringArea(b)-ringArea(a));let outers=[],holes=[];rings.forEach(r=>{if(signedRingArea(r)>=0)outers.push({ring:r,holes:[]});else holes.push(r);});if(!outers.length){outers=[{ring:rings[0],holes:[]}];holes=rings.slice(1);}holes.forEach(h=>{const sample=ringOpenPoints(h)[0]||h[0];let best=-1,bestArea=Infinity;outers.forEach((o,i)=>{const a=ringArea(o.ring);if(pointInRing(sample,o.ring)&&a<bestArea){best=i;bestArea=a;}});if(best>=0)outers[best].holes.push(h);else outers.push({ring:h.slice().reverse(),holes:[]});});return outers.map(o=>[closedRing(o.ring)].concat(o.holes.map(h=>closedRing(h)))).filter(g=>g[0]&&g[0].length>=4);}\n",
    "function brushProtectionForClass(className,targetIndex=-1){return annotationProtectionRois(targetIndex,className);}\n",
    "function brushProtectionForTarget(index){return index>=0&&rois[index]?brushProtectionForClass(roiClassName(rois[index]),index):[];}\n",
    "function drawProtectedRoisOnMask(mctx,frame,protectedRois){(protectedRois||[]).forEach(roi=>drawRoiOnMask(mctx,roi,frame,'destination-out'));}\n",
    "function brushMaskGeometry(points,radius,baseRoi=null,operation='new',protectedRois=[]){const bounds=brushMaskBounds(points,radius,baseRoi);if(!bounds)return null;const frame=brushMaskFrame(bounds,radius),canvas=maskCanvas(frame),mctx=canvas.getContext('2d',{willReadFrequently:true});if(baseRoi&&isDrawable(baseRoi))drawRoiOnMask(mctx,baseRoi,frame,'source-over');drawBrushStrokeOnMask(mctx,points,frame,radius,operation==='subtract'?'destination-out':'source-over');if(operation!=='subtract')drawProtectedRoisOnMask(mctx,frame,protectedRois);const groups=maskContoursFromCanvas(canvas,frame),rings=brushGroupRings(groups);return groups.length?{type:groups.length>1?'MultiPolygon':'Polygon',groups:groups,rings:rings,ring:rings[0],bbox:boundsFromRings(rings),pixel_size:frame.pixel,mask_contour:true,protected_class_boundaries:(protectedRois||[]).length}:null;}\n",
    "function bufferedBrushGeometry(){syncBrushRadiusToZoom();const geom=brushMaskGeometry(brushPoints,brushRadius,null,'new');return geom;}\n",
    "function brushRingIntersectsRoiGroup(brushRing,group){const positive=group&&group.rings?group.rings:[],holes=group&&group.holes?group.holes:[],groupBounds=boundsFromRings(positive),brushBounds=boundsFromRing(brushRing);if(!boundsOverlap(brushBounds,groupBounds))return false;for(const ring of positive){if(ringsHaveSegmentIntersection(brushRing,ring))return true;}for(const hole of holes){if(ringsHaveSegmentIntersection(brushRing,hole))return true;}if(brushRing.some(p=>pointInRingsEvenOdd(p,positive)&&!pointInRingsEvenOdd(p,holes)))return true;for(const ring of positive){if((ring||[]).some(p=>pointInRing(p,brushRing)))return true;}return false;}\n",
    "function brushStrokeIntersectsRoi(roi,brushGeometry){if(!brushGeometry||!roi||!visibleRoi(roi)||!isDrawable(roi))return false;const rb=roiBounds(roi);if(!boundsOverlap(brushGeometry.bbox,rb))return false;const rings=brushGeometry.rings&&brushGeometry.rings.length?brushGeometry.rings:[brushGeometry.ring];return roiDrawGroups(roi).some(group=>rings.some(ring=>brushRingIntersectsRoiGroup(ring,group)));}\n",
    "function updateBrushSelectionFromStroke(e={}){if(brushSelectionIsAdditive(e))brushAdditiveSelection=true;const geometry=bufferedBrushGeometry();if(!geometry)return [];const touched=[];rois.forEach((roi,i)=>{if(brushStrokeIntersectsRoi(roi,geometry)){if(!brushTouchedSelection.has(i))touched.push(i);brushTouchedSelection.add(i);selectRoiForBrush(i);}});if(touched.length){if(brushTargetRoi>=0)selectedRoi=brushTargetRoi;updateRoiList();scheduleViewerStateSync('brush_selection_updated',{indices:Array.from(brushTouchedSelection),additive:brushAdditiveSelection});}return touched;}\n",
    "function startBrush(p,e={}){if(!pointInsideSlide(p))return;updateBrushControls();brushClass='';brushClass=currentRoiClass();brushAltDown=brushSubtractModifier(e);const blocked=brushBlockedAt(p);if(blocked>=0){brushClass='';selectedRoi=blocked;updateRoiList();updateCursorFeedback(e);notify('ROI locked','warning');draw();return;}brushAdditiveSelection=brushSelectionIsAdditive(e);if(!brushAdditiveSelection)clearBrushSelection();brushTouchedSelection=new Set();brushTargetRoi=brushAltDown?brushEditableTarget(p):-1;if(brushTargetRoi<0&&!brushAdditiveSelection)selectedRoi=-1;brushOperation=brushTargetRoi>=0&&brushAltDown?'subtract':'new';brushing=true;brushPoints=[];if(brushTargetRoi>=0)selectRoiForBrush(brushTargetRoi);updateRoiList();updateCursorFeedback(e);addBrushPoint(p,e);}\n",
    "function addBrushPoint(p,e={}){if(!brushing||!pointInsideSlide(p))return;updateCursorFeedback(e);syncBrushRadiusToZoom();const last=brushPoints[brushPoints.length-1];if(!last||Math.hypot(p.x-last.x,p.y-last.y)>=brushPointSpacing(brushRadius)){brushPoints.push({x:p.x,y:p.y});updateBrushSelectionFromStroke(e);draw();}}\n",
    "function applyBrushMaskToSelectedRoi(groups,index=selectedRoi,operation='extend'){const roi=rois[index],clean=normaliseBrushGroups(groups),all=brushGroupRings(clean);if(!roi||!isDrawable(roi)||!editableRoi(roi)||!clean.length)return false;pushAnnotationUndo(operation==='subtract'?'roi_brush_subtract':'roi_brush_extend');selectedRoi=index;roi.rings=clean[0];roi.add_groups=clean.slice(1);roi.add_rings=[];roi.subtract_rings=[];roi.brush_edited=true;roi.brush_mask_contour=true;roi.brush_ring_count=(Number(roi.brush_ring_count)||0)+all.length;refreshRoiGeometry(roi);const nonOverlap=enforceRoiNonOverlap(index),currentIndex=rois.indexOf(roi);buildRoiList();updateButtons();recordAnnotationHistory(operation==='subtract'?'roi_brush_subtract':'roi_brush_extend',{id:roi.id||null,name:roiLabelText(roi,currentIndex>=0?currentIndex:index),operation:operation,selected_indices:Array.from(brushTouchedSelection),brush_ring_count:all.length,brush_mask_contour:true,same_label_merged:!!nonOverlap.merged,overlap_clipped:!!nonOverlap.clipped,non_overlapping:true});scheduleViewerStateSync('roi_brush_edited',{id:roi.id||null,operation:operation,selected_indices:Array.from(brushTouchedSelection),brush_ring_count:all.length,same_label_merged:!!nonOverlap.merged,overlap_clipped:!!nonOverlap.clipped,non_overlapping:true});notify(nonOverlap.merged?'ROI merged with same label':(operation==='subtract'?'ROI refined':'ROI extended'),'success');return true;}\n",
    "function extendSelectedRoiWithBrush(groups,index=selectedRoi){return applyBrushMaskToSelectedRoi(groups,index,'extend');}\n",
    "function subtractSelectedRoiWithBrush(groups,index=selectedRoi){return applyBrushMaskToSelectedRoi(groups,index,'subtract');}\n",
    "function finishBrush(){if(!brushing)return;syncBrushRadiusToZoom();updateBrushSelectionFromStroke();brushing=false;const pts=brushPoints.slice(),op=brushOperation,target=brushTargetRoi,cls=brushClass||currentRoiClass();let geometry=null;if(op==='subtract'&&target>=0)geometry=brushMaskGeometry(pts,brushRadius,rois[target],'subtract',[]);else geometry=brushMaskGeometry(pts,brushRadius,null,'new',brushProtectionForClass(cls,-1));brushPoints=[];brushOperation='new';brushTargetRoi=-1;if(geometry&&geometry.groups&&geometry.groups.length){if(op==='subtract'&&target>=0&&subtractSelectedRoiWithBrush(geometry.groups,target)){brushClass='';brushTouchedSelection=new Set();brushAdditiveSelection=false;updateCursorFeedback();if(typeof closeAllToolMenus==='function')closeAllToolMenus();draw();return;}addRoiFromBrushGroups(geometry.groups,'brush','Painted ROI',cls);brushClass='';updateRoiList();updateButtons();if(typeof closeAllToolMenus==='function')closeAllToolMenus();}else if(op==='subtract'&&target>=0){brushClass='';notify('Brush removed all contour pixels; ROI left unchanged','warning');}brushClass='';brushTouchedSelection=new Set();brushAdditiveSelection=false;updateCursorFeedback();draw();}\n",
    "function slideUnitScale(){const a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:1,y:0});return Math.max(.0001,Math.hypot(b.x-a.x,b.y-a.y));}\n",
    "function drawBrushGlyph(q,r,state){ctx.save();ctx.lineWidth=2;ctx.lineCap='round';ctx.strokeStyle=state.blocked?'#ef4444':(state.subtract?'#ef4444':'#22c55e');ctx.beginPath();ctx.moveTo(q.x-r,q.y);ctx.lineTo(q.x+r,q.y);ctx.stroke();if(state.blocked){ctx.beginPath();ctx.moveTo(q.x-r*.72,q.y-r*.72);ctx.lineTo(q.x+r*.72,q.y+r*.72);ctx.moveTo(q.x+r*.72,q.y-r*.72);ctx.lineTo(q.x-r*.72,q.y+r*.72);ctx.stroke();}else if(!state.subtract){ctx.beginPath();ctx.moveTo(q.x,q.y-r);ctx.lineTo(q.x,q.y+r);ctx.stroke();}ctx.restore();}\n",
    "function finiteCanvasPoint(p){return !!(p&&Number.isFinite(Number(p.x))&&Number.isFinite(Number(p.y)));}\n",
    "function brushPreviewCanvasPoint(){if(finiteCanvasPoint(lastCanvasPointer)&&lastPointer&&pointInsideSlide(lastPointer))return lastCanvasPointer;return lastPointer&&pointInsideSlide(lastPointer)?slideToCanvas(lastPointer):null;}\n",
    "function drawBrushPreview(){if(mode!=='brush')return;syncBrushRadiusToZoom();const px=slideUnitScale(),state=brushCursorState(),remove=state.subtract,blocked=state.blocked;ctx.save();ctx.strokeStyle=blocked?'rgba(239,68,68,.95)':(remove?'rgba(248,113,113,.9)':'rgba(34,197,94,.88)');ctx.fillStyle=blocked?'rgba(239,68,68,.10)':(remove?'rgba(248,113,113,.2)':'rgba(34,197,94,.16)');ctx.lineWidth=Math.max(1,brushRadius*2*px);ctx.lineCap='round';ctx.lineJoin='round';if(brushPoints.length&&!blocked){ctx.beginPath();brushPoints.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.stroke();}const q=brushPreviewCanvasPoint();if(q){const glyph=Math.max(6,Math.min(14,brushRadius*px*.45));ctx.beginPath();ctx.arc(q.x,q.y,Math.max(2,brushRadius*px),0,Math.PI*2);ctx.fill();ctx.strokeStyle=blocked?'#ef4444':(remove?'#ef4444':'#22c55e');ctx.lineWidth=1.5;ctx.stroke();drawBrushGlyph(q,glyph,state);}ctx.restore();}\n",
    "function canvasPoint(clientX,clientY){const rect=canvas.getBoundingClientRect();return {x:clientX-rect.left,y:clientY-rect.top};}\n",
    "function ringClosed(ring){if(!ring||ring.length<2)return false;const f=ring[0],l=ring[ring.length-1];return f.x===l.x&&f.y===l.y;}\n",
    "function isTrajectoryAreaRoi(roi){return !!(roi&&(roi.trajectory_area===true||roi.trajectoryArea===true||String(roi.source||'').toLowerCase()==='trajectory'||(roi.properties&&roi.properties.wsiToolsTrajectory)));}\n",
    "function trajectoryBorderRingLimit(ring){return ringClosed(ring)?ring.length-1:ring.length;}\n",
    "function trajectoryBorderEditRadius(roi){const width=Number((roi&&roi.trajectory_width_px)||(roi&&roi.area_width_px)||(typeof trajectoryAreaWidth==='function'?trajectoryAreaWidth():512));return Math.max(48,Math.min(5000,(Number.isFinite(width)?width:512)*0.75));}\n",
    "function trajectoryBorderPerimeter(points){let total=0;for(let i=1;i<(points||[]).length;i++)total+=Math.hypot(points[i].x-points[i-1].x,points[i].y-points[i-1].y);if(points&&points.length>2)total+=Math.hypot(points[0].x-points[points.length-1].x,points[0].y-points[points.length-1].y);return total;}\n",
    "function trajectoryBorderInfluenceSteps(vertex,roi,ring){const limit=trajectoryBorderRingLimit(ring);if(limit<4)return 0;const open=(vertex&&vertex.original_ring&&vertex.original_ring.length===limit)?vertex.original_ring:ring.slice(0,limit),spacing=trajectoryBorderPerimeter(open)/Math.max(1,limit),byRadius=Math.ceil(trajectoryBorderEditRadius(roi)/Math.max(1,spacing||1)),byCount=Math.ceil(limit*.035);return Math.max(2,Math.min(Math.floor(limit/3),Math.max(byRadius,byCount)));}\n",
    "function prepareVertexDrag(vertex){if(!vertex)return null;const roi=rois[vertex.roi],ring=roi&&roi.rings?roi.rings[vertex.ring]:null;if(!ring||!editableRoi(roi))return vertex;const limit=trajectoryBorderRingLimit(ring);vertex.point=Math.max(0,Math.min(limit-1,Number(vertex.point)||0));vertex.original_ring=ring.slice(0,limit).map(p=>({x:Number(p.x),y:Number(p.y)}));vertex.original_point=vertex.original_ring[vertex.point]?{x:vertex.original_ring[vertex.point].x,y:vertex.original_ring[vertex.point].y}:null;vertex.trajectory_soft_drag=isTrajectoryAreaRoi(roi);vertex.changed=false;return vertex;}\n",
    "function softMoveTrajectoryBorderVertex(vertex,p){const roi=rois[vertex.roi],ring=roi&&roi.rings?roi.rings[vertex.ring]:null;if(!roi||!ring||!editableRoi(roi)||!vertex.original_point||!vertex.original_ring)return false;const wasClosed=ringClosed(ring),limit=trajectoryBorderRingLimit(ring);if(limit<3||vertex.original_ring.length!==limit)return false;const target={x:Math.round(clamp(p.x,0,cfg.slide_width)),y:Math.round(clamp(p.y,0,cfg.slide_height))},dx=target.x-vertex.original_point.x,dy=target.y-vertex.original_point.y,span=trajectoryBorderInfluenceSteps(vertex,roi,ring);for(let i=0;i<limit;i++){const step=Math.abs(i-vertex.point),dist=Math.min(step,limit-step);if(dist>span)continue;const weight=dist===0?1:.5*(1+Math.cos(Math.PI*dist/Math.max(1,span)));const src=vertex.original_ring[i];ring[i]={x:Math.round(clamp(src.x+dx*weight,0,cfg.slide_width)),y:Math.round(clamp(src.y+dy*weight,0,cfg.slide_height))};}if(wasClosed)ring[ring.length-1]={x:ring[0].x,y:ring[0].y};vertex.changed=vertex.changed||Math.hypot(dx,dy)>0;selectedRoi=vertex.roi;activeVertex=vertex;refreshRoiGeometry(roi);scheduleViewerStateSync('trajectory_border_vertex_moved',{id:roi.id||null,point:vertex.point,influence_points:span});draw();return true;}\n",
    "function finishActiveVertexDrag(){if(!draggingVertex)return;const vertex=draggingVertex,roi=rois[vertex.roi],event=vertex.trajectory_soft_drag?'trajectory_border_vertex_moved':'roi_vertex_moved';if(vertex.changed&&roi){recordAnnotationHistory(event,{id:roi.id||null,name:roiLabelText(roi,vertex.roi),point:vertex.point,soft_drag:!!vertex.trajectory_soft_drag},false);scheduleViewerStateSync(event,{id:roi.id||null,point:vertex.point,soft_drag:!!vertex.trajectory_soft_drag});}draggingVertex=null;buildRoiList();updateButtons();}\n",
    "function findVertexAt(clientX,clientY){const c=canvasPoint(clientX,clientY),order=selectedRoi>=0?[selectedRoi]:rois.map((_,i)=>i);for(const ri of order){const roi=rois[ri];if(!isDrawable(roi)||!editableRoi(roi))continue;for(let r=0;r<roi.rings.length;r++){const ring=roi.rings[r],limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=slideToCanvas(ring[j]);if(Math.hypot(q.x-c.x,q.y-c.y)<=9)return {roi:ri,ring:r,point:j};}}}return null;}\n",
    "let curveEditStroke=null;\n",
    "function nearestBoundarySegmentAtCanvas(c,projectPoint=slideToCanvas){if(!c)return null;const order=selectedRoi>=0?[selectedRoi]:rois.map((_,i)=>i);let best=null;for(const ri of order){const roi=rois[ri];if(!isDrawable(roi)||!editableRoi(roi))continue;for(let r=0;r<(roi.rings||[]).length;r++){const ring=roi.rings[r],limit=ringClosed(ring)?ring.length-1:ring.length;if(limit<3)continue;for(let j=0;j<limit;j++){const a=projectPoint(ring[j]),b=projectPoint(ring[(j+1)%limit]),d=segmentDistance(c,a,b);if(!best||d<best.d)best={d:d,roi:ri,ring:r,after:j};}}}return best&&best.d<=16?best:null;}\n",
    "function startCurveEditStroke(c,p,projectPoint=slideToCanvas){const seg=nearestBoundarySegmentAtCanvas(c,projectPoint);if(!seg||!pointInsideSlide(p))return false;selectAnnotation(seg.roi,false);pushAnnotationUndo('roi_curve_edited');curveEditStroke={roi:seg.roi,ring:seg.ring,startAfter:seg.after,points:[{x:p.x,y:p.y}],changed:false};activeVertex=null;draggingVertex=null;updateRoiList();draw();return true;}\n",
    "function curveEditPointSpacing(){const px=(typeof slideUnitScale==='function')?slideUnitScale():1;return Math.max(1,Math.min(80,5/Math.max(px,.0001)));}\n",
    "function addCurveEditPoint(p){if(!curveEditStroke||!pointInsideSlide(p))return false;const last=curveEditStroke.points[curveEditStroke.points.length-1],spacing=curveEditPointSpacing();if(!last||Math.hypot(p.x-last.x,p.y-last.y)>=spacing){curveEditStroke.points.push({x:p.x,y:p.y});curveEditStroke.changed=true;draw();return true;}return false;}\n",
    "function chaikinSmoothOpenPoints(points,iterations=2){let pts=(points||[]).map(clonePoint);if(pts.length<3)return pts;for(let it=0;it<iterations;it++){const out=[clonePoint(pts[0])];for(let i=0;i<pts.length-1;i++){const p=pts[i],q=pts[i+1];out.push({x:p.x*.75+q.x*.25,y:p.y*.75+q.y*.25});out.push({x:p.x*.25+q.x*.75,y:p.y*.25+q.y*.75});}out.push(clonePoint(pts[pts.length-1]));pts=out;}return pts;}\n",
    "function dedupeOpenCurvePoints(points,minDistance=1){const out=[];(points||[]).forEach(p=>{if(!p||!Number.isFinite(Number(p.x))||!Number.isFinite(Number(p.y)))return;const q={x:Number(p.x),y:Number(p.y)},last=out[out.length-1];if(!last||Math.hypot(q.x-last.x,q.y-last.y)>=minDistance)out.push(q);});return out;}\n",
    "function catmullRomPoint(p0,p1,p2,p3,t){const t2=t*t,t3=t2*t;return {x:.5*((2*p1.x)+(-p0.x+p2.x)*t+(2*p0.x-5*p1.x+4*p2.x-p3.x)*t2+(-p0.x+3*p1.x-3*p2.x+p3.x)*t3),y:.5*((2*p1.y)+(-p0.y+p2.y)*t+(2*p0.y-5*p1.y+4*p2.y-p3.y)*t2+(-p0.y+3*p1.y-3*p2.y+p3.y)*t3)};}\n",
    "function catmullRomSmoothOpenPoints(points,samplesPerSegment=8){const pts=dedupeOpenCurvePoints(points,.5);if(pts.length<3)return pts.map(clonePoint);const samples=Math.max(3,Math.min(18,Math.round(samplesPerSegment)||8)),out=[clonePoint(pts[0])];for(let i=0;i<pts.length-1;i++){const p0=pts[Math.max(0,i-1)],p1=pts[i],p2=pts[i+1],p3=pts[Math.min(pts.length-1,i+2)];for(let s=1;s<=samples;s++){const q=catmullRomPoint(p0,p1,p2,p3,s/samples);out.push({x:clamp(q.x,0,cfg.slide_width),y:clamp(q.y,0,cfg.slide_height)});}}return dedupeOpenCurvePoints(out,.75);}\n",
    "function smoothCurveEditPoints(points){const pts=dedupeOpenCurvePoints(points,.75);if(pts.length<3)return pts.map(clonePoint);const scale=(typeof slideUnitScale==='function')?slideUnitScale():1,samples=Math.max(5,Math.min(14,Math.round(12/Math.max(scale,.25))));return catmullRomSmoothOpenPoints(pts,samples);}\n",
    "function nearestSegmentInRingBySlidePoint(p,ring){let best=null,limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const d=pointLineDistance(p,ring[j],ring[(j+1)%limit]);if(!best||d<best.d)best={d:d,after:j};}return best;}\n",
    "function replaceRingForwardArc(open,startAfter,endAfter,curve){const limit=open.length;if(limit<3)return null;const endOffset=(endAfter+1-startAfter+limit)%limit,linear=[];for(let i=0;i<limit;i++)linear.push(clonePoint(open[(startAfter+i)%limit]));if(endOffset<=0||endOffset>=limit)return null;const interior=curve.slice(1,-1).map(p=>({x:Math.round(clamp(p.x,0,cfg.slide_width)),y:Math.round(clamp(p.y,0,cfg.slide_height))}));const out=[linear[0]].concat(interior,linear.slice(endOffset));return out.length>=3?closedRing(out):null;}\n",
    "function finishCurveEditStroke(){const stroke=curveEditStroke;if(!stroke)return false;curveEditStroke=null;const roi=rois[stroke.roi],ring=roi&&roi.rings?roi.rings[stroke.ring]:null;if(!roi||!ring||!editableRoi(roi)||stroke.points.length<2){draw();return false;}const open=ringOpenPoints(ring),limit=open.length,end=nearestSegmentInRingBySlidePoint(stroke.points[stroke.points.length-1],ring);if(!end||limit<3){draw();return false;}let forward=(end.after-stroke.startAfter+limit)%limit,backward=(stroke.startAfter-end.after+limit)%limit,curve=smoothCurveEditPoints([open[stroke.startAfter]].concat(stroke.points,open[(end.after+1)%limit])),newRing=null;if(forward<=backward){newRing=replaceRingForwardArc(open,stroke.startAfter,end.after,curve);}else{const reversedCurve=smoothCurveEditPoints([open[end.after]].concat(stroke.points.slice().reverse(),open[(stroke.startAfter+1)%limit]));newRing=replaceRingForwardArc(open,end.after,stroke.startAfter,reversedCurve);}if(!newRing||newRing.length<4){draw();return false;}roi.rings[stroke.ring]=newRing;roi.curve_edited=true;roi.smooth_curve_boundary=true;refreshRoiGeometry(roi);enforceRoiNonOverlap(stroke.roi);buildRoiList();updateButtons();recordAnnotationHistory('roi_curve_edited',{id:roi.id||null,name:roiLabelText(roi,stroke.roi),points:stroke.points.length,spline_points:newRing.length,smooth_curve:true,smooth_curve_boundary:true},false);scheduleViewerStateSync('roi_curve_edited',{id:roi.id||null,points:stroke.points.length,spline_points:newRing.length,smooth_curve:true,smooth_curve_boundary:true});notify('ROI boundary curve updated','success');draw();return true;}\n",
    "function cancelCurveEditStroke(){curveEditStroke=null;draw();}\n",
    "function drawSmoothOpenCurvePath(targetCtx,points,projectPoint=slideToCanvas){const pts=(points||[]).filter(Boolean);if(!pts.length)return;if(pts.length<3){targetCtx.beginPath();pts.forEach((p,i)=>{const q=projectPoint(p);if(i===0)targetCtx.moveTo(q.x,q.y);else targetCtx.lineTo(q.x,q.y);});targetCtx.stroke();return;}const projected=pts.map(projectPoint);targetCtx.beginPath();targetCtx.moveTo(projected[0].x,projected[0].y);for(let i=1;i<projected.length-1;i++){const p=projected[i],next=projected[i+1],mid={x:(p.x+next.x)/2,y:(p.y+next.y)/2};targetCtx.quadraticCurveTo(p.x,p.y,mid.x,mid.y);}const last=projected[projected.length-1];targetCtx.lineTo(last.x,last.y);targetCtx.stroke();}\n",
    "function drawCurveEditPreview(targetCtx=ctx,projectPoint=slideToCanvas){if(!curveEditStroke||!curveEditStroke.points.length)return;targetCtx.save();targetCtx.strokeStyle='#facc15';targetCtx.lineWidth=3;targetCtx.lineCap='round';targetCtx.lineJoin='round';targetCtx.setLineDash([8,5]);drawSmoothOpenCurvePath(targetCtx,catmullRomSmoothOpenPoints(curveEditStroke.points,5),projectPoint);targetCtx.setLineDash([]);targetCtx.fillStyle='rgba(250,204,21,.9)';[curveEditStroke.points[0],curveEditStroke.points[curveEditStroke.points.length-1]].filter(Boolean).forEach(p=>{const q=projectPoint(p);targetCtx.beginPath();targetCtx.arc(q.x,q.y,3.5,0,Math.PI*2);targetCtx.fill();});targetCtx.restore();}\n",
    "function moveActiveVertex(p){if(!activeVertex||!pointInsideSlide(p))return;const roi=rois[activeVertex.roi],ring=roi&&roi.rings?roi.rings[activeVertex.ring]:null;if(!ring||!editableRoi(roi))return;if(activeVertex.trajectory_soft_drag&&softMoveTrajectoryBorderVertex(activeVertex,p))return;const closed=ringClosed(ring),pt={x:Math.round(p.x),y:Math.round(p.y)};ring[activeVertex.point]=pt;if(activeVertex.point===0&&closed)ring[ring.length-1]={x:pt.x,y:pt.y};activeVertex.changed=true;selectedRoi=activeVertex.roi;refreshRoiGeometry(roi);scheduleViewerStateSync('roi_edited',{id:roi.id||null});draw();}\n",
    "function drawEditHandles(){if(mode!=='edit'||selectedRoi<0||!isDrawable(rois[selectedRoi])||!editableRoi(rois[selectedRoi]))return;const roi=rois[selectedRoi],trajectoryBorder=isTrajectoryAreaRoi(roi);ctx.save();roi.rings.forEach((ring,r)=>{const limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=slideToCanvas(ring[j]),active=activeVertex&&activeVertex.roi===selectedRoi&&activeVertex.ring===r&&activeVertex.point===j;ctx.beginPath();ctx.arc(q.x,q.y,active?7:(trajectoryBorder?4.8:4),0,Math.PI*2);ctx.fillStyle=active?'#facc15':(trajectoryBorder?'#dbeafe':'#ffffff');ctx.strokeStyle=trajectoryBorder?'#2563eb':'#111';ctx.lineWidth=active?2.4:2;ctx.fill();ctx.stroke();}});ctx.restore();drawCurveEditPreview(ctx,slideToCanvas);}\n",
    "function segmentDistance(c,a,b){const dx=b.x-a.x,dy=b.y-a.y,len2=dx*dx+dy*dy;if(!len2)return Math.hypot(c.x-a.x,c.y-a.y);let t=((c.x-a.x)*dx+(c.y-a.y)*dy)/len2;t=clamp(t,0,1);return Math.hypot(c.x-(a.x+t*dx),c.y-(a.y+t*dy));}\n",
    "function insertVertexAt(p,clientX,clientY){if(selectedRoi<0||!isDrawable(rois[selectedRoi])||!editableRoi(rois[selectedRoi])||!pointInsideSlide(p))return false;const c=canvasPoint(clientX,clientY),roi=rois[selectedRoi];let best=null;roi.rings.forEach((ring,r)=>{const limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const a=slideToCanvas(ring[j]),b=slideToCanvas(ring[(j+1)%ring.length]),d=segmentDistance(c,a,b);if(!best||d<best.d)best={d:d,ring:r,after:j};}});if(!best||best.d>14)return false;pushAnnotationUndo('roi_vertex_inserted');const ring=roi.rings[best.ring];ring.splice(best.after+1,0,{x:Math.round(p.x),y:Math.round(p.y)});activeVertex={roi:selectedRoi,ring:best.ring,point:best.after+1};refreshRoiGeometry(roi);buildRoiList();draw();return true;}\n",
    "function deleteSelectedVertex(){if(!activeVertex)return false;const roi=rois[activeVertex.roi],ring=roi&&roi.rings?roi.rings[activeVertex.ring]:null;if(!ring||!editableRoi(roi)){notify('ROI locked','warning');return false;}const closed=ringClosed(ring),limit=closed?ring.length-1:ring.length;if(limit<=3){notify('Keep at least 3 vertices','warning');return false;}pushAnnotationUndo('roi_vertex_deleted');ring.splice(activeVertex.point,1);if(activeVertex.point===0&&closed)ring[ring.length-1]={x:ring[0].x,y:ring[0].y};activeVertex=null;refreshRoiGeometry(roi);buildRoiList();scheduleViewerStateSync('roi_edited',{id:roi.id||null});draw();return true;}\n",
    "function deleteRoi(index=selectedRoi){const i=Number(index);if(i<0||!rois[i]){notify('Select an ROI','warning');return;}if(lockedRoi(rois[i])){notify('ROI locked','warning');return;}const deleteLabel=roiLabelText(rois[i],i);pushAnnotationUndo('roi_deleted');const removed=rois.splice(i,1)[0];selectedRoi=Math.min(i,rois.length-1);activeVertex=null;buildRoiList();updateButtons();draw();recordAnnotationHistory('roi_deleted',{id:removed.id||null,name:deleteLabel});scheduleViewerStateSync('roi_deleted',{id:removed.id||null});notifyAction('Deleted '+deleteLabel+'.','Undo',()=>restoreAnnotationUndo(),'success',7000);}\n",
    "function deleteSelectedRoi(){deleteRoi(selectedRoi);}\n",
    "function deleteSelectedLayerObject(){const rec=selectedLayerObject();if(!rec){notify('Select a marker or layer object','warning');return false;}const itemId=String(rec.item.id||rec.item.name||rec.item.label||('item_'+(Number(rec.itemIndex)+1))),label=layerObjectLabel(rec),items=Array.isArray(rec.layer.items)?rec.layer.items:[];items.splice(rec.itemIndex,1);rec.layer.count=layerCount(rec.layer);clearSelectedLayerObject(false);buildLayerList();updateButtons();draw();recordAnnotationHistory('layer_object_deleted',{id:itemId,name:label,layer:rec.layer.id||rec.layer.name||null});scheduleViewerStateSync('layer_object_deleted',{layer_id:rec.layer.id||null,layer_name:rec.layer.name||null,item_id:itemId,item_name:label,count:layerCount(rec.layer)});notify('Deleted '+label,'success');return true;}\n",
    "function deleteSelectedMeasure(){const i=Number(selectedMeasure);if(i<0||!measures[i]){notify('Select a distance measurement','warning');return false;}const removed=measures.splice(i,1)[0];selectedMeasure=-1;if(typeof updateMeasureList==='function')updateMeasureList();updateButtons();draw();recordAnnotationHistory('measurement_deleted',{id:removed.id||('measure_'+(i+1)),distance_px:Number.isFinite(Number(removed.distance_px))?Number(removed.distance_px).toFixed(1):null});scheduleViewerStateSync('measurement_deleted',{id:removed.id||null,index:i+1});notify('Deleted distance measurement '+(i+1),'success');return true;}\n",
    "function deleteSelectedObject(){if(selectedRoi>=0&&rois[selectedRoi]){deleteSelectedRoi();return true;}if(typeof deleteSelectedTrajectory==='function'&&typeof selectedTrajectory!=='undefined'&&selectedTrajectory>=0&&typeof trajectories!=='undefined'&&trajectories[selectedTrajectory]){deleteSelectedTrajectory();return true;}if(selectedLayerObject())return deleteSelectedLayerObject();if(Number.isFinite(Number(selectedMeasure))&&selectedMeasure>=0&&measures[selectedMeasure])return deleteSelectedMeasure();notify('Select an annotation, trajectory, marker, layer object, or measurement','warning');return false;}\n",
    "function updateRoiColor(index,colour){const i=Number(index),roi=rois[i];if(!roi)return;if(lockedRoi(roi)){notify('ROI locked','warning');buildRoiList();return;}pushAnnotationUndo('roi_color_updated');const cls=roi.class||'annotation',c=setClassColour(cls,colour,true);selectedRoi=i;buildRoiList();draw();recordAnnotationHistory('roi_color_updated',{id:roi.id||null,name:roiLabelText(roi,i),class:cls,color:c});scheduleViewerStateSync('roi_color_updated',{id:roi.id||null,class:cls,color:c,class_presets:roiClassPresets});notify('Class color updated','success');}\n",
    "function toggleRoiVisibility(index){const i=Number(index),roi=rois[i];if(!roi)return;pushAnnotationUndo('roi_visibility_updated');roi.visible=!visibleRoi(roi);selectedRoi=i;buildRoiList();draw();recordAnnotationHistory('roi_visibility_updated',{id:roi.id||null,name:roiLabelText(roi,i),visible:visibleRoi(roi)});scheduleViewerStateSync('roi_visibility_updated',{id:roi.id||null,visible:visibleRoi(roi)});notify(visibleRoi(roi)?'ROI shown':'ROI hidden','success');}\n",
    "function toggleRoiLock(index){const i=Number(index),roi=rois[i];if(!roi)return;pushAnnotationUndo('roi_lock_updated');roi.locked=!lockedRoi(roi);roi.isLocked=roi.locked;selectedRoi=i;buildRoiList();draw();recordAnnotationHistory('roi_lock_updated',{id:roi.id||null,name:roiLabelText(roi,i),locked:lockedRoi(roi)});scheduleViewerStateSync('roi_lock_updated',{id:roi.id||null,locked:lockedRoi(roi)});notify(lockedRoi(roi)?'ROI locked':'ROI unlocked','success');}\n",
    "function duplicateRoi(index=selectedRoi){const i=Number(index),roi=rois[i];if(!roi){notify('Select an ROI','warning');return;}pushAnnotationUndo('roi_duplicated');newRoiCount++;const clone=JSON.parse(JSON.stringify(roi));clone.id=String(roi.id||('roi_'+(i+1)))+'_copy_'+newRoiCount;clone.name=(roi.name||roi.label||clone.id)+' copy';clone.label=clone.name;clone.locked=false;clone.isLocked=false;clone.visible=visibleRoi(roi);clone.export_selected=false;clone.edited=true;rois.splice(i+1,0,clone);selectedRoi=i+1;buildRoiList();draw();recordAnnotationHistory('roi_duplicated',{source_id:roi.id||null,id:clone.id,name:clone.name});scheduleViewerStateSync('roi_duplicated',{source_id:roi.id||null,id:clone.id});notify('ROI duplicated','success');}\n",
    "function exportSelectedAnnotations(){const indices=roiExportIndices();if(!indices.length){notify(respectClassExportRules()?'No selected ROIs pass class export rules':'Select ROIs to export','warning');return;}if(typeof openAnnotationExportDialog==='function'){openAnnotationExportDialog('selected');return;}const features=indices.map(i=>roiFeature(rois[i],i)).filter(Boolean);if(!features.length){notify('No exportable ROI geometry','warning');return;}const allFeatures=exportableRoiFeatures();const name=(cfg.annotation_filename||'wsiTools_annotations.geojson').replace(/\\.geojson$/i,'')+'_selected.geojson';downloadText(JSON.stringify({type:'FeatureCollection',features:features},null,2),name);if(features.length>=allFeatures.length)markAnnotationsSaved('geojson_exported');scheduleViewerStateSync('roi_exported',{count:features.length,dirty:annotationsDirty,respect_export_rules:respectClassExportRules()});notify('GeoJSON exported','success');}\n",
    "let annotationSpotAssociationCacheKey='',annotationSpotAssociationCache=[];\n",
    "function invalidateAnnotationSpotAssociations(){annotationSpotAssociationCacheKey='';annotationSpotAssociationCache=[];}\n",
    "function annotationSpotLayer(layer){if(!layer||!Array.isArray(layer.items))return false;const type=String(layer.source_type||layer.type||'').toLowerCase(),id=String(layer.id||'').toLowerCase(),name=String(layer.name||'').toLowerCase();return type==='seurat_spots'||type==='spatial_spots'||type==='spots'||id.includes('spot')||name.includes('spot');}\n",
    "function annotationSpotLayers(){return (layers||[]).filter(annotationSpotLayer);}\n",
    "function spotItemId(item,index){return String(item.id||item.label||item.barcode||item.name||item.cell_id||('spot_'+(index+1)));}\n",
    "function annotationSpotItems(){const out=[];annotationSpotLayers().forEach(layer=>{if(layer.visible===false)return;(layer.items||[]).forEach((item,i)=>{const x=Number(item.x),y=Number(item.y);if(!Number.isFinite(x)||!Number.isFinite(y))return;const id=spotItemId(item,i);out.push({x:x,y:y,id:id,label:String(item.label||item.barcode||item.name||item.id||id),layer_id:String(layer.id||''),layer_name:String(layer.name||layer.id||'spots')});});});return out;}\n",
    "function annotationSpotProjectLabel(){const project=(typeof projectStatePayload==='function'?projectStatePayload():null);if(!project)return {image:'',section:''};const item=project.active_item||project.active||project.item||{},section=project.active_section||project.section||{};return {image:String(item.label||item.name||item.path||project.label||''),section:String(section.label||section.name||section.id||project.section_label||'')};}\n",
    "function annotationSpotAssociationKey(){const layerKey=annotationSpotLayers().map(l=>[l.id||'',l.name||'',l.visible!==false,(l.items||[]).length].join(':')).join('|'),roiKey=rois.map((r,i)=>[r.id||'',r.name||r.label||'',r.class||'',visibleRoi(r),pointCount(r),Number(r.area)||''].join(':')).join('|'),project=(typeof projectStatePayload==='function'?JSON.stringify(projectStatePayload()||{}):'');return layerKey+'#'+roiKey+'#'+project;}\n",
    "function annotationSpotAssociations(force=false){const key=annotationSpotAssociationKey();if(!force&&key===annotationSpotAssociationCacheKey)return annotationSpotAssociationCache.slice();const spots=annotationSpotItems(),project=annotationSpotProjectLabel(),rows=[];rois.forEach((roi,i)=>{if(!visibleRoi(roi)||!isDrawable(roi)||(typeof roiIsCellLike==='function'&&roiIsCellLike(roi)))return;spots.forEach(spot=>{if(roiContainsPoint(roi,spot)){rows.push({annotation_index:i+1,annotation_id:String(roi.id||('roi_'+(i+1))),annotation_name:String(roiLabelText(roi,i)||''),annotation_class:String(roi.class||'annotation'),spot_id:spot.id,spot_label:spot.label,spot_x:spot.x,spot_y:spot.y,spot_layer_id:spot.layer_id,spot_layer_name:spot.layer_name,project_image:project.image,project_section:project.section});}});});annotationSpotAssociationCacheKey=key;annotationSpotAssociationCache=rows;return rows.slice();}\n",
    "function annotationSpotAssociationPayload(){return annotationSpotAssociations(false);}\n",
    "function csvValue(value){if(value===null||typeof value==='undefined')return '';const s=String(value),q=String.fromCharCode(34),needs=s.includes(',')||s.includes(String.fromCharCode(10))||s.includes(String.fromCharCode(13))||s.includes(q);return needs?q+s.split(q).join(q+q)+q:s;}\n",
    "function annotationSpotRowsToCsv(rows){const headers=['annotation_index','annotation_id','annotation_name','annotation_class','spot_id','spot_label','spot_x','spot_y','spot_layer_id','spot_layer_name','project_image','project_section'];return [headers.join(',')].concat((rows||[]).map(row=>headers.map(h=>csvValue(row[h])).join(','))).join('\\n')+'\\n';}\n",
    "function downloadCsvText(text,name){const blob=new Blob([text],{type:'text/csv'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name||'annotation_spots.csv';document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "function annotationSpotCsvName(){const base=(typeof projectAnnotationFilename==='function'?projectAnnotationFilename():null)||cfg.annotation_filename||'wsiTools_annotations.geojson',stem=String(base);return stem.toLowerCase().endsWith('.geojson')?stem.slice(0,-8)+'_spots.csv':stem+'_spots.csv';}\n",
    "async function saveAnnotationSpotsCsv(){if(brushing)finishBrush();if(draft.length>=3)finishDraft();const rows=annotationSpotAssociations(true);scheduleViewerStateSync('annotation_spots_exported',{count:rows.length,annotation_spots:rows});if(!rows.length){notify(annotationSpotItems().length?'No spots fall inside visible annotations':'No spatial spots available in the viewer','warning',2600);return;}const csv=annotationSpotRowsToCsv(rows),name=annotationSpotCsvName(),blob=new Blob([csv],{type:'text/csv'});try{const mode=typeof saveBlobWithLocation==='function'?await saveBlobWithLocation(blob,name,{description:'CSV',accept:{'text/csv':['.csv']}}):'unsupported';if(mode==='cancelled'||mode==='unsupported')return;recordAnnotationHistory('annotation_spots_exported',{count:rows.length});notify('Annotation-spot CSV exported','success');}catch(e){notify('Annotation-spot CSV export failed: '+e.message,'error',5200);}}\n",
    "function selectAllAnnotations(){const entries=(typeof currentRoiListEntries==='function')?currentRoiListEntries():rois.map((roi,i)=>({roi:roi,index:i}));entries.forEach(entry=>{if(rois[entry.index])rois[entry.index].export_selected=true;});buildRoiList();updateButtons();}\n",
    "function selectNoAnnotations(){const entries=(typeof currentRoiListEntries==='function')?currentRoiListEntries():rois.map((roi,i)=>({roi:roi,index:i}));entries.forEach(entry=>{if(rois[entry.index])rois[entry.index].export_selected=false;});selectedRoi=-1;activeVertex=null;draggingVertex=null;brushTargetRoi=-1;brushClass='';brushOperation='new';brushTouchedSelection=new Set();buildRoiList();updateButtons();scheduleViewerStateSync('roi_deselected',{reason:'annotation_panel'});notify('Annotations deselected','info');draw();}\n",
    "function slideCoordinateSize(size=null){const w=Number(size&&size.width||cfg.slide_width||0),h=Number(size&&size.height||cfg.slide_height||0);return {width:Number.isFinite(w)&&w>0?w:null,height:Number.isFinite(h)&&h>0?h:null};}\n",
    "function normaliseSlidePoint(p,size=null){const dim=slideCoordinateSize(size),x=Number(p&&p.x),y=Number(p&&p.y);return {x:dim.width?clamp(Number.isFinite(x)?x:0,0,dim.width):x,y:dim.height?clamp(Number.isFinite(y)?y:0,0,dim.height):y};}\n",
    "function normaliseSlideCoordinatePair(pair,size=null){const p=Array.isArray(pair)?{x:pair[0],y:pair[1]}:pair,q=normaliseSlidePoint(p,size);return [Math.round(q.x),Math.round(q.y)];}\n",
    "function normaliseSlideRingCoordinates(r,size=null){const ring=(r||[]).map(p=>normaliseSlideCoordinatePair(p,size)).filter(xy=>Number.isFinite(xy[0])&&Number.isFinite(xy[1]));if(ring.length){const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);}return ring;}\n",
    "function normaliseGeojsonCoordinates(coords,type,size=null){if(!Array.isArray(coords))return coords;if(type==='Point')return normaliseSlideCoordinatePair(coords,size);if(type==='MultiPoint'||type==='LineString')return coords.map(xy=>normaliseSlideCoordinatePair(xy,size));if(type==='MultiLineString'||type==='Polygon')return coords.map(r=>normaliseSlideRingCoordinates(r,size));if(type==='MultiPolygon')return coords.map(poly=>(poly||[]).map(r=>normaliseSlideRingCoordinates(r,size)));return coords;}\n",
    "function normaliseGeojsonGeometry(geometry,size=null){if(!geometry||!geometry.type)return geometry;return {type:geometry.type,coordinates:normaliseGeojsonCoordinates(geometry.coordinates,geometry.type,size)};}\n",
    "function ringCoordinates(r){return normaliseSlideRingCoordinates(r);}\n",
    "function roiCompositeGeometry(roi){const groups=roiDrawGroups(roi).map(g=>g.rings.concat(g.holes).map(ringCoordinates)).filter(g=>g.length&&g[0].length>=4);if(!groups.length)return null;if(groups.length===1)return {type:'Polygon',coordinates:groups[0]};return {type:'MultiPolygon',coordinates:groups};}\n",
    "function roiFeature(roi,i){let geometry=null;if(isDrawable(roi)){geometry=roiCompositeGeometry(roi);}else if(roi.coordinates){geometry=normaliseGeojsonGeometry({type:geometryType(roi),coordinates:roi.coordinates});}if(!geometry)return null;const name=roiLabelText(roi,i),cls=roi.class||'annotation',preset=ensureClassPreset(cls,roi.colour||''),colour=classColour(cls,roi.colour||'#00BFC4'),originalColour=normaliseHexColour(roi.original_colour||'',''),props=clonePlain(roi.properties||{});let classification=(props.classification&&typeof props.classification==='object'&&!Array.isArray(props.classification))?clonePlain(props.classification):{};classification.name=cls;const colourChanged=!!(originalColour&&colour&&colour!==originalColour);if(colourChanged){classification.color=colour;delete classification.colorRGB;delete classification.color_rgb;delete classification.colour;}else if(colour&&!classification.color&&!classification.colorRGB)classification.color=colour;else if(colour)classification.color=colour;props.objectType=props.objectType||(roi.source==='stardist'?'detection':'annotation');props.name=name;props.label=name;props.classification=classification;props.class=cls;props.isLocked=lockedRoi(roi);props.visible=visibleRoi(roi);props.wsiTools=Object.assign({},clonePlain(props.wsiTools||{}),{classPreset:preset?preset.class:cls,export:classPresetExportable(cls),exportRule:classPresetExportRule(cls),coordinate_space:'level0_slide_pixels',coordinateSpace:'level0_slide_pixels',slide_width:Number(cfg.slide_width||0),slide_height:Number(cfg.slide_height||0),display_transform_applied:false});if(roi.measurements)props.measurements=roi.measurements;if(roi.centroid)props.centroid=normaliseSlidePoint(roi.centroid);if(roi.source_file)props.source_file=roi.source_file;delete props.export_selected;const feature=clonePlain(roi.feature||{});feature.type='Feature';feature.id=roi.id||('roi_'+(i+1));feature.properties=props;feature.geometry=geometry;const b=roiBounds(roi);if(b&&Number.isFinite(Number(b.xmin)))feature.bbox=[b.xmin,b.ymin,b.xmax,b.ymax].map(v=>Math.round(Number(v)));return feature;}\n",
    "function exportableRoiFeatures(){return rois.map((roi,i)=>roiAllowedByExportRules(roi)?roiFeature(roi,i):null).filter(Boolean);}\n",
    "function geojsonText(){if(brushing)finishBrush();if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:exportableRoiFeatures()},null,2);}\n",
    "function annotationPanelNameValue(){return '';}\n",
    "function annotationPanelClassValue(){const custom=el('annotationClassCustom');if(custom&&custom.value.trim())return custom.value.trim();const select=el('annotationClassSelect');return select&&select.value?select.value:(activeRoiClass||'annotation');}\n",
    "function applySelectedRoiMetadata(name,cls,colour){if(selectedRoi<0||!rois[selectedRoi]){notify('Select an ROI','warning');return false;}const roi=rois[selectedRoi];const oldName=roi.name||roi.label||roi.id||'ROI',oldClass=roi.class||'annotation',selectedClass=String(cls||'annotation').trim()||'annotation',classChanged=classPresetKey(oldClass)!==classPresetKey(selectedClass);pushAnnotationUndo(classChanged?'roi_label_updated':'roi_metadata_updated');ensureRoiClassOption(selectedClass);setSelectValue(el('annotationClassSelect'),selectedClass);roi.class=selectedClass;const newName=automaticAnnotationName(selectedClass,'ROI',selectedRoi);roi.name=newName;roi.label=newName;roi.automatic_name=true;roi.category_label=newName;const explicitColour=normaliseHexColour(colour||'',''),classColor=explicitColour?setClassColour(selectedClass,explicitColour,true):classColour(selectedClass);setRoiColour(roi,classColor,true);roi.edited=true;buildRoiList();recordAnnotationHistory(classChanged?'roi_label_updated':'roi_metadata_updated',{id:roi.id||null,name:roi.name||null,old_name:oldName,class:roi.class||null,old_class:oldClass,color:roi.colour||null,automatic_name:!!roi.automatic_name,locked:lockedRoi(roi),name_updated:oldName!==newName,consistent_category_label:true});scheduleViewerStateSync(classChanged?'roi_label_updated':'roi_updated',{id:roi.id||null,class:roi.class||null,old_class:oldClass,name:roi.name||null,old_name:oldName,color:roi.colour||null,automatic_name:!!roi.automatic_name,locked:lockedRoi(roi),class_presets:roiClassPresets,name_updated:oldName!==newName,consistent_category_label:true});notify(classChanged?('Annotation category changed: '+oldClass+' -> '+selectedClass+'; label '+newName):(lockedRoi(roi)?'ROI metadata updated; shape remains locked':'ROI updated: '+newName),'success');draw();return true;}\n",
    "function applySelectedRoiClass(){const cls=setNextRoiClass(currentRoiClass());activeRoiName='';saveRoiClassPreference();notify('Next annotation class: '+cls,'success',1800);}\n",
    "function applyAnnotationPanelMetadata(){applySelectedRoiMetadata('',annotationPanelClassValue(),el('annotationColorInput')?el('annotationColorInput').value:null);}\n",
    "function bindRoiClassControls(){applyClassPresetColoursToRois(false);populateRoiClassSelects();const panelNextSelect=el('panelRoiClassSelect'),panelNextCustom=el('panelRoiClassCustom'),brush=el('brushSize'),panelSelect=el('annotationClassSelect'),panelCustom=el('annotationClassCustom'),panelColor=el('annotationColorInput');const syncPanelColour=cls=>{const c=classColour(cls);if(c&&panelColor)panelColor.value=c;};const clearNextCustom=()=>syncNextRoiCustomInputs('',null);const commitNextClass=cls=>{const value=setNextRoiClass(cls);clearNextAnnotationName();saveRoiClassPreference();notify('Next annotation class: '+value,'info',1600);return value;};if(panelNextSelect){panelNextSelect.onchange=e=>{clearNextCustom();commitNextClass(e.target.value);};}if(panelNextCustom){panelNextCustom.oninput=e=>{const cls=e.target.value.trim();syncNextRoiCustomInputs(cls,panelNextCustom);if(cls){nextRoiClass=cls;activeRoiClass=cls;clearNextAnnotationName();}saveRoiClassPreference();};panelNextCustom.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();const cls=setNextRoiClass(currentRoiClass());notify('Next annotation class: '+cls,'info',1600);}};}if(panelSelect){panelSelect.onchange=e=>{if(panelCustom)panelCustom.value='';const cls=annotationPanelClassValue();ensureRoiClassOption(cls);syncPanelColour(cls);notify('Click Apply to update the selected annotation category to '+cls,'info',2200);};}if(panelCustom){panelCustom.oninput=e=>{const cls=annotationPanelClassValue();if(cls){ensureRoiClassOption(cls);syncPanelColour(cls);}};panelCustom.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();applyAnnotationPanelMetadata();}};}if(panelColor){panelColor.onchange=()=>notify('Click Apply to update selected annotation','info',1600);}if(brush){brush.oninput=()=>{updateBrushControls();saveBrushPreference();draw();};}updateBrushControls();const panelNextApply=el('panelApplyRoiClass');if(panelNextApply)panelNextApply.onclick=applySelectedRoiClass;const panelApply=el('annotationApply');if(panelApply)panelApply.onclick=applyAnnotationPanelMetadata;const del=el('deleteRoi');if(del)del.onclick=deleteSelectedRoi;const panelDelete=el('annotationDelete');if(panelDelete)panelDelete.onclick=deleteSelectedRoi;const panelExport=el('annotationExportSelected');if(panelExport)panelExport.onclick=exportSelectedAnnotations;const spotCsv=el('saveAnnotationSpotsCsv');if(spotCsv)spotCsv.onclick=saveAnnotationSpotsCsv;const spatialSpotCsv=el('seuratAnnotationSpotsCsv');if(spatialSpotCsv)spatialSpotCsv.onclick=saveAnnotationSpotsCsv;const undo=el('undoAnnotation'),redo=el('redoAnnotation'),smooth=el('smoothRoi'),fill=el('fillRoiHoles'),merge=el('mergeRois'),split=el('splitRoi');if(undo)undo.onclick=restoreAnnotationUndo;if(redo)redo.onclick=restoreAnnotationRedo;if(smooth)smooth.onclick=smoothSelectedRoi;if(fill)fill.onclick=fillSelectedRoiHoles;if(merge)merge.onclick=mergeSelectedAnnotations;if(split)split.onclick=splitSelectedAnnotation;const all=el('annotationSelectAll'),none=el('annotationSelectNone');if(all)all.onclick=selectAllAnnotations;if(none)none.onclick=selectNoAnnotations;}\n",
    "function updateButtons(){const has=rois.length>0,hasLayers=layers.length>0,drawable=hasDrawable(),selected=selectedRoi>=0&&!!rois[selectedRoi],editable=selected&&editableRoi(rois[selectedRoi]),editableDrawable=selected&&editable&&isDrawable(rois[selectedRoi]),exportable=roiExportIndices().length>0,mergeable=mergeCandidateIndices().length>=2,spotsAvailable=typeof annotationSpotItems==='function'&&annotationSpotItems().length>0,setDisabled=(id,value)=>{const button=el(id);if(button)button.disabled=!!value;},setActive=(id,value)=>{const button=el(id);if(button)button.classList.toggle('active',!!value);};['roiToggle','labelsToggle','prevRoi','nextRoi'].forEach(id=>setDisabled(id,!drawable));setDisabled('layersToggle',!has&&!hasLayers);setDisabled('annotationPanelToggle',false);setDisabled('layerPanelToggle',false);setDisabled('finishRoi',draft.length<3);setDisabled('undoPoint',draft.length<1&&!brushing);setDisabled('saveGeojson',!has&&draft.length<3&&brushPoints.length<2);setDisabled('saveAnnotationSpotsCsv',!drawable||!spotsAvailable);setDisabled('seuratAnnotationSpotsCsv',!drawable||!spotsAvailable);if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();['deleteRoi','annotationApply','annotationDelete'].forEach(id=>setDisabled(id,!selected));['smoothRoi','fillRoiHoles','splitRoi'].forEach(id=>setDisabled(id,!editableDrawable));setDisabled('mergeRois',!mergeable);const undoButton=el('undoAnnotation'),redoButton=el('redoAnnotation');if(undoButton)undoButton.disabled=annotationUndo.length<1;if(redoButton)redoButton.disabled=annotationRedo.length<1;['annotationClassSelect','annotationClassCustom','annotationColorInput'].forEach(id=>{const input=el(id);if(input)input.disabled=!selected;});const exportButton=el('annotationExportSelected');if(exportButton)exportButton.disabled=!exportable;const selectAll=el('annotationSelectAll'),selectNone=el('annotationSelectNone');if(selectAll)selectAll.disabled=!has;if(selectNone)selectNone.disabled=!has;setActive('roiToggle',showRois&&drawable);setActive('labelsToggle',showLabels&&drawable);setActive('crosshairToggle',showCrosshair);const panel=el('roiPanel'),open=!!(panel&&panel.classList.contains('open'));['layersToggle','annotationPanelToggle','layerPanelToggle'].forEach(id=>setActive(id,open));}\n"
  )
}

wsi_viewer_layers_js <- function() {
  paste0(
    "function layerStatePayload(){return (layers||[]).map(layer=>({id:layer.id||null,name:layer.name||null,type:layer.source_type||layer.type||null,visible:layer.visible!==false,opacity:layerOpacity(layer),count:layer.count||layerCount(layer)||0}));}\n",
    "function layerSlug(name){return String(name||'layer').toLowerCase().replace(/[^a-z0-9]+/g,'_').replace(/^_+|_+$/g,'')||'layer';}\n",
    "function layerOpacity(layer){const v=Number(layer&&layer.opacity);return Number.isFinite(v)?clamp(v,0,1):1;}\n",
    "function layerVisible(layer){return !!(layer&&layer.visible!==false);}\n",
    "function layerLabel(layer){return String((layer&&(layer.name||layer.id))||'R layer');}\n",
    "function layerCount(layer){if(!layer)return 0;if(Array.isArray(layer.items))return layer.items.length;if(Array.isArray(layer.values))return (Number(layer.nrow)||layer.values.length)*(Number(layer.ncol)||((layer.values[0]||[]).length));if(layer.data_uri)return 1;return Number(layer.count)||0;}\n",
    "function layerFindIndex(key){const value=String(key||'');return (layers||[]).findIndex(layer=>String(layer.id||'')===value||String(layer.name||'')===value);}\n",
    "function normaliseViewerLayer(layer){layer=layer||{};layer.name=layer.name||layer.id||'R layer';layer.id=layer.id||layerSlug(layer.name);layer.type=layer.type||'vector';if(typeof layer.visible==='undefined')layer.visible=true;if(typeof layer.opacity==='undefined')layer.opacity=1;if(!layer.colour)layer.colour='#38bdf8';layer.count=layer.count||layerCount(layer);return layer;}\n",
    "function layerLegendObject(layer){return (layer&&(layer.legend||(layer.metadata&&layer.metadata.legend)))||null;}\n",
    "function layerLegendStops(legend){const stops=legend&&Array.isArray(legend.stops)?legend.stops:[];return stops.filter(s=>s&&s.colour);}\n",
    "function formatLayerLegendValue(value,unit){const n=Number(value),u=String(unit||'').trim();if(!Number.isFinite(n))return 'NA';const digits=u==='px'?0:(Math.abs(n)>=100?0:(Math.abs(n)>=10?1:2));const text=(typeof fmt==='function'?fmt(n,digits):n.toFixed(digits));return u?text+' '+u:text;}\n",
    "function layerLegendGradientCss(stops){if(!stops.length)return 'linear-gradient(90deg,#64748b,#f8fafc)';if(stops.length===1)return 'linear-gradient(90deg,'+stops[0].colour+','+stops[0].colour+')';const last=Math.max(1,stops.length-1);return 'linear-gradient(90deg,'+stops.map((s,i)=>String(s.colour)+' '+Math.round(i/last*100)+'%').join(',')+')';}\n",
    "function layerLegendNode(layer){const legend=layerLegendObject(layer),stops=layerLegendStops(legend);if(!legend||!stops.length)return null;const box=document.createElement('div');box.className='layerLegend';const title=document.createElement('div');title.className='layerLegendTitle';const name=document.createElement('span');name.textContent=String(legend.title||'Legend');const unit=document.createElement('span');unit.textContent=String(legend.unit||'');title.append(name,unit);const grad=document.createElement('div');grad.className='layerLegendGradient';grad.style.background=layerLegendGradientCss(stops);const ticks=document.createElement('div');ticks.className='layerLegendTicks';stops.forEach(stop=>{const tick=document.createElement('span');tick.textContent=formatLayerLegendValue(stop.value,legend.unit);tick.title=(stop.name?String(stop.name)+': ':'')+tick.textContent;ticks.appendChild(tick);});box.append(title,grad,ticks);return box;}\n",
    "function proximityLegendLayer(){const idx=layerFindIndex('wsi_proximity_distance');return idx>=0?layers[idx]:null;}\n",
    "function proximityLegendBounds(left,top,width=null){const box=el('proximityLegend');if(!box)return null;const rect=box.getBoundingClientRect(),w=Math.max(120,Number(width)||rect.width||260),h=Math.max(48,rect.height||90),fixedWidth=Math.round(Math.min(w,innerWidth-16)),maxLeft=Math.max(8,innerWidth-fixedWidth-8),maxTop=Math.max(8,innerHeight-h-8);return {left:Math.round(clamp(Number(left)||8,8,maxLeft)),top:Math.round(clamp(Number(top)||8,8,maxTop)),width:fixedWidth};}\n",
    "function setProximityLegendPosition(left,top,save=false,width=null){const box=el('proximityLegend'),pos=proximityLegendBounds(left,top,width);if(!box||!pos)return;box.style.left=pos.left+'px';box.style.top=pos.top+'px';box.style.right='auto';box.style.bottom='auto';box.style.width=pos.width+'px';box.classList.add('moved');if(save&&typeof saveViewerPreferences==='function')saveViewerPreferences({proximity_legend:{left:pos.left,top:pos.top,width:pos.width}});}\n",
    "function applyProximityLegendPreferences(prefs=null){const box=el('proximityLegend');if(!box)return;const pos=(prefs||((typeof loadViewerPreferences==='function')?loadViewerPreferences():{})).proximity_legend||{};if(Number.isFinite(Number(pos.left))&&Number.isFinite(Number(pos.top)))setProximityLegendPosition(Number(pos.left),Number(pos.top),false,Number(pos.width)||null);}\n",
    "function startProximityLegendDrag(e){const box=el('proximityLegend');if(!box||e.button!==0||e.target.closest('button,input,select,textarea,a'))return;e.preventDefault();e.stopPropagation();const rect=box.getBoundingClientRect(),width=Math.round(rect.width||260);box.style.width=width+'px';proximityLegendDragState={startX:e.clientX,startY:e.clientY,left:rect.left,top:rect.top,width:width,moved:false};box.classList.add('dragging');window.addEventListener('mousemove',moveProximityLegendDrag);window.addEventListener('mouseup',finishProximityLegendDrag,{once:true});}\n",
    "function moveProximityLegendDrag(e){if(!proximityLegendDragState)return;const dx=e.clientX-proximityLegendDragState.startX,dy=e.clientY-proximityLegendDragState.startY;if(Math.abs(dx)+Math.abs(dy)>2)proximityLegendDragState.moved=true;if(proximityLegendDragState.moved){e.preventDefault();setProximityLegendPosition(proximityLegendDragState.left+dx,proximityLegendDragState.top+dy,false,proximityLegendDragState.width);}}\n",
    "function finishProximityLegendDrag(){const box=el('proximityLegend');window.removeEventListener('mousemove',moveProximityLegendDrag);if(box)box.classList.remove('dragging');if(proximityLegendDragState&&proximityLegendDragState.moved){const rect=box&&box.getBoundingClientRect();if(rect)setProximityLegendPosition(rect.left,rect.top,true,proximityLegendDragState.width);}proximityLegendDragState=null;}\n",
    "function bindProximityLegendDrag(){const box=el('proximityLegend');if(!box||box.dataset.dragBound==='1')return;box.dataset.dragBound='1';box.title='Drag to reposition this distance legend';box.addEventListener('mousedown',startProximityLegendDrag);window.addEventListener('resize',()=>{const b=el('proximityLegend');if(b&&b.classList.contains('moved')){const r=b.getBoundingClientRect();setProximityLegendPosition(r.left,r.top,false,r.width);}});}\n",
    "function renderProximityLegend(){const box=el('proximityLegend');if(!box)return;const layer=proximityLegendLayer(),legend=layer&&layerVisible(layer)?layerLegendObject(layer):null,stops=layerLegendStops(legend);box.innerHTML='';if(!legend||!stops.length){box.classList.remove('open');return;}const node=layerLegendNode(layer);if(node)box.appendChild(node);box.classList.add('open');bindProximityLegendDrag();applyProximityLegendPreferences();}\n",
    "function layerObjectSelected(layerIndex,itemIndex){return Number(layerIndex)===Number(selectedLayerIndex)&&Number(itemIndex)===Number(selectedLayerItemIndex);}\n",
    "function layerItemIsPoint(item){return !!(item&&(item.type==='point'||(Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y)))));}\n",
    "function layerHitTolerance(){return Math.max(4,10/Math.max(.0001,slideUnitScale()));}\n",
    "function layerPointHit(p,item,layer){const x=Number(item&&item.x),y=Number(item&&item.y);if(!Number.isFinite(x)||!Number.isFinite(y))return false;const r=Math.max(Number(item.radius||layer.radius||6),layerHitTolerance());return Math.hypot(Number(p.x)-x,Number(p.y)-y)<=r;}\n",
    "function layerPolygonHit(p,item){return !!(item&&isDrawable(item)&&roiContainsPoint(item,p));}\n",
    "function layerScopeText(value){return String(value??'').trim();}\n",
    "function layerScopeNumber(value){const n=Number(value);return Number.isFinite(n)?Math.round(n):null;}\n",
    "function layerItemHasProjectScope(item){if(!item)return false;return ['project_key','wsi_project_key','project_image','project_section','image_id','section_id','sample_id','project_image_index','project_section_index'].some(k=>layerScopeText(item[k])!=='');}\n",
    "function layerItemMatchesActiveProject(item,layer=null){if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||!projectItems.length)return true;if(!layerItemHasProjectScope(item))return true;const key=layerScopeText(item.project_key||item.wsi_project_key);if(key&&typeof projectAnnotationKey==='function')return key===String(projectAnnotationKey());const imageIndex=layerScopeNumber(item.project_image_index),sectionIndex=layerScopeNumber(item.project_section_index);if(imageIndex!==null&&imageIndex!==activeProjectIndex)return false;if(sectionIndex!==null&&sectionIndex!==activeProjectSectionIndex)return false;const activeItem=projectItems[activeProjectIndex]||{},activeSection=(typeof activeProjectSection==='function')?activeProjectSection():null,imageText=layerScopeText(item.project_image||item.image_id||item.sample_id),sectionText=layerScopeText(item.project_section||item.section_id);if(imageText){const imageValues=[activeItem.label,activeItem.id,activeItem.path].map(layerScopeText).filter(Boolean);if(imageValues.length&&!imageValues.includes(imageText))return false;}if(sectionText){const sectionValues=activeSection?[activeSection.label,activeSection.id,activeSection.scene].map(layerScopeText).filter(Boolean):['image'];if(sectionValues.length&&!sectionValues.includes(sectionText))return false;}return true;}\n",
    "function layerObjectAt(p){if(!p||!Array.isArray(layers))return null;for(let li=layers.length-1;li>=0;li--){const layer=layers[li];if(!layerVisible(layer)||String(layer.type||'vector').toLowerCase()!=='vector'||!Array.isArray(layer.items))continue;for(let ii=layer.items.length-1;ii>=0;ii--){const item=layer.items[ii];if(!item||item.visible===false||!layerItemMatchesActiveProject(item,layer))continue;if(layerItemIsPoint(item)?layerPointHit(p,item,layer):layerPolygonHit(p,item))return {layer:layer,item:item,layerIndex:li,itemIndex:ii};}}return null;}\n",
    "function upsertViewerLayer(layer){layer=normaliseViewerLayer(layer);const idx=layerFindIndex(layer.id);if(idx>=0&&layer.replace!==false)layers[idx]=layer;else layers.push(layer);buildLayerList();updateButtons();draw();scheduleViewerStateSync('layer_added',{id:layer.id,name:layer.name,type:layer.source_type||layer.type,count:layerCount(layer)});}\n",
    "function setViewerLayerVisible(key,visible=true){const idx=layerFindIndex(key);if(idx<0){notify('Layer not found','warning');return false;}layers[idx].visible=!!visible;buildLayerList();updateButtons();draw();scheduleViewerStateSync('layer_visibility_updated',{id:layers[idx].id,name:layers[idx].name,visible:layers[idx].visible});notify(layers[idx].visible?'Layer shown':'Layer hidden','success');return true;}\n",
    "function removeViewerLayer(key){const idx=layerFindIndex(key);if(idx<0)return false;const removed=layers.splice(idx,1)[0];buildLayerList();updateButtons();draw();scheduleViewerStateSync('layer_removed',{id:removed.id,name:removed.name});return true;}\n",
    "function heatmapLayerColour(layer,value){if(!Number.isFinite(value))return null;const min=Number(layer.min),max=Number(layer.max),den=Number.isFinite(max-min)&&Math.abs(max-min)>1e-12?max-min:1,t=clamp((value-(Number.isFinite(min)?min:0))/den,0,1),a=layerOpacity(layer);if(layer.source_type==='mask'||(Number.isFinite(min)&&Number.isFinite(max)&&min>=0&&max<=1&&layer.colour)){if(value<=0)return null;return hexToRgba(normaliseHexColour(layer.colour||'#22c55e'),Math.max(.05,a*.45));}const r=Math.round(40+215*t),g=Math.round(180*(1-Math.abs(t-.5)*2)+70*t),b=Math.round(255*(1-t)+40*t);return 'rgba('+r+','+g+','+b+','+Math.max(.05,a*.6)+')';}\n",
    "function drawHeatmapLayer(layer){const values=layer.values||[],rows=values.length,cols=rows?(values[0]||[]).length:0,ext=layer.extent||{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};if(!rows||!cols)return;const cw=(Number(ext.xmax)-Number(ext.xmin))/cols,ch=(Number(ext.ymax)-Number(ext.ymin))/rows;ctx.save();for(let r=0;r<rows;r++){const row=values[r]||[];for(let c=0;c<cols;c++){const color=heatmapLayerColour(layer,Number(row[c]));if(!color)continue;const p0=slideToCanvas({x:Number(ext.xmin)+c*cw,y:Number(ext.ymin)+r*ch}),p1=slideToCanvas({x:Number(ext.xmin)+(c+1)*cw,y:Number(ext.ymin)+(r+1)*ch}),x=Math.min(p0.x,p1.x),y=Math.min(p0.y,p1.y),w=Math.abs(p1.x-p0.x),h=Math.abs(p1.y-p0.y);if(x>innerWidth||y>innerHeight||x+w<0||y+h<0)continue;ctx.fillStyle=color;ctx.fillRect(Math.floor(x),Math.floor(y),Math.ceil(w)+1,Math.ceil(h)+1);}}ctx.restore();}\n",
    "function drawImageLayer(layer){if(!layer.data_uri)return;const ext=layer.extent||{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};if(!layer._image){const img=new Image();img.onload=()=>draw();img.src=layer.data_uri;layer._image=img;return;}if(!layer._image.complete)return;const p0=slideToCanvas({x:Number(ext.xmin),y:Number(ext.ymin)}),p1=slideToCanvas({x:Number(ext.xmax),y:Number(ext.ymax)});ctx.save();ctx.globalAlpha=layerOpacity(layer);ctx.drawImage(layer._image,Math.min(p0.x,p1.x),Math.min(p0.y,p1.y),Math.abs(p1.x-p0.x),Math.abs(p1.y-p0.y));ctx.restore();}\n",
    "function drawVectorLayer(layer,layerIndex=-1){const items=layer.items||[];if(!items.length)return;ctx.save();items.forEach((item,itemIndex)=>{if(item.visible===false||!layerItemMatchesActiveProject(item,layer))return;const opacity=layerOpacity(layer),colour=item.colour||layer.colour||'#38bdf8',selected=layerObjectSelected(layerIndex,itemIndex);if(layerItemIsPoint(item)){const q=slideToCanvas({x:Number(item.x),y:Number(item.y)}),r=Math.max(2,Number(item.radius||layer.radius||6)*slideUnitScale());if(q.x+r<0||q.y+r<0||q.x-r>innerWidth||q.y-r>innerHeight)return;ctx.globalAlpha=opacity;ctx.beginPath();ctx.arc(q.x,q.y,r,0,Math.PI*2);ctx.fillStyle=item.fill||hexToRgba(colour,.28);ctx.strokeStyle=selected?'#ffffff':colour;ctx.lineWidth=selected?3:1.5;ctx.fill();ctx.stroke();if(selected){ctx.globalAlpha=1;ctx.beginPath();ctx.arc(q.x,q.y,r+5,0,Math.PI*2);ctx.strokeStyle='#facc15';ctx.lineWidth=2;ctx.stroke();}ctx.globalAlpha=1;return;}if(!isDrawable(item))return;const groups=roiDrawGroups(item);groups.forEach(group=>{ctx.beginPath();drawPathRings(group.rings);drawPathRings(group.holes);ctx.globalAlpha=opacity;ctx.fillStyle=item.fill||hexToRgba(colour,.12);ctx.strokeStyle=selected?'#ffffff':colour;ctx.lineWidth=selected?Math.max(3,Number(layer.line_width||item.line_width||2)+2):Number(layer.line_width||item.line_width||2);ctx.fill('evenodd');ctx.stroke();if(selected){ctx.globalAlpha=1;ctx.strokeStyle='#facc15';ctx.lineWidth=1.5;ctx.stroke();}ctx.globalAlpha=1;});});ctx.restore();}\n",
    "function drawLayers(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('layers'))return;(layers||[]).forEach((layer,i)=>{if(!layerVisible(layer))return;const type=String(layer.type||'vector').toLowerCase();if(type==='heatmap'||type==='mask')drawHeatmapLayer(layer);else if(type==='image')drawImageLayer(layer);else drawVectorLayer(layer,i);});}\n",
    "function buildLayerList(){const list=el('layerList'),summary=el('layerSummary');if(!list||!summary){if(typeof renderProximityLegend==='function')renderProximityLegend();return;}list.innerHTML='';summary.textContent=layers.length?(layers.length+' R-controlled layer'+(layers.length===1?'':'s')+(selectedLayerObject()?(' | selected '+layerObjectLabel()):'')):'No R layers yet.';layers.forEach((layer,i)=>{normaliseViewerLayer(layer);const item=document.createElement('div');item.className='layerItem';if(!layerVisible(layer))item.classList.add('hidden');if(i===selectedLayerIndex)item.classList.add('objectSelected');const top=document.createElement('div');top.className='layerTop';const box=document.createElement('input');box.type='checkbox';box.checked=layerVisible(layer);box.title='Toggle layer visibility';box.onchange=e=>{layer.visible=!!e.target.checked;buildLayerList();draw();scheduleViewerStateSync('layer_visibility_updated',{id:layer.id,name:layer.name,visible:layer.visible});};const sw=document.createElement('span');sw.className='swatch';sw.style.background=layer.colour||'#38bdf8';const nm=document.createElement('span');nm.className='roiName';nm.textContent=layerLabel(layer);const meta=document.createElement('span');meta.className='roiClass';meta.textContent=(layer.source_type||layer.type||'layer')+' '+layerCount(layer);top.append(box,sw,nm,meta);const controls=document.createElement('div');controls.className='layerControls';const op=document.createElement('input');op.type='range';op.min='0';op.max='1';op.step='0.05';op.value=String(layerOpacity(layer));op.title='Layer opacity';op.oninput=e=>{layer.opacity=Number(e.target.value);draw();};op.onchange=()=>scheduleViewerStateSync('layer_opacity_updated',{id:layer.id,name:layer.name,opacity:layerOpacity(layer)});const remove=document.createElement('button');remove.type='button';remove.textContent='Remove';remove.title='Remove this R-controlled layer from the viewer';remove.onclick=()=>removeViewerLayer(layer.id);controls.append(document.createTextNode('opacity'),op,remove);item.append(top,controls);const legendNode=layerLegendNode(layer);if(legendNode)item.appendChild(legendNode);list.appendChild(item);});if(typeof renderProximityLegend==='function')renderProximityLegend();if(layers.length)setRoiPanelOpen(true,{automatic:true});}\n"
  )
}

wsi_viewer_cell_controls_js <- function() {
  paste0(
    "function cellphenotyperConfig(){return cfg.cellphenotyper||{};}\n",
    "function cellphenotyperLayerId(){const cp=cellphenotyperConfig();return String(cp.stardist_layer_id||'cellphenotyper_stardist_cells');}\n",
    "function findCellphenotyperLayer(){const id=cellphenotyperLayerId();return (layers||[]).find(layer=>String(layer.id||'')===id||String(layer.source_type||'')==='cellphenotyper_stardist')||null;}\n",
    "function cellLayerBounds(layer){const items=(layer&&Array.isArray(layer.items))?layer.items:[],xs=[],ys=[];items.forEach(item=>{const x=Number(item.x),y=Number(item.y);if(Number.isFinite(x)&&Number.isFinite(y)){xs.push(x);ys.push(y);}});if(!xs.length)return null;return {xmin:Math.min(...xs),ymin:Math.min(...ys),xmax:Math.max(...xs),ymax:Math.max(...ys)};}\n",
    "function updateCellControls(){const layer=findCellphenotyperLayer(),cp=cellphenotyperConfig(),count=layer?layerCount(layer):Number(cp.cell_count||0),has=!!layer&&count>0,toggle=el('cellToggle'),zoom=el('cellZoom'),opacity=el('cellOpacity'),radius=el('cellRadius'),radiusValue=el('cellRadiusValue'),summary=el('cellSummary');if(toggle){toggle.disabled=!has;toggle.classList.toggle('active',has&&layerVisible(layer));}if(zoom)zoom.disabled=!has;if(opacity){opacity.disabled=!has;if(layer)opacity.value=String(layerOpacity(layer));}if(radius){radius.disabled=!has;const r=layer&&Array.isArray(layer.items)&&layer.items.length?Number(layer.items[0].radius||layer.radius||6):Number(layer&&layer.radius||6);if(Number.isFinite(r))radius.value=String(Math.round(r));if(radiusValue)radiusValue.textContent=(Number.isFinite(r)?Math.round(r):6)+' px';}if(summary){if(!cp.enabled&&!has)summary.textContent='No CellPhenotyper project is attached to this viewer.';else if(!has)summary.textContent='No CellPhenotyper cell table was found in this project.';else summary.textContent=(layerVisible(layer)?'Showing ':'Hidden ')+count.toLocaleString()+' CellPhenotyper cell'+(count===1?'':'s')+'.';}}\n",
    "function setCellLayerVisible(visible){const layer=findCellphenotyperLayer();if(!layer){notify('No CellPhenotyper cell layer is loaded','warning');updateCellControls();return false;}layer.visible=!!visible;buildLayerList();updateCellControls();draw();scheduleViewerStateSync('cell_layer_visibility_updated',{id:layer.id,name:layer.name,visible:layer.visible,count:layerCount(layer)});notify(layer.visible?'CellPhenotyper cells shown':'CellPhenotyper cells hidden',layer.visible?'success':'info',1600);return true;}\n",
    "function toggleCellLayer(){const layer=findCellphenotyperLayer();setCellLayerVisible(!(layer&&layerVisible(layer)));}\n",
    "function setCellLayerOpacity(value){const layer=findCellphenotyperLayer();if(!layer)return;layer.opacity=clamp(Number(value),0,1);buildLayerList();updateCellControls();draw();scheduleViewerStateSync('cell_layer_opacity_updated',{id:layer.id,name:layer.name,opacity:layerOpacity(layer)});}\n",
    "function setCellLayerRadius(value){const layer=findCellphenotyperLayer();if(!layer)return;const r=clamp(Math.round(Number(value)||6),1,40);layer.radius=r;(layer.items||[]).forEach(item=>{if(item.type==='point'||(Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y))))item.radius=r;});updateCellControls();draw();scheduleViewerStateSync('cell_layer_radius_updated',{id:layer.id,name:layer.name,radius:r});}\n",
    "function zoomToCellLayer(){const layer=findCellphenotyperLayer(),b=cellLayerBounds(layer);if(!b){notify('No CellPhenotyper cell positions are available','warning');return;}if(typeof zoomToSlideBounds==='function')zoomToSlideBounds(b,1.2);else{const w=Math.max(1,b.xmax-b.xmin),h=Math.max(1,b.ymax-b.ymin),cx=(b.xmin+b.xmax)/2,cy=(b.ymin+b.ymax)/2,corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymax}].map(slideToImage),iw=Math.abs(corners[1].x-corners[0].x),ih=Math.abs(corners[1].y-corners[0].y);scale=clamp(Math.min(innerWidth/Math.max(1,iw*1.2),innerHeight/Math.max(1,ih*1.2)),minScale*.8,80);const c=slideToImage({x:cx,y:cy});offsetX=innerWidth/2-c.x*scale;offsetY=innerHeight/2-c.y*scale;draw();}notify('Zoomed to CellPhenotyper cells','success',1400);}\n",
    "function bindCellControls(){const toggle=el('cellToggle'),zoom=el('cellZoom'),opacity=el('cellOpacity'),radius=el('cellRadius');if(toggle)toggle.onclick=toggleCellLayer;if(zoom)zoom.onclick=zoomToCellLayer;if(opacity)opacity.oninput=e=>setCellLayerOpacity(e.target.value);if(radius)radius.oninput=e=>setCellLayerRadius(e.target.value);updateCellControls();}\n"
  )
}

wsi_viewer_seurat_js <- function() {
  paste0(
    "function seuratConfig(){return cfg.seurat||{enabled:false,plots:[]};}\n",
    "function seuratSourceName(){return String(seuratConfig().source_name||'Spatial');}\n",
    "function seuratEnabled(){return !!seuratConfig().enabled;}\n",
    "function seuratLayerId(){return String(seuratConfig().spot_layer_id||'seurat_spots');}\n",
    "function findSeuratLayer(){const id=seuratLayerId();return (layers||[]).find(layer=>String(layer.id||'')===id||String(layer.source_type||'')==='seurat_spots')||null;}\n",
    "function seuratPlots(){const plots=seuratConfig().plots||[];return Array.isArray(plots)?plots:[];}\n",
    "function seuratPlotItem(){const plots=seuratPlots();if(!plots.length)return null;if(!Number.isInteger(seuratActivePlotIndex)||seuratActivePlotIndex<0||seuratActivePlotIndex>=plots.length)seuratActivePlotIndex=0;return plots[seuratActivePlotIndex]||null;}\n",
    "function seuratPlotReductionKey(plot){return String((plot&&plot.reduction)||'').trim().toLowerCase();}\n",
    "function seuratSameReduction(plot,target){const key=seuratPlotReductionKey(plot),want=String(target||'').trim().toLowerCase();return !!key&&!!want&&key===want;}\n",
    "function seuratRecordPayload(item,section){if(section)return section.seurat||null;if(item&&item.seurat)return item.seurat;return null;}\n",
    "function seuratProjectRecords(){const records=[];if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||!projectItems.length)return records;projectItems.forEach((item,itemIndex)=>{const sections=(typeof projectSections==='function')?projectSections(item):[],sectionPayloads=[];sections.forEach((section,sectionIndex)=>{const payload=seuratRecordPayload(item,section);if(payload&&payload.enabled&&Array.isArray(payload.plots)&&payload.plots.length)sectionPayloads.push({seurat:payload,item:item,section:section,itemIndex:itemIndex,sectionIndex:sectionIndex});});if(sectionPayloads.length){sectionPayloads.forEach(x=>records.push(x));return;}const payload=seuratRecordPayload(item,null);if(payload&&payload.enabled&&Array.isArray(payload.plots)&&payload.plots.length)records.push({seurat:payload,item:item,section:null,itemIndex:itemIndex,sectionIndex:-1});});return records;}\n",
    "function seuratAllTissuesAvailable(){return seuratProjectRecords().length>1;}\n",
    "function seuratRecordLabel(record){return String((record&&record.section&&(record.section.label||record.section.id))||(record&&record.item&&(record.item.label||record.item.id||record.item.path))||(record&&record.seurat&&record.seurat.source_name)||seuratSourceName()||'tissue');}\n",
    "function seuratAllTissuePlotPoints(){const active=seuratPlotItem(),target=(active&&active.reduction)||seuratConfig().reduction||'',records=seuratProjectRecords();if(records.length<2)return[];const out=[];records.forEach(record=>{const plots=Array.isArray(record.seurat&&record.seurat.plots)?record.seurat.plots:[];let plot=plots.find(p=>seuratSameReduction(p,target));if(!plot&&plots[seuratActivePlotIndex])plot=plots[seuratActivePlotIndex];if(!plot||!Array.isArray(plot.points))return;const tissue=seuratRecordLabel(record),prefix=tissue+'::';plot.points.forEach(point=>{if(!point)return;const copy=Object.assign({},point),raw=String(point.label??point.spot_id??point.barcode??point.id??'').trim();copy.__all_tissues=true;copy.__project_item_index=record.itemIndex;copy.__project_section_index=record.sectionIndex;copy.tissue_label=tissue;copy.__wsi_plot_label=prefix+(raw||('spot_'+out.length));out.push(copy);});});return out;}\n",
    "function seuratEffectivePlotScope(){return seuratPlotScope==='all'&&seuratAllTissuesAvailable()?'all':'current';}\n",
    "function seuratPlotPoints(){if(seuratEffectivePlotScope()==='all'){const pts=seuratAllTissuePlotPoints();if(pts.length)return pts;}const plot=seuratPlotItem(),pts=plot&&plot.points||[];return Array.isArray(pts)?pts:[];}\n",
    "function seuratSummary(msg){const box=el('seuratSummary');if(box&&msg)box.textContent=msg;}\n",
    "function middleEllipsis(text,max=20,head=8,tail=7){const chars=Array.from(String(text||''));if(chars.length<=max)return chars.join('');if(head+tail+3>max){const available=Math.max(2,max-3);head=Math.ceil(available/2);tail=Math.floor(available/2);}return chars.slice(0,head).join('')+'...'+chars.slice(-tail).join('');}\n",
    "function reductionDisplayName(value){return middleEllipsis(String(value||'reduction').toUpperCase(),12,4,4);}\n",
    "function seuratClusterConfig(){return seuratConfig().clusters||{enabled:false,fields:[]};}\n",
    "function seuratClusterFields(){const fields=seuratClusterConfig().fields||[];return Array.isArray(fields)?fields.filter(f=>f&&String(f.field||'').trim()):[];}\n",
    "function seuratFindClusterField(name){const query=String(name||'').trim();if(!query)return null;return seuratClusterFields().find(f=>String(f.field||'')===query)||seuratClusterFields().find(f=>String(f.field||'').toLowerCase()===query.toLowerCase())||null;}\n",
    "function seuratClusterDefaultField(){return String(seuratClusterConfig().default_field||((seuratClusterFields()[0]||{}).field)||'');}\n",
    "function updateSeuratControls(){const layer=findSeuratLayer(),cfgs=seuratConfig(),source=seuratSourceName(),has=seuratEnabled()&&!!layer&&layerCount(layer)>0,toggle=el('seuratSpotToggle'),opacity=el('seuratSpotOpacity'),help=el('seuratSpotOpacityHelp'),plotButtons=document.querySelectorAll('.seuratPlotOpen'),clear=el('seuratClearSelection'),summary=el('seuratSummary'),geneInput=el('seuratGeneInput'),geneApply=el('seuratGeneApply'),geneClear=el('seuratGeneClear'),geneSummary=el('seuratGeneSummary'),clusterSelect=el('seuratClusterSelect'),clusterApply=el('seuratClusterApply'),clusterClear=el('seuratClusterClear'),clusterSummary=el('seuratClusterSummary'),scopeCurrent=el('seuratPlotScopeCurrent'),scopeAll=el('seuratPlotScopeAll');const allAvailable=seuratAllTissuesAvailable(),effectiveScope=seuratEffectivePlotScope();if(scopeCurrent)scopeCurrent.classList.toggle('active',effectiveScope==='current');if(scopeAll){scopeAll.disabled=!allAvailable;scopeAll.classList.toggle('active',effectiveScope==='all');}if(toggle){toggle.disabled=!has;toggle.classList.toggle('active',has&&layerVisible(layer));}if(opacity){opacity.disabled=!has;if(layer)opacity.value=String(layerOpacity(layer));}if(help)help.disabled=!seuratEnabled();const genes=seuratExpressionGenes(),dynamic=seuratDynamicGeneAvailable(),hasGenes=seuratEnabled()&&(genes.length>0||dynamic),canClearGene=has||!!seuratActiveGene;if(geneInput){geneInput.disabled=!hasGenes;if(seuratActiveGene&&!geneInput.value)geneInput.value=seuratActiveGene;}if(geneApply)geneApply.disabled=!hasGenes;if(geneClear)geneClear.disabled=!canClearGene;if(geneSummary){if(!hasGenes)geneSummary.textContent='No live '+source+' gene lookup or embedded gene values are available.';else if(dynamic)geneSummary.textContent=(seuratActiveGene?('Loaded '+seuratActiveGene+'. '):'')+'Type any gene name; values are fetched from R when selected.'+(has?'':' Main slide spot overlay is hidden/unavailable, but reduction plots and R sync can still use the gene.');else geneSummary.textContent=(seuratActiveGene?('Loaded '+seuratActiveGene+'. '):'')+genes.length.toLocaleString()+' gene'+(genes.length===1?'':'s')+' available.';}const clusterFields=seuratClusterFields(),hasClusters=has&&clusterFields.length>0;if(seuratActiveCluster&&!seuratFindClusterField(seuratActiveCluster))seuratActiveCluster='';if(clusterSelect){clusterSelect.disabled=!hasClusters;if(hasClusters&&!clusterSelect.value)clusterSelect.value=seuratActiveCluster||seuratClusterDefaultField();}if(clusterApply)clusterApply.disabled=!hasClusters;if(clusterClear)clusterClear.disabled=!has;if(clusterSummary){if(!hasClusters)clusterSummary.textContent='No clustering metadata was detected in this '+source+' object.';else{const active=seuratFindClusterField(seuratActiveCluster),count=clusterFields.length;clusterSummary.textContent=(active?('Colouring by '+(active.label||active.field)+'. '):'')+count.toLocaleString()+' clustering/annotation field'+(count===1?'':'s')+' available.';}}const activePlot=seuratPlotItem(),hasPlot=seuratEnabled()&&seuratPlotPoints().length>0;plotButtons.forEach(btn=>{const idx=Number(btn.dataset.plotIndex||0);btn.disabled=!seuratEnabled()||!seuratPlots()[idx];btn.classList.toggle('active',idx===seuratActivePlotIndex);});if(clear)clear.disabled=!hasPlot&&!seuratSelectedLabels.size;if(summary){if(!seuratEnabled())summary.textContent='No spatial object is attached to this viewer.';else if(!has)summary.textContent='No '+source+' spatial spots were found.';else summary.textContent=(layerVisible(layer)?'Showing ':'Hidden ')+layerCount(layer).toLocaleString()+' '+source+' spot'+(layerCount(layer)===1?'':'s')+' | '+reductionDisplayName((activePlot&&activePlot.reduction)||cfgs.reduction||'reduction')+(effectiveScope==='all'?' | all tissues':'')+(seuratActiveGene?' | gene '+seuratActiveGene:'')+(seuratActiveCluster?' | cluster '+seuratActiveCluster:'');}if(typeof updateSpatialTileControls==='function')updateSpatialTileControls();}\n",
    "function toggleSeuratSpots(){const layer=findSeuratLayer();if(!layer)return;setViewerLayerVisible(layer.id,!layerVisible(layer));updateSeuratControls();}\n",
    "function setSeuratSpotOpacity(value){const layer=findSeuratLayer();if(!layer)return;layer.opacity=clamp(Number(value),0,1);draw();scheduleViewerStateSync('layer_opacity_updated',{id:layer.id,name:layer.name,opacity:layerOpacity(layer)});}\n",
    "function seuratSpotOpacityHelpText(){return 'Spot size is fixed by the spatial transcriptomics platform metadata. Visium spots are 55 microns; this viewer renders the mapped slide-pixel radius without manual resizing.';}\n",
    "function showSeuratSpotOpacityHelp(control=null){notify(seuratSpotOpacityHelpText(),'info',7000);if(control&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);}\n",
    "function zoomToSeuratSpots(){const layer=findSeuratLayer();if(!layer||!Array.isArray(layer.items)||!layer.items.length)return;const b=cellLayerBounds(layer);if(!b)return;const corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(slideToViewImagePoint),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),pad=1.25;scale=clamp(Math.min(innerWidth/Math.max(1,(Math.max(...xs)-Math.min(...xs))*pad),innerHeight/Math.max(1,(Math.max(...ys)-Math.min(...ys))*pad)),minScale*0.8,40);offsetX=innerWidth/2-((Math.min(...xs)+Math.max(...xs))/2)*scale;offsetY=innerHeight/2-((Math.min(...ys)+Math.max(...ys))/2)*scale;draw();}\n",
    "let seuratPlotTransform=null,seuratSelectionDrag=null,seuratSelectionPolygon=[],seuratSelectedLabels=new Set(),seuratSelectionMatchedCount=0,seuratPlotDrag=null,seuratPlotResizeObserver=null,seuratActivePlotIndex=0,seuratPlotScope='current',seuratActiveGene=String(((seuratConfig().gene_expression||{}).default_gene)||''),seuratActiveCluster='',seuratGeneFetchToken=0,spatialTilePreviewRows=[],spatialTileDrag=null;\n",
    "function seuratPointLabel(point){if(!point)return '';return String(point.__wsi_plot_label??point.label??point.spot_id??point.id??'');}\n",
    "function seuratDynamicGeneUrl(){return String(cfg.seurat_gene_url||'');}\n",
    "function seuratDynamicGeneAvailable(){return !!seuratDynamicGeneUrl();}\n",
    "function spatialTileExportUrl(){return String(cfg.spatial_tile_export_url||'');}\n",
    "function spatialTileExt(format){const f=String(format||'png').toLowerCase();return f==='jpeg'?'jpg':(f==='tiff'?'tiff':f);}\n",
    "function spatialTileSafeName(value,fallback='spot'){return String(value||fallback).replace(/[^A-Za-z0-9_.-]+/g,'_').replace(/^_+|_+$/g,'')||fallback;}\n",
    "function spatialTileStatus(msg){const box=el('spatialTileSummary');if(box)box.textContent=msg||'';}\n",
    "function spatialTileHelpText(){return 'Preview tile boxes on the slide before export. If an annotation is selected, only tiles fully inside that annotation are included.';}\n",
    "function showSpatialTileHelp(control=null){notify(spatialTileHelpText(),'info',7000);if(control&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);}\n",
    "function spatialTilePanel(){return el('spatialTileWindow');}\n",
    "function spatialTileSelectedRoi(){return selectedRoi>=0&&rois[selectedRoi]?rois[selectedRoi]:null;}\n",
    "function spatialTileProjectLabels(){let image='',section='';try{if(typeof projectStatePayload==='function'){const project=projectStatePayload()||{},item=project.active_item||project.item||{},sec=project.active_section||project.section||{};image=String(item.label||item.name||item.path||project.label||'');section=String(sec.label||sec.name||sec.id||project.section_label||'');}}catch(e){}if(!image&&typeof projectItems!=='undefined'&&Array.isArray(projectItems)){const item=projectItems[activeProjectIndex]||{};image=String(item.label||item.name||item.path||'');if(typeof activeProjectSection==='function'){const sec=activeProjectSection()||{};section=String(sec.label||sec.name||sec.id||'');}}return {image:image,section:section};}\n",
    "function spatialTileInputs(){const size=Number((el('spatialTileSize')||{}).value||512),units=String((el('spatialTileUnits')||{}).value||'px'),format=String((el('spatialTileFormat')||{}).value||'png'),output=String((el('spatialTileOutputDir')||{}).value||'spot_tiles').trim(),overwrite=!!((el('spatialTileOverwrite')||{}).checked);return {size:Number.isFinite(size)&&size>0?size:512,units:units==='um'?'um':'px',format:format==='jpg'?'jpeg':format,output_dir:output||'spot_tiles',overwrite:overwrite};}\n",
    "function spatialTilePixelSize(spec=spatialTileInputs()){if(spec.units==='px'){const v=Math.max(1,Math.round(spec.size));return {width:v,height:v};}const mpp=cfg.mpp||{},mx=Number(mpp.x),my=Number(mpp.y||mpp.x);if(!Number.isFinite(mx)||mx<=0||!Number.isFinite(my)||my<=0)return null;return {width:Math.max(1,Math.round(spec.size/mx)),height:Math.max(1,Math.round(spec.size/my))};}\n",
    "function spatialTileActiveSpots(){const layer=findSeuratLayer();if(!layer||!Array.isArray(layer.items))return[];return layer.items.map((item,i)=>Object.assign({__spot_index:i},item)).filter(item=>Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y)));}\n",
    "function spatialTileInsideSelected(tile,roi){if(!roi)return true;const x=tile.x,y=tile.y,w=tile.width,h=tile.height;return roiContainsPoint(roi,{x:x,y:y})&&roiContainsPoint(roi,{x:x+w,y:y})&&roiContainsPoint(roi,{x:x+w,y:y+h})&&roiContainsPoint(roi,{x:x,y:y+h});}\n",
    "function spatialTileRows(){const spec=spatialTileInputs(),px=spatialTilePixelSize(spec),spots=spatialTileActiveSpots(),roi=spatialTileSelectedRoi(),project=spatialTileProjectLabels();if(!px){spatialTileStatus('Micron tile sizes need microns-per-pixel metadata. Use pixels for this image.');return [];}const ext=spatialTileExt(spec.format),rows=[];spots.forEach((spot,i)=>{const cx=Number(spot.x),cy=Number(spot.y),x=Math.round(cx-px.width/2),y=Math.round(cy-px.height/2),row={tile_id:'spot_'+spatialTileSafeName(spot.id||spot.barcode||spot.label||String(i+1),'spot_'+(i+1)),x:x,y:y,width:px.width,height:px.height,level:0,row:i+1,col:1,downsample:1,tissue_fraction:null,spot_id:String(spot.id||spot.barcode||spot.label||('spot_'+(i+1))),spot_label:String(spot.label||spot.name||spot.barcode||spot.id||('spot '+(i+1))),roi_id:roi?String(roi.id||roi.name||'selected_roi'):'',project_image:project.image,project_section:project.section};row.output_file=spatialTileSafeName(row.tile_id,'spot_'+(i+1))+'.'+ext;if(x<0||y<0||x+px.width>Number(cfg.slide_width)||y+px.height>Number(cfg.slide_height))return;if(!spatialTileInsideSelected(row,roi))return;rows.push(row);});return rows;}\n",
    "function spatialTileLayerFromRows(rows,spec=spatialTileInputs()){const items=(rows||[]).map(row=>({id:'tile_'+row.tile_id,name:row.spot_label||row.tile_id,type:'polygon',rings:[[{x:row.x,y:row.y},{x:row.x+row.width,y:row.y},{x:row.x+row.width,y:row.y+row.height},{x:row.x,y:row.y+row.height},{x:row.x,y:row.y}]],colour:'#facc15',fill:'rgba(250,204,21,.06)',line_width:1.5,spot_id:row.spot_id,spot_label:row.spot_label}));return {id:'spatial_spot_tile_preview',name:seuratSourceName()+' spot tile preview',type:'vector',source_type:'spatial_spot_tiles',visible:true,opacity:.95,colour:'#facc15',replace:true,count:items.length,items:items,metadata:{tile_size:spec.size,units:spec.units,format:spec.format,selected_roi:spatialTileSelectedRoi()?true:false}};}\n",
    "function updateSpatialTileControls(){const open=el('seuratTileWindowOpen'),preview=el('spatialTilePreview'),clear=el('spatialTileClear'),save=el('spatialTileSave'),subtitle=el('spatialTileSubtitle'),spots=spatialTileActiveSpots(),has=seuratEnabled()&&spots.length>0;if(open)open.disabled=!has;if(preview)preview.disabled=!has;if(clear)clear.disabled=!spatialTilePreviewRows.length;if(save)save.disabled=!spatialTilePreviewRows.length||!spatialTileExportUrl();if(subtitle)subtitle.textContent=has?(spots.length.toLocaleString()+' spots available'+(spatialTileSelectedRoi()?' | selected annotation filter active':'')):'No spatial spots available.';}\n",
    "function previewSpatialTiles(sync=true){const rows=spatialTileRows(),spec=spatialTileInputs(),roi=spatialTileSelectedRoi();spatialTilePreviewRows=rows;upsertViewerLayer(spatialTileLayerFromRows(rows,spec));updateSpatialTileControls();const msg=rows.length?('Previewing '+rows.length.toLocaleString()+' spot-centered tile'+(rows.length===1?'':'s')+(roi?' inside selected annotation.':'.')):'No tiles matched the current spots, slide bounds, and selected annotation.';spatialTileStatus(msg);if(sync)scheduleViewerStateSync('tile_preview_created',{source:'spatial_spots',count:rows.length,tile_size:spec.size,units:spec.units,format:spec.format,selected_roi:roi?roi.id||roi.name||null:null,tile_preview:rows});notify(rows.length?'Spot tile preview ready':'No spot tiles to preview',rows.length?'success':'warning',2600);}\n",
    "function clearSpatialTilePreview(redraw=true,sync=true){spatialTilePreviewRows=[];if(typeof layerFindIndex==='function'){const idx=layerFindIndex('spatial_spot_tile_preview');if(idx>=0)layers.splice(idx,1);}buildLayerList();updateSpatialTileControls();spatialTileStatus('Spot tile preview cleared.');if(redraw)draw();if(sync)scheduleViewerStateSync('tile_preview_cleared',{source:'spatial_spots'});}\n",
    "async function saveSpatialTiles(){if(!spatialTilePreviewRows.length)previewSpatialTiles(false);if(!spatialTilePreviewRows.length){notify('No spot tiles to save','warning');return;}const url=spatialTileExportUrl();if(!url){notify('Saving spot tiles requires a live R viewer session. Reopen with wsi_viewer_live(...) or wsi_viewer_session(...).','warning',6200);return;}const spec=spatialTileInputs();spatialTileStatus('Saving '+spatialTilePreviewRows.length.toLocaleString()+' spot tile'+(spatialTilePreviewRows.length===1?'':'s')+' through R...');try{const response=await fetch(url,{method:'POST',headers:{'Accept':'application/json','Content-Type':'application/json'},body:JSON.stringify({output_dir:spec.output_dir,format:spec.format,overwrite:spec.overwrite,tile_size:spec.size,units:spec.units,source_name:seuratSourceName(),selected_roi:selectedRoiFeatureObject(),tiles:spatialTilePreviewRows})});let payload=null;try{payload=await response.json();}catch(e){}if(!response.ok||payload&&payload.ok===false)throw new Error(payload&&payload.error||('HTTP '+response.status));handleViewerCommands(payload);const result=payload&&payload.spatial_tiles||payload||{},count=result.tile_count||spatialTilePreviewRows.length,index=result.spot_index_file||null;spatialTileStatus('Saved '+count.toLocaleString()+' tile'+(count===1?'':'s')+' to '+(result.output_dir||spec.output_dir)+(index?(' | spot index CSV: '+index):'')+'.');scheduleViewerStateSync('tiles_extracted',{source:'spatial_spots',count:count,output_dir:result.output_dir||spec.output_dir,manifest_file:result.manifest_file||null,spot_index_file:index});notify(index?'Spot tiles and spot index CSV saved':'Spot tiles saved','success',3600);}catch(e){spatialTileStatus('Tile export failed: '+e.message);notify('Tile export failed: '+e.message,'error',6200);}}\n",
    "function openSpatialTileWindow(){const panel=spatialTilePanel();if(!panel)return;panel.classList.add('open');panel.setAttribute('aria-hidden','false');updateSpatialTileControls();spatialTileStatus(spatialTilePreviewRows.length?('Preview contains '+spatialTilePreviewRows.length.toLocaleString()+' tile'+(spatialTilePreviewRows.length===1?'':'s')+'.'):'Choose a tile size, preview boxes, then save from a live viewer session.');}\n",
    "function closeSpatialTileWindow(){const panel=spatialTilePanel();if(panel){panel.classList.remove('open');panel.setAttribute('aria-hidden','true');}}\n",
    "function moveSpatialTilePanel(left,top){const panel=spatialTilePanel();if(!panel)return;const rect=panel.getBoundingClientRect(),margin=6,maxLeft=Math.max(margin,innerWidth-rect.width-margin),maxTop=Math.max(margin,innerHeight-rect.height-margin);panel.style.right='auto';panel.style.bottom='auto';panel.style.left=clamp(left,margin,maxLeft)+'px';panel.style.top=clamp(top,margin,maxTop)+'px';}\n",
    "function bindSpatialTileWindow(){const open=el('seuratTileWindowOpen'),help=el('seuratTileHelp'),close=el('spatialTileClose'),preview=el('spatialTilePreview'),clear=el('spatialTileClear'),save=el('spatialTileSave'),panel=spatialTilePanel(),head=panel&&panel.querySelector('.spatialTileHead');if(open)open.onclick=openSpatialTileWindow;if(help)help.onclick=e=>showSpatialTileHelp(e.currentTarget);if(close)close.onclick=closeSpatialTileWindow;if(preview)preview.onclick=()=>previewSpatialTiles(true);if(clear)clear.onclick=()=>clearSpatialTilePreview(true,true);if(save)save.onclick=saveSpatialTiles;['spatialTileSize','spatialTileUnits','spatialTileFormat'].forEach(id=>{const input=el(id);if(input)input.onchange=()=>{if(spatialTilePreviewRows.length)previewSpatialTiles(true);else updateSpatialTileControls();};});if(panel&&head&&head.dataset.spatialTileMoveBound!=='1'){head.dataset.spatialTileMoveBound='1';head.title='Drag to move; resize from the bottom-right corner.';head.addEventListener('mousedown',evt=>{if(evt.button!==0||evt.target.closest('button,input,select,textarea'))return;const rect=panel.getBoundingClientRect();panel.style.width=rect.width+'px';panel.style.left=rect.left+'px';panel.style.top=rect.top+'px';panel.style.right='auto';panel.style.bottom='auto';spatialTileDrag={dx:evt.clientX-rect.left,dy:evt.clientY-rect.top};panel.classList.add('moving');evt.preventDefault();});window.addEventListener('mousemove',evt=>{if(!spatialTileDrag)return;moveSpatialTilePanel(evt.clientX-spatialTileDrag.dx,evt.clientY-spatialTileDrag.dy);});window.addEventListener('mouseup',()=>{if(!spatialTileDrag)return;spatialTileDrag=null;panel.classList.remove('moving');});}updateSpatialTileControls();}\n",
    "function seuratExpressionConfig(){return seuratConfig().gene_expression||{enabled:false,genes:[],ranges:{}};}\n",
    "function seuratFeatureType(payload=null){const fromPayload=payload&&String(payload.feature_type||payload.feature||'').toLowerCase();const fromConfig=String(seuratConfig().feature_type||'').toLowerCase();return (fromPayload==='cell'||fromConfig==='cell')?'cell':'spot';}\n",
    "function seuratFeaturePlural(payload=null){return seuratFeatureType(payload)==='cell'?'cells':'spots';}\n",
    "function seuratExpressionGenes(){const genes=seuratExpressionConfig().genes||[];return Array.isArray(genes)?genes.map(g=>String(g||'')).filter(Boolean):[];}\n",
    "function seuratFindExpressionGene(name){const query=String(name||'').trim();if(!query)return '';const genes=seuratExpressionGenes();return genes.find(g=>g===query)||genes.find(g=>g.toLowerCase()===query.toLowerCase())||'';}\n",
    "function seuratExpressionRange(gene){const ranges=seuratExpressionConfig().ranges||{},range=ranges[gene]||{};return {min:Number(range.min),max:Number(range.max)};}\n",
    "function seuratGeneValue(item,gene){if(!item||!gene)return NaN;const values=item.gene_values||{};if(Object.prototype.hasOwnProperty.call(values,gene))return Number(values[gene]);const key=Object.keys(values).find(k=>k.toLowerCase()===gene.toLowerCase());return key?Number(values[key]):NaN;}\n",
    "function seuratGeneHasEmbeddedValues(gene){gene=String(gene||'').trim();if(!gene)return false;const layer=findSeuratLayer();if(layer&&Array.isArray(layer.items)&&layer.items.some(item=>Number.isFinite(seuratGeneValue(item,gene))))return true;if(seuratPlotPoints().some(point=>Number.isFinite(seuratGeneValue(point,gene))))return true;const geneLayer=seuratGeneLayer(seuratFeatureType());return !!(geneLayer&&Array.isArray(geneLayer.items)&&geneLayer.items.some(item=>String(item.gene||'').toLowerCase()===gene.toLowerCase()&&Number.isFinite(Number(item.gene_value))));}\n",
    "function seuratRgbHex(r,g,b){const h=v=>Math.max(0,Math.min(255,Math.round(v))).toString(16).padStart(2,'0');return '#'+h(r)+h(g)+h(b);}\n",
    "function seuratExpressionColour(value,gene){const v=Number(value);if(!Number.isFinite(v))return '#d1d5db';const range=seuratExpressionRange(gene);let min=range.min,max=range.max;if(!Number.isFinite(min)||!Number.isFinite(max)){const vals=[];seuratPlotPoints().forEach(p=>{const x=seuratGeneValue(p,gene);if(Number.isFinite(x))vals.push(x);});if(vals.length){min=Math.min(...vals);max=Math.max(...vals);}else{min=0;max=1;}}let t=(Math.abs(max-min)>1e-12)?((v-min)/(max-min)):(v>0?1:0);t=clamp(t,0,1);let r,g,b;if(t<.5){const u=t/.5;r=219+(254-219)*u;g=234+(243-234)*u;b=254+(199-254)*u;}else{const u=(t-.5)/.5;r=254+(220-254)*u;g=243+(38-243)*u;b=199+(38-199)*u;}return seuratRgbHex(r,g,b);}\n",
    "function seuratClusterValue(item,field){if(!item||!field)return '';const values=item.cluster_values||{};if(Object.prototype.hasOwnProperty.call(values,field))return String(values[field]??'');const key=Object.keys(values).find(k=>k.toLowerCase()===field.toLowerCase());return key?String(values[key]??''):'';}\n",
    "function seuratClusterPalette(field){const info=seuratFindClusterField(field)||{},out={};(info.levels||[]).forEach(row=>{if(row&&String(row.value||''))out[String(row.value)]=normaliseHexColour(row.colour||row.color||'','#2B6CB0');});const palette=['#2563eb','#dc2626','#16a34a','#9333ea','#ea580c','#0891b2','#be123c','#4f46e5','#65a30d','#c026d3','#ca8a04','#0f766e','#7c3aed','#db2777','#0284c7','#84cc16'];seuratPlotPoints().forEach(point=>{const value=seuratClusterValue(point,field);if(value&&!out[value])out[value]=palette[Object.keys(out).length%palette.length];});const layer=findSeuratLayer();if(layer&&Array.isArray(layer.items))layer.items.forEach(item=>{const value=seuratClusterValue(item,field);if(value&&!out[value])out[value]=palette[Object.keys(out).length%palette.length];});return out;}\n",
    "function seuratSetClusterItemColour(item,field,palette=null){if(!item)return;seuratRememberBaseColour(item);palette=palette||seuratClusterPalette(field);const value=seuratClusterValue(item,field),colour=normaliseHexColour(value&&palette[value]||'','#d1d5db');item.colour=colour;item.color=colour;item.cluster_field=field;item.cluster_value=value||null;item.fill=hexToRgba(colour,.42);}\n",
    "function applySeuratClusterColour(field=null,sync=true){const selected=String(field||((el('seuratClusterSelect')||{}).value)||seuratActiveCluster||seuratClusterDefaultField()||'').trim(),info=seuratFindClusterField(selected);if(!info){notify('No clustering field is available in this object','warning',3200);return false;}const actual=String(info.field||selected),palette=seuratClusterPalette(actual),layer=findSeuratLayer();if(layer&&Array.isArray(layer.items))layer.items.forEach(item=>seuratSetClusterItemColour(item,actual,palette));seuratPlotPoints().forEach(point=>seuratSetClusterItemColour(point,actual,palette));seuratActiveCluster=actual;seuratActiveGene='';const input=el('seuratGeneInput');if(input)input.value='';const sel=el('seuratClusterSelect');if(sel)sel.value=actual;updateSeuratControls();drawSeuratPlot();draw();if(sync)scheduleViewerStateSync('seurat_cluster_coloured',{detail:{field:actual}});notify(seuratSourceName()+' spots coloured by '+(info.label||actual),'success',2200);return true;}\n",
    "function clearSeuratClusterColour(sync=true){const layer=findSeuratLayer();if(layer&&Array.isArray(layer.items))layer.items.forEach(seuratRestoreItemColour);seuratPlotPoints().forEach(seuratRestoreItemColour);seuratActiveCluster='';updateSeuratControls();drawSeuratPlot();draw();if(sync)scheduleViewerStateSync('seurat_cluster_coloured',{detail:{field:null}});notify(seuratSourceName()+' cluster colours reset','info',1600);}\n",
    "function seuratRememberBaseColour(item){if(!item)return;if(!item.seurat_base_colour)item.seurat_base_colour=normaliseHexColour(item.base_colour||item.base_color||item.colour||item.color||'','#2B6CB0');}\n",
    "function seuratSetGeneItemColour(item,gene){if(!item)return;seuratRememberBaseColour(item);const value=seuratGeneValue(item,gene),colour=seuratExpressionColour(value,gene);item.colour=colour;item.color=colour;item.gene=gene;item.gene_value=Number.isFinite(value)?value:null;item.fill=hexToRgba(colour,.42);}\n",
    "function seuratRestoreItemColour(item){if(!item)return;const colour=normaliseHexColour(item.seurat_base_colour||item.base_colour||item.base_color||item.colour||item.color||'','#2B6CB0');item.colour=colour;item.color=colour;item.fill=hexToRgba(colour,.35);delete item.gene;delete item.gene_value;delete item.cluster_field;delete item.cluster_value;}\n",
    "function seuratGeneKeys(item){return [item&&item.id,item&&item.label,item&&item.barcode,item&&item.spot_id].map(v=>String(v??'').trim().toLowerCase()).filter(Boolean);}\n",
    "function seuratGeneLayer(featureType=seuratFeatureType()){const id=featureType==='cell'?'seurat_cell_gene_expression':'seurat_gene_expression';return (layers||[]).find(layer=>String(layer.id||'')===id)||null;}\n",
    "function seuratGenePayloadLayer(payload,gene){const pts=Array.isArray(payload&&payload.points)?payload.points:[],items=[],featureType=seuratFeatureType(payload),featurePlural=seuratFeaturePlural(payload),layerId=featureType==='cell'?'seurat_cell_gene_expression':'seurat_gene_expression',projectIndex=(typeof activeProjectIndex==='number'?activeProjectIndex:null),sectionIndex=(typeof activeProjectSectionIndex==='number'?activeProjectSectionIndex:null),projectLabel=(typeof projectItems!=='undefined'&&Array.isArray(projectItems)&&projectItems[activeProjectIndex]?String(projectItems[activeProjectIndex].label||projectItems[activeProjectIndex].id||projectItems[activeProjectIndex].path||''):'');let positiveCount=0;pts.forEach(point=>{const x=Number(point.slide_x??point.x),y=Number(point.slide_y??point.y),value=Number(point.value);if(!Number.isFinite(x)||!Number.isFinite(y)||!Number.isFinite(value))return;if(value>0)positiveCount++;const colour=normaliseHexColour(point.colour||point.color||'','#d1d5db'),radius=Number(point.radius),fallbackRadius=Math.max(2,Math.min(featureType==='cell'?4:6,Number(seuratConfig().spot_radius||6))),drawRadius=Number.isFinite(radius)&&radius>0?(featureType==='cell'?Math.max(2,Math.min(radius,5)):Math.min(radius,6)):fallbackRadius;items.push({id:'seurat_gene_'+String(point.id||point.barcode||point.label||items.length),name:String(point.label||point.barcode||point.id||''),label:String(point.label||point.barcode||point.id||''),barcode:String(point.barcode||point.id||''),feature_type:featureType,type:'point',x:x,y:y,radius:drawRadius,colour:colour,color:colour,fill:hexToRgba(colour,featureType==='cell'?.72:.62),gene:gene,gene_value:value,gene_values:{[gene]:value},source:layerId,project_image_index:projectIndex,project_section_index:sectionIndex,project_image:projectLabel});});if(!items.length)return false;if(typeof removeViewerLayer==='function'){removeViewerLayer(featureType==='cell'?'seurat_gene_expression':'seurat_cell_gene_expression');}if(typeof upsertViewerLayer==='function'){upsertViewerLayer({id:layerId,name:seuratSourceName()+' '+featurePlural+' '+gene+' expression',type:'vector',source_type:layerId,visible:true,opacity:.9,colour:'#dc2626',replace:true,count:items.length,items:items,metadata:{gene:gene,feature_type:featureType,feature_label:featurePlural,range:payload.range||null,total_count:pts.length,represented_count:items.length,positive_count:positiveCount}});return true;}return false;}\n",
    "function seuratMergeGenePayload(payload){if(!payload||payload.ok===false)throw new Error(payload&&payload.error||'No spatial gene payload returned');const gene=String(payload.gene||payload.requested_gene||'').trim();if(!gene)throw new Error('The spatial gene response did not include a gene name.');const featureType=String(payload.feature_type||payload.feature||'').toLowerCase();if(featureType==='cell'||featureType==='spot')seuratConfig().feature_type=featureType;const cfgExpr=seuratExpressionConfig();cfgExpr.enabled=true;cfgExpr.genes=Array.isArray(cfgExpr.genes)?cfgExpr.genes:[];if(!seuratFindExpressionGene(gene))cfgExpr.genes.push(gene);cfgExpr.ranges=cfgExpr.ranges||{};if(payload.range)cfgExpr.ranges[gene]=payload.range;seuratConfig().gene_expression=cfgExpr;const valueMap=new Map();(payload.points||[]).forEach(point=>{const raw=point.value,value=(raw===null||typeof raw==='undefined')?NaN:Number(raw);[point.id,point.label,point.barcode,point.spot_id].forEach(key=>{key=String(key??'').trim().toLowerCase();if(key)valueMap.set(key,value);});});const attach=item=>{if(!item)return;let value=NaN;for(const key of seuratGeneKeys(item)){if(valueMap.has(key)){value=valueMap.get(key);break;}}item.gene_values=item.gene_values||{};item.gene_values[gene]=Number.isFinite(value)?value:NaN;};const layer=findSeuratLayer();if(layer&&Array.isArray(layer.items))layer.items.forEach(attach);seuratPlotPoints().forEach(attach);seuratGenePayloadLayer(payload,gene);return gene;}\n",
    "async function seuratFetchGene(gene,options={}){const url=seuratDynamicGeneUrl();if(!url)return '';const token=++seuratGeneFetchToken,request={gene:String(gene||'').trim()};if(options&&options.feature_source)request.feature_source=String(options.feature_source);if(options&&options.point_source)request.point_source=String(options.point_source);if(options&&options.reduction_dims)request.reduction_dims=options.reduction_dims;const response=await fetch(url,{method:'POST',headers:{'Accept':'application/json','Content-Type':'application/json'},body:JSON.stringify(request)});let payload=null;try{payload=await response.json();}catch(e){}if(token!==seuratGeneFetchToken)return '';if(!response.ok){throw new Error((payload&&payload.error)||('HTTP '+response.status));}return seuratMergeGenePayload(payload);}\n",
    "function applySeuratEmbeddedGeneColour(actual,sync=true){const layer=findSeuratLayer();if(layer&&Array.isArray(layer.items)){layer.visible=true;layer.items.forEach(item=>seuratSetGeneItemColour(item,actual));}const geneLayer=seuratGeneLayer(seuratFeatureType());if(geneLayer){geneLayer.visible=true;geneLayer.opacity=Math.max(Number(geneLayer.opacity)||0,.85);}seuratPlotPoints().forEach(point=>seuratSetGeneItemColour(point,actual));seuratActiveGene=actual;seuratActiveCluster='';const input=el('seuratGeneInput');if(input)input.value=actual;updateSeuratControls();if(typeof buildLayerList==='function')buildLayerList();drawSeuratPlot();draw();if(sync)scheduleViewerStateSync('seurat_gene_coloured',{detail:{gene:actual,feature_type:seuratFeatureType(),layer_visible:!!(layer||geneLayer)}});notify(seuratSourceName()+' '+seuratFeaturePlural()+' coloured by '+actual,'success',2200);return true;}\n",
    "async function applySeuratGeneColour(gene=null,sync=true,options={}){const requested=String(gene||((el('seuratGeneInput')||{}).value)||seuratActiveGene||'').trim();if(!requested){notify('Type a gene name first','warning',2600);return false;}let actual=seuratFindExpressionGene(requested),hasValues=actual&&seuratGeneHasEmbeddedValues(actual);if(seuratDynamicGeneAvailable()&&(!actual||!hasValues||options.feature_source)){try{notify('Fetching '+requested+' from R...','info',1800);actual=await seuratFetchGene(requested,options);hasValues=actual&&seuratGeneHasEmbeddedValues(actual);}catch(e){notify('Could not fetch '+requested+': '+e.message,'warning',5200);return false;}}if(!actual){notify('Gene not found. In live mode, keep the R session running and reopen this viewer with live = TRUE.','warning',5200);return false;}if(!hasValues){notify('No expression values were available for '+actual+'. In live mode, values are fetched from R when selected.','warning',5200);return false;}return applySeuratEmbeddedGeneColour(actual,sync);}\n",
    "function clearSeuratGeneColour(sync=true){const layer=findSeuratLayer();if(layer&&Array.isArray(layer.items))layer.items.forEach(seuratRestoreItemColour);seuratPlotPoints().forEach(seuratRestoreItemColour);if(typeof removeViewerLayer==='function'){removeViewerLayer('seurat_gene_expression');removeViewerLayer('seurat_cell_gene_expression');}seuratActiveGene='';const input=el('seuratGeneInput');if(input)input.value='';updateSeuratControls();drawSeuratPlot();draw();if(sync)scheduleViewerStateSync('seurat_gene_coloured',{detail:{gene:null,feature_type:seuratFeatureType()}});notify(seuratSourceName()+' '+seuratFeaturePlural()+' colours reset','info',1600);}\n",
    "function seuratPointColour(point){if(!point)return '#2B6CB0';return normaliseHexColour(point.colour||point.color||'','#2B6CB0');}\n",
    "function seuratPointBounds(points){const pts=(points||[]).map(p=>({x:Number(p.x),y:Number(p.y)})).filter(p=>Number.isFinite(p.x)&&Number.isFinite(p.y));return pts.length?boundsFromPoints(pts):null;}\n",
    "function resizeSeuratPlotCanvas(canvas,bounds){const viewport=el('seuratPlotViewport'),maxW=Math.max(320,(viewport&&viewport.clientWidth?viewport.clientWidth-8:720)),maxH=Math.max(180,(viewport&&viewport.clientHeight?viewport.clientHeight-8:Math.min(620,innerHeight-280))),bw=Math.max(1,bounds.xmax-bounds.xmin),bh=Math.max(1,bounds.ymax-bounds.ymin),scale0=Math.min(maxW/bw,maxH/bh);canvas.width=Math.max(320,Math.round(bw*scale0));canvas.height=Math.max(180,Math.round(bh*scale0));seuratPlotTransform={scale:scale0,xmin:bounds.xmin,ymin:bounds.ymin,pad:6,sx:(canvas.width-12)/Math.max(1,canvas.width),sy:(canvas.height-12)/Math.max(1,canvas.height)};return seuratPlotTransform;}\n",
    "function seuratPointCanvasPosition(point,tx=seuratPlotTransform){if(!point||!tx)return null;const x0=(Number(point.x)-tx.xmin)*tx.scale,y0=(Number(point.y)-tx.ymin)*tx.scale;if(!Number.isFinite(x0)||!Number.isFinite(y0))return null;return {x:tx.pad+x0*tx.sx,y:tx.pad+y0*tx.sy};}\n",
    "function drawSeuratSelection(ctx2){const points=(seuratSelectionDrag&&seuratSelectionDrag.points&&seuratSelectionDrag.points.length?seuratSelectionDrag.points:seuratSelectionPolygon)||[];if(!points.length)return;ctx2.save();ctx2.strokeStyle='#facc15';ctx2.fillStyle='rgba(250,204,21,.14)';ctx2.lineWidth=1.5;ctx2.setLineDash([5,4]);ctx2.beginPath();points.forEach((p,i)=>{if(i===0)ctx2.moveTo(p.x,p.y);else ctx2.lineTo(p.x,p.y);});if(points.length>2){ctx2.closePath();ctx2.fill();}ctx2.stroke();ctx2.restore();}\n",
    "function drawSeuratSelectedPointOutlines(points,ctx2){if(!seuratSelectedLabels.size)return;ctx2.save();points.forEach(point=>{const key=seuratPointLabel(point).toLowerCase();if(!key||!seuratSelectedLabels.has(key))return;const q=seuratPointCanvasPosition(point);if(!q)return;ctx2.lineWidth=2.2;ctx2.strokeStyle='rgba(17,24,39,.95)';ctx2.beginPath();ctx2.arc(q.x,q.y,4.5,0,Math.PI*2);ctx2.stroke();ctx2.lineWidth=1.2;ctx2.strokeStyle='#facc15';ctx2.beginPath();ctx2.arc(q.x,q.y,6.5,0,Math.PI*2);ctx2.stroke();});ctx2.restore();}\n",
    "function renderSeuratLegend(points){const legend=el('seuratPlotLegend');if(!legend)return;legend.innerHTML='';if(seuratActiveGene){const low=document.createElement('span');low.className='kodamaLegendItem';const sw1=document.createElement('span');sw1.className='swatch';sw1.style.background='#dbeafe';const tx1=document.createElement('span');tx1.textContent='low '+seuratActiveGene;low.append(sw1,tx1);const high=document.createElement('span');high.className='kodamaLegendItem';const sw2=document.createElement('span');sw2.style.background='#dc2626';const tx2=document.createElement('span');tx2.textContent='high '+seuratActiveGene;high.append(sw2,tx2);legend.append(low,high);return;}if(seuratActiveCluster){const palette=seuratClusterPalette(seuratActiveCluster),entries=Object.entries(palette).slice(0,18);entries.forEach(entry=>{const row=document.createElement('span');row.className='kodamaLegendItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=entry[1];const tx=document.createElement('span');tx.textContent=entry[0];row.append(sw,tx);legend.appendChild(row);});if(Object.keys(palette).length>entries.length){const more=document.createElement('span');more.className='kodamaLegendItem';more.textContent='+'+(Object.keys(palette).length-entries.length)+' more';legend.appendChild(more);}return;}const row=document.createElement('span');row.className='kodamaLegendItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background='#2B6CB0';const tx=document.createElement('span');tx.textContent='reduction colour';row.append(sw,tx);legend.appendChild(row);}\n",
    "function drawSeuratPlot(){const canvas=el('seuratPlotCanvas'),points=seuratPlotPoints();if(!canvas)return;const ctx2=canvas.getContext('2d'),bounds=seuratPointBounds(points);if(!points.length||!bounds){canvas.width=520;canvas.height=280;ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);ctx2.fillStyle='#111827';ctx2.font='14px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx2.fillText('No '+seuratSourceName()+' reduction points are available.',24,36);return;}const tx=resizeSeuratPlotCanvas(canvas,bounds),pad=tx.pad,effectiveScope=seuratEffectivePlotScope();ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);ctx2.save();ctx2.translate(pad,pad);ctx2.scale(tx.sx,tx.sy);const radius=2.4;points.forEach(point=>{const x=(Number(point.x)-tx.xmin)*tx.scale,y=(Number(point.y)-tx.ymin)*tx.scale;if(!Number.isFinite(x)||!Number.isFinite(y))return;const active=seuratPointInActiveTissue(point);ctx2.fillStyle=seuratPointColour(point);ctx2.globalAlpha=(effectiveScope==='all'&&!active)?.34:.82;ctx2.beginPath();ctx2.arc(x,y,radius,0,Math.PI*2);ctx2.fill();});ctx2.restore();ctx2.globalAlpha=1;drawSeuratSelectedPointOutlines(points,ctx2);drawSeuratSelection(ctx2);renderSeuratLegend(points);}\n",
    "function seuratSelectionPayload(){const plot=seuratPlotItem();return {labels:Array.from(seuratSelectedLabels),count:seuratSelectedLabels.size,matched_count:seuratSelectionMatchedCount,reduction:(plot&&plot.reduction)||seuratConfig().reduction||null};}\n",
    "function updateSeuratSelectionStatus(message=null){const box=el('seuratPlotSelectionStatus'),clear=el('seuratPlotClearSelection'),clear2=el('seuratClearSelection');if(clear)clear.disabled=!seuratSelectedLabels.size;if(clear2)clear2.disabled=!seuratSelectedLabels.size;if(!box)return;if(message){box.textContent=message;return;}if(seuratSelectedLabels.size)box.textContent='Selected '+seuratSelectedLabels.size.toLocaleString()+' '+seuratSourceName()+' spot'+(seuratSelectedLabels.size===1?'':'s')+'; highlighted '+seuratSelectionMatchedCount.toLocaleString()+' on the slide.';else box.textContent='Draw a lasso around reduction points to highlight matching spots on the slide. Shift/Ctrl/Command adds; Alt subtracts.';}\n",
    "function removeSeuratSelectionLayer(){if(typeof layerFindIndex!=='function')return;const idx=layerFindIndex('seurat_selected_spots');if(idx>=0){layers.splice(idx,1);buildLayerList();}}\n",
    "function seuratPointInActiveTissue(point){if(seuratEffectivePlotScope()!=='all'||!point||!point.__all_tissues)return true;return Number(point.__project_item_index)===Number(activeProjectIndex)&&Number(point.__project_section_index)===Number(activeProjectSectionIndex);}\n",
    "function updateSeuratSpotHighlights(){const labels=Array.from(seuratSelectedLabels),points=seuratPlotPoints(),wanted=new Set(labels),scope=seuratEffectivePlotScope(),matched=points.filter(p=>(scope==='all'||seuratPointInActiveTissue(p))&&wanted.has(seuratPointLabel(p).toLowerCase())&&Number.isFinite(Number(p.slide_x))&&Number.isFinite(Number(p.slide_y)));seuratSelectionMatchedCount=matched.length;if(!labels.length||!matched.length){removeSeuratSelectionLayer();buildLayerList();draw();return matched.length;}const items=matched.map(p=>({id:'seurat_selected_'+seuratPointLabel(p),name:seuratPointLabel(p),label:seuratPointLabel(p),type:'point',x:Number(p.slide_x),y:Number(p.slide_y),radius:Math.max(12,Number(seuratConfig().spot_radius||8)*1.7),colour:'#facc15',fill:'rgba(250,204,21,.38)',source:'seurat_pca_selection',project_image_index:Number.isFinite(Number(p.__project_item_index))?Number(p.__project_item_index):activeProjectIndex,project_section_index:Number.isFinite(Number(p.__project_section_index))?Number(p.__project_section_index):activeProjectSectionIndex,project_image:String(p.tissue_label||'')}));upsertViewerLayer({id:'seurat_selected_spots',name:seuratSourceName()+' selected spots',type:'vector',source_type:'seurat_selection',visible:true,opacity:.95,colour:'#facc15',replace:true,count:items.length,items:items});return matched.length;}\n",
    "function seuratSelectionProjectWide(){return seuratEffectivePlotScope()==='all'&&seuratSelectedLabels&&seuratSelectedLabels.size>0;}\n",
    "function refreshSeuratSelectionAfterProjectSwitch(){const keep=seuratSelectionProjectWide();seuratSelectionDrag=null;seuratSelectionPolygon=[];if(!keep){if(seuratSelectedLabels&&seuratSelectedLabels.clear)seuratSelectedLabels.clear();seuratSelectionMatchedCount=0;removeSeuratSelectionLayer();return false;}updateSeuratSpotHighlights();return true;}\n",
    "function clearSeuratSelection(sync=true){seuratSelectedLabels.clear();seuratSelectionMatchedCount=0;seuratSelectionDrag=null;seuratSelectionPolygon=[];removeSeuratSelectionLayer();updateSeuratSelectionStatus();drawSeuratPlot();draw();if(sync)scheduleViewerStateSync('seurat_spots_selected',{count:0,matched_count:0,labels:[]});}\n",
    "function applySeuratSelection(labels,mode='replace'){const unique=Array.from(new Set((labels||[]).map(v=>String(v||'').trim().toLowerCase()).filter(Boolean)));if(mode==='replace')seuratSelectedLabels.clear();if(mode==='subtract')unique.forEach(label=>seuratSelectedLabels.delete(label));else unique.forEach(label=>seuratSelectedLabels.add(label));const matched=updateSeuratSpotHighlights();updateSeuratSelectionStatus();drawSeuratPlot();const plot=seuratPlotItem();scheduleViewerStateSync('seurat_spots_selected',{count:seuratSelectedLabels.size,matched_count:matched,labels:Array.from(seuratSelectedLabels),mode:mode,reduction:(plot&&plot.reduction)||seuratConfig().reduction||null});if(seuratSelectedLabels.size&&matched)notify(seuratSourceName()+' selection highlighted '+matched.toLocaleString()+' spot'+(matched===1?'':'s'),'success',2200);}\n",
    "function seuratCanvasPoint(evt){const canvas=el('seuratPlotCanvas'),rect=canvas.getBoundingClientRect();return {x:(evt.clientX-rect.left)*canvas.width/Math.max(1,rect.width),y:(evt.clientY-rect.top)*canvas.height/Math.max(1,rect.height)};}\n",
    "function seuratPointInPolygon(p,poly){let inside=false;for(let i=0,j=poly.length-1;i<poly.length;j=i++){const xi=poly[i].x,yi=poly[i].y,xj=poly[j].x,yj=poly[j].y,hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function seuratNearestPointLabels(a){const points=seuratPlotPoints();let best=null,bestDist=Infinity;points.forEach(point=>{const q=seuratPointCanvasPosition(point);if(!q)return;const d=Math.hypot(q.x-a.x,q.y-a.y);if(d<bestDist){bestDist=d;best=point;}});return best&&bestDist<=12?[seuratPointLabel(best)]:[];}\n",
    "function seuratPointsInPolygon(poly){const points=seuratPlotPoints();if(!points.length||!seuratPlotTransform||!poly.length)return[];if(poly.length<3)return seuratNearestPointLabels(poly[0]);return points.filter(point=>{const q=seuratPointCanvasPosition(point);return q&&seuratPointInPolygon(q,poly);}).map(seuratPointLabel);}\n",
    "function bindSeuratPlotCanvasSelection(){const canvas=el('seuratPlotCanvas');if(!canvas||canvas.dataset.seuratSelectionBound==='1')return;canvas.dataset.seuratSelectionBound='1';canvas.addEventListener('mousedown',evt=>{if(evt.button!==0)return;const points=seuratPlotPoints();if(!points.length){notify('This '+seuratSourceName()+' plot has no selectable points','warning');return;}evt.preventDefault();const p=seuratCanvasPoint(evt);seuratSelectionDrag={points:[p],additive:!!(evt.shiftKey||evt.ctrlKey||evt.metaKey),subtract:!!evt.altKey};seuratSelectionPolygon=[];drawSeuratPlot();});window.addEventListener('mousemove',evt=>{if(!seuratSelectionDrag)return;const p=seuratCanvasPoint(evt),pts=seuratSelectionDrag.points,last=pts[pts.length-1];if(!last||Math.hypot(p.x-last.x,p.y-last.y)>=2){pts.push(p);drawSeuratPlot();}});window.addEventListener('mouseup',()=>{if(!seuratSelectionDrag)return;const drag=seuratSelectionDrag,poly=(drag.points||[]).slice();seuratSelectionDrag=null;seuratSelectionPolygon=poly.length>2?poly:[];const labels=seuratPointsInPolygon(poly),mode=drag.subtract?'subtract':(drag.additive?'add':'replace');if(!labels.length){if(mode==='replace')clearSeuratSelection(true);else{drawSeuratPlot();updateSeuratSelectionStatus('No '+seuratSourceName()+' spots were touched by the lasso.');}return;}applySeuratSelection(labels,mode);});}\n",
    "function setSeuratPlotScope(scope){const next=scope==='all'?'all':'current';if(next!==seuratPlotScope){seuratPlotScope=next;clearSeuratSelection(false);}updateSeuratControls();renderSeuratPlotWindow();scheduleViewerStateSync('seurat_plot_scope_changed',{scope:seuratEffectivePlotScope(),requested_scope:seuratPlotScope});}\n",
    "function renderSeuratPlotWindow(){const plot=seuratPlotItem(),title=el('seuratPlotTitle'),sub=el('seuratPlotSubtitle'),source=seuratSourceName(),points=seuratPlotPoints(),effectiveScope=seuratEffectivePlotScope();if(title){const full=plot?(plot.label||(source+' reduction plot')):(source+' reduction plot');title.textContent=plot?(source+' '+reductionDisplayName(plot.reduction||plot.label||'reduction')+' plot'):middleEllipsis(full,30,13,14);title.title=full;}if(sub){const scope=effectiveScope==='all'?'all tissues':'current tissue';sub.textContent=plot?(points.length.toLocaleString()+' spots | '+scope):'No plot selected';}updateSeuratSelectionStatus();drawSeuratPlot();}\n",
    "function openSeuratPlot(index=null){if(index!==null&&Number.isFinite(Number(index)))seuratActivePlotIndex=Number(index);if(!seuratPlotPoints().length){notify('No '+seuratSourceName()+' reduction points found','warning');updateSeuratControls();return;}const panel=el('seuratPlotWindow');if(panel){panel.classList.add('open');panel.setAttribute('aria-hidden','false');}updateSeuratControls();setTimeout(renderSeuratPlotWindow,0);}\n",
    "function closeSeuratPlot(){const panel=el('seuratPlotWindow');if(panel){panel.classList.remove('open');panel.setAttribute('aria-hidden','true');}}\n",
    "function moveSeuratPlotPanel(left,top){const panel=el('seuratPlotWindow');if(!panel)return;const rect=panel.getBoundingClientRect(),margin=6,maxLeft=Math.max(margin,innerWidth-rect.width-margin),maxTop=Math.max(margin,innerHeight-rect.height-margin);panel.style.right='auto';panel.style.bottom='auto';panel.style.left=clamp(left,margin,maxLeft)+'px';panel.style.top=clamp(top,margin,maxTop)+'px';}\n",
    "function bindSeuratPlotMove(){const panel=el('seuratPlotWindow'),head=panel&&panel.querySelector('.seuratPlotHead');if(!panel||!head||head.dataset.seuratMoveBound==='1')return;head.dataset.seuratMoveBound='1';head.title='Drag to move; resize from the bottom-right corner.';head.addEventListener('mousedown',evt=>{if(evt.button!==0||evt.target.closest('button,input,select,textarea'))return;const rect=panel.getBoundingClientRect();panel.style.width=rect.width+'px';panel.style.height=rect.height+'px';panel.style.left=rect.left+'px';panel.style.top=rect.top+'px';panel.style.right='auto';panel.style.bottom='auto';seuratPlotDrag={dx:evt.clientX-rect.left,dy:evt.clientY-rect.top};panel.classList.add('moving');evt.preventDefault();});window.addEventListener('mousemove',evt=>{if(!seuratPlotDrag)return;moveSeuratPlotPanel(evt.clientX-seuratPlotDrag.dx,evt.clientY-seuratPlotDrag.dy);});window.addEventListener('mouseup',()=>{if(!seuratPlotDrag)return;seuratPlotDrag=null;panel.classList.remove('moving');});}\n",
    "function bindSeuratPlotResize(){const panel=el('seuratPlotWindow');if(!panel||seuratPlotResizeObserver||typeof ResizeObserver==='undefined')return;seuratPlotResizeObserver=new ResizeObserver(()=>{if(panel.classList.contains('open'))requestAnimationFrame(drawSeuratPlot);});seuratPlotResizeObserver.observe(panel);}\n",
    "function bindSeuratControls(){const toggle=el('seuratSpotToggle'),opacity=el('seuratSpotOpacity'),help=el('seuratSpotOpacityHelp'),close=el('seuratPlotClose'),reset=el('seuratPlotReset'),clear=el('seuratClearSelection'),clear2=el('seuratPlotClearSelection'),geneInput=el('seuratGeneInput'),geneApply=el('seuratGeneApply'),geneClear=el('seuratGeneClear'),clusterSelect=el('seuratClusterSelect'),clusterApply=el('seuratClusterApply'),clusterClear=el('seuratClusterClear'),scopeCurrent=el('seuratPlotScopeCurrent'),scopeAll=el('seuratPlotScopeAll');if(toggle)toggle.onclick=e=>{toggleSeuratSpots();if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};if(opacity)opacity.oninput=e=>setSeuratSpotOpacity(e.target.value);if(help)help.onclick=e=>showSeuratSpotOpacityHelp(e.currentTarget);document.querySelectorAll('.seuratPlotOpen').forEach(btn=>{btn.onclick=e=>{openSeuratPlot(Number(btn.dataset.plotIndex||0));if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};});if(geneApply)geneApply.onclick=async e=>{const control=e.currentTarget;if(await applySeuratGeneColour()&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);};if(geneClear)geneClear.onclick=e=>{clearSeuratGeneColour(true);if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};if(geneInput)geneInput.onkeydown=async e=>{if(e.key==='Enter'){e.preventDefault();const control=e.currentTarget;if(await applySeuratGeneColour()&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);}};if(clusterApply)clusterApply.onclick=e=>{if(applySeuratClusterColour()&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};if(clusterClear)clusterClear.onclick=e=>{clearSeuratClusterColour(true);if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};if(clusterSelect)clusterSelect.onchange=e=>{if(seuratActiveCluster){if(applySeuratClusterColour(clusterSelect.value,true)&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);}else updateSeuratControls();};if(scopeCurrent)scopeCurrent.onclick=e=>{setSeuratPlotScope('current');if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};if(scopeAll)scopeAll.onclick=e=>{setSeuratPlotScope('all');if(typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(e.currentTarget);};if(close)close.onclick=closeSeuratPlot;if(reset)reset.onclick=renderSeuratPlotWindow;if(clear)clear.onclick=()=>clearSeuratSelection(true);if(clear2)clear2.onclick=()=>clearSeuratSelection(true);bindSeuratPlotCanvasSelection();bindSeuratPlotMove();bindSeuratPlotResize();bindSpatialTileWindow();if(seuratActiveGene)applySeuratGeneColour(seuratActiveGene,false);else updateSeuratControls();updateSeuratSelectionStatus();window.addEventListener('resize',()=>{if(el('seuratPlotWindow')&&el('seuratPlotWindow').classList.contains('open'))drawSeuratPlot();});}\n"
  )
}

wsi_viewer_prediction_js <- function() {
  paste0(
    "let predictionDrag=null;\n",
    "function predictionConfig(){return cfg.prediction||{enabled:false,sources:[]};}\n",
    "function predictionEnabled(){return !!predictionConfig().enabled;}\n",
    "function predictionUrl(){return String(cfg.prediction_url||'');}\n",
    "function predictionSources(){const sources=predictionConfig().sources||[];return Array.isArray(sources)?sources:[];}\n",
    "function predictionStatus(msg){const box=el('predictionSummary'),menu=el('predictionMenuSummary');if(box)box.textContent=msg||'';if(menu&&msg)menu.textContent=msg;}\n",
    "function predictionFeatureSelects(){return [el('predictionFeatureSource'),el('predictionMenuFeatureSource')].filter(Boolean);}\n",
    "function predictionSourceId(){const sel=el('predictionFeatureSource')||el('predictionMenuFeatureSource'),sources=predictionSources();return String((sel&&sel.value)||((sources[0]||{}).id)||'');}\n",
    "function predictionSelectedSource(){const id=predictionSourceId();return predictionSources().find(src=>String(src.id||'')===id)||predictionSources()[0]||{};}\n",
    "function predictionSourceIsReduction(){const src=predictionSelectedSource();return String(src.type||'')==='reduction'||predictionSourceId().startsWith('spatial:reduction:');}\n",
    "function predictionSourceDimensionCount(){const src=predictionSelectedSource(),n=Number(src.dimension_count||src.dimensions||src.n_dimensions||0);return Number.isFinite(n)&&n>0?Math.round(n):2;}\n",
    "function syncPredictionReductionDimsControl(){const wrap=el('predictionReductionDimsControl'),input=el('predictionReductionDims'),isReduction=predictionSourceIsReduction(),maxDims=predictionSourceDimensionCount();if(wrap)wrap.hidden=!isReduction;if(!input)return;if(isReduction){input.disabled=false;input.max=String(maxDims);let value=Math.round(Number(input.value||0));if(!Number.isFinite(value)||value<1)value=Math.min(10,maxDims);if(value>maxDims)value=maxDims;input.value=String(value);input.title='Use the first '+value+' of '+maxDims+' available reduction dimensions.';}else{input.disabled=true;}}\n",
    "function syncPredictionRefineSvmControl(){const input=el('predictionRefineSvm'),wrap=el('predictionRefineSvmControl'),cfgp=predictionConfig(),ok=!!cfgp.svm_refinement_installed;if(!input)return;input.disabled=!ok;if(!ok)input.checked=false;if(wrap)wrap.title=ok?'Optionally refine PLS-LDA labels in R using SVM over the selected feature matrix.':'SVM refinement needs the optional e1071 package. Run install.packages(\"e1071\") in R.';}\n",
    "function setPredictionSource(value){predictionFeatureSelects().forEach(sel=>{if(sel)sel.value=value;});updatePredictionControls();}\n",
    "function fillPredictionSources(){const sources=predictionSources();predictionFeatureSelects().forEach(sel=>{const old=sel.value;sel.innerHTML='';sources.forEach(src=>{const opt=document.createElement('option');opt.value=String(src.id||'');opt.textContent=String(src.label||src.id||'Feature source');sel.appendChild(opt);});if(old&&sources.some(src=>String(src.id)===old))sel.value=old;});}\n",
    "function predictionEntryProjectLabel(entry){const item=entry&&entry.item||{},section=entry&&entry.section||null,image=String(item.label||item.path||item.id||('Image '+((entry&&entry.project_image_index||0)+1))),sec=section?String(section.label||section.id||('Section '+((entry&&entry.project_section_index||0)+1))):'';return sec?image+' / '+sec:image;}\n",
    "function predictionProjectAnnotationEntries(){const hasProject=(typeof projectItems!=='undefined')&&Array.isArray(projectItems)&&projectItems.length&&(typeof projectAnnotationStore!=='undefined')&&projectAnnotationStore&&(typeof projectAnnotationKey==='function');if(!hasProject)return null;if(typeof saveActiveProjectAnnotations==='function')saveActiveProjectAnnotations();const out=[];projectItems.forEach((item,itemIndex)=>{const sections=(typeof projectSections==='function')?projectSections(item):[];if(sections.length){sections.forEach((section,sectionIndex)=>{const key=projectAnnotationKey(itemIndex,sectionIndex),state=projectAnnotationStore.get(key)||{};out.push({key:key,item:item,section:section,project_image_index:itemIndex,project_section_index:sectionIndex,rois:Array.isArray(state.rois)?state.rois:[]});});}else{const key=projectAnnotationKey(itemIndex,-1),state=projectAnnotationStore.get(key)||{};out.push({key:key,item:item,section:null,project_image_index:itemIndex,project_section_index:-1,rois:Array.isArray(state.rois)?state.rois:[]});}});return out;}\n",
    "function predictionSelectableRois(){const projectEntries=predictionProjectAnnotationEntries();let entries=[];if(projectEntries){projectEntries.forEach(scope=>{(scope.rois||[]).forEach((roi,i)=>entries.push({roi:roi,index:i,project_key:scope.key,project_image_index:scope.project_image_index,project_section_index:scope.project_section_index,item:scope.item,section:scope.section,project_label:predictionEntryProjectLabel(scope)}));});}else entries=(rois||[]).map((roi,i)=>({roi:roi,index:i}));return entries.filter(entry=>entry.roi&&isDrawable(entry.roi)&&!(typeof roiIsCellLike==='function'&&roiIsCellLike(entry.roi)));}\n",
    "function predictionEntryIsActiveSelection(entry){if(selectedRoi!==entry.index)return false;if(!entry.project_key)return true;return (typeof projectAnnotationKey==='function')&&entry.project_key===projectAnnotationKey();}\n",
    "function predictionRoiValue(entry){const base=String(entry.roi.id||entry.roi.name||('roi_'+(entry.index+1)));return entry.project_key?String(entry.project_key)+'::'+base:base;}\n",
    "function predictionRoiLabel(entry){const name=(typeof roiLabelText==='function')?roiLabelText(entry.roi,entry.index):(entry.roi.name||entry.roi.id||('ROI '+(entry.index+1))),cls=entry.roi.class?(' | '+entry.roi.class):'',count=entry.roi.point_count?(' | '+entry.roi.point_count+' pts'):'',scope=entry.project_label?entry.project_label+' | ':'';return scope+name+cls+count;}\n",
    "function predictionFeatureForEntry(entry){if(typeof roiFeature!=='function')return null;const raw=roiFeature(entry.roi,entry.index);if(!raw)return null;const feature=(typeof clonePlain==='function')?clonePlain(raw):JSON.parse(JSON.stringify(raw));const scoped=predictionRoiValue(entry),props=feature.properties||{};feature.id=scoped;props.original_roi_id=String(entry.roi.id||entry.roi.name||('roi_'+(entry.index+1)));if(entry.project_key){props.project_key=String(entry.project_key);props.project_image=String((entry.item&&entry.item.label)||(entry.item&&entry.item.id)||(entry.item&&entry.item.path)||'');props.project_section=entry.section?String(entry.section.label||entry.section.id||''):'';props.project_image_index=entry.project_image_index;props.project_section_index=entry.project_section_index;props.wsiToolsProject={key:String(entry.project_key),image:props.project_image,section:props.project_section,image_index:entry.project_image_index,section_index:entry.project_section_index};}feature.properties=props;return feature;}\n",
    "function predictionRoiGeojsonObject(){const features=predictionSelectableRois().map(predictionFeatureForEntry).filter(Boolean);return {type:'FeatureCollection',features:features};}\n",
    "function selectedOptionValues(select){if(!select)return[];return Array.from(select.selectedOptions||[]).map(opt=>String(opt.value||'')).filter(Boolean);}\n",
    "function fillPredictionAnnotations(){const train=el('predictionTrainAnnotations'),test=el('predictionTestAnnotations'),entries=predictionSelectableRois();if(!train||!test)return;const oldTrain=new Set(selectedOptionValues(train)),oldTest=new Set(selectedOptionValues(test));train.innerHTML='';test.innerHTML='';entries.forEach(entry=>{const value=predictionRoiValue(entry),label=predictionRoiLabel(entry),opt=document.createElement('option');opt.value=value;opt.textContent=label;if(oldTrain.has(value)||(oldTrain.size===0&&predictionEntryIsActiveSelection(entry)))opt.selected=true;train.appendChild(opt);const opt2=document.createElement('option');opt2.value=value;opt2.textContent=label;if(oldTest.has(value))opt2.selected=true;test.appendChild(opt2);});const all=document.createElement('option');all.value='__all_unlabelled__';all.textContent='All non-training spots/cells';if(!oldTest.size||oldTest.has(all.value))all.selected=true;test.insertBefore(all,test.firstChild);updatePredictionControls();}\n",
    "function predictionPayload(){const isReduction=predictionSourceIsReduction(),dimsInput=el('predictionReductionDims'),dims=isReduction?Math.max(1,Math.round(Number((dimsInput||{}).value||predictionSourceDimensionCount()))):0,refine=!!(el('predictionRefineSvm')&&el('predictionRefineSvm').checked&&!el('predictionRefineSvm').disabled);return {feature_source:predictionSourceId(),train_annotations:selectedOptionValues(el('predictionTrainAnnotations')),test_annotations:selectedOptionValues(el('predictionTestAnnotations')),ncomp:Math.max(1,Math.round(Number((el('predictionNcomp')||{}).value||2))),method:String((el('predictionMethod')||{}).value||'simpls'),scaling:String((el('predictionScaling')||{}).value||'autoscaling'),max_features:Math.max(0,Math.round(Number((el('predictionMaxFeatures')||{}).value||5000))),reduction_dims:dims,refine_svm:refine,rois:predictionRoiGeojsonObject()};}\n",
    "function updatePredictionControls(){const has=predictionEnabled()&&predictionSources().length>0,live=!!predictionUrl(),entries=predictionSelectableRois(),run=el('predictionRun'),open=el('predictionWindowOpen'),clear=el('predictionClear'),clear2=el('predictionLayerClear'),subtitle=el('predictionSubtitle'),refine=el('predictionRefineSvm');syncPredictionReductionDimsControl();syncPredictionRefineSvmControl();if(open)open.disabled=!has;if(run)run.disabled=!has||!entries.length;if(clear)clear.disabled=!(typeof layerFindIndex==='function'&&layerFindIndex('wsi_prediction_pls_lda')>=0);if(clear2)clear2.disabled=clear?clear.disabled:false;if(subtitle)subtitle.textContent=has?(entries.length.toLocaleString()+' annotation'+(entries.length===1?'':'s')+' available | '+(live?'live R ready':'live R needed')+(predictionSourceIsReduction()?(' | '+(el('predictionReductionDims')?el('predictionReductionDims').value:predictionSourceDimensionCount())+' reduction dims'):'')+(refine&&refine.checked?' | SVM refinement':'') ):'No spatial/CellPhenotyper prediction source.';if(!has)predictionStatus('Prediction is available only for Seurat, Giotto, SpatialExperiment, or CellPhenotyper viewers.');else if(!live)predictionStatus('Prediction needs a live R session. Reopen with live = TRUE or wsi_viewer_live().');else if(refine&&refine.checked)predictionStatus('SVM refinement is enabled; PLS-LDA labels will be refined in R after prediction.');else predictionStatus(predictionSourceIsReduction()?'Select annotations and choose how many reduction dimensions to use.':'Select training annotations; test can be selected annotations or all non-training spots/cells.');}\n",
    "function openPredictionWindow(){if(!predictionEnabled()){notify('Prediction is available only for spatial or CellPhenotyper projects','warning');return;}fillPredictionSources();fillPredictionAnnotations();const panel=el('predictionWindow');if(panel){panel.classList.add('open');panel.setAttribute('aria-hidden','false');}updatePredictionControls();}\n",
    "function closePredictionWindow(){const panel=el('predictionWindow');if(panel){panel.classList.remove('open');panel.setAttribute('aria-hidden','true');}}\n",
    "function movePredictionPanel(left,top){const panel=el('predictionWindow');if(!panel)return;const rect=panel.getBoundingClientRect(),margin=6,maxLeft=Math.max(margin,innerWidth-rect.width-margin),maxTop=Math.max(margin,innerHeight-rect.height-margin);panel.style.right='auto';panel.style.bottom='auto';panel.style.left=clamp(left,margin,maxLeft)+'px';panel.style.top=clamp(top,margin,maxTop)+'px';}\n",
    "function clearPredictionLayer(sync=true){let removed=false;if(typeof removeViewerLayer==='function')removed=removeViewerLayer('wsi_prediction_pls_lda');if(sync)scheduleViewerStateSync('prediction_cleared',{layer:'wsi_prediction_pls_lda'});updatePredictionControls();if(removed)notify('Prediction layer removed','success');else notify('No prediction layer to remove','info');}\n",
    "async function runPrediction(){const url=predictionUrl();if(!url){notify('Prediction requires live R. Reopen with live = TRUE, and install fastPLS if needed.','warning',6200);return;}const payload=predictionPayload();if(!payload.train_annotations.length){notify('Select at least one training annotation','warning');return;}predictionStatus('Running PLS-LDA in R...');notify('Running PLS-LDA prediction in R','info',1800);try{const response=await fetch(url,{method:'POST',headers:{'Accept':'application/json','Content-Type':'application/json'},body:JSON.stringify(payload)});let body=null;try{body=await response.json();}catch(e){}if(!response.ok||body&&body.ok===false)throw new Error((body&&body.error)||('HTTP '+response.status));handleViewerCommands(body);const info=body&&body.prediction||{},count=Number(info.count||0),classes=Array.isArray(info.classes)?info.classes:[],unit=String(info.point_unit||'point'),unitLabel=unit==='spot'?'spot':(unit==='cell'?'cell':'point');predictionStatus('Predicted '+count.toLocaleString()+' '+unitLabel+' label'+(count===1?'':'s')+(classes.length?(' across '+classes.length+' class'+(classes.length===1?'':'es')):'' )+'. Results are synced to R with viewer$get_prediction().');notify('PLS-LDA finished: '+count.toLocaleString()+' prediction'+(count===1?'':'s'),'success',3600);updatePredictionControls();}catch(e){predictionStatus('Prediction failed: '+e.message);notify('Prediction failed: '+e.message,'error',6200);}}\n",
    "function bindPredictionWindowMove(){const panel=el('predictionWindow'),head=panel&&panel.querySelector('.predictionHead');if(!panel||!head||head.dataset.predictionMoveBound==='1')return;head.dataset.predictionMoveBound='1';head.title='Drag to move; resize from the bottom-right corner.';head.addEventListener('mousedown',evt=>{if(evt.button!==0||evt.target.closest('button,input,select,textarea'))return;const rect=panel.getBoundingClientRect();panel.style.width=rect.width+'px';panel.style.left=rect.left+'px';panel.style.top=rect.top+'px';panel.style.right='auto';panel.style.bottom='auto';predictionDrag={dx:evt.clientX-rect.left,dy:evt.clientY-rect.top};panel.classList.add('moving');evt.preventDefault();});window.addEventListener('mousemove',evt=>{if(!predictionDrag)return;movePredictionPanel(evt.clientX-predictionDrag.dx,evt.clientY-predictionDrag.dy);});window.addEventListener('mouseup',()=>{if(!predictionDrag)return;predictionDrag=null;panel.classList.remove('moving');});}\n",
    "function bindPredictionControls(){const open=el('predictionWindowOpen'),close=el('predictionClose'),refresh=el('predictionRefreshAnnotations'),run=el('predictionRun'),clear=el('predictionClear'),clear2=el('predictionLayerClear');if(open)open.onclick=e=>{openPredictionWindow();closeContainingToolMenu(e.currentTarget);};if(close)close.onclick=closePredictionWindow;if(refresh)refresh.onclick=fillPredictionAnnotations;if(run)run.onclick=runPrediction;if(clear)clear.onclick=()=>clearPredictionLayer(true);if(clear2)clear2.onclick=()=>clearPredictionLayer(true);predictionFeatureSelects().forEach(sel=>{sel.onchange=e=>setPredictionSource(e.target.value);});['predictionNcomp','predictionReductionDims','predictionRefineSvm','predictionMethod','predictionScaling','predictionMaxFeatures'].forEach(id=>{const input=el(id);if(input)input.onchange=updatePredictionControls;});fillPredictionSources();bindPredictionWindowMove();updatePredictionControls();}\n"
  )
}

wsi_viewer_kodama_js <- function() {
  paste0(
    "function kodamaConfig(){const cp=(typeof cellphenotyperConfig==='function')?cellphenotyperConfig():(cfg.cellphenotyper||{});return cp.kodama||{enabled:false,geojsons:[]};}\n",
    "function kodamaItems(){const items=kodamaConfig().geojsons||[];return Array.isArray(items)?items:[];}\n",
    "function kodamaPlots(){const plots=kodamaConfig().plots||[];return Array.isArray(plots)?plots:[];}\n",
    "function kodamaSource(item){return 'KODAMA: '+String((item&&(item.label||item.id))||'MedSAM refined GeoJSON');}\n",
    "function kodamaStatus(msg){const box=el('kodamaSummary');if(box)box.textContent=msg||'';}\n",
    "let kodamaPlotIndex=-1,kodamaPlotTransform=null,kodamaSelectionDrag=null,kodamaSelectionPolygon=[],kodamaSelectedLabels=new Set(),kodamaSelectionMatchedCount=0;\n",
    "function kodamaPlotItem(){return kodamaPlots()[kodamaPlotIndex]||null;}\n",
    "function kodamaGeojsonForPlot(plot){const items=kodamaItems();if(!items.length)return null;if(plot&&plot.profile){const hit=items.find(item=>String(item.profile||'').toLowerCase()===String(plot.profile||'').toLowerCase());if(hit)return hit;}return items[0];}\n",
    "function kodamaPlotPoints(plot){const pts=plot&&plot.points||[];return Array.isArray(pts)?pts:[];}\n",
    "function kodamaPointClass(point){if(!point)return 'annotation';if(point.class)return String(point.class);if(point.cluster!==undefined&&point.cluster!==null)return 'color_'+String(point.cluster);return 'annotation';}\n",
    "function kodamaPointColour(point){if(!point)return classColour('annotation');const explicit=normaliseHexColour(point.colour||point.color||'','');return explicit||classColour(kodamaPointClass(point));}\n",
    "function kodamaPointLabel(point){if(!point)return '';return String(point.label??point.id??point.cell_id??point.cell??'');}\n",
    "function kodamaCanonicalLabels(value){const raw=String(value??'').trim();if(!raw)return[];const lower=raw.toLowerCase(),out=[lower],stripped=lower.replace(/^cell[_: -]+/,'');if(stripped&&stripped!==lower)out.push(stripped);const numeric=stripped.replace(/\\.0+$/,'');if(/^[0-9]+$/.test(numeric))out.push(String(Number(numeric)));return Array.from(new Set(out.filter(Boolean)));}\n",
    "function kodamaNormaliseCellLabel(value){return kodamaCanonicalLabels(value)[0]||'';}\n",
    "function kodamaCellItemLabels(item){const props=item&&item.properties||{};const values=[item&&item.kodama_label,item&&item.source_label,item&&item.original_label,item&&item.cell_id,item&&item.id,item&&item.name,item&&item.label,props.label,props.id,props.cell_id];let out=[];values.forEach(v=>{out=out.concat(kodamaCanonicalLabels(v));});return Array.from(new Set(out.filter(Boolean)));}\n",
    "function kodamaPlotFeatures(plot){const item=kodamaGeojsonForPlot(plot);return item&&item.geojson?geojsonFeatures(item.geojson):[];}\n",
    "function kodamaPlotPointBounds(features){const pts=[];(features||[]).forEach(feature=>collectGeojsonPoints((feature.geometry||{}).coordinates||[]).forEach(p=>pts.push(p)));return pts.length?boundsFromPoints(pts):null;}\n",
    "function kodamaPointBounds(points){const pts=(points||[]).map(p=>({x:Number(p.x),y:Number(p.y)})).filter(p=>Number.isFinite(p.x)&&Number.isFinite(p.y));return pts.length?boundsFromPoints(pts):null;}\n",
    "function kodamaPlotClasses(features){const map=new Map();(features||[]).forEach(feature=>{const cls=importedFeatureClass(feature.properties||{}),colour=classColour(cls);if(!map.has(cls))map.set(cls,colour);});return Array.from(map.entries()).map(([label,colour])=>({label:label,colour:colour}));}\n",
    "function kodamaPointClasses(points){const map=new Map();(points||[]).forEach(point=>{const cls=kodamaPointClass(point),colour=kodamaPointColour(point);if(!map.has(cls))map.set(cls,colour);});return Array.from(map.entries()).map(([label,colour])=>({label:label,colour:colour}));}\n",
    "function renderKodamaLegendItems(items){const legend=el('kodamaPlotLegend');if(!legend)return;legend.innerHTML='';(items||[]).forEach(item=>{const row=document.createElement('span');row.className='kodamaLegendItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=item.colour;const tx=document.createElement('span');tx.textContent=item.label;row.append(sw,tx);legend.appendChild(row);});}\n",
    "function renderKodamaPlotLegend(features){renderKodamaLegendItems(kodamaPlotClasses(features));}\n",
    "function renderKodamaPointLegend(points){renderKodamaLegendItems(kodamaPointClasses(points));}\n",
    "function resizeKodamaPlotCanvas(canvas,bounds){const viewport=el('kodamaPlotViewport'),maxW=Math.max(320,(viewport&&viewport.clientWidth?viewport.clientWidth-8:720)),maxH=Math.max(180,(viewport&&viewport.clientHeight?viewport.clientHeight-8:Math.min(620,innerHeight-280))),bw=Math.max(1,bounds.xmax-bounds.xmin),bh=Math.max(1,bounds.ymax-bounds.ymin),scale=Math.min(maxW/bw,maxH/bh);canvas.width=Math.max(320,Math.round(bw*scale));canvas.height=Math.max(180,Math.round(bh*scale));kodamaPlotTransform={scale:scale,xmin:bounds.xmin,ymin:bounds.ymin,pad:6,sx:(canvas.width-12)/Math.max(1,canvas.width),sy:(canvas.height-12)/Math.max(1,canvas.height)};return kodamaPlotTransform;}\n",
    "function drawKodamaPlotGeometry(ctx,part,tx,colour){const groups=[part.rings].concat(part.add_groups||[]).filter(g=>g&&g.length);if(!groups.length)return;ctx.beginPath();groups.forEach(group=>group.forEach(ring=>{ring.forEach((p,i)=>{const x=(p.x-tx.xmin)*tx.scale,y=(p.y-tx.ymin)*tx.scale;if(i===0)ctx.moveTo(x,y);else ctx.lineTo(x,y);});ctx.closePath();}));ctx.fillStyle=hexToRgba(colour,.72);ctx.strokeStyle=colour;ctx.lineWidth=1.1;ctx.fill('evenodd');ctx.stroke();}\n",
    "function kodamaPointCanvasPosition(point,tx=kodamaPlotTransform){if(!point||!tx)return null;const x0=(Number(point.x)-tx.xmin)*tx.scale,y0=(Number(point.y)-tx.ymin)*tx.scale;if(!Number.isFinite(x0)||!Number.isFinite(y0))return null;return {x:tx.pad+x0*tx.sx,y:tx.pad+y0*tx.sy};}\n",
    "function drawKodamaSelectionBox(ctx2){const points=(kodamaSelectionDrag&&kodamaSelectionDrag.points&&kodamaSelectionDrag.points.length?kodamaSelectionDrag.points:kodamaSelectionPolygon)||[];if(!points.length)return;const subtract=!!(kodamaSelectionDrag&&kodamaSelectionDrag.subtract);ctx2.save();ctx2.strokeStyle=subtract?'#ef4444':'#facc15';ctx2.fillStyle=subtract?'rgba(239,68,68,.12)':'rgba(250,204,21,.14)';ctx2.lineWidth=1.5;ctx2.setLineDash([5,4]);ctx2.beginPath();points.forEach((p,i)=>{if(i===0)ctx2.moveTo(p.x,p.y);else ctx2.lineTo(p.x,p.y);});if(points.length>2){ctx2.closePath();ctx2.fill();}ctx2.stroke();ctx2.setLineDash([]);points.forEach((p,i)=>{if(i%4!==0&&i!==points.length-1)return;ctx2.beginPath();ctx2.arc(p.x,p.y,2.4,0,Math.PI*2);ctx2.fillStyle=subtract?'#ef4444':'#facc15';ctx2.fill();});ctx2.restore();}\n",
    "function drawKodamaSelectedPointOutlines(points,ctx2){if(!kodamaSelectedLabels.size)return;ctx2.save();points.forEach(point=>{const key=kodamaNormaliseCellLabel(kodamaPointLabel(point));if(!key||!kodamaSelectedLabels.has(key))return;const q=kodamaPointCanvasPosition(point);if(!q)return;ctx2.lineWidth=2.2;ctx2.strokeStyle='rgba(17,24,39,.95)';ctx2.beginPath();ctx2.arc(q.x,q.y,4.5,0,Math.PI*2);ctx2.stroke();ctx2.lineWidth=1.2;ctx2.strokeStyle='#facc15';ctx2.beginPath();ctx2.arc(q.x,q.y,6.5,0,Math.PI*2);ctx2.stroke();});ctx2.restore();}\n",
    "function drawKodamaPointPlot(plot,canvas,ctx2){const points=kodamaPlotPoints(plot),bounds=kodamaPointBounds(points);if(!points.length||!bounds)return false;const tx=resizeKodamaPlotCanvas(canvas,bounds),pad=tx.pad;ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);ctx2.save();ctx2.translate(pad,pad);ctx2.scale(tx.sx,tx.sy);const radius=Math.max(.9,Math.min(2.2,120/Math.sqrt(points.length)));points.forEach(point=>{const x=(Number(point.x)-tx.xmin)*tx.scale,y=(Number(point.y)-tx.ymin)*tx.scale;if(!Number.isFinite(x)||!Number.isFinite(y))return;ctx2.fillStyle=kodamaPointColour(point);ctx2.globalAlpha=.78;ctx2.beginPath();ctx2.arc(x,y,radius,0,Math.PI*2);ctx2.fill();});ctx2.restore();ctx2.globalAlpha=1;drawKodamaSelectedPointOutlines(points,ctx2);drawKodamaSelectionBox(ctx2);renderKodamaPointLegend(points);return true;}\n",
    "function kodamaSelectionPayload(){const plot=kodamaPlotItem();return {plot_id:plot&&plot.id||null,plot_label:plot&&plot.label||null,labels:Array.from(kodamaSelectedLabels),count:kodamaSelectedLabels.size,matched_count:kodamaSelectionMatchedCount};}\n",
    "function updateKodamaSelectionStatus(message=null){const box=el('kodamaPlotSelectionStatus'),clear=el('kodamaClearSelection');if(clear)clear.disabled=!kodamaSelectedLabels.size;if(!box)return;if(message){box.textContent=message;return;}if(kodamaSelectedLabels.size)box.textContent='Selected '+kodamaSelectedLabels.size.toLocaleString()+' KODAMA cell'+(kodamaSelectedLabels.size===1?'':'s')+'; highlighted '+kodamaSelectionMatchedCount.toLocaleString()+' on the slide.';else box.textContent='Draw a lasso around KODAMA points to highlight matching cells on the slide. Shift/Ctrl/Command adds; Alt subtracts.';}\n",
    "function removeKodamaSelectionLayer(){if(typeof layerFindIndex!=='function')return;const idx=layerFindIndex('kodama_selected_cells');if(idx>=0){layers.splice(idx,1);buildLayerList();}}\n",
    "function kodamaMatchedCellItems(labels){const layer=(typeof findCellphenotyperLayer==='function')?findCellphenotyperLayer():null,items=layer&&Array.isArray(layer.items)?layer.items:[];if(!labels.length||!items.length)return[];const wanted=new Set();labels.forEach(label=>kodamaCanonicalLabels(label).forEach(v=>wanted.add(v)));return items.filter(item=>kodamaCellItemLabels(item).some(v=>wanted.has(v)));}\n",
    "function updateKodamaCellHighlights(){const labels=Array.from(kodamaSelectedLabels),matched=kodamaMatchedCellItems(labels);kodamaSelectionMatchedCount=matched.length;if(!labels.length||!matched.length){removeKodamaSelectionLayer();buildLayerList();draw();return matched.length;}const items=matched.map(item=>Object.assign({},item,{type:'point',radius:Math.max(10,Number(item.radius||6)*2.4),colour:'#facc15',fill:'rgba(250,204,21,.38)',kodama_selected:true}));upsertViewerLayer({id:'kodama_selected_cells',name:'KODAMA selected cells',type:'vector',source_type:'kodama_selection',visible:true,opacity:.95,colour:'#facc15',replace:true,count:items.length,items:items});return matched.length;}\n",
    "function clearKodamaCellSelection(sync=true){kodamaSelectedLabels.clear();kodamaSelectionMatchedCount=0;kodamaSelectionDrag=null;kodamaSelectionPolygon=[];removeKodamaSelectionLayer();updateKodamaSelectionStatus();drawKodamaAnnotationPlot();draw();if(sync)scheduleViewerStateSync('kodama_cells_selected',{count:0,matched_count:0,labels:[]});}\n",
    "function applyKodamaPointSelection(labels,mode='replace'){const unique=Array.from(new Set((labels||[]).map(kodamaNormaliseCellLabel).filter(Boolean)));if(mode==='replace')kodamaSelectedLabels.clear();if(mode==='subtract')unique.forEach(label=>kodamaSelectedLabels.delete(label));else unique.forEach(label=>kodamaSelectedLabels.add(label));const matched=updateKodamaCellHighlights();updateKodamaSelectionStatus();drawKodamaAnnotationPlot();scheduleViewerStateSync('kodama_cells_selected',{count:kodamaSelectedLabels.size,matched_count:matched,labels:Array.from(kodamaSelectedLabels),mode:mode});if(kodamaSelectedLabels.size&&matched)notify('KODAMA selection highlighted '+matched.toLocaleString()+' cell'+(matched===1?'':'s'),'success',2200);else if(kodamaSelectedLabels.size)notify('KODAMA cells selected, but no matching slide cells were found','warning',3200);else notify('KODAMA selection cleared','info',1600);}\n",
    "function kodamaCanvasPoint(evt){const canvas=el('kodamaPlotCanvas'),rect=canvas.getBoundingClientRect();return {x:(evt.clientX-rect.left)*canvas.width/Math.max(1,rect.width),y:(evt.clientY-rect.top)*canvas.height/Math.max(1,rect.height)};}\n",
    "function kodamaPointInPolygon(p,poly){let inside=false;for(let i=0,j=poly.length-1;i<poly.length;j=i++){const xi=poly[i].x,yi=poly[i].y,xj=poly[j].x,yj=poly[j].y,hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function kodamaNearestPointLabels(a){const points=kodamaPlotPoints(kodamaPlotItem());let best=null,bestDist=Infinity;points.forEach(point=>{const q=kodamaPointCanvasPosition(point);if(!q)return;const d=Math.hypot(q.x-a.x,q.y-a.y);if(d<bestDist){bestDist=d;best=point;}});return best&&bestDist<=12?[kodamaPointLabel(best)]:[];}\n",
    "function kodamaPointsInCanvasPolygon(poly){const points=kodamaPlotPoints(kodamaPlotItem());if(!points.length||!kodamaPlotTransform||!poly.length)return[];if(poly.length<3)return kodamaNearestPointLabels(poly[0]);return points.filter(point=>{const q=kodamaPointCanvasPosition(point);return q&&kodamaPointInPolygon(q,poly);}).map(kodamaPointLabel);}\n",
    "function bindKodamaPlotCanvasSelection(){const canvas=el('kodamaPlotCanvas');if(!canvas||canvas.dataset.kodamaSelectionBound==='1')return;canvas.dataset.kodamaSelectionBound='1';canvas.addEventListener('mousedown',evt=>{if(evt.button!==0)return;const points=kodamaPlotPoints(kodamaPlotItem());if(!points.length){notify('This KODAMA plot has no selectable cell points','warning');return;}evt.preventDefault();const p=kodamaCanvasPoint(evt);kodamaSelectionDrag={points:[p],additive:!!(evt.shiftKey||evt.ctrlKey||evt.metaKey),subtract:!!evt.altKey};kodamaSelectionPolygon=[];drawKodamaAnnotationPlot();});window.addEventListener('mousemove',evt=>{if(!kodamaSelectionDrag)return;const p=kodamaCanvasPoint(evt),pts=kodamaSelectionDrag.points,last=pts[pts.length-1];if(!last||Math.hypot(p.x-last.x,p.y-last.y)>=2){pts.push(p);drawKodamaAnnotationPlot();}});window.addEventListener('mouseup',()=>{if(!kodamaSelectionDrag)return;const drag=kodamaSelectionDrag,poly=(drag.points||[]).slice();kodamaSelectionDrag=null;kodamaSelectionPolygon=poly.length>2?poly:[];const labels=kodamaPointsInCanvasPolygon(poly),mode=drag.subtract?'subtract':(drag.additive?'add':'replace');if(!labels.length){if(mode==='replace')clearKodamaCellSelection(true);else{drawKodamaAnnotationPlot();updateKodamaSelectionStatus('No KODAMA cells were touched by the lasso.');}return;}applyKodamaPointSelection(labels,mode);});}\n",
    "function drawKodamaAnnotationPlot(){const plot=kodamaPlotItem(),features=kodamaPlotFeatures(plot),canvas=el('kodamaPlotCanvas');if(!canvas)return;canvas.style.display='block';const ctx2=canvas.getContext('2d');if(drawKodamaPointPlot(plot,canvas,ctx2))return;const bounds=kodamaPlotPointBounds(features);if(!features.length||!bounds){canvas.width=520;canvas.height=280;ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);ctx2.fillStyle='#111827';ctx2.font='14px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx2.fillText('No KODAMA embedding or GeoJSON is available for this plot.',24,36);renderKodamaPlotLegend([]);return;}const tx=resizeKodamaPlotCanvas(canvas,bounds);ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);features.forEach(feature=>{const cls=importedFeatureClass(feature.properties||{}),colour=classColour(cls),parts=geojsonGeometryParts(feature.geometry||{});parts.forEach(part=>drawKodamaPlotGeometry(ctx2,part,tx,colour));});renderKodamaPlotLegend(features);}\n",
    "function updateKodamaPlotModeButtons(){const ann=el('kodamaPlotAnnotation');if(ann)ann.classList.add('active');const clear=el('kodamaClearSelection');if(clear)clear.disabled=!kodamaSelectedLabels.size;}\n",
    "function renderKodamaPlotWindow(){const plot=kodamaPlotItem(),title=el('kodamaPlotTitle'),sub=el('kodamaPlotSubtitle');if(title)title.textContent=plot?(plot.label||'KODAMA plot'):'KODAMA plot';if(sub){const n=plot&&plot.point_count?(Number(plot.point_count).toLocaleString()+' KODAMA points'):'Annotation-colour KODAMA view';sub.textContent=plot?n:'No plot selected';}updateKodamaPlotModeButtons();updateKodamaSelectionStatus();drawKodamaAnnotationPlot();}\n",
    "function openKodamaPlot(index){const plots=kodamaPlots();if(!plots.length){kodamaStatus('No KODAMA plot PNG was found in this project.');notify('No KODAMA plot found','warning');return;}if(kodamaPlotIndex!==Number(index))clearKodamaCellSelection(false);kodamaPlotIndex=clamp(Number(index)||0,0,plots.length-1);const panel=el('kodamaPlotWindow');if(panel){panel.classList.add('open');panel.setAttribute('aria-hidden','false');}setTimeout(renderKodamaPlotWindow,0);kodamaStatus('Opened '+(plots[kodamaPlotIndex].label||'KODAMA plot')+'.');}\n",
    "function closeKodamaPlot(){const panel=el('kodamaPlotWindow');if(panel){panel.classList.remove('open');panel.setAttribute('aria-hidden','true');}}\n",
    "let kodamaPlotDrag=null;\n",
    "function moveKodamaPlotPanel(left,top){const panel=el('kodamaPlotWindow');if(!panel)return;const rect=panel.getBoundingClientRect(),margin=6,maxLeft=Math.max(margin,innerWidth-rect.width-margin),maxTop=Math.max(margin,innerHeight-rect.height-margin);panel.style.right='auto';panel.style.bottom='auto';panel.style.left=clamp(left,margin,maxLeft)+'px';panel.style.top=clamp(top,margin,maxTop)+'px';}\n",
    "function bindKodamaPlotMove(){const panel=el('kodamaPlotWindow'),head=panel&&panel.querySelector('.kodamaPlotHead');if(!panel||!head||head.dataset.kodamaMoveBound==='1')return;head.dataset.kodamaMoveBound='1';head.title='Drag to move; resize from the bottom-right corner.';head.addEventListener('mousedown',evt=>{if(evt.button!==0||evt.target.closest('button,input,select,textarea'))return;const rect=panel.getBoundingClientRect();panel.style.width=rect.width+'px';panel.style.height=rect.height+'px';panel.style.left=rect.left+'px';panel.style.top=rect.top+'px';panel.style.right='auto';panel.style.bottom='auto';kodamaPlotDrag={dx:evt.clientX-rect.left,dy:evt.clientY-rect.top};panel.classList.add('moving');evt.preventDefault();});window.addEventListener('mousemove',evt=>{if(!kodamaPlotDrag)return;moveKodamaPlotPanel(evt.clientX-kodamaPlotDrag.dx,evt.clientY-kodamaPlotDrag.dy);});window.addEventListener('mouseup',()=>{if(!kodamaPlotDrag)return;kodamaPlotDrag=null;panel.classList.remove('moving');});window.addEventListener('resize',()=>{if(panel.classList.contains('open')){const rect=panel.getBoundingClientRect();moveKodamaPlotPanel(rect.left,rect.top);}});}\n",
    "let kodamaPlotResizeObserver=null;\n",
    "function bindKodamaPlotResize(){const panel=el('kodamaPlotWindow');if(!panel||kodamaPlotResizeObserver||typeof ResizeObserver==='undefined')return;kodamaPlotResizeObserver=new ResizeObserver(()=>{if(panel.classList.contains('open'))requestAnimationFrame(drawKodamaAnnotationPlot);});kodamaPlotResizeObserver.observe(panel);}\n",
    "function isKodamaRoi(roi,id=null){if(!roi)return false;const props=roi.properties||{},source=String(roi.source||'');if(id&&String(props.kodama_id||'')===String(id))return true;return roi.kodama===true||source.indexOf('KODAMA:')===0||props.source_menu==='KODAMA';}\n",
    "function clearKodamaRois(redraw=true){let removed=0;for(let i=rois.length-1;i>=0;i--){if(isKodamaRoi(rois[i])){if(!removed)pushAnnotationUndo('kodama_cleared');rois.splice(i,1);removed++;}}if(selectedRoi>=rois.length)selectedRoi=rois.length-1;if(removed){recordAnnotationHistory('kodama_cleared',{removed:removed});scheduleViewerStateSync('kodama_cleared',{removed:removed});markAnnotationsDirty('kodama_cleared');}if(redraw){buildRoiList();updateButtons();draw();kodamaStatus(removed?('Removed '+removed+' KODAMA ROI'+(removed===1?'':'s')+'.'):'No KODAMA annotations were loaded.');}return removed;}\n",
    "function tagKodamaRois(start,item){const source=kodamaSource(item),colourSeed=(item&&item.profile==='fine')?'#f59e0b':((item&&item.profile==='standard')?'#22c55e':'#38bdf8');let tagged=0;for(let i=start;i<rois.length;i++){const roi=rois[i];if(!roi)continue;roi.kodama=true;roi.source=source;roi.properties=roi.properties||{};roi.properties.source_menu='KODAMA';roi.properties.kodama_id=item&&item.id||null;roi.properties.kodama_profile=item&&item.profile||null;if(!roi.colour){roi.colour=colourSeed;roi.fill=hexToRgba(roi.colour,.18);}tagged++;}return tagged;}\n",
    "function appendKodamaGeojson(item){if(!item||!item.geojson){notify('No KODAMA GeoJSON is available for this entry','warning');return 0;}const before=rois.length;addImportedGeojson(item.geojson,kodamaSource(item));const added=tagKodamaRois(before,item);return added;}\n",
    "function loadKodamaGeojson(index){const item=kodamaItems()[Number(index)];if(!item){kodamaStatus('KODAMA entry not found.');return;}clearKodamaRois(false);const added=appendKodamaGeojson(item);buildRoiList();updateButtons();draw();kodamaStatus('Loaded '+added+' ROI'+(added===1?'':'s')+' from '+(item.label||item.id||'KODAMA')+'.');notify('KODAMA GeoJSON loaded','success');}\n",
    "function loadAllKodamaGeojsons(){const items=kodamaItems();if(!items.length){kodamaStatus('No KODAMA GeoJSON was found in this project.');notify('No KODAMA GeoJSON found','warning');return;}clearKodamaRois(false);let added=0;items.forEach(item=>{added+=appendKodamaGeojson(item);});buildRoiList();updateButtons();draw();kodamaStatus('Loaded '+added+' KODAMA ROI'+(added===1?'':'s')+' from '+items.length+' file'+(items.length===1?'':'s')+'.');notify('KODAMA annotations loaded','success');}\n",
    "function bindKodamaControls(){const items=kodamaItems(),plots=kodamaPlots(),loadAll=el('kodamaLoadAll'),clear=el('kodamaClear'),close=el('kodamaPlotClose'),ann=el('kodamaPlotAnnotation'),clearSelection=el('kodamaClearSelection');document.querySelectorAll('.kodamaLoad').forEach(button=>{button.onclick=()=>loadKodamaGeojson(button.dataset.kodamaIndex);button.disabled=!items.length;});document.querySelectorAll('.kodamaPlot').forEach(button=>{button.onclick=()=>openKodamaPlot(button.dataset.kodamaPlotIndex);button.disabled=!plots.length;});if(loadAll){loadAll.onclick=loadAllKodamaGeojsons;loadAll.disabled=!items.length;}if(clear){clear.onclick=()=>clearKodamaRois(true);clear.disabled=!items.length;}if(close)close.onclick=closeKodamaPlot;if(ann)ann.onclick=renderKodamaPlotWindow;if(clearSelection)clearSelection.onclick=()=>clearKodamaCellSelection(true);bindKodamaPlotCanvasSelection();bindKodamaPlotMove();bindKodamaPlotResize();updateKodamaSelectionStatus();window.addEventListener('resize',()=>{if(el('kodamaPlotWindow')&&el('kodamaPlotWindow').classList.contains('open'))drawKodamaAnnotationPlot();});kodamaStatus((items.length?(items.length+' KODAMA/MedSAM GeoJSON file'+(items.length===1?'':'s')):'No KODAMA/MedSAM GeoJSON')+(plots.length?(' | '+plots.length+' plot'+(plots.length===1?'':'s')+' available.'):'.'));}\n"
  )
}

wsi_viewer_measure_js <- function() {
  paste0(
    "function measurePixelSize(){const m=cfg.mpp||{};const x=Number(m.x),y=Number(m.y);return Number.isFinite(x)&&Number.isFinite(y)&&x>0&&y>0?{x:x,y:y}:null;}\n",
    "function measurementRecord(p1,p2){const dx=p2.x-p1.x,dy=p2.y-p1.y,px=measurePixelSize(),distancePx=Math.hypot(dx,dy),distanceUm=px?Math.hypot(dx*px.x,dy*px.y):NaN;return {id:'measure_'+(measures.length+1),start:{x:p1.x,y:p1.y},end:{x:p2.x,y:p2.y},distance_px:distancePx,distance_um:distanceUm};}\n",
    "function formatMeasure(m){let text=fmt(m.distance_px,1)+' px';if(Number.isFinite(m.distance_um))text+=' | '+fmt(m.distance_um,1)+' um';return text;}\n",
    "function measurementAt(p){if(!p||!Array.isArray(measures))return -1;const tol=Math.max(6,10/Math.max(.0001,slideUnitScale()));for(let i=measures.length-1;i>=0;i--){const m=measures[i];if(m&&m.start&&m.end&&pointLineDistance(p,m.start,m.end)<=tol)return i;}return -1;}\n",
    "function drawMeasureLine(m,preview=false,selected=false){const a=slideToCanvas(m.start),b=slideToCanvas(m.end),mx=(a.x+b.x)/2,my=(a.y+b.y)/2,text=formatMeasure(m);ctx.save();ctx.strokeStyle=selected?'#ffffff':(preview?'#facc15':'#38bdf8');ctx.fillStyle=preview?'#facc15':(selected?'#facc15':'#38bdf8');ctx.lineWidth=selected?4:2;if(preview)ctx.setLineDash([6,4]);ctx.beginPath();ctx.moveTo(a.x,a.y);ctx.lineTo(b.x,b.y);ctx.stroke();ctx.setLineDash([]);[a,b].forEach(p=>{ctx.beginPath();ctx.arc(p.x,p.y,selected?5:4,0,Math.PI*2);ctx.fill();ctx.strokeStyle='#111';ctx.stroke();ctx.strokeStyle=selected?'#ffffff':(preview?'#facc15':'#38bdf8');});const w=ctx.measureText(text).width+8,x=clamp(mx-w/2,4,innerWidth-w-4),y=clamp(my-22,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.76)';ctx.fillRect(x,y,w,18);ctx.fillStyle=selected?'#facc15':(preview?'#facc15':'#e0f2fe');ctx.fillText(text,x+4,y+3);ctx.restore();}\n",
    "function drawMeasurements(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('measurements'))return;measures.forEach((m,i)=>drawMeasureLine(m,false,i===selectedMeasure));if(mode==='measure'&&measureStart&&lastPointer&&pointInsideSlide(lastPointer))drawMeasureLine(measurementRecord(measureStart,lastPointer),true,false);}\n",
    "function updateMeasureList(){const summary=el('measureSummary'),list=el('measureList'),clear=el('clearMeasures');if(summary)summary.textContent=measures.length?(measures.length+' distance measurement'+(measures.length===1?'':'s')+(selectedMeasure>=0&&measures[selectedMeasure]?(' | selected '+(selectedMeasure+1)):'') ):'No measurements yet.';if(list){list.innerHTML='';measures.forEach((m,i)=>{const b=document.createElement('button');b.type='button';b.className='measureItem';b.classList.toggle('active',i===selectedMeasure);b.innerHTML='Distance '+(i+1)+'<br><code>'+formatMeasure(m)+'</code>';b.onclick=()=>{measureStart=null;selectMeasure(i,true);notify('Measurement '+(i+1)+': '+formatMeasure(m),'info',4200);draw();};list.appendChild(b);});}if(clear)clear.disabled=measures.length===0;}\n",
    "function addMeasurePoint(p){if(!pointInsideSlide(p))return;if(!measureStart){measureStart={x:p.x,y:p.y};notify('Measurement start set','info');draw();return;}const rec=measurementRecord(measureStart,p);measures.push(rec);measureStart=null;updateMeasureList();updateButtons();recordAnnotationHistory('measurement_added',{id:rec.id,distance_px:Number.isFinite(rec.distance_px)?rec.distance_px.toFixed(1):null});scheduleViewerStateSync('measurement_added',{id:rec.id});notify('Distance measured','success');draw();}\n",
    "function clearMeasurements(){measures=[];measureStart=null;selectedMeasure=-1;updateMeasureList();updateButtons();scheduleViewerStateSync('measurements_cleared',{});draw();}\n",
    "function measureStatus(){if(mode==='measure'){if(measureStart&&lastPointer&&pointInsideSlide(lastPointer))return ' | measuring '+formatMeasure(measurementRecord(measureStart,lastPointer));return ' | click two points to measure';}return measures.length?(' | measures '+measures.length):'';}\n",
    "function bindMeasureControls(){const tool=el('toolMeasure'),clear=el('clearMeasures');if(tool)tool.onclick=e=>{setMode('measure');closeMenuAfterToolAction(e.currentTarget);};if(clear)clear.onclick=e=>{clearMeasurements();closeMenuAfterToolAction(e.currentTarget);};updateMeasureList();}\n"
  )
}

wsi_viewer_scale_bar_js <- function() {
  paste0(
    "function scaleBarMpp(){const raw=cfg.mpp;if(typeof raw==='number'||typeof raw==='string'){const n=Number(raw);if(Number.isFinite(n)&&n>0)return n;}const m=raw||{};const x=Number(m.x??cfg.mpp_x??cfg.microns_per_pixel),y=Number(m.y??cfg.mpp_y);if(Number.isFinite(x)&&x>0)return x;if(Number.isFinite(y)&&y>0)return y;return NaN;}\n",
    "function scaleBarSlideUnitScale(){try{const w=Number(cfg.slide_width||0),step=Math.max(1,Math.min(1000,w>0?w/8:1000)),a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:step,y:0}),d=Math.hypot(b.x-a.x,b.y-a.y)/step;if(Number.isFinite(d)&&d>0)return d;}catch(e){}return Number.isFinite(scale)&&scale>0?scale:NaN;}\n",
    "function niceScaleLength(value){if(!Number.isFinite(value)||value<=0)return NaN;const exponent=Math.floor(Math.log10(value)),base=Math.pow(10,exponent),fraction=value/base;let nice=1;if(fraction<1.5)nice=1;else if(fraction<3.5)nice=2;else if(fraction<7.5)nice=5;else nice=10;return nice*base;}\n",
    "function formatScaleMicrons(um){if(!Number.isFinite(um)||um<=0)return '';let digits=0;if(um<1)digits=2;else if(um<10)digits=1;return String(Number(um.toFixed(digits)))+' \\u00b5m';}\n",
    "function defaultObjectivePower(){const v=Number(cfg.default_objective_power||40);return Number.isFinite(v)&&v>0?v:40;}\n",
    "function objectivePowerFromMetadata(){const obj=Number(cfg.objective_power);if(Number.isFinite(obj)&&obj>0)return obj;const mpp=scaleBarMpp();return Number.isFinite(mpp)&&mpp>0?10/mpp:NaN;}\n",
    "function objectivePowerEstimated(){return !Number.isFinite(objectivePowerFromMetadata());}\n",
    "function baseObjectivePower(){const meta=objectivePowerFromMetadata();return Number.isFinite(meta)&&meta>0?meta:defaultObjectivePower();}\n",
    "function currentMagnification(){const base=baseObjectivePower(),pane=(typeof activeMultiViewPane==='function'?activeMultiViewPane():null),slideScale=pane&&typeof multiViewCanvasUnitScale==='function'?multiViewCanvasUnitScale(pane):scaleBarSlideUnitScale();return Number.isFinite(base)&&base>0&&Number.isFinite(slideScale)&&slideScale>0?base*slideScale:NaN;}\n",
    "function formatMagnification(value){if(!Number.isFinite(value)||value<=0)return 'NA';const digits=value<10?1:0;return Number(value.toFixed(digits))+'x';}\n",
    "function magnificationStatus(){const mag=currentMagnification();return Number.isFinite(mag)?(' | Mag '+(objectivePowerEstimated()?'~':'')+formatMagnification(mag)):'';}\n",
    "function setMagnificationPower(power){const target=Number(power),base=baseObjectivePower(),pane=(typeof activeMultiViewPane==='function'?activeMultiViewPane():null),slideScale=pane&&typeof multiViewCanvasUnitScale==='function'?multiViewCanvasUnitScale(pane):scaleBarSlideUnitScale();if(!Number.isFinite(target)||target<=0||!Number.isFinite(base)||base<=0||!Number.isFinite(slideScale)||slideScale<=0){notify('Magnification cannot be applied until the viewer has an image scale','warning',3600);return;}const factor=target/base/slideScale;if(pane&&typeof multiViewZoomAt==='function')multiViewZoomAt(factor);else zoomAt(factor,innerWidth/2,innerHeight/2);notify('Magnification '+(objectivePowerEstimated()?'~':'')+formatMagnification(target)+(objectivePowerEstimated()?' (estimated)':''),'success',1600);}\n",
    "function resetInitialMagnification(){let restored=false;if(typeof multiViewLayout!=='undefined'&&multiViewLayout>1&&typeof multiViewFitView==='function')restored=multiViewFitView();if(!restored&&typeof projectItems!=='undefined'&&projectItems.length&&typeof activeProjectSection==='function'&&typeof zoomToProjectContent==='function')restored=zoomToProjectContent(projectItems[activeProjectIndex]||null,activeProjectSection());if(!restored&&typeof fitView==='function'){fitView();restored=true;}if(restored){if(typeof draw==='function')draw();notify('Returned to initial magnification','success',1600);}else notify('Initial magnification is not available yet','warning',2400);}\n",
    "function updateMagnificationControls(){const summary=el('magnificationSummary'),mag=currentMagnification(),base=baseObjectivePower(),estimated=objectivePowerEstimated();document.querySelectorAll('.magnificationPreset').forEach(button=>{const target=Number(button.dataset.magnification);const enabled=Number.isFinite(base)&&base>0;button.disabled=!enabled;button.classList.toggle('active',Number.isFinite(mag)&&Math.abs(mag-target)<=Math.max(0.75,target*0.08));button.title='Zoom to '+(estimated?'estimated ':'approximately ')+formatMagnification(target);});if(summary){if(Number.isFinite(mag)){summary.textContent=(estimated?'Estimated current ':'Current ')+(estimated?'~':'')+formatMagnification(mag)+' | full resolution '+(estimated?'~':'')+formatMagnification(base)+(estimated?' fallback':'');}else{summary.textContent='Magnification presets use a 40x full-resolution fallback until calibrated MPP/objective metadata is available.';}}}\n",
    "function bindMagnificationControls(){document.querySelectorAll('.magnificationPreset').forEach(button=>{button.onclick=()=>setMagnificationPower(button.dataset.magnification);});const initial=el('magnificationInitial');if(initial)initial.onclick=e=>{resetInitialMagnification();closeMenuAfterToolAction(e.currentTarget);};updateMagnificationControls();}\n",
    "function showUnavailableScaleBar(){const bar=el('scaleBar'),line=el('scaleBarLine'),label=el('scaleBarLabel');if(line)line.style.width='120px';if(label)label.textContent='scale unavailable';if(bar)bar.classList.add('unavailable');}\n",
    "function updateScaleBar(){const bar=el('scaleBar'),line=el('scaleBarLine'),label=el('scaleBarLabel');if(!bar||!line||!label){updateMagnificationControls();return;}if(typeof multiViewLayout!=='undefined'&&multiViewLayout>1){bar.style.display='none';updateMagnificationControls();return;}bar.style.display='';const mpp=scaleBarMpp(),slideScale=scaleBarSlideUnitScale();if(!Number.isFinite(mpp)||mpp<=0||!Number.isFinite(slideScale)||slideScale<=0){showUnavailableScaleBar();updateMagnificationControls();return;}const targetPx=clamp(innerWidth*0.16,90,180),targetUm=targetPx*mpp/slideScale,niceUm=niceScaleLength(targetUm),barPx=niceUm/mpp*slideScale;if(!Number.isFinite(barPx)||barPx<24){showUnavailableScaleBar();updateMagnificationControls();return;}line.style.width=Math.round(clamp(barPx,32,Math.max(48,innerWidth*.42)))+'px';label.textContent=formatScaleMicrons(niceUm);bar.classList.remove('unavailable');updateMagnificationControls();}\n"
  )
}

wsi_viewer_screenshot_js <- function() {
  paste0(
    "let screenshotSelecting=false,screenshotRect=null,pendingScreenshotRect=null,pendingScreenshotPane=null,screenshotRenderOptions=null;\n",
    "function screenshotCanvasPoint(evt){const rect=canvas.getBoundingClientRect();return {x:clamp(evt.clientX-rect.left,0,rect.width),y:clamp(evt.clientY-rect.top,0,rect.height)};}\n",
    "function normalizedScreenshotRect(rect=screenshotRect){if(!rect)return null;const w=canvas.getBoundingClientRect().width||innerWidth,h=canvas.getBoundingClientRect().height||innerHeight,x0=clamp(Math.min(rect.x0,rect.x1),0,w),y0=clamp(Math.min(rect.y0,rect.y1),0,h),x1=clamp(Math.max(rect.x0,rect.x1),0,w),y1=clamp(Math.max(rect.y0,rect.y1),0,h);return {x:x0,y:y0,width:x1-x0,height:y1-y0};}\n",
    "function screenshotStatus(){if(mode!=='screenshot')return '';const r=normalizedScreenshotRect();if(screenshotSelecting&&r&&r.width>1&&r.height>1)return ' | screenshot '+Math.round(r.width)+' x '+Math.round(r.height)+' px';return ' | drag screenshot area';}\n",
    "function drawScreenshotSelection(){const r=normalizedScreenshotRect();if(!r||r.width<1||r.height<1)return;ctx.save();ctx.fillStyle='rgba(94,234,212,.12)';ctx.strokeStyle='#5eead4';ctx.lineWidth=2;ctx.setLineDash([8,5]);ctx.fillRect(r.x,r.y,r.width,r.height);ctx.strokeRect(r.x,r.y,r.width,r.height);ctx.setLineDash([]);const text=Math.round(r.width)+' x '+Math.round(r.height)+' px';ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';const tw=ctx.measureText(text).width+10,tx=clamp(r.x,4,Math.max(4,innerWidth-tw-4)),ty=clamp(r.y-22,4,Math.max(4,innerHeight-22));ctx.fillStyle='rgba(0,0,0,.72)';ctx.fillRect(tx,ty,tw,18);ctx.fillStyle='#ccfbf1';ctx.fillText(text,tx+5,ty+4);ctx.restore();}\n",
    "function screenshotIncludeComponent(name){return !screenshotRenderOptions||screenshotRenderOptions[name]!==false;}\n",
    "function screenshotNormaliseFormat(format){const value=String(format||'png').toLowerCase();if(value==='jpg')return 'jpeg';return ['png','jpeg','svg','pdf'].includes(value)?value:'png';}\n",
    "function screenshotFormat(){const input=el('screenshotDialogFormat')||el('screenshotFormat');return screenshotNormaliseFormat((input&&input.value)||'png');}\n",
    "function screenshotFormatExtension(format=screenshotFormat()){const fmt=screenshotNormaliseFormat(format);return fmt==='jpeg'?'jpg':fmt;}\n",
    "function screenshotFormatLabel(format=screenshotFormat()){const fmt=screenshotNormaliseFormat(format);return fmt==='jpeg'?'JPEG':fmt.toUpperCase();}\n",
    "function screenshotDefaultFileName(format='png'){const ext=screenshotFormatExtension(format),stamp=new Date().toISOString().replace(/[-:]/g,'').replace(/\\..*$/,'').replace('T','_');return 'wsiTools_screenshot_'+stamp+'.'+ext;}\n",
    "function screenshotFileName(format=screenshotFormat()){const ext=screenshotFormatExtension(format),input=el('screenshotFileName');let value=String((input&&input.value)||'').trim();value=value.replace(/[\\\\/:*?\"<>|]+/g,'_').replace(/^\\.+/,'').replace(/\\s+/g,'_');if(!value)value=screenshotDefaultFileName(format);if(!value.toLowerCase().endsWith('.'+ext))value=value.replace(/\\.(png|jpe?g|svg|pdf)$/i,'')+'.'+ext;return value;}\n",
    "function screenshotElementVisible(node){if(!node)return false;let cur=node;while(cur&&cur!==document.body){const s=getComputedStyle(cur);if(s.display==='none'||s.visibility==='hidden'||Number(s.opacity)===0)return false;cur=cur.parentElement;}return true;}\n",
    "function screenshotOsdInternalCanvases(){const out=[];try{const drawer=typeof osdViewer!=='undefined'&&osdViewer&&osdViewer.drawer;if(drawer){[drawer.canvas,drawer.context&&drawer.context.canvas,drawer._canvas,drawer._context&&drawer._context.canvas].forEach(c=>{if(c&&!out.includes(c))out.push(c);});}}catch(e){}return out;}\n",
    "function screenshotBaseElements(){const nodes=[];screenshotOsdInternalCanvases().forEach(node=>{if(node&&!nodes.includes(node))nodes.push(node);});if(typeof viewerEl!=='undefined'&&viewerEl&&viewerEl.querySelectorAll){Array.from(viewerEl.querySelectorAll('canvas,img')).forEach(node=>{if(node&&!nodes.includes(node))nodes.push(node);});}return nodes.filter(node=>{if(!node||!screenshotElementVisible(node))return false;const r=node.getBoundingClientRect?node.getBoundingClientRect():(typeof viewerEl!=='undefined'&&viewerEl?viewerEl.getBoundingClientRect():{width:innerWidth,height:innerHeight}),w=node.naturalWidth||node.videoWidth||node.width,h=node.naturalHeight||node.videoHeight||node.height;return w>0&&h>0&&r.width>0&&r.height>0;});}\n",
    "function screenshotSourceSize(source){return {width:source.naturalWidth||source.videoWidth||source.width||1,height:source.naturalHeight||source.videoHeight||source.height||1};}\n",
    "function screenshotSourceRect(source){if(source&&source.getBoundingClientRect){const r=source.getBoundingClientRect();if(r&&r.width>0&&r.height>0)return r;}if(typeof viewerEl!=='undefined'&&viewerEl&&viewerEl.getBoundingClientRect){const r=viewerEl.getBoundingClientRect();if(r&&r.width>0&&r.height>0)return r;}return {left:0,top:0,right:innerWidth,bottom:innerHeight,width:innerWidth,height:innerHeight};}\n",
    "function drawElementIntoScreenshot(targetCtx,source,rect,dpr){const sr=screenshotSourceRect(source),size=screenshotSourceSize(source),ix=Math.max(rect.x,sr.left),iy=Math.max(rect.y,sr.top),ix2=Math.min(rect.x+rect.width,sr.right),iy2=Math.min(rect.y+rect.height,sr.bottom),iw=ix2-ix,ih=iy2-iy;if(iw<=0||ih<=0||sr.width<=0||sr.height<=0||size.width<=0||size.height<=0)return false;const sx=(ix-sr.left)*size.width/sr.width,sy=(iy-sr.top)*size.height/sr.height,sw=iw*size.width/sr.width,sh=ih*size.height/sr.height,dx=(ix-rect.x)*dpr,dy=(iy-rect.y)*dpr;try{targetCtx.drawImage(source,sx,sy,sw,sh,dx,dy,iw*dpr,ih*dpr);return true;}catch(e){return false;}}\n",
    "function canvasIsReadable(c){try{const g=c.getContext('2d');g.getImageData(0,0,1,1);return true;}catch(e){return false;}}\n",
    "function screenshotPreviewImage(){if(typeof navigatorImage!=='undefined'&&navigatorImage&&navigatorImage.complete&&navigatorImage.naturalWidth)return navigatorImage;if(typeof image!=='undefined'&&image&&image.complete&&image.naturalWidth)return image;return null;}\n",
    "function drawPreviewBaseIntoScreenshot(targetCtx,r,dpr){const img=screenshotPreviewImage();if(!img||!img.naturalWidth||!img.naturalHeight||typeof canvasToSlidePoint!=='function')return false;let pts=[];try{pts=[canvasToSlidePoint(r.x,r.y),canvasToSlidePoint(r.x+r.width,r.y),canvasToSlidePoint(r.x+r.width,r.y+r.height),canvasToSlidePoint(r.x,r.y+r.height)].filter(pointInsideSlide);}catch(e){pts=[];}if(!pts.length)return false;const xs=pts.map(p=>p.x),ys=pts.map(p=>p.y),xmin=clamp(Math.min(...xs),0,Number(cfg.slide_width||1)),xmax=clamp(Math.max(...xs),0,Number(cfg.slide_width||1)),ymin=clamp(Math.min(...ys),0,Number(cfg.slide_height||1)),ymax=clamp(Math.max(...ys),0,Number(cfg.slide_height||1));if(xmax<=xmin||ymax<=ymin)return false;const sx=xmin/Number(cfg.slide_width||1)*img.naturalWidth,sy=ymin/Number(cfg.slide_height||1)*img.naturalHeight,sw=(xmax-xmin)/Number(cfg.slide_width||1)*img.naturalWidth,sh=(ymax-ymin)/Number(cfg.slide_height||1)*img.naturalHeight;try{targetCtx.drawImage(img,sx,sy,sw,sh,0,0,Math.round(r.width*dpr),Math.round(r.height*dpr));return true;}catch(e){return false;}}\n",
    "function screenshotCheckboxValue(id,fallback=true){const input=el(id);return input?!!input.checked:!!fallback;}\n",
    "function screenshotOptionsFromDialog(){const annotations=screenshotCheckboxValue('screenshotIncludeAnnotations',true);return {tissue:screenshotCheckboxValue('screenshotIncludeTissue',true),layers:screenshotCheckboxValue('screenshotIncludeLayers',true),annotations:annotations,labels:annotations&&screenshotCheckboxValue('screenshotIncludeLabels',true),measurements:screenshotCheckboxValue('screenshotIncludeMeasurements',true),trajectories:screenshotCheckboxValue('screenshotIncludeTrajectories',true),tile_grid:screenshotCheckboxValue('screenshotIncludeTileGrid',true),artifacts:screenshotCheckboxValue('screenshotIncludeArtifacts',true)};}\n",
    "function screenshotSummaryText(){const r=pendingScreenshotRect;if(!r)return '';const format=screenshotFormatLabel(),options=screenshotOptionsFromDialog(),parts=[];if(options.tissue)parts.push('tissue');if(options.layers)parts.push('spots/layers');if(options.annotations)parts.push(options.labels?'annotations + labels':'annotations');if(options.measurements)parts.push('measurements');if(options.trajectories)parts.push('trajectories');if(options.tile_grid)parts.push('tile grid');if(options.artifacts)parts.push('artifacts');return format+' | '+Math.round(r.width)+' x '+Math.round(r.height)+' px | '+(parts.length?parts.join(', '):'blank canvas');}\n",
    "function updateScreenshotDialogSummary(){const target=el('screenshotDialogSummary');if(target)target.textContent=screenshotSummaryText();}\n",
    "function openScreenshotDialog(r,pane=null){pendingScreenshotRect=r;pendingScreenshotPane=pane||null;const dialog=el('screenshotDialog'),backdrop=el('screenshotDialogBackdrop'),format=el('screenshotDialogFormat'),file=el('screenshotFileName'),location=el('screenshotSaveLocation');if(format&&!format.value)format.value='png';if(file)file.value=screenshotDefaultFileName(screenshotFormat());if(location)location.textContent=savePickerAvailable()?'Choose location opens a native desktop or browser Save As dialog so you can choose the folder and filename.':'Choose Location is not available in this browser window. Open through wsiTools Desktop, Chrome, or Edge to choose a folder.';updateScreenshotDialogSummary();if(dialog){dialog.classList.add('open');dialog.setAttribute('aria-hidden','false');}if(backdrop)backdrop.classList.add('open');setTimeout(()=>{const save=el('screenshotDialogSave');if(save)save.focus();},0);}\n",
    "function closeScreenshotDialog(clear=true){const dialog=el('screenshotDialog'),backdrop=el('screenshotDialogBackdrop');if(dialog){dialog.classList.remove('open');dialog.setAttribute('aria-hidden','true');}if(backdrop)backdrop.classList.remove('open');if(clear){pendingScreenshotRect=null;pendingScreenshotPane=null;}}\n",
    "function bindScreenshotDialogControls(){const save=el('screenshotDialogSave'),cancel=el('screenshotDialogCancel'),close=el('screenshotDialogClose'),backdrop=el('screenshotDialogBackdrop'),format=el('screenshotDialogFormat'),file=el('screenshotFileName');if(save&&!save._wsiBound){save._wsiBound=true;save.onclick=()=>saveScreenshotFromDialog();}if(cancel&&!cancel._wsiBound){cancel._wsiBound=true;cancel.onclick=()=>closeScreenshotDialog(true);}if(close&&!close._wsiBound){close._wsiBound=true;close.onclick=()=>closeScreenshotDialog(true);}if(backdrop&&!backdrop._wsiBound){backdrop._wsiBound=true;backdrop.onclick=()=>closeScreenshotDialog(true);}if(format&&!format._wsiBound){format._wsiBound=true;format.addEventListener('change',()=>{const file=el('screenshotFileName'),fmt=screenshotFormat();if(file){const base=String(file.value||'').replace(/\\.(png|jpe?g|svg|pdf)$/i,'');file.value=(base||screenshotDefaultFileName(fmt).replace(/\\.(png|jpe?g|svg|pdf)$/i,''))+'.'+screenshotFormatExtension(fmt);}updateScreenshotDialogSummary();if(typeof saveDisplayPreference==='function')saveDisplayPreference();});}if(file&&!file._wsiBound){file._wsiBound=true;file.addEventListener('input',updateScreenshotDialogSummary);}['screenshotIncludeTissue','screenshotIncludeLayers','screenshotIncludeAnnotations','screenshotIncludeLabels','screenshotIncludeMeasurements','screenshotIncludeTrajectories','screenshotIncludeTileGrid','screenshotIncludeArtifacts'].forEach(id=>{const input=el(id);if(input&&!input._wsiBound){input._wsiBound=true;input.addEventListener('change',updateScreenshotDialogSummary);}});}\n",
    "function annotationExportFormat(){const input=el('annotationExportFormat');const value=String((input&&input.value)||'geojson').toLowerCase();return ['geojson','json','csv'].includes(value)?value:'geojson';}\n",
    "function annotationExportScope(){const input=el('annotationExportScope');return String((input&&input.value)||'all').toLowerCase()==='selected'?'selected':'all';}\n",
    "function annotationExportExtension(format=annotationExportFormat()){return format==='csv'?'csv':(format==='json'?'json':'geojson');}\n",
    "function annotationExportBaseName(){const base=(typeof projectAnnotationFilename==='function'?projectAnnotationFilename():null)||cfg.annotation_filename||'wsiTools_annotations.geojson';return String(base).replace(/\\.(geojson|json|csv)$/i,'')||'wsiTools_annotations';}\n",
    "function annotationExportDefaultFileName(format=annotationExportFormat(),scope=annotationExportScope()){const suffix=scope==='selected'?'_selected':'';return annotationExportBaseName()+suffix+'.'+annotationExportExtension(format);}\n",
    "function annotationExportFileName(format=annotationExportFormat()){const ext=annotationExportExtension(format),input=el('annotationExportFileName');let value=String((input&&input.value)||'').trim();value=value.replace(/[\\\\/:*?\"<>|]+/g,'_').replace(/^\\.+/,'').replace(/\\s+/g,'_');if(!value)value=annotationExportDefaultFileName(format,annotationExportScope());if(!value.toLowerCase().endsWith('.'+ext))value=value.replace(/\\.(geojson|json|csv)$/i,'')+'.'+ext;return value;}\n",
    "function annotationExportSelectedIndices(){if(typeof roiExportIndices==='function')return roiExportIndices();return selectedRoi>=0&&rois[selectedRoi]?[selectedRoi]:[];}\n",
    "function annotationExportFeatures(scope=annotationExportScope()){if(scope==='selected')return annotationExportSelectedIndices().map(i=>roiFeature(rois[i],i)).filter(Boolean);if(typeof exportableRoiFeatures==='function')return exportableRoiFeatures();return rois.map((roi,i)=>roiFeature(roi,i)).filter(Boolean);}\n",
    "function annotationExportCsv(features){const headers=['id','name','class','geometry','xmin','ymin','xmax','ymax','area','points','visible','locked'];const rows=(features||[]).map((feature,i)=>{const props=feature.properties||{},bbox=feature.bbox||[],geometry=feature.geometry||{},roi=rois[i]||{};return {id:feature.id||props.id||'',name:props.name||props.label||'',class:props.class||(props.classification&&props.classification.name)||'',geometry:geometry.type||'',xmin:bbox[0]??'',ymin:bbox[1]??'',xmax:bbox[2]??'',ymax:bbox[3]??'',area:(props.measurements&&props.measurements.area)||'',points:geometry.coordinates?JSON.stringify(geometry.coordinates).length:'',visible:props.visible!==false,locked:!!props.isLocked};});return [headers.join(',')].concat(rows.map(row=>headers.map(h=>csvValue(row[h])).join(','))).join('\\n')+'\\n';}\n",
    "function annotationExportPayload(format=annotationExportFormat(),scope=annotationExportScope()){if(typeof brushing!=='undefined'&&brushing&&typeof finishBrush==='function')finishBrush();if(typeof draft!=='undefined'&&draft.length>=3&&typeof finishDraft==='function')finishDraft();const features=annotationExportFeatures(scope);if(!features.length){notify(scope==='selected'?'Select or check annotations to export':'No exportable annotations','warning',2600);return null;}if(format==='csv')return {text:annotationExportCsv(features),mime:'text/csv;charset=utf-8',count:features.length};const object={type:'FeatureCollection',features:features};return {text:JSON.stringify(object,null,2),mime:(format==='json'?'application/json':'application/geo+json')+';charset=utf-8',count:features.length};}\n",
    "function annotationExportPickerType(format=annotationExportFormat()){if(format==='csv')return {description:'CSV annotation summary',accept:{'text/csv':['.csv']}};if(format==='json')return {description:'JSON annotations',accept:{'application/json':['.json']}};return {description:'GeoJSON annotations',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}};}\n",
    "function annotationExportSummaryText(){const format=annotationExportFormat().toUpperCase(),scope=annotationExportScope(),count=annotationExportFeatures(scope).length;return format+' | '+(scope==='selected'?'selected/checked':'all exportable')+' | '+count+' annotation'+(count===1?'':'s');}\n",
    "function updateAnnotationExportDialogSummary(){const target=el('annotationExportDialogSummary');if(target)target.textContent=annotationExportSummaryText();}\n",
    "function openAnnotationExportDialog(scope='all'){const draftCount=(typeof draft!=='undefined'&&draft)?draft.length:0,brushCount=(typeof brushPoints!=='undefined'&&brushPoints)?brushPoints.length:0;if(!rois.length&&draftCount<3&&brushCount<2){notify('Draw an ROI first','warning');return;}const dialog=el('annotationExportDialog'),backdrop=el('annotationExportDialogBackdrop'),format=el('annotationExportFormat'),scopeInput=el('annotationExportScope'),file=el('annotationExportFileName'),location=el('annotationExportSaveLocation');if(format&&!format.value)format.value='geojson';if(scopeInput)scopeInput.value=scope==='selected'?'selected':'all';if(file)file.value=annotationExportDefaultFileName(annotationExportFormat(),annotationExportScope());if(location)location.textContent=savePickerAvailable()?'Choose location and save opens a native desktop or browser Save As dialog so you can pick the folder and filename.':'Choose Location is not available in this browser window. Use Download to save annotations to the browser download folder.';updateAnnotationExportDialogSummary();if(dialog){dialog.classList.add('open');dialog.setAttribute('aria-hidden','false');}if(backdrop)backdrop.classList.add('open');setTimeout(()=>{const save=el('annotationExportDialogSave');if(save)save.focus();},0);}\n",
    "function closeAnnotationExportDialog(){const dialog=el('annotationExportDialog'),backdrop=el('annotationExportDialogBackdrop');if(dialog){dialog.classList.remove('open');dialog.setAttribute('aria-hidden','true');}if(backdrop)backdrop.classList.remove('open');}\n",
    "async function saveAnnotationExportBlob(blob,name,format,preferPicker=true){if(preferPicker)return await saveBlobWithLocation(blob,name,annotationExportPickerType(format));await downloadBlob(blob,name);return 'downloaded';}\n",
    "async function saveAnnotationExportFromDialog(preferPicker=true){const format=annotationExportFormat(),scope=annotationExportScope(),payload=annotationExportPayload(format,scope);if(!payload)return;const name=annotationExportFileName(format),blob=new Blob([payload.text],{type:payload.mime});try{const mode=await saveAnnotationExportBlob(blob,name,format,preferPicker);if(mode==='cancelled'||mode==='unsupported')return;if(scope==='all')markAnnotationsSaved(format+'_'+(mode==='saved'?'saved':'exported'));scheduleViewerStateSync('roi_exported',{count:payload.count,format:format,scope:scope,dirty:annotationsDirty,respect_export_rules:typeof respectClassExportRules==='function'?respectClassExportRules():false,save_mode:mode});notify(mode==='saved'?('Annotations saved as '+format.toUpperCase()):('Annotations downloaded as '+format.toUpperCase()),'success',mode==='saved'?2200:2600);closeAnnotationExportDialog();}catch(e){notify('Annotation export failed: '+e.message,'error',5200);}}\n",
    "function bindAnnotationExportDialogControls(){const save=el('annotationExportDialogSave'),download=el('annotationExportDialogDownload'),cancel=el('annotationExportDialogCancel'),close=el('annotationExportDialogClose'),backdrop=el('annotationExportDialogBackdrop'),format=el('annotationExportFormat'),scope=el('annotationExportScope'),file=el('annotationExportFileName');if(save&&!save._wsiBound){save._wsiBound=true;save.onclick=()=>saveAnnotationExportFromDialog(true);}if(download&&!download._wsiBound){download._wsiBound=true;download.onclick=()=>saveAnnotationExportFromDialog(false);}if(cancel&&!cancel._wsiBound){cancel._wsiBound=true;cancel.onclick=closeAnnotationExportDialog;}if(close&&!close._wsiBound){close._wsiBound=true;close.onclick=closeAnnotationExportDialog;}if(backdrop&&!backdrop._wsiBound){backdrop._wsiBound=true;backdrop.onclick=closeAnnotationExportDialog;}const syncName=()=>{const fmt=annotationExportFormat(),current=file?String(file.value||''):'';if(file){const base=current.replace(/\\.(geojson|json|csv)$/i,'')||annotationExportDefaultFileName(fmt,annotationExportScope()).replace(/\\.(geojson|json|csv)$/i,'');file.value=base+'.'+annotationExportExtension(fmt);}updateAnnotationExportDialogSummary();};if(format&&!format._wsiBound){format._wsiBound=true;format.addEventListener('change',syncName);}if(scope&&!scope._wsiBound){scope._wsiBound=true;scope.addEventListener('change',()=>{if(file)file.value=annotationExportDefaultFileName(annotationExportFormat(),annotationExportScope());updateAnnotationExportDialogSummary();});}if(file&&!file._wsiBound){file._wsiBound=true;file.addEventListener('input',updateAnnotationExportDialogSummary);}}\n",
    "function prepareScreenshotOverlayForOptions(options,pane=null){const previous=screenshotRenderOptions,prevShowRois=typeof showRois==='undefined'?null:showRois,prevShowLabels=typeof showLabels==='undefined'?null:showLabels;screenshotRenderOptions=options||null;if(options){try{if(typeof showRois!=='undefined'&&options.annotations===false)showRois=false;if(typeof showLabels!=='undefined'&&(options.annotations===false||options.labels===false))showLabels=false;}catch(e){}}try{if(pane&&typeof drawMultiViewOverlays==='function')drawMultiViewOverlays();else if(typeof draw==='function')draw();}catch(e){}return ()=>{screenshotRenderOptions=previous;try{if(prevShowRois!==null)showRois=prevShowRois;if(prevShowLabels!==null)showLabels=prevShowLabels;if(pane&&typeof drawMultiViewOverlays==='function')drawMultiViewOverlays();else if(typeof draw==='function')draw();}catch(e){}};}\n",
    "function screenshotCanvasFromRect(r,includeBase=true,options=null){const dpr=window.devicePixelRatio||1,canvasRect=canvas.getBoundingClientRect(),rect={x:r.x+canvasRect.left,y:r.y+canvasRect.top,width:r.width,height:r.height},out=document.createElement('canvas'),outCtx=out.getContext('2d');out.width=Math.max(1,Math.round(rect.width*dpr));out.height=Math.max(1,Math.round(rect.height*dpr));outCtx.fillStyle='#ffffff';outCtx.fillRect(0,0,out.width,out.height);let drewBase=false,usedPreviewFallback=false;if(includeBase){screenshotBaseElements().forEach(c=>{drewBase=drawElementIntoScreenshot(outCtx,c,rect,dpr)||drewBase;});if(!drewBase){usedPreviewFallback=drawPreviewBaseIntoScreenshot(outCtx,r,dpr);drewBase=usedPreviewFallback;}}const restore=prepareScreenshotOverlayForOptions(options,null);try{drawElementIntoScreenshot(outCtx,canvas,rect,dpr);}finally{if(restore)restore();}out._wsiScreenshotBaseIncluded=!includeBase||drewBase;out._wsiScreenshotPreviewFallback=includeBase&&usedPreviewFallback;out._wsiScreenshotReadable=canvasIsReadable(out);return out;}\n",
    "function pngBlobFromCanvas(c){return new Promise((resolve,reject)=>{try{if(c.toBlob){c.toBlob(blob=>blob?resolve(blob):reject(new Error('Could not encode PNG screenshot.')),'image/png');return;}const data=c.toDataURL('image/png'),raw=atob(data.split(',')[1]||''),bytes=new Uint8Array(raw.length);for(let i=0;i<raw.length;i++)bytes[i]=raw.charCodeAt(i);resolve(new Blob([bytes],{type:'image/png'}));}catch(e){reject(e);}});}\n",
    "function jpegBlobFromCanvas(c,quality=.92){return new Promise((resolve,reject)=>{try{if(c.toBlob){c.toBlob(blob=>blob?resolve(blob):reject(new Error('Could not encode JPEG screenshot.')),'image/jpeg',quality);return;}const data=c.toDataURL('image/jpeg',quality),raw=atob(data.split(',')[1]||''),bytes=new Uint8Array(raw.length);for(let i=0;i<raw.length;i++)bytes[i]=raw.charCodeAt(i);resolve(new Blob([bytes],{type:'image/jpeg'}));}catch(e){reject(e);}});}\n",
    "function svgEscape(value){return String(value??'').replace(/[&<>\"']/g,ch=>{if(ch==='&')return '&amp;';if(ch==='<')return '&lt;';if(ch==='>')return '&gt;';if(ch.charCodeAt(0)===34)return '&quot;';return '&apos;';});}\n",
    "function svgBlobFromCanvas(c){return new Promise((resolve,reject)=>{try{const width=Math.max(1,Number(c.width)||1),height=Math.max(1,Number(c.height)||1),data=c.toDataURL('image/png'),title=svgEscape((cfg.title||'wsiTools')+' screenshot'),svg=`<?xml version=\"1.0\" encoding=\"UTF-8\"?>\\n<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"${width}\" height=\"${height}\" viewBox=\"0 0 ${width} ${height}\" role=\"img\" aria-label=\"${title}\"><title>${title}</title><image href=\"${data}\" x=\"0\" y=\"0\" width=\"${width}\" height=\"${height}\" preserveAspectRatio=\"none\"/></svg>\\n`;resolve(new Blob([svg],{type:'image/svg+xml;charset=utf-8'}));}catch(e){reject(e);}});}\n",
    "function asciiBytes(text){const s=String(text),out=new Uint8Array(s.length);for(let i=0;i<s.length;i++)out[i]=s.charCodeAt(i)&255;return out;}\n",
    "function dataUrlBytes(data){const raw=atob(String(data).split(',')[1]||''),out=new Uint8Array(raw.length);for(let i=0;i<raw.length;i++)out[i]=raw.charCodeAt(i);return out;}\n",
    "function concatBytes(parts){const total=parts.reduce((n,p)=>n+p.length,0),out=new Uint8Array(total);let offset=0;parts.forEach(p=>{out.set(p,offset);offset+=p.length;});return out;}\n",
    "function pdfBlobFromCanvas(c){return new Promise((resolve,reject)=>{try{const width=Math.max(1,Math.round(Number(c.width)||1)),height=Math.max(1,Math.round(Number(c.height)||1)),jpeg=dataUrlBytes(c.toDataURL('image/jpeg',.92)),content=asciiBytes(`q ${width} 0 0 ${height} 0 0 cm /Im0 Do Q\\n`),objects=[{body:'<< /Type /Catalog /Pages 2 0 R >>'},{body:'<< /Type /Pages /Kids [3 0 R] /Count 1 >>'},{body:`<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ${width} ${height}] /Resources << /XObject << /Im0 4 0 R >> >> /Contents 5 0 R >>`},{dict:`<< /Type /XObject /Subtype /Image /Width ${width} /Height ${height} /ColorSpace /DeviceRGB /BitsPerComponent 8 /Filter /DCTDecode /Length ${jpeg.length} >>`,stream:jpeg},{dict:`<< /Length ${content.length} >>`,stream:content}],parts=[],offsets=[0];let offset=0;function add(part){const bytes=part instanceof Uint8Array?part:asciiBytes(part);parts.push(bytes);offset+=bytes.length;}add('%PDF-1.4\\n');objects.forEach((obj,i)=>{offsets[i+1]=offset;add(`${i+1} 0 obj\\n`);if(obj.stream){add(obj.dict+'\\nstream\\n');add(obj.stream);add('\\nendstream\\nendobj\\n');}else add(obj.body+'\\nendobj\\n');});const xref=offset;let tail=`xref\\n0 ${objects.length+1}\\n0000000000 65535 f \\n`;for(let i=1;i<=objects.length;i++)tail+=String(offsets[i]).padStart(10,'0')+' 00000 n \\n';tail+=`trailer\\n<< /Size ${objects.length+1} /Root 1 0 R >>\\nstartxref\\n${xref}\\n%%EOF\\n`;add(tail);resolve(new Blob([concatBytes(parts)],{type:'application/pdf'}));}catch(e){reject(e);}});}\n",
    "function screenshotBlobFromCanvas(c,format=screenshotFormat()){const fmt=screenshotNormaliseFormat(format);if(fmt==='svg')return svgBlobFromCanvas(c);if(fmt==='pdf')return pdfBlobFromCanvas(c);if(fmt==='jpeg')return jpegBlobFromCanvas(c);return pngBlobFromCanvas(c);}\n",
    "function screenshotPickerType(format){const fmt=screenshotNormaliseFormat(format);if(fmt==='svg')return {description:'SVG screenshot',accept:{'image/svg+xml':['.svg']}};if(fmt==='pdf')return {description:'PDF screenshot',accept:{'application/pdf':['.pdf']}};if(fmt==='jpeg')return {description:'JPEG screenshot',accept:{'image/jpeg':['.jpg','.jpeg']}};return {description:'PNG screenshot',accept:{'image/png':['.png']}};}\n",
    "function tauriViewerInvoke(){return window.__TAURI__&&window.__TAURI__.core&&typeof window.__TAURI__.core.invoke==='function'?window.__TAURI__.core.invoke:null;}\n",
    "function savePickerAvailable(){return !!(tauriViewerInvoke()||window.showSaveFilePicker);}\n",
    "function pickerTypeFilters(type){const filters=[];if(type&&type.description&&type.accept){Object.keys(type.accept).forEach(mime=>{const extensions=(type.accept[mime]||[]).map(x=>String(x).replace(/^\\./,'')).filter(Boolean);if(extensions.length)filters.push({name:type.description,extensions:extensions});});}return filters.length?filters:[{name:'wsiTools file',extensions:[]}];}\n",
    "async function blobToBase64(blob){const bytes=new Uint8Array(await blob.arrayBuffer());let binary='',chunk=32768;for(let i=0;i<bytes.length;i+=chunk)binary+=String.fromCharCode.apply(null,bytes.subarray(i,i+chunk));return btoa(binary);}\n",
    "async function saveBlobWithLocation(blob,name,type){const invoke=tauriViewerInvoke();if(invoke){const path=await invoke('save_viewer_file',{fileName:name,dataBase64:await blobToBase64(blob),filters:pickerTypeFilters(type)});return path?'saved':'cancelled';}if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[type],excludeAcceptAllOption:false});const w=await h.createWritable();await w.write(blob);await w.close();return 'saved';}catch(e){if(e&&e.name==='AbortError')return 'cancelled';throw e;}}notify('Choose Location is not available in this browser window. Open the viewer in wsiTools Desktop, Chrome, or Edge, or use Download when available.','warning',7600);return 'unsupported';}\n",
    "function downloadBlob(blob,name){return new Promise((resolve,reject)=>{let url=null;try{if(navigator.msSaveOrOpenBlob){navigator.msSaveOrOpenBlob(blob,name);resolve(true);return;}url=URL.createObjectURL(blob);const a=document.createElement('a');a.href=url;a.download=name;a.rel='noopener';a.style.display='none';document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(url);a.remove();},4000);resolve(true);}catch(e){try{if(url&&window.open(url,'_blank')){setTimeout(()=>URL.revokeObjectURL(url),60000);resolve(true);return;}}catch(e2){}if(url)URL.revokeObjectURL(url);reject(e);}});}\n",
    "async function saveScreenshot(c,fallbackCanvas=null,forcedFormat=null,forcedName=null){const format=screenshotNormaliseFormat(forcedFormat||screenshotFormat()),label=screenshotFormatLabel(format),name=forcedName||screenshotFileName(format);if(c&&c._wsiScreenshotBaseIncluded===false){notify('Screenshot was not saved because the tissue image could not be captured. Reopen through the live localhost viewer or enable same-origin/dynamic tiles.','error',7600);return;}try{const blob=await screenshotBlobFromCanvas(c,format),mode=await saveBlobWithLocation(blob,name,screenshotPickerType(format));if(mode==='cancelled'||mode==='unsupported')return;notify(c&&c._wsiScreenshotPreviewFallback?('Screenshot saved as '+label+' using preview-resolution tissue plus overlays'):('Screenshot saved as '+label),c&&c._wsiScreenshotPreviewFallback?'warning':'success',c&&c._wsiScreenshotPreviewFallback?5200:1800);}catch(e){if(fallbackCanvas){try{const fallback=typeof fallbackCanvas==='function'?fallbackCanvas():fallbackCanvas,blob=await screenshotBlobFromCanvas(fallback,format),mode=await saveBlobWithLocation(blob,name,screenshotPickerType(format));if(mode==='cancelled'||mode==='unsupported')return;notify('Screenshot saved as '+label+' without base image because browser blocked tile pixels. Open through localhost/live tiles for full image screenshots.','warning',7200);return;}catch(e2){}}notify('Could not save '+label+' screenshot. Open the viewer through localhost/live tiles so browser canvas access is allowed.','error',7200);}}\n",
    "async function saveScreenshotPng(c,fallbackCanvas=null){return saveScreenshot(c,fallbackCanvas,'png');}\n",
    "function saveScreenshotFromDialog(){if(!pendingScreenshotRect){closeScreenshotDialog(true);notify('No screenshot area selected','warning',1800);return;}const r=pendingScreenshotRect,pane=pendingScreenshotPane,options=screenshotOptionsFromDialog(),format=screenshotFormat(),name=screenshotFileName(format),includeBase=options.tissue;closeScreenshotDialog(false);try{let shot=null,fallback=null;if(pane&&typeof multiViewScreenshotCanvasFromRect==='function'){shot=multiViewScreenshotCanvasFromRect(pane,r,includeBase,options);fallback=()=>multiViewScreenshotCanvasFromRect(pane,r,false,options);}else{shot=screenshotCanvasFromRect(r,includeBase,options);fallback=()=>screenshotCanvasFromRect(r,false,options);}saveScreenshot(shot,shot&&shot._wsiScreenshotReadable?null:fallback,format,name);}catch(e){try{const fallback=pane&&typeof multiViewScreenshotCanvasFromRect==='function'?multiViewScreenshotCanvasFromRect(pane,r,false,options):screenshotCanvasFromRect(r,false,options);saveScreenshot(fallback,null,format,name);}catch(e2){notify('Could not prepare screenshot export','error',4200);}}pendingScreenshotRect=null;pendingScreenshotPane=null;}\n",
    "function startScreenshotSelection(evt){evt.preventDefault();const p=screenshotCanvasPoint(evt);screenshotSelecting=true;screenshotRect={x0:p.x,y0:p.y,x1:p.x,y1:p.y};draw();}\n",
    "function updateScreenshotSelection(evt){if(!screenshotSelecting)return;const p=screenshotCanvasPoint(evt);screenshotRect.x1=p.x;screenshotRect.y1=p.y;draw();}\n",
    "function finishScreenshotSelection(evt){if(!screenshotSelecting)return;updateScreenshotSelection(evt);const r=normalizedScreenshotRect();screenshotSelecting=false;screenshotRect=null;if(!r||r.width<8||r.height<8){setMode('pan');notify('Screenshot area too small','warning',1800);draw();return;}draw();setMode('pan');openScreenshotDialog(r,null);}\n",
    "function cancelScreenshotSelection(){screenshotSelecting=false;screenshotRect=null;if(typeof multiViewScreenshotPane!=='undefined')multiViewScreenshotPane=null;const button=el('screenshotTool');if(button)button.classList.remove('active');draw();}\n",
    "function beginScreenshotMode(){screenshotSelecting=false;screenshotRect=null;setMode('screenshot');notify('Drag an area to save a PNG, JPEG, SVG, or PDF screenshot','info',3200);}\n",
    "function imageExportUrl(){return String(cfg.image_export_url||'');}\n",
    "function imageExportOutputDir(){const input=el('imageExportDir');return input?String(input.value||'').trim():'';}\n",
    "function setImageExportSummary(message){const target=el('imageExportSummary');if(target)target.textContent=message||'';}\n",
    "function imageExportRegionFromBounds(bounds){if(!bounds)return null;const sw=Number(cfg.slide_width||0),sh=Number(cfg.slide_height||0),xmin=clamp(Math.floor(Math.min(bounds.xmin,bounds.xmax)),0,sw),ymin=clamp(Math.floor(Math.min(bounds.ymin,bounds.ymax)),0,sh),xmax=clamp(Math.ceil(Math.max(bounds.xmin,bounds.xmax)),0,sw),ymax=clamp(Math.ceil(Math.max(bounds.ymin,bounds.ymax)),0,sh),width=Math.max(1,xmax-xmin),height=Math.max(1,ymax-ymin);if(width<1||height<1)return null;return {x:xmin,y:ymin,width:width,height:height,level:0};}\n",
    "function currentViewportExportRegion(){let points=[];try{points=[canvasToSlidePoint(0,0),canvasToSlidePoint(innerWidth,0),canvasToSlidePoint(innerWidth,innerHeight),canvasToSlidePoint(0,innerHeight)].filter(pointInsideSlide);}catch(e){points=[];}if(!points.length){points=[{x:0,y:0},{x:Number(cfg.slide_width||0),y:Number(cfg.slide_height||0)}];}const xs=points.map(p=>p.x),ys=points.map(p=>p.y);return imageExportRegionFromBounds({xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)});}\n",
    "function selectedRoiExportRegion(){if(selectedRoi<0||!rois[selectedRoi]){notify('Select an annotation first','warning',2400);return null;}return imageExportRegionFromBounds(roiBounds(rois[selectedRoi]));}\n",
    "async function exportImageRegion(scope='viewport',format='tiff'){const url=imageExportUrl();if(!url){const msg='TIFF export needs a live R viewer. Reopen with wsi_viewer_live(..., dynamic_tiles = TRUE) or wsi_open_viewer(..., live = \"yes\").';setImageExportSummary(msg);notify(msg,'warning',7200);return;}const region=scope==='selected_roi'?selectedRoiExportRegion():currentViewportExportRegion();if(!region){notify('No valid export region','warning',2400);return;}const payload={scope:scope,format:format,region:region,output_dir:imageExportOutputDir(),selected_roi:scope==='selected_roi'?selectedRoiFeatureObject():null,annotation_count:rois.length,viewport:{scale:scale,offset_x:offsetX,offset_y:offsetY}};setImageExportSummary('Exporting '+region.width+' x '+region.height+' px '+format.toUpperCase()+' through R...');try{const response=await fetch(url,{method:'POST',headers:{'Accept':'application/json','Content-Type':'application/json'},body:JSON.stringify(payload)});let body=null;try{body=await response.json();}catch(e){}if(!response.ok||body&&body.ok===false)throw new Error(body&&body.error||('HTTP '+response.status));handleViewerCommands(body);const result=body&&body.image_export||body||{};const file=result.file||'image file';setImageExportSummary('Saved '+String(result.format||format).toUpperCase()+' export: '+file);scheduleViewerStateSync('image_exported',{scope:scope,format:format,file:file,region:region});notify('Image exported as '+String(result.format||format).toUpperCase(),'success',3200);}catch(e){const msg='Image export failed: '+e.message;setImageExportSummary(msg);notify(msg,'error',7200);}}\n"
  )
}

wsi_viewer_multiview_js <- function() {
  paste0(
    "let multiViewLayout=1,multiViewPanes=[],multiViewSync=false,multiViewCustomMode=false,multiViewApplying=false,multiViewActiveIndex=0,multiViewControlPaneIndex=0,multiViewAssignments=[],multiViewPointerPane=null,multiViewPaneDragging=false,multiViewPaneMoved=false,multiViewPaneLastX=0,multiViewPaneLastY=0,multiViewPaneDragStartX=0,multiViewPaneDragStartY=0,multiViewScreenshotPane=null,multiViewColFractions=[],multiViewRowFractions=[],multiViewResizeDrag=null;\n",
    "function multiViewHost(){return el('multiViewGrid');}\n",
    "function multiViewSupported(){return typeof OpenSeadragon!=='undefined'&&typeof tileSourceFromConfig==='function';}\n",
    "function multiViewStatus(){return multiViewLayout>1?(' | views '+multiViewLayout+(multiViewSync?' linked':' independent')):'';}\n",
    "function multiViewProjectEntries(){if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||typeof projectTileSourceFromItem!=='function'||typeof tileSourceFromConfig!=='function')return[];const entries=[];projectItems.forEach((item,itemIndex)=>{const sections=(typeof projectSections==='function')?projectSections(item):[];if(Array.isArray(sections)&&sections.length){sections.forEach((section,sectionIndex)=>{if(projectTileSourceFromItem(item,section)){entries.push({item:item,section:section,itemIndex:itemIndex,sectionIndex:sectionIndex,label:(item.label||item.path||('Image '+(itemIndex+1)))+' / '+(section.label||section.id||('Section '+(sectionIndex+1)))});}});}else{const src=projectTileSourceFromItem(item,null);if(src){entries.push({item:item,section:null,itemIndex:itemIndex,sectionIndex:-1,label:item.label||item.path||('Image '+(itemIndex+1))});}else if(itemIndex===activeProjectIndex){entries.push({item:item,section:null,itemIndex:itemIndex,sectionIndex:-1,useActiveSource:true,label:item.label||item.path||cfg.title||('Image '+(itemIndex+1))});}}});if(entries.length<=1)return entries;let active=entries.findIndex(entry=>entry.itemIndex===activeProjectIndex&&(entry.sectionIndex===activeProjectSectionIndex||(entry.sectionIndex<0&&activeProjectSectionIndex<0)));if(active<0)active=entries.findIndex(entry=>entry.itemIndex===activeProjectIndex);if(active>0)return entries.slice(active).concat(entries.slice(0,active));return entries;}\n",
    "function multiViewProjectEntryKey(entry){if(!entry)return '';const item=entry.item||{},section=entry.section||null,itemId=String(item.id||item.path||(entry.itemIndex!=null?entry.itemIndex:'image')),sectionId=section?String(section.id||section.label||(entry.sectionIndex!=null?entry.sectionIndex:'section')):'main';return itemId+'::'+sectionId;}\n",
    "function multiViewProjectEntryLabel(item,itemIndex,section=null,sectionIndex=-1){const itemLabel=String((item&&(item.label||item.path||item.id))||('Image '+(Number(itemIndex)+1)));if(section)return itemLabel+' / '+String(section.label||section.id||section.scene||('Section '+(Number(sectionIndex)+1)));return itemLabel;}\n",
    "function multiViewEntryFromIndices(itemIndex,sectionIndex=-1){if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||typeof projectTileSourceFromItem!=='function')return null;itemIndex=Number(itemIndex);sectionIndex=Number(sectionIndex);if(!Number.isInteger(itemIndex)||itemIndex<0||itemIndex>=projectItems.length)return null;const item=projectItems[itemIndex]||null;if(!item)return null;const sections=(typeof projectSections==='function')?projectSections(item):[];function entryFor(section,sidx,useActive=false){return {item:item,section:section||null,itemIndex:itemIndex,sectionIndex:Number.isInteger(sidx)?sidx:-1,useActiveSource:!!useActive,label:multiViewProjectEntryLabel(item,itemIndex,section,sidx)};}if(Number.isInteger(sectionIndex)&&sectionIndex>=0){const section=sections[sectionIndex]||null;return section?entryFor(section,sectionIndex,false):null;}const activeSection=(itemIndex===activeProjectIndex&&Number.isInteger(Number(activeProjectSectionIndex)))?Number(activeProjectSectionIndex):-1;if(activeSection>=0&&sections[activeSection]&&projectTileSourceFromItem(item,sections[activeSection]))return entryFor(sections[activeSection],activeSection,false);const defaultSection=(typeof defaultProjectSectionIndex==='function')?Number(defaultProjectSectionIndex(item)):-1;if(defaultSection>=0&&sections[defaultSection]&&projectTileSourceFromItem(item,sections[defaultSection]))return entryFor(sections[defaultSection],defaultSection,false);const firstSection=sections.findIndex(section=>projectTileSourceFromItem(item,section));if(firstSection>=0)return entryFor(sections[firstSection],firstSection,false);if(projectTileSourceFromItem(item,null))return entryFor(null,-1,false);if(itemIndex===activeProjectIndex)return entryFor(null,-1,true);return entryFor(null,-1,false);}\n",
    "function multiViewEntryByKey(key){key=String(key||'');if(!key)return null;return multiViewProjectEntries().find(entry=>multiViewProjectEntryKey(entry)===key)||null;}\n",
    "function multiViewEntryFromPayload(payload){if(!payload)return null;if(typeof payload==='string'){try{payload=JSON.parse(payload);}catch(e){if(/^\\d+$/.test(payload))payload={itemIndex:Number(payload),sectionIndex:-1};else return null;}}if(payload.type&&payload.type!=='wsiTools.projectEntry'&&!String(payload.type).includes('project'))return null;if(Number.isFinite(Number(payload.itemIndex))){const direct=multiViewEntryFromIndices(Number(payload.itemIndex),Number(payload.sectionIndex));if(direct)return direct;}if(payload.key){const byKey=multiViewEntryByKey(payload.key);if(byKey)return byKey;}const itemIndex=Number(payload.itemIndex),sectionIndex=Number(payload.sectionIndex),entries=multiViewProjectEntries();const exact=entries.find(entry=>entry.itemIndex===itemIndex&&entry.sectionIndex===sectionIndex);if(exact)return exact;const activeSection=Number(typeof activeProjectSectionIndex==='undefined'?-1:activeProjectSectionIndex);return entries.find(entry=>entry.itemIndex===itemIndex&&entry.sectionIndex===activeSection)||entries.find(entry=>entry.itemIndex===itemIndex)||null;}\n",
    "function multiViewDragCacheEntry(){try{if(typeof projectDragPayloadCache==='undefined'||!projectDragPayloadCache)return null;const payload=projectDragPayloadCache.payload||projectDragPayloadCache;return multiViewEntryFromPayload(payload);}catch(e){return null;}}\n",
    "function multiViewDropPayload(dt){if(dt){const types=['application/x-wsitools-project-entry','text/x-wsitools-project-entry','text/plain','application/x-wsitools-project-reorder'];for(const type of types){try{const raw=dt.getData(type);if(raw){const entry=multiViewEntryFromPayload(raw);if(entry)return entry;}}catch(e){}}}return multiViewDragCacheEntry();}\n",
    "function multiViewUsesProjectSources(){return multiViewProjectEntries().length>1;}\n",
    "function multiViewAssignmentUsed(key,exceptIndex=-1){key=String(key||'');if(!key)return false;return multiViewAssignments.some((value,i)=>i!==Number(exceptIndex)&&String(value||'')===key);}\n",
    "function multiViewNormalizeAssignments(count=multiViewAssignments.length){const seen=new Set();for(let i=0;i<count;i++){const key=String(multiViewAssignments[i]||''),entry=multiViewEntryByKey(key);if(!entry||seen.has(key)){multiViewAssignments[i]='';continue;}seen.add(key);}}\n",
    "function multiViewPaneIsBlank(index){const assigned=multiViewEntryByKey(multiViewAssignments[index]);if(assigned)return false;const entries=multiViewProjectEntries();return entries.length?Number(index)>=entries.length:Number(index)>0;}\n",
    "function multiViewEntry(index){const assigned=multiViewEntryByKey(multiViewAssignments[index]);if(assigned)return assigned;const entries=multiViewProjectEntries();if(!entries.length)return null;if(Number(index)>=entries.length)return null;return entries[index];}\n",
    "function multiViewBlankTileSource(){return {type:'image',url:'data:image/svg+xml;charset=utf-8,'+encodeURIComponent('<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"1\"><rect width=\"1\" height=\"1\" fill=\"#090b10\"/></svg>')};}\n",
    "function multiViewTileSource(index,count){if(multiViewPaneIsBlank(index))return multiViewBlankTileSource();const entry=multiViewEntry(index);if(entry){if(entry.useActiveSource)return tileSourceFromConfig();const src=projectTileSourceFromItem(entry.item,entry.section);if(src)return src;}return Number(index)===0?tileSourceFromConfig():multiViewBlankTileSource();}\n",
    "function multiViewPaneLabel(index){const entry=multiViewEntry(index);if(entry)return entry.label;if(multiViewPaneIsBlank(index))return 'Empty view '+(index+1);return 'View '+(index+1);}\n",
    "function updateMultiViewControls(){document.querySelectorAll('.multiViewLayout').forEach(button=>{const n=Number(button.dataset.layout||1);button.classList.toggle('active',!multiViewCustomMode&&n===multiViewLayout);});const custom=el('multiViewCustomCount');if(custom&&multiViewLayout>0)custom.value=String(multiViewLayout);const customButton=el('multiViewCustom');if(customButton)customButton.classList.toggle('active',!!multiViewCustomMode&&multiViewLayout>1);const sync=el('multiViewSync');if(sync)sync.checked=!!multiViewSync;const summary=el('multiViewSummary');if(summary){const projectCount=multiViewProjectEntries().length;if(multiViewLayout<=1)summary.textContent=projectCount>1?('Single view. Use 2, 4, 6, or custom views to compare '+projectCount+' project images/sections side by side. Drag a project item onto a pane to replace it.'):'Single view. Use 2, 4, 6, or custom views to compare tissue regions side by side.';else summary.textContent=multiViewLayout+' tissue views, '+(multiViewSync?'linked zoom/pan':'independent zoom/pan')+(multiViewCustomMode?' with empty custom slots left blank':(projectCount>1?' across project images/sections side by side':''))+'. Click a pane to make it active; + and - zoom that active pane. Drag the turquoise borders to resize panes; drag project images/sections onto panes to replace them.';}}\n",
    "function setMultiViewActive(index,pin=true){multiViewActiveIndex=clamp(Number(index)||0,0,Math.max(0,multiViewPanes.length-1));if(pin)multiViewControlPaneIndex=multiViewActiveIndex;multiViewPanes.forEach((pane,i)=>{if(pane&&pane.element)pane.element.classList.toggle('active',i===multiViewActiveIndex);});}\n",
    "function destroyMultiViewPanes(){multiViewPanes.forEach(pane=>{try{if(pane.viewer&&typeof pane.viewer.destroy==='function')pane.viewer.destroy();}catch(e){}});multiViewPanes=[];multiViewResizeDrag=null;document.body.classList.remove('multiViewResizing');const host=multiViewHost();if(host)host.innerHTML='';multiViewActiveIndex=0;multiViewControlPaneIndex=0;}\n",
    "function multiViewColumns(count){count=Math.max(1,Number(count)||1);if(count<=1)return 1;if(count===2)return 2;if(count<=4)return 2;if(count<=6)return 3;return Math.ceil(Math.sqrt(count));}\n",
    "function multiViewEnsureFractions(cols,rows){if(!Array.isArray(multiViewColFractions)||multiViewColFractions.length!==cols)multiViewColFractions=Array.from({length:cols},()=>1);if(!Array.isArray(multiViewRowFractions)||multiViewRowFractions.length!==rows)multiViewRowFractions=Array.from({length:rows},()=>1);}\n",
    "function multiViewTrackTemplate(values,min=120){return (values||[]).map(v=>'minmax('+min+'px, '+Math.max(.12,Number(v)||1).toFixed(4)+'fr)').join(' ');}\n",
    "function multiViewGridMetrics(){const host=multiViewHost(),rect=host&&host.getBoundingClientRect?host.getBoundingClientRect():null;if(!host||!rect)return null;const style=getComputedStyle(host),px=v=>Number.parseFloat(v)||0,colGap=px(style.columnGap||style.gap),rowGap=px(style.rowGap||style.gap),padL=px(style.paddingLeft),padR=px(style.paddingRight),padT=px(style.paddingTop),padB=px(style.paddingBottom),cols=Math.max(1,multiViewColFractions.length||1),rows=Math.max(1,multiViewRowFractions.length||1),width=Math.max(1,rect.width-padL-padR-colGap*Math.max(0,cols-1)),height=Math.max(1,rect.height-padT-padB-rowGap*Math.max(0,rows-1)),sumCols=multiViewColFractions.reduce((a,b)=>a+(Number(b)||1),0)||cols,sumRows=multiViewRowFractions.reduce((a,b)=>a+(Number(b)||1),0)||rows,colPixels=multiViewColFractions.map(v=>(Number(v)||1)/sumCols*width),rowPixels=multiViewRowFractions.map(v=>(Number(v)||1)/sumRows*height);return {host,rect,colGap,rowGap,padL,padR,padT,padB,width,height,cols,rows,sumCols,sumRows,colPixels,rowPixels};}\n",
    "function multiViewApplyGridTemplate(){const host=multiViewHost();if(!host)return;host.style.gridTemplateColumns=multiViewTrackTemplate(multiViewColFractions,110);host.style.gridTemplateRows=multiViewTrackTemplate(multiViewRowFractions,110);updateMultiViewResizeHandles();}\n",
    "function multiViewResizeTrackFractions(values,index,delta,total){values=(values||[]).map(v=>Number(v)||1);index=Number(index);if(index<0||index>=values.length-1||!Number.isFinite(delta)||!Number.isFinite(total)||total<=0)return values;const sum=values.reduce((a,b)=>a+b,0)||values.length,pixels=values.map(v=>v/sum*total),minSize=Math.min(140,Math.max(56,total/(values.length*4))),adj=clamp(delta,minSize-pixels[index],pixels[index+1]-minSize);pixels[index]+=adj;pixels[index+1]-=adj;return pixels.map(px=>Math.max(minSize,px)/total*sum);}\n",
    "function updateMultiViewResizeHandles(){const metrics=multiViewGridMetrics();if(!metrics)return;const host=metrics.host;let layer=host.querySelector('.multiViewResizeLayer');if(!layer){layer=document.createElement('div');layer.className='multiViewResizeLayer';host.appendChild(layer);}layer.innerHTML='';if(multiViewLayout<=1)return;let x=metrics.padL;for(let i=0;i<metrics.cols-1;i++){x+=metrics.colPixels[i];const h=document.createElement('div');h.className='multiViewResizeHandle col';h.dataset.axis='col';h.dataset.index=String(i);h.style.left=(x+metrics.colGap/2)+'px';h.title='Drag to resize multi-view columns';h.addEventListener('mousedown',startMultiViewResize,true);layer.appendChild(h);x+=metrics.colGap;}let y=metrics.padT;for(let i=0;i<metrics.rows-1;i++){y+=metrics.rowPixels[i];const h=document.createElement('div');h.className='multiViewResizeHandle row';h.dataset.axis='row';h.dataset.index=String(i);h.style.top=(y+metrics.rowGap/2)+'px';h.title='Drag to resize multi-view rows';h.addEventListener('mousedown',startMultiViewResize,true);layer.appendChild(h);y+=metrics.rowGap;}}\n",
    "function startMultiViewResize(e){e.preventDefault();e.stopPropagation();if(typeof e.stopImmediatePropagation==='function')e.stopImmediatePropagation();const target=e.currentTarget,metrics=multiViewGridMetrics();if(!target||!metrics)return;multiViewResizeDrag={axis:String(target.dataset.axis||'col'),index:Number(target.dataset.index||0),startX:e.clientX,startY:e.clientY,cols:multiViewColFractions.slice(),rows:multiViewRowFractions.slice(),metrics:metrics,handle:target};target.classList.add('active');document.body.classList.add('multiViewResizing');}\n",
    "function multiViewResizeMove(e){if(!multiViewResizeDrag)return false;e.preventDefault();e.stopPropagation();const d=multiViewResizeDrag,dx=e.clientX-d.startX,dy=e.clientY-d.startY;if(d.axis==='col')multiViewColFractions=multiViewResizeTrackFractions(d.cols,d.index,dx,d.metrics.width);else multiViewRowFractions=multiViewResizeTrackFractions(d.rows,d.index,dy,d.metrics.height);multiViewApplyGridTemplate();multiViewPanes.forEach(pane=>resizeMultiViewPaneViewer(pane,false));drawMultiViewOverlays();return true;}\n",
    "function finishMultiViewResize(e){if(!multiViewResizeDrag)return false;if(e){e.preventDefault();e.stopPropagation();}if(multiViewResizeDrag.handle)multiViewResizeDrag.handle.classList.remove('active');multiViewResizeDrag=null;document.body.classList.remove('multiViewResizing');multiViewPanes.forEach(pane=>scheduleMultiViewPaneRefresh(pane,false));scheduleViewerStateSync('multi_view_layout_updated',{layout:multiViewLayout,sync:multiViewSync,assignments:multiViewAssignments.slice(),column_fractions:multiViewColFractions.slice(),row_fractions:multiViewRowFractions.slice()});return true;}\n",
    "function multiViewInitialBounds(index,count){const cols=multiViewColumns(count),rows=Math.ceil(count/cols),col=index%cols,row=Math.floor(index/cols),w=Number(cfg.slide_width||1)/cols,h=Number(cfg.slide_height||1)/rows;return {xmin:col*w,ymin:row*h,xmax:Math.min(Number(cfg.slide_width||1),(col+1)*w),ymax:Math.min(Number(cfg.slide_height||1),(row+1)*h)};}\n",
    "function copyViewportBetween(source,target,immediate=true){if(!source||!target||!source.viewport||!target.viewport)return false;try{const center=source.viewport.getCenter(true),zoom=source.viewport.getZoom(true);target.viewport.zoomTo(zoom,null,immediate);target.viewport.panTo(center,immediate);target.viewport.applyConstraints(immediate);return true;}catch(e){return false;}}\n",
    "function zoomPaneToSlideBounds(viewer,b){if(!viewer||!viewer.world||!b)return false;const item=viewer.world.getItemAt(0);if(!item||typeof item.imageToViewportCoordinates!=='function')return false;const p0=item.imageToViewportCoordinates(Number(b.xmin),Number(b.ymin)),p1=item.imageToViewportCoordinates(Number(b.xmax),Number(b.ymax)),rect=new OpenSeadragon.Rect(p0.x,p0.y,p1.x-p0.x,p1.y-p0.y);viewer.viewport.fitBoundsWithConstraints(rect,true);return true;}\n",
    "function multiViewPaneElement(pane){return pane&&(pane.viewer&&pane.viewer.element||pane.element&&pane.element.querySelector('.multiViewPaneViewer')||pane.element)||null;}\n",
    "function resizeMultiViewPaneViewer(pane,goHome=false){if(!pane||!pane.viewer||!pane.viewer.viewport)return false;const node=multiViewPaneElement(pane),rect=node&&node.getBoundingClientRect?node.getBoundingClientRect():null;if(!rect||rect.width<2||rect.height<2)return false;try{pane.viewer.viewport.resize(new OpenSeadragon.Point(rect.width,rect.height),true);}catch(e){}try{if(goHome&&typeof pane.viewer.viewport.goHome==='function')pane.viewer.viewport.goHome(true);}catch(e){}try{if(typeof pane.viewer.forceRedraw==='function')pane.viewer.forceRedraw();}catch(e){}resizeMultiViewOverlay(pane);return true;}\n",
    "function multiViewSourceForPane(pane){const entry=pane&&pane.entry||null;if(entry&&typeof projectDisplaySource==='function')return projectDisplaySource(entry.item,entry.section)||entry.section||entry.item||null;return entry&&(entry.section||entry.item)||null;}\n",
    "function multiViewChannelIdentitySet(pane){const out=new Set(),entry=pane&&pane.entry||null;if(!entry){if(typeof activeChannelIdentitySet==='function')return activeChannelIdentitySet();channelPushIdentity(out,'active_project_image');return out;}const source=multiViewSourceForPane(pane);[entry.item,entry.section,source].forEach(obj=>{if(typeof channelIdentitySetFrom==='function')channelIdentitySetFrom(obj).forEach(v=>out.add(v));});if(entry.item&&entry.item.active&&typeof channelPushIdentity==='function')channelPushIdentity(out,'active_project_image');return out;}\n",
    "function multiViewChannelSourceMatchesPane(src,pane){if(typeof channelSourceMatchesActive!=='function'||typeof projectItems==='undefined'||!Array.isArray(projectItems)||projectItems.length<=1)return typeof channelSourceMatchesActive==='function'?channelSourceMatchesActive(src):true;src=typeof normaliseChannelSource==='function'?normaliseChannelSource(src):src;const active=multiViewChannelIdentitySet(pane),targets=typeof channelExplicitTargets==='function'?channelExplicitTargets(src):new Set();if(targets&&targets.size)return Array.from(targets).some(v=>active.has(v));const sourceIds=typeof channelIdentitySetFrom==='function'?channelIdentitySetFrom(src):new Set(),meta=(src&&src.metadata)||{};['source_path','path','tile_source_id'].forEach(key=>{if(typeof channelPushIdentity==='function')channelPushIdentity(sourceIds,meta[key]);});return Array.from(sourceIds).some(v=>active.has(v));}\n",
    "function multiViewSetChannelItemSettings(pane,src){if(!pane||!pane.channelItems)return;src=typeof normaliseChannelSource==='function'?normaliseChannelSource(src):src;const item=pane.channelItems.get(src.id);if(item&&typeof item.setOpacity==='function'){const active=multiViewChannelSourceMatchesPane(src,pane),canvasFilter=typeof channelMaskCanvasFilterActive==='function'&&channelMaskCanvasFilterActive(src);item.setOpacity((!active||src.visible===false||canvasFilter)?0:clamp(Number(src.opacity??1),0,1));}}\n",
    "function multiViewRemoveChannelItem(pane,id){if(!pane||!pane.channelItems)return;id=String(id||'');if(pane.channelPendingItems)pane.channelPendingItems.delete(id);const item=pane.channelItems.get(id);if(item&&pane.viewer&&pane.viewer.world&&typeof pane.viewer.world.removeItem==='function'){try{pane.viewer.world.removeItem(item);}catch(e){}}pane.channelItems.delete(id);}\n",
    "function multiViewClearChannelItems(pane){if(!pane||!pane.channelItems)return;Array.from(pane.channelItems.keys()).forEach(id=>multiViewRemoveChannelItem(pane,id));if(pane.channelPendingItems)pane.channelPendingItems.clear();}\n",
    "function multiViewUpsertChannelSource(pane,src){if(!pane||!pane.viewer||typeof channelTileSource!=='function')return;src=typeof normaliseChannelSource==='function'?normaliseChannelSource(src):src;if(!src)return;const id=String(src.id||''),canvasFilter=typeof channelMaskCanvasFilterActive==='function'&&channelMaskCanvasFilterActive(src);if(pane.channelItems&&pane.channelItems.has(id)){multiViewSetChannelItemSettings(pane,src);return;}if(pane.channelPendingItems&&pane.channelPendingItems.has(id))return;const tileSource=channelTileSource(src);if(!tileSource||typeof pane.viewer.addTiledImage!=='function')return;if(!pane.channelItems)pane.channelItems=new Map();if(!pane.channelPendingItems)pane.channelPendingItems=new Set();pane.channelPendingItems.add(id);const opts=Object.assign({tileSource:tileSource,opacity:(!multiViewChannelSourceMatchesPane(src,pane)||src.visible===false||canvasFilter)?0:clamp(Number(src.opacity??1),0,1),success:event=>{const pending=pane.channelPendingItems&&pane.channelPendingItems.has(id);if(pane.channelPendingItems)pane.channelPendingItems.delete(id);const latest=typeof channelSourceById==='function'&&typeof normaliseChannelSource==='function'?normaliseChannelSource(channelSourceById(id)||src):src;if(!pending||!latest||latest.visible===false||!multiViewChannelSourceMatchesPane(latest,pane)){try{if(pane.viewer&&pane.viewer.world&&typeof pane.viewer.world.removeItem==='function')pane.viewer.world.removeItem(event.item);}catch(e){}return;}pane.channelItems.set(id,event.item);multiViewSetChannelItemSettings(pane,latest);},error:()=>{pane.channelPendingItems.delete(id);recordViewerLog('Multi-view channel failed to load.','warning',{pane:multiViewPanes.indexOf(pane)+1,id:id,name:src.name||id},'multi-view');}},typeof channelPlacementOptions==='function'?channelPlacementOptions(src):{});pane.viewer.addTiledImage(opts);}\n",
    "function syncMultiViewChannelSourcesForPane(pane){if(!pane||!Array.isArray(channelSources))return;if(pane.blank){multiViewClearChannelItems(pane);return;}const activeIds=new Set();channelSources.map(src=>typeof normaliseChannelSource==='function'?normaliseChannelSource(src):src).filter(Boolean).forEach(src=>{if(multiViewChannelSourceMatchesPane(src,pane)){activeIds.add(src.id);multiViewUpsertChannelSource(pane,src);}else multiViewRemoveChannelItem(pane,src.id);});if(pane.channelItems)Array.from(pane.channelItems.keys()).forEach(id=>{if(!activeIds.has(id))multiViewRemoveChannelItem(pane,id);});}\n",
    "function syncMultiViewChannelSources(){if(multiViewLayout<=1||!multiViewPanes.length)return;multiViewPanes.forEach(syncMultiViewChannelSourcesForPane);}\n",
    "function applyMultiViewImageTransform(){if(!Array.isArray(multiViewPanes)||!multiViewPanes.length||typeof effectiveOpenSeadragonTransform!=='function')return false;const t=effectiveOpenSeadragonTransform();multiViewPanes.forEach(pane=>{try{const vp=pane&&pane.viewer&&pane.viewer.viewport;if(!vp)return;if(typeof vp.setRotation==='function')vp.setRotation(t.rotation,false);if(typeof vp.setFlip==='function')vp.setFlip(t.flip);if(pane.viewer&&typeof pane.viewer.forceRedraw==='function')pane.viewer.forceRedraw();}catch(e){}});drawMultiViewOverlays();return true;}\n",
    "function multiViewSetSourceMetadata(source){source=source||{};const w=Number(source.width||source.slide_width||cfg.slide_width),h=Number(source.height||source.slide_height||cfg.slide_height);if(Number.isFinite(w)&&w>0)cfg.slide_width=w;if(Number.isFinite(h)&&h>0)cfg.slide_height=h;const mpp=Number(source.mpp||source.mpp_x||source.microns_per_pixel);if(Number.isFinite(mpp)&&mpp>0){cfg.mpp=mpp;cfg.mpp_x=Number(source.mpp_x||mpp);cfg.mpp_y=Number(source.mpp_y||mpp);}if(source.objective_power)cfg.objective_power=source.objective_power;}\n",
    "function multiViewFocusPane(index,refresh=true){index=clamp(Number(index)||0,0,Math.max(0,multiViewPanes.length-1));const pane=multiViewPanes[index]||null;setMultiViewActive(index,true);if(pane&&pane.entry)multiViewSetSourceMetadata(multiViewSourceForPane(pane));if(refresh)drawMultiViewOverlays();return true;}\n",
    "function multiViewActivatePane(index,refresh=true){index=clamp(Number(index)||0,0,Math.max(0,multiViewPanes.length-1));const pane=multiViewPanes[index]||null;multiViewFocusPane(index,false);if(!pane||!pane.entry||typeof projectItems==='undefined'||!Array.isArray(projectItems)||!projectItems.length){if(refresh)drawMultiViewOverlays();return true;}if(pane.entry.itemIndex===activeProjectIndex&&pane.entry.sectionIndex===activeProjectSectionIndex){multiViewSetSourceMetadata(multiViewSourceForPane(pane));if(refresh)drawMultiViewOverlays();return true;}if(typeof saveActiveProjectAnnotations==='function')saveActiveProjectAnnotations();activeProjectIndex=Number(pane.entry.itemIndex);activeProjectSectionIndex=Number.isFinite(Number(pane.entry.sectionIndex))?Number(pane.entry.sectionIndex):-1;multiViewSetSourceMetadata(multiViewSourceForPane(pane));if(typeof loadProjectAnnotations==='function')loadProjectAnnotations(false);if(typeof applyProjectPayloads==='function')applyProjectPayloads(pane.entry.item,pane.entry.section);if(typeof renderProjectPanel==='function')renderProjectPanel();if(typeof buildRoiList==='function')buildRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateButtons==='function')updateButtons();if(refresh)drawMultiViewOverlays();return true;}\n",
    "function multiViewEnsureEditingContext(pane,index){if(!pane||!pane.entry)return true;if(pane.entry.itemIndex===activeProjectIndex&&pane.entry.sectionIndex===activeProjectSectionIndex)return true;return multiViewActivatePane(index,false);}\n",
    "function multiViewSlideSize(pane){const source=multiViewSourceForPane(pane)||{},item=(pane&&pane.entry&&pane.entry.item)||{};return {width:Number(source.width||item.width||cfg.slide_width||1),height:Number(source.height||item.height||cfg.slide_height||1)};}\n",
    "function multiViewPaneState(pane){if(!pane||!pane.entry||typeof projectItems==='undefined'||!Array.isArray(projectItems)||!projectItems.length)return {rois:rois,selectedRoi:selectedRoi,trajectories:trajectories,selectedTrajectory:selectedTrajectory,measures:measures,selectedMeasure:selectedMeasure};const active=pane.entry.itemIndex===activeProjectIndex&&pane.entry.sectionIndex===activeProjectSectionIndex;if(active)return {rois:rois,selectedRoi:selectedRoi,trajectories:trajectories,selectedTrajectory:selectedTrajectory,measures:measures,selectedMeasure:selectedMeasure};try{const key=projectAnnotationKey(pane.entry.itemIndex,pane.entry.sectionIndex),state=projectAnnotationStore.get(key);return state||{rois:[],selectedRoi:-1,trajectories:[],selectedTrajectory:-1,measures:[],selectedMeasure:-1};}catch(e){return {rois:[],selectedRoi:-1,trajectories:[],selectedTrajectory:-1,measures:[],selectedMeasure:-1};}}\n",
    "function resizeMultiViewOverlay(pane){if(!pane||!pane.overlay)return null;const rect=pane.element&&pane.element.getBoundingClientRect?pane.element.getBoundingClientRect():null;if(!rect||rect.width<2||rect.height<2)return null;const dpr=window.devicePixelRatio||1,w=Math.max(1,Math.round(rect.width*dpr)),h=Math.max(1,Math.round(rect.height*dpr));if(pane.overlay.width!==w)pane.overlay.width=w;if(pane.overlay.height!==h)pane.overlay.height=h;pane.overlay.style.width=rect.width+'px';pane.overlay.style.height=rect.height+'px';const cx=pane.overlay.getContext('2d');cx.setTransform(dpr,0,0,dpr,0,0);cx.clearRect(0,0,rect.width,rect.height);return {ctx:cx,rect:rect};}\n",
    "function multiViewPanePixelRect(pane){const node=(pane&&pane.overlay)||multiViewPaneElement(pane);return node&&node.getBoundingClientRect?node.getBoundingClientRect():null;}\n",
    "function multiViewOverlayPixelForOsdDisplay(p,pane){const rect=multiViewPanePixelRect(pane);if(typeof osdDisplayPixelForOverlayInRect==='function')return osdDisplayPixelForOverlayInRect(p,rect&&rect.width,rect&&rect.height);if(typeof osdDisplayPixelForOverlay==='function')return osdDisplayPixelForOverlay(p);return {x:Number(p.x),y:Number(p.y)};}\n",
    "function multiViewSlideToCanvas(p,pane){if(!p)return {x:NaN,y:NaN};if(!pane||!pane.viewer||!pane.viewer.viewport){if(typeof slideToViewImagePoint==='function'){const q=slideToViewImagePoint(p);return {x:q.x,y:q.y};}return {x:Number(p.x),y:Number(p.y)};}try{const item=pane.viewer.world&&pane.viewer.world.getItemAt(0);if(item&&typeof item.imageToViewportCoordinates==='function'){const vp=item.imageToViewportCoordinates(Number(p.x),Number(p.y)),px=pane.viewer.viewport.pixelFromPoint(vp,true);return multiViewOverlayPixelForOsdDisplay({x:px.x,y:px.y},pane);}}catch(e){}if(typeof slideToViewImagePoint==='function'){const q=slideToViewImagePoint(p);return {x:q.x,y:q.y};}return {x:Number(p.x),y:Number(p.y)};}\n",
    "function multiViewCanvasPoint(evt,pane){const rect=multiViewPanePixelRect(pane);return rect?{x:evt.clientX-rect.left,y:evt.clientY-rect.top}:null;}\n",
    "function multiViewPointerToSlide(evt,pane){const rect=multiViewPanePixelRect(pane);if(!rect)return {x:0,y:0};const px=evt.clientX-rect.left,py=evt.clientY-rect.top;let out=null;try{const item=pane.viewer&&pane.viewer.world&&pane.viewer.world.getItemAt(0);if(item&&pane.viewer.viewport){const q=multiViewOverlayPixelForOsdDisplay({x:px,y:py},pane),vp=pane.viewer.viewport.pointFromPixel(new OpenSeadragon.Point(q.x,q.y),true),img=item.viewportToImageCoordinates(vp);out={x:img.x,y:img.y};}}catch(e){}if(!out&&typeof viewToImagePoint==='function'&&typeof imageToSlide==='function')out=imageToSlide(viewToImagePoint({x:px,y:py}));if(!out)out={x:px,y:py};return (typeof normaliseSlidePoint==='function')?normaliseSlidePoint(out,multiViewSlideSize(pane)):out;}\n",
    "function multiViewCanvasUnitScale(pane){try{const viewer=pane&&pane.viewer,item=viewer&&viewer.world&&viewer.world.getItemAt(0),vp=viewer&&viewer.viewport;if(item&&vp&&typeof vp.deltaPointsFromPixels==='function'&&typeof item.viewportToImageCoordinates==='function'){const center=vp.getCenter(true),delta=vp.deltaPointsFromPixels(new OpenSeadragon.Point(1,0),true),p0=item.viewportToImageCoordinates(center),p1=item.viewportToImageCoordinates(new OpenSeadragon.Point(center.x+delta.x,center.y+delta.y)),slidePerPx=Math.hypot(p1.x-p0.x,p1.y-p0.y),px=1/slidePerPx;if(Number.isFinite(px)&&px>.0001)return px;}const a=multiViewSlideToCanvas({x:0,y:0},pane),b=multiViewSlideToCanvas({x:1,y:0},pane),fallbackPx=Math.hypot(b.x-a.x,b.y-a.y);return Number.isFinite(fallbackPx)&&fallbackPx>.0001?fallbackPx:1;}catch(e){return 1;}}\n",
    "function refreshMultiViewOverlaysSoon(){drawMultiViewOverlays();if(window.requestAnimationFrame)window.requestAnimationFrame(drawMultiViewOverlays);setTimeout(drawMultiViewOverlays,80);setTimeout(drawMultiViewOverlays,180);}\n",
    "function multiViewDrawPathRings(cx,pane,rings){(rings||[]).forEach(ring=>{(ring||[]).forEach((p,j)=>{const q=multiViewSlideToCanvas(p,pane);if(j===0)cx.moveTo(q.x,q.y);else cx.lineTo(q.x,q.y);});cx.closePath();});}\n",
    "function multiViewRoiGroups(roi){try{if(typeof roiDrawGroups==='function')return roiDrawGroups(roi);}catch(e){}return [{rings:(roi&&roi.rings)||[],holes:[]}];}\n",
    "function multiViewRoiLabelPoint(roi,pane){const b=(typeof roiBounds==='function')?roiBounds(roi):(roi&&roi.bbox);if(b)return multiViewSlideToCanvas({x:(Number(b.xmin)+Number(b.xmax))/2,y:(Number(b.ymin)+Number(b.ymax))/2},pane);const ring=roi&&roi.rings&&roi.rings[0];return ring&&ring[0]?multiViewSlideToCanvas(ring[0],pane):null;}\n",
    "function multiViewLabelCandidates(anchor,w,h,rect){const gap=14,near=h+gap,far=h*2+gap,offsets=[[0,-h/2],[0,-near],[0,gap],[w/2+gap,-h/2],[-w/2-gap,-h/2],[w/2+gap,gap],[-w/2-gap,gap],[w/2+gap,-near],[-w/2-gap,-near],[0,-far],[0,h+gap],[w+gap,-h/2],[-w-gap,-h/2],[w+gap,gap],[-w-gap,gap]];const seen=new Set(),rw=Math.max(12,rect&&rect.width||innerWidth),rh=Math.max(12,rect&&rect.height||innerHeight);return offsets.map((o,rank)=>{const x=clamp(anchor.x+o[0]-w/2,6,Math.max(6,rw-w-6)),y=clamp(anchor.y+o[1],6,Math.max(6,rh-h-6)),key=Math.round(x)+'|'+Math.round(y);if(seen.has(key))return null;seen.add(key);return {x:x,y:y,w:w,h:h,rank:rank};}).filter(Boolean);}\n",
    "function multiViewPlaceLabel(anchor,w,h,occupied,rect){const score=c=>Math.hypot(c.x+c.w/2-anchor.x,c.y+c.h/2-anchor.y)+(c.rank||0)*4,candidates=multiViewLabelCandidates(anchor,w,h,rect).sort((a,b)=>score(a)-score(b));for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,10)))return c;}for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,2)))return c;}return null;}\n",
    "function multiViewDrawLabelLeader(cx,item,r,colour){if(!item.anchor)return;const t={x:clamp(item.anchor.x,r.x,r.x+r.w),y:clamp(item.anchor.y,r.y,r.y+r.h)},dist=Math.hypot(t.x-item.anchor.x,t.y-item.anchor.y);if(dist<8)return;cx.save();cx.strokeStyle='rgba(0,0,0,.84)';cx.lineWidth=3;cx.beginPath();cx.moveTo(item.anchor.x,item.anchor.y);cx.lineTo(t.x,t.y);cx.stroke();cx.strokeStyle=colour;cx.lineWidth=1.6;cx.beginPath();cx.moveTo(item.anchor.x,item.anchor.y);cx.lineTo(t.x,t.y);cx.stroke();cx.fillStyle=colour;cx.strokeStyle='rgba(0,0,0,.9)';cx.lineWidth=1;cx.beginPath();cx.arc(item.anchor.x,item.anchor.y,2.8,0,Math.PI*2);cx.fill();cx.stroke();cx.restore();}\n",
    "function multiViewDrawPlacedLabel(cx,item,rect){const r=crispLabelRect(rect),colour=item.colour||'#5eead4';cx.save();multiViewDrawLabelLeader(cx,item,r,colour);cx.font='700 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';cx.textBaseline='middle';cx.shadowColor='rgba(0,0,0,.55)';cx.shadowBlur=4;cx.fillStyle='rgba(0,0,0,.92)';cx.fillRect(r.x,r.y,r.w,r.h);cx.shadowBlur=0;cx.strokeStyle='rgba(255,255,255,.86)';cx.lineWidth=2.2;cx.strokeRect(r.x+.5,r.y+.5,Math.max(1,r.w-1),Math.max(1,r.h-1));cx.strokeStyle=colour;cx.lineWidth=1.4;cx.strokeRect(r.x+2.5,r.y+2.5,Math.max(1,r.w-5),Math.max(1,r.h-5));cx.fillStyle=colour;cx.fillRect(r.x+1,r.y+1,6,Math.max(1,r.h-2));cx.fillStyle='#fff';cx.fillText(item.text,r.x+13,r.y+r.h/2);cx.restore();}\n",
    "function multiViewDrawLabels(cx,items,rect){const occupied=[];items.sort((a,b)=>(b.priority||0)-(a.priority||0));items.forEach(item=>{const placed=multiViewPlaceLabel(item.anchor,item.w,item.h,occupied,rect);if(!placed)return;occupied.push(placed);multiViewDrawPlacedLabel(cx,item,placed);});}\n",
    "function multiViewDrawRoiSet(cx,pane,state,rect){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('annotations'))return;const list=(state&&state.rois)||[];if(!showRois||!list.length)return;cx.save();cx.lineWidth=2;cx.font='600 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';cx.textBaseline='top';const highlightActive=typeof annotationHighlightActive==='function'&&annotationHighlightActive(),labelItems=[];list.forEach((roi,i)=>{const visible=(typeof visibleRoi==='function')?visibleRoi(roi):!(roi&&roi.visible===false),drawable=(typeof isDrawable==='function')?isDrawable(roi):(roi&&roi.rings&&roi.rings.length);if(!visible||!drawable)return;const selected=i===Number(state.selectedRoi),highlighted=typeof roiClassHighlighted==='function'&&roiClassHighlighted(roi),dimmed=highlightActive&&!highlighted,groups=multiViewRoiGroups(roi);groups.forEach(group=>{cx.beginPath();multiViewDrawPathRings(cx,pane,group.rings);multiViewDrawPathRings(cx,pane,group.holes);cx.globalAlpha=dimmed?Math.min(.12,roiOpacity*.35):roiOpacity;cx.fillStyle=roi.fill||hexToRgba(roi.colour||'#5eead4',.16);cx.fill('evenodd');cx.globalAlpha=dimmed?0.32:1;cx.strokeStyle=highlighted?'rgba(255,255,255,.96)':'rgba(0,0,0,.72)';cx.lineWidth=highlighted?8:(selected?6:4);cx.stroke();cx.strokeStyle=selected?'#ffffff':(((typeof lockedRoi==='function')&&lockedRoi(roi))?'#facc15':(roi.colour||'#5eead4'));cx.lineWidth=highlighted?4:(selected?3:2);if(highlighted){cx.shadowColor=roi.colour||'#5eead4';cx.shadowBlur=6;}cx.stroke();cx.shadowBlur=0;cx.globalAlpha=1;});if(showLabels&&!dimmed){const label=multiViewRoiLabelPoint(roi,pane),text=(typeof roiLabelText==='function')?roiLabelText(roi,i):(roi.name||roi.id||('ROI '+(i+1)));if(label&&text){const colour=roi.colour||'#5eead4',w=cx.measureText(text).width+22,h=24;labelItems.push({anchor:label,text:text,w:w,h:h,colour:colour,priority:highlighted?20:(selected?10:0)});}}});if(showLabels)multiViewDrawLabels(cx,labelItems,rect);cx.restore();}\n",
    "function multiViewVisibleSlideBounds(pane){const node=multiViewPaneElement(pane),rect=node&&node.getBoundingClientRect?node.getBoundingClientRect():null;if(!rect)return {xmin:0,ymin:0,xmax:Number(cfg.slide_width||1),ymax:Number(cfg.slide_height||1)};try{const item=pane.viewer&&pane.viewer.world&&pane.viewer.world.getItemAt(0),vp=pane.viewer&&pane.viewer.viewport;if(item&&vp){const pts=[[0,0],[rect.width,0],[rect.width,rect.height],[0,rect.height]].map(xy=>{const q=multiViewOverlayPixelForOsdDisplay({x:xy[0],y:xy[1]},pane);return item.viewportToImageCoordinates(vp.pointFromPixel(new OpenSeadragon.Point(q.x,q.y),true));});const xs=pts.map(p=>p.x),ys=pts.map(p=>p.y);return {xmin:Math.min(...xs),ymin:Math.min(...ys),xmax:Math.max(...xs),ymax:Math.max(...ys)};}}catch(e){}return {xmin:0,ymin:0,xmax:Number(cfg.slide_width||1),ymax:Number(cfg.slide_height||1)};}\n",
    "function multiViewLayerItemMatchesPane(item,layer,pane){if(typeof projectItems==='undefined'||!Array.isArray(projectItems)||!projectItems.length)return true;if(typeof layerItemHasProjectScope==='function'&&!layerItemHasProjectScope(item))return true;const entry=pane&&pane.entry;if(!entry)return typeof layerItemMatchesActiveProject==='function'?layerItemMatchesActiveProject(item,layer):true;const key=typeof layerScopeText==='function'?layerScopeText(item.project_key||item.wsi_project_key):String(item.project_key||item.wsi_project_key||'').trim();if(key&&typeof projectAnnotationKey==='function')return key===String(projectAnnotationKey(Number(entry.itemIndex),Number(entry.sectionIndex)));const imageIndex=typeof layerScopeNumber==='function'?layerScopeNumber(item.project_image_index):null,sectionIndex=typeof layerScopeNumber==='function'?layerScopeNumber(item.project_section_index):null;if(imageIndex!==null&&imageIndex!==Number(entry.itemIndex))return false;if(sectionIndex!==null&&sectionIndex!==Number(entry.sectionIndex))return false;const text=typeof layerScopeText==='function'?layerScopeText:(v=>String(v??'').trim()),itemObj=entry.item||{},section=entry.section||null,imageText=text(item.project_image||item.image_id||item.sample_id),sectionText=text(item.project_section||item.section_id);if(imageText){const imageValues=[itemObj.label,itemObj.id,itemObj.path].map(text).filter(Boolean);if(imageValues.length&&!imageValues.includes(imageText))return false;}if(sectionText){const sectionValues=section?[section.label,section.id,section.scene].map(text).filter(Boolean):['image'];if(sectionValues.length&&!sectionValues.includes(sectionText))return false;}return true;}\n",
    "function multiViewDrawVectorLayer(cx,pane,layer,layerIndex=-1){const items=layer&&layer.items||[];if(!items.length)return;cx.save();items.forEach((item,itemIndex)=>{if(item.visible===false||!multiViewLayerItemMatchesPane(item,layer,pane))return;const opacity=typeof layerOpacity==='function'?layerOpacity(layer):1,colour=item.colour||layer.colour||'#38bdf8',selected=typeof layerObjectSelected==='function'&&layerObjectSelected(layerIndex,itemIndex);if(typeof layerItemIsPoint==='function'&&layerItemIsPoint(item)){const q=multiViewSlideToCanvas({x:Number(item.x),y:Number(item.y)},pane),r=Math.max(2,Number(item.radius||layer.radius||6)*multiViewCanvasUnitScale(pane));if(!Number.isFinite(q.x)||!Number.isFinite(q.y)||q.x+r<0||q.y+r<0||q.x-r>pane.overlay.width||q.y-r>pane.overlay.height)return;cx.globalAlpha=opacity;cx.beginPath();cx.arc(q.x,q.y,r,0,Math.PI*2);cx.fillStyle=item.fill||hexToRgba(colour,.28);cx.strokeStyle=selected?'#ffffff':colour;cx.lineWidth=selected?3:1.5;cx.fill();cx.stroke();if(selected){cx.globalAlpha=1;cx.beginPath();cx.arc(q.x,q.y,r+5,0,Math.PI*2);cx.strokeStyle='#facc15';cx.lineWidth=2;cx.stroke();}cx.globalAlpha=1;return;}if(typeof isDrawable!=='function'||!isDrawable(item))return;const groups=multiViewRoiGroups(item);groups.forEach(group=>{cx.beginPath();multiViewDrawPathRings(cx,pane,group.rings);multiViewDrawPathRings(cx,pane,group.holes);cx.globalAlpha=opacity;cx.fillStyle=item.fill||hexToRgba(colour,.12);cx.strokeStyle=selected?'#ffffff':colour;cx.lineWidth=selected?Math.max(3,Number(layer.line_width||item.line_width||2)+2):Number(layer.line_width||item.line_width||2);cx.fill('evenodd');cx.stroke();if(selected){cx.globalAlpha=1;cx.strokeStyle='#facc15';cx.lineWidth=1.5;cx.stroke();}cx.globalAlpha=1;});});cx.restore();}\n",
    "function multiViewDrawHeatmapLayer(cx,pane,layer){const values=layer.values||[],rows=values.length,cols=rows?(values[0]||[]).length:0,ext=layer.extent||{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};if(!rows||!cols||typeof heatmapLayerColour!=='function')return;const cw=(Number(ext.xmax)-Number(ext.xmin))/cols,ch=(Number(ext.ymax)-Number(ext.ymin))/rows;cx.save();for(let r=0;r<rows;r++){const row=values[r]||[];for(let c=0;c<cols;c++){const color=heatmapLayerColour(layer,Number(row[c]));if(!color)continue;const p0=multiViewSlideToCanvas({x:Number(ext.xmin)+c*cw,y:Number(ext.ymin)+r*ch},pane),p1=multiViewSlideToCanvas({x:Number(ext.xmin)+(c+1)*cw,y:Number(ext.ymin)+(r+1)*ch},pane),x=Math.min(p0.x,p1.x),y=Math.min(p0.y,p1.y),w=Math.abs(p1.x-p0.x),h=Math.abs(p1.y-p0.y);if(!Number.isFinite(x)||!Number.isFinite(y)||x>pane.overlay.width||y>pane.overlay.height||x+w<0||y+h<0)continue;cx.fillStyle=color;cx.fillRect(Math.floor(x),Math.floor(y),Math.ceil(w)+1,Math.ceil(h)+1);}}cx.restore();}\n",
    "function multiViewDrawImageLayer(cx,pane,layer){if(!layer||!layer.data_uri)return;const ext=layer.extent||{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};if(!layer._multiViewImage){const img=new Image();img.onload=()=>drawMultiViewOverlays();img.src=layer.data_uri;layer._multiViewImage=img;return;}if(!layer._multiViewImage.complete)return;const p0=multiViewSlideToCanvas({x:Number(ext.xmin),y:Number(ext.ymin)},pane),p1=multiViewSlideToCanvas({x:Number(ext.xmax),y:Number(ext.ymax)},pane);cx.save();cx.globalAlpha=typeof layerOpacity==='function'?layerOpacity(layer):Number(layer.opacity??1);cx.drawImage(layer._multiViewImage,Math.min(p0.x,p1.x),Math.min(p0.y,p1.y),Math.abs(p1.x-p0.x),Math.abs(p1.y-p0.y));cx.restore();}\n",
    "function multiViewDrawLayers(cx,pane){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('layers'))return;if(!Array.isArray(layers)||!layers.length)return;(layers||[]).forEach((layer,i)=>{if(typeof layerVisible==='function'&&!layerVisible(layer))return;const type=String(layer&&layer.type||'vector').toLowerCase();if(type==='heatmap'||type==='mask')multiViewDrawHeatmapLayer(cx,pane,layer);else if(type==='image')multiViewDrawImageLayer(cx,pane,layer);else if(type==='vector'||type==='points'||type==='markers'||Array.isArray(layer&&layer.items))multiViewDrawVectorLayer(cx,pane,layer,i);});}\n",
    "function multiViewBaseCanvasCandidates(pane){const nodes=[];if(!pane||!pane.element)return nodes;try{if(pane.viewer&&pane.viewer.drawer){const d=pane.viewer.drawer;[d.canvas,d.context&&d.context.canvas,d._canvas,d._context&&d._context.canvas].forEach(node=>{if(node&&node!==pane.overlay&&!nodes.includes(node))nodes.push(node);});}}catch(e){}try{Array.from(pane.element.querySelectorAll('canvas')).forEach(node=>{if(node&&node!==pane.overlay&&!nodes.includes(node))nodes.push(node);});}catch(e){}return nodes.filter(node=>{const r=node.getBoundingClientRect?node.getBoundingClientRect():null;return node.width>0&&node.height>0&&r&&r.width>0&&r.height>0;}).sort((a,b)=>(b.width*b.height)-(a.width*a.height));}\n",
    "let multiViewStainCanvasWarningShown=false;\n",
    "function multiViewApplyStainToPane(cx,pane){if(typeof hasTiledStainChannels==='function'&&hasTiledStainChannels())return false;if(!stainEnabled||!stainOn||!stainInv||!stainChannels.length||typeof applyStainToCanvas!=='function')return false;const bases=multiViewBaseCanvasCandidates(pane);for(const base of bases){try{if(applyStainToCanvas(cx,pane.overlay,base))return true;}catch(e){}}if(!multiViewStainCanvasWarningShown){multiViewStainCanvasWarningShown=true;notify('Stain channel selection needs readable multi-view tiles. Use live dynamic tiles or tiled channel sources for full-resolution multi-view stains.','warning',7000);}return false;}\n",
    "function multiViewDrawTileGrid(cx,pane,rect){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('tile_grid'))return;if(!tileGridVisible||typeof tileGridSize!=='function')return;const b=multiViewVisibleSlideBounds(pane);let step=tileGridSize(),base=step,a=multiViewSlideToCanvas({x:0,y:0},pane),c=multiViewSlideToCanvas({x:step,y:0},pane),spacing=Math.abs(c.x-a.x);while(Number.isFinite(spacing)&&spacing>0&&spacing<22&&step<base*64){step*=2;spacing*=2;}const x0=Math.floor(b.xmin/step)*step,y0=Math.floor(b.ymin/step)*step;cx.save();cx.strokeStyle='rgba(250,204,21,.55)';cx.lineWidth=1;cx.setLineDash([4,5]);cx.beginPath();for(let x=x0;x<=b.xmax+step;x+=step){const p0=multiViewSlideToCanvas({x:x,y:b.ymin},pane),p1=multiViewSlideToCanvas({x:x,y:b.ymax},pane);cx.moveTo(p0.x,p0.y);cx.lineTo(p1.x,p1.y);}for(let y=y0;y<=b.ymax+step;y+=step){const p0=multiViewSlideToCanvas({x:b.xmin,y:y},pane),p1=multiViewSlideToCanvas({x:b.xmax,y:y},pane);cx.moveTo(p0.x,p0.y);cx.lineTo(p1.x,p1.y);}cx.stroke();cx.setLineDash([]);cx.fillStyle='rgba(250,204,21,.9)';cx.font='11px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';cx.fillText('tile grid '+Math.round(tileGridSize())+' px',10,Math.max(14,rect.height-12));cx.restore();}\n",
    "function multiViewMppFromValue(value){if(!value)return null;if(Array.isArray(value)){const x=Number(value[0]),y=Number(value.length>1?value[1]:value[0]);return Number.isFinite(x)&&x>0&&Number.isFinite(y)&&y>0?{x:x,y:y}:null;}if(typeof value==='number'||typeof value==='string'){const n=Number(value);return Number.isFinite(n)&&n>0?{x:n,y:n}:null;}const x=Number(value.x??value.mpp_x??value.microns_per_pixel??value.value),y=Number(value.y??value.mpp_y??value.mpp_x??value.microns_per_pixel??value.value);return Number.isFinite(x)&&x>0&&Number.isFinite(y)&&y>0?{x:x,y:y}:null;}\n",
    "function multiViewPaneMpp(pane){const entry=pane&&pane.entry||null;if(entry&&typeof projectMppValue==='function'){try{const p=projectMppValue(entry.item,entry.section);if(p)return p;}catch(e){}}const source=(typeof multiViewSourceForPane==='function'?multiViewSourceForPane(pane):null)||{};return multiViewMppFromValue(source.mpp||source.pixel_size)||multiViewMppFromValue(source)||multiViewMppFromValue(entry&&entry.section)||multiViewMppFromValue(entry&&entry.item)||multiViewMppFromValue(cfg.mpp)||multiViewMppFromValue({x:cfg.mpp_x||cfg.microns_per_pixel,y:cfg.mpp_y||cfg.mpp_x||cfg.microns_per_pixel});}\n",
    "function multiViewDrawScaleBar(cx,pane,rect){if(!rect||rect.width<90||rect.height<64)return;const mppObj=multiViewPaneMpp(pane),mpp=mppObj?Number(mppObj.x):NaN,slideScale=multiViewCanvasUnitScale(pane);let unavailable=false,barPx,label;if(!Number.isFinite(mpp)||mpp<=0||!Number.isFinite(slideScale)||slideScale<=0){unavailable=true;barPx=clamp(rect.width*.18,70,120);label='scale unavailable';}else{const targetPx=clamp(rect.width*.18,70,150),targetUm=targetPx*mpp/slideScale,niceUm=niceScaleLength(targetUm);barPx=niceUm/mpp*slideScale;if(!Number.isFinite(barPx)||barPx<24){unavailable=true;barPx=clamp(rect.width*.18,70,120);label='scale unavailable';}else{barPx=clamp(barPx,32,Math.max(48,rect.width*.42));label=formatScaleMicrons(niceUm);}}const x=Math.round((rect.width-barPx)/2),y=Math.round(rect.height-34),h=7;cx.save();cx.globalAlpha=unavailable?.72:1;cx.lineCap='square';cx.shadowColor='rgba(0,0,0,.88)';cx.shadowBlur=3;cx.strokeStyle='rgba(255,255,255,.96)';cx.lineWidth=2;cx.beginPath();cx.moveTo(x,y);cx.lineTo(x,y+h);cx.lineTo(x+barPx,y+h);cx.lineTo(x+barPx,y);cx.stroke();cx.shadowBlur=0;cx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';cx.textBaseline='middle';const textWidth=cx.measureText(label).width+16,labelX=clamp(x+barPx/2-textWidth/2,6,Math.max(6,rect.width-textWidth-6)),labelY=y+h+13;cx.fillStyle='rgba(18,18,18,.82)';cx.strokeStyle='rgba(255,255,255,.18)';if(typeof roundRectPath==='function'){roundRectPath(cx,labelX,labelY-9,textWidth,18,9);cx.fill();cx.stroke();}else{cx.fillRect(labelX,labelY-9,textWidth,18);cx.strokeRect(labelX,labelY-9,textWidth,18);}cx.fillStyle='#f8fafc';cx.textAlign='center';cx.fillText(label,labelX+textWidth/2,labelY);cx.restore();}\n",
    "function multiViewDrawDraft(cx,pane){if(multiViewPanes[multiViewActiveIndex]!==pane||!draft.length)return;cx.save();cx.strokeStyle='#facc15';cx.fillStyle='rgba(250,204,21,.18)';cx.lineWidth=2;cx.setLineDash([6,4]);cx.beginPath();draft.forEach((p,i)=>{const q=multiViewSlideToCanvas(p,pane);if(i===0)cx.moveTo(q.x,q.y);else cx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=multiViewSlideToCanvas(lastPointer,pane);cx.lineTo(q.x,q.y);}if(draft.length>2){const q=multiViewSlideToCanvas(draft[0],pane);cx.lineTo(q.x,q.y);cx.fill();}cx.stroke();cx.setLineDash([]);draft.forEach(p=>{const q=multiViewSlideToCanvas(p,pane);cx.beginPath();cx.arc(q.x,q.y,4,0,Math.PI*2);cx.fillStyle='#facc15';cx.fill();cx.strokeStyle='#111';cx.stroke();});cx.restore();}\n",
    "function multiViewBrushPreviewPoint(pane){if(finiteCanvasPoint(multiViewLastCanvasPointer)&&lastPointer&&pointInsideSlide(lastPointer))return multiViewLastCanvasPointer;return lastPointer&&pointInsideSlide(lastPointer)?multiViewSlideToCanvas(lastPointer,pane):null;}\n",
    "function multiViewDrawBrushPreview(cx,pane){if(multiViewPanes[multiViewActiveIndex]!==pane||mode!=='brush')return;const old=multiViewPointerPane;multiViewPointerPane=pane;syncBrushRadiusToZoom();const px=multiViewCanvasUnitScale(pane),state=brushCursorState(),remove=state.subtract,blocked=state.blocked;cx.save();cx.strokeStyle=blocked?'rgba(239,68,68,.95)':(remove?'rgba(248,113,113,.9)':'rgba(34,197,94,.88)');cx.fillStyle=blocked?'rgba(239,68,68,.10)':(remove?'rgba(248,113,113,.2)':'rgba(34,197,94,.16)');cx.lineWidth=Math.max(1,brushRadius*2*px);cx.lineCap='round';cx.lineJoin='round';if(brushPoints.length&&!blocked){cx.beginPath();brushPoints.forEach((p,i)=>{const q=multiViewSlideToCanvas(p,pane);if(i===0)cx.moveTo(q.x,q.y);else cx.lineTo(q.x,q.y);});cx.stroke();}const q=multiViewBrushPreviewPoint(pane);if(q){const glyph=Math.max(6,Math.min(14,brushRadius*px*.45));cx.beginPath();cx.arc(q.x,q.y,Math.max(2,brushRadius*px),0,Math.PI*2);cx.fill();cx.strokeStyle=blocked?'#ef4444':(remove?'#ef4444':'#22c55e');cx.lineWidth=1.5;cx.stroke();cx.lineWidth=2;cx.beginPath();cx.moveTo(q.x-glyph,q.y);cx.lineTo(q.x+glyph,q.y);if(blocked){cx.moveTo(q.x-glyph*.72,q.y-glyph*.72);cx.lineTo(q.x+glyph*.72,q.y+glyph*.72);cx.moveTo(q.x+glyph*.72,q.y-glyph*.72);cx.lineTo(q.x-glyph*.72,q.y+glyph*.72);}else if(!remove){cx.moveTo(q.x,q.y-glyph);cx.lineTo(q.x,q.y+glyph);}cx.stroke();}cx.restore();multiViewPointerPane=old;}\n",
    "function multiViewDrawEditHandles(cx,pane){if(multiViewPanes[multiViewActiveIndex]!==pane||mode!=='edit'||selectedRoi<0||!rois[selectedRoi]||!isDrawable(rois[selectedRoi])||!editableRoi(rois[selectedRoi]))return;const roi=rois[selectedRoi],trajectoryBorder=(typeof isTrajectoryAreaRoi==='function')&&isTrajectoryAreaRoi(roi);cx.save();(roi.rings||[]).forEach((ring,r)=>{const limit=(typeof ringClosed==='function'&&ringClosed(ring))?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=multiViewSlideToCanvas(ring[j],pane),active=activeVertex&&activeVertex.roi===selectedRoi&&activeVertex.ring===r&&activeVertex.point===j;cx.beginPath();cx.arc(q.x,q.y,active?7:(trajectoryBorder?4.8:4),0,Math.PI*2);cx.fillStyle=active?'#facc15':(trajectoryBorder?'#dbeafe':'#ffffff');cx.strokeStyle=trajectoryBorder?'#2563eb':'#111';cx.lineWidth=active?2.4:2;cx.fill();cx.stroke();}});cx.restore();if(typeof drawCurveEditPreview==='function')drawCurveEditPreview(cx,p=>multiViewSlideToCanvas(p,pane));}\n",
    "function multiViewDrawMeasureLine(cx,pane,m,preview=false,selected=false){if(!m||!m.start||!m.end)return;const a=multiViewSlideToCanvas(m.start,pane),b=multiViewSlideToCanvas(m.end,pane);cx.save();cx.strokeStyle=selected?'#ffffff':(preview?'#facc15':'#38bdf8');cx.lineWidth=selected?4:3;cx.beginPath();cx.moveTo(a.x,a.y);cx.lineTo(b.x,b.y);cx.stroke();cx.restore();}\n",
    "function multiViewDrawMeasurements(cx,pane,state){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('measurements'))return;const list=(state&&state.measures)||[];list.forEach((m,i)=>multiViewDrawMeasureLine(cx,pane,m,false,i===Number(state.selectedMeasure)));if(multiViewPanes[multiViewActiveIndex]===pane&&mode==='measure'&&measureStart&&lastPointer&&pointInsideSlide(lastPointer))multiViewDrawMeasureLine(cx,pane,measurementRecord(measureStart,lastPointer),true,false);}\n",
    "function multiViewDrawTrajectoryPath(cx,pane,points,trajectory=null,preview=false){points=(points||[]).filter(p=>p&&Number.isFinite(Number(p.x))&&Number.isFinite(Number(p.y)));if(points.length<2)return;cx.save();cx.lineCap='round';cx.lineJoin='round';cx.beginPath();points.forEach((p,i)=>{const q=multiViewSlideToCanvas(p,pane);if(i===0)cx.moveTo(q.x,q.y);else cx.lineTo(q.x,q.y);});cx.strokeStyle='rgba(255,255,255,.92)';cx.lineWidth=preview?5:6;if(preview)cx.setLineDash([8,5]);cx.stroke();cx.setLineDash([]);cx.beginPath();points.forEach((p,i)=>{const q=multiViewSlideToCanvas(p,pane);if(i===0)cx.moveTo(q.x,q.y);else cx.lineTo(q.x,q.y);});cx.strokeStyle=preview?'#facc15':((trajectory&&trajectory.colour)||'#ef4444');cx.lineWidth=preview?3:3.5;cx.stroke();const controls=(trajectory&&trajectory.control_points)||(multiViewPanes[multiViewActiveIndex]===pane?trajectoryDraft:[]);(controls||[]).forEach((p,i)=>{const q=multiViewSlideToCanvas(p,pane);cx.beginPath();if(i===0)cx.rect(q.x-5,q.y-5,10,10);else cx.arc(q.x,q.y,4,0,Math.PI*2);cx.fillStyle='#eeeeee';cx.strokeStyle=preview?'#facc15':((trajectory&&trajectory.colour)||'#ef4444');cx.lineWidth=2;cx.fill();cx.stroke();});cx.restore();}\n",
    "function multiViewDrawTrajectories(cx,pane,state){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('trajectories'))return;const list=(state&&state.trajectories)||[];list.forEach((t,i)=>multiViewDrawTrajectoryPath(cx,pane,t.points,Object.assign({},t,{colour:i===Number(state.selectedTrajectory)?'#facc15':(t.colour||'#ef4444')}),false));if(multiViewPanes[multiViewActiveIndex]===pane&&mode==='trajectory'&&trajectoryDraft.length)multiViewDrawTrajectoryPath(cx,pane,currentTrajectoryPreview(),{control_points:trajectoryDraft,colour:'#facc15'},true);}\n",
    "function multiViewSyncOverlayClasses(pane){if(!pane||!pane.overlay)return;['selecting','drawing','brushing','editing','measuring','trajectory','screenshot'].forEach(cls=>pane.overlay.classList.remove(cls));pane.overlay.classList.toggle('selecting',mode==='select');pane.overlay.classList.toggle('drawing',mode==='draw');pane.overlay.classList.toggle('brushing',mode==='brush');pane.overlay.classList.toggle('editing',mode==='edit');pane.overlay.classList.toggle('measuring',mode==='measure');pane.overlay.classList.toggle('trajectory',mode==='trajectory');pane.overlay.classList.toggle('screenshot',mode==='screenshot');pane.overlay.classList.toggle('dragging',multiViewPointerPane===pane&&multiViewPaneDragging);}\n",
    "function multiViewNormalizedScreenshotRect(pane){if(!screenshotRect||multiViewScreenshotPane!==pane)return null;const rect=pane&&pane.overlay&&pane.overlay.getBoundingClientRect?pane.overlay.getBoundingClientRect():null,w=rect?rect.width:innerWidth,h=rect?rect.height:innerHeight,x0=clamp(Math.min(screenshotRect.x0,screenshotRect.x1),0,w),y0=clamp(Math.min(screenshotRect.y0,screenshotRect.y1),0,h),x1=clamp(Math.max(screenshotRect.x0,screenshotRect.x1),0,w),y1=clamp(Math.max(screenshotRect.y0,screenshotRect.y1),0,h);return {x:x0,y:y0,width:x1-x0,height:y1-y0};}\n",
    "function multiViewDrawScreenshotSelection(cx,pane){const r=multiViewNormalizedScreenshotRect(pane);if(!r||r.width<1||r.height<1)return;cx.save();cx.fillStyle='rgba(94,234,212,.12)';cx.strokeStyle='#5eead4';cx.lineWidth=2;cx.setLineDash([8,5]);cx.fillRect(r.x,r.y,r.width,r.height);cx.strokeRect(r.x,r.y,r.width,r.height);cx.setLineDash([]);const text=Math.round(r.width)+' x '+Math.round(r.height)+' px';cx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';const tw=cx.measureText(text).width+10,tx=clamp(r.x,4,Math.max(4,(pane.overlay.getBoundingClientRect().width||innerWidth)-tw-4)),ty=clamp(r.y-22,4,Math.max(4,(pane.overlay.getBoundingClientRect().height||innerHeight)-22));cx.fillStyle='rgba(0,0,0,.72)';cx.fillRect(tx,ty,tw,18);cx.fillStyle='#ccfbf1';cx.fillText(text,tx+5,ty+4);cx.restore();}\n",
    "function multiViewDrawCrosshair(cx,pane,rect){if(!showCrosshair||multiViewPanes[multiViewActiveIndex]!==pane||!lastPointer||!pointInsideSlide(lastPointer))return;const q=multiViewSlideToCanvas(lastPointer,pane);cx.save();cx.strokeStyle='rgba(255,255,255,.55)';cx.setLineDash([5,5]);cx.beginPath();cx.moveTo(q.x,0);cx.lineTo(q.x,rect.height);cx.moveTo(0,q.y);cx.lineTo(rect.width,q.y);cx.stroke();cx.restore();}\n",
    "function drawMultiViewOverlays(){if(multiViewLayout<=1||!multiViewPanes.length)return;multiViewPanes.forEach(pane=>{if(!pane||!pane.overlay)return;multiViewSyncOverlayClasses(pane);const pack=resizeMultiViewOverlay(pane);if(!pack||pane.blank)return;const state=multiViewPaneState(pane);multiViewApplyStainToPane(pack.ctx,pane);if(typeof multiViewDrawFilteredMaskChannels==='function')multiViewDrawFilteredMaskChannels(pack.ctx,pane);multiViewDrawLayers(pack.ctx,pane);multiViewDrawTileGrid(pack.ctx,pane,pack.rect);multiViewDrawRoiSet(pack.ctx,pane,state,pack.rect);multiViewDrawDraft(pack.ctx,pane);multiViewDrawBrushPreview(pack.ctx,pane);multiViewDrawEditHandles(pack.ctx,pane);multiViewDrawMeasurements(pack.ctx,pane,state);multiViewDrawTrajectories(pack.ctx,pane,state);multiViewDrawCrosshair(pack.ctx,pane,pack.rect);multiViewDrawScaleBar(pack.ctx,pane,pack.rect);multiViewDrawScreenshotSelection(pack.ctx,pane);});}\n",
    "function multiViewStopEvent(e){if(!e)return;e.preventDefault();e.stopPropagation();if(typeof e.stopImmediatePropagation==='function')e.stopImmediatePropagation();}\n",
    "function multiViewStartScreenshotSelection(evt,pane){evt.preventDefault();multiViewScreenshotPane=pane;const p=multiViewCanvasPoint(evt,pane)||{x:0,y:0};screenshotSelecting=true;screenshotRect={x0:p.x,y0:p.y,x1:p.x,y1:p.y};drawMultiViewOverlays();}\n",
    "function multiViewUpdateScreenshotSelection(evt,pane=multiViewScreenshotPane){if(!screenshotSelecting||!pane)return;const p=multiViewCanvasPoint(evt,pane)||{x:0,y:0};screenshotRect.x1=p.x;screenshotRect.y1=p.y;drawMultiViewOverlays();}\n",
    "function multiViewScreenshotBaseElements(pane){const nodes=[];if(!pane||!pane.element)return nodes;try{if(pane.viewer&&pane.viewer.drawer){const d=pane.viewer.drawer;[d.canvas,d.context&&d.context.canvas,d._canvas,d._context&&d._context.canvas].forEach(node=>{if(node&&!nodes.includes(node))nodes.push(node);});}}catch(e){}try{Array.from(pane.element.querySelectorAll('canvas,img')).forEach(node=>{if(node&&node!==pane.overlay&&!nodes.includes(node))nodes.push(node);});}catch(e){}return nodes.filter(node=>{if(typeof screenshotElementVisible==='function'&&!screenshotElementVisible(node))return false;const r=node.getBoundingClientRect?node.getBoundingClientRect():null,w=node.naturalWidth||node.width,h=node.naturalHeight||node.height;return w>0&&h>0&&r&&r.width>0&&r.height>0;});}\n",
    "function multiViewScreenshotCanvasFromRect(pane,r,includeBase=true,options=null){const dpr=window.devicePixelRatio||1,paneRect=pane.overlay.getBoundingClientRect(),rect={x:r.x+paneRect.left,y:r.y+paneRect.top,width:r.width,height:r.height},out=document.createElement('canvas'),outCtx=out.getContext('2d');out.width=Math.max(1,Math.round(rect.width*dpr));out.height=Math.max(1,Math.round(rect.height*dpr));outCtx.fillStyle='#ffffff';outCtx.fillRect(0,0,out.width,out.height);let drewBase=false;if(includeBase&&typeof drawElementIntoScreenshot==='function')multiViewScreenshotBaseElements(pane).forEach(node=>{drewBase=drawElementIntoScreenshot(outCtx,node,rect,dpr)||drewBase;});const restore=typeof prepareScreenshotOverlayForOptions==='function'?prepareScreenshotOverlayForOptions(options,pane):null;try{if(typeof drawElementIntoScreenshot==='function')drawElementIntoScreenshot(outCtx,pane.overlay,rect,dpr);}finally{if(restore)restore();}out._wsiScreenshotBaseIncluded=!includeBase||drewBase;out._wsiScreenshotPreviewFallback=false;out._wsiScreenshotReadable=typeof canvasIsReadable==='function'?canvasIsReadable(out):true;return out;}\n",
    "function multiViewFinishScreenshotSelection(evt){const pane=multiViewScreenshotPane;if(!screenshotSelecting||!pane)return false;multiViewUpdateScreenshotSelection(evt,pane);const r=multiViewNormalizedScreenshotRect(pane);screenshotSelecting=false;screenshotRect=null;multiViewScreenshotPane=null;if(!r||r.width<8||r.height<8){setMode('pan');notify('Screenshot area too small','warning',1800);drawMultiViewOverlays();return true;}drawMultiViewOverlays();setMode('pan');if(typeof openScreenshotDialog==='function')openScreenshotDialog(r,pane);else notify('Screenshot dialog is unavailable','error',4200);return true;}\n",
    "function multiViewPanViewerByPixels(viewer,dx,dy){if(!viewer||!viewer.viewport)return false;try{const delta=viewer.viewport.deltaPointsFromPixels(new OpenSeadragon.Point(-dx,-dy),true);viewer.viewport.panBy(delta,true);viewer.viewport.applyConstraints(false);return true;}catch(e){return false;}}\n",
    "function multiViewSyncedPanByPixels(sourcePane,dx,dy){if(!multiViewSync||multiViewLayout<=1)return false;multiViewApplying=true;multiViewPanes.forEach(pane=>{if(pane&&pane.viewer)multiViewPanViewerByPixels(pane.viewer,dx,dy);});multiViewApplying=false;if(sourcePane&&sourcePane.viewer&&typeof osdViewer!=='undefined'&&osdViewer&&!multiViewUsesProjectSources()){copyViewportBetween(sourcePane.viewer,osdViewer,true);if(typeof syncViewState==='function')syncViewState();if(typeof requestDraw==='function')requestDraw();}drawMultiViewOverlays();return true;}\n",
    "function multiViewPanByPixels(pane,dx,dy){if(!pane||!pane.viewer||!pane.viewer.viewport)return;try{if(multiViewSyncedPanByPixels(pane,dx,dy))return;multiViewPanViewerByPixels(pane.viewer,dx,dy);drawMultiViewOverlays();}catch(e){}}\n",
    "function multiViewZoomPaneAt(pane,factor,px=null,py=null){if(!pane||pane.blank||!pane.viewer||!pane.viewer.viewport)return false;const viewer=pane.viewer;resizeMultiViewPaneViewer(pane,false);const node=multiViewPaneElement(pane),rect=node&&node.getBoundingClientRect?node.getBoundingClientRect():{width:innerWidth,height:innerHeight},w=Math.max(1,Number(rect.width)||1),h=Math.max(1,Number(rect.height)||1),x=Number.isFinite(Number(px))?clamp(Number(px),0,w):w/2,y=Number.isFinite(Number(py))?clamp(Number(py),0,h):h/2,point=viewer.viewport.pointFromPixel(new OpenSeadragon.Point(x,y),true);viewer.viewport.zoomBy(Number(factor)||1,point,false);viewer.viewport.applyConstraints(false);if(typeof settleOpenSeadragonHome==='function')settleOpenSeadragonHome(viewer,false);return true;}\n",
    "function multiViewZoomAtEvent(e,pane){if(!pane||!pane.viewer||!pane.viewer.viewport)return;const rect=multiViewPanePixelRect(pane);if(!rect)return;const q=multiViewOverlayPixelForOsdDisplay({x:e.clientX-rect.left,y:e.clientY-rect.top},pane),factor=e.deltaY<0?1.2:(1/1.2);if(multiViewZoomPaneAt(pane,factor,q.x,q.y)){if(multiViewSync)syncMultiViewFrom(pane.viewer);refreshMultiViewOverlaysSoon();if(typeof updateMagnificationControls==='function')updateMagnificationControls();if(typeof updateStatus==='function')updateStatus(lastPointer);}}\n",
    "function multiViewFindVertexAt(pane,clientX,clientY){if(!pane||!pane.overlay)return null;const rect=pane.overlay.getBoundingClientRect(),c={x:clientX-rect.left,y:clientY-rect.top},order=selectedRoi>=0?[selectedRoi]:rois.map((_,i)=>i);for(const ri of order){const roi=rois[ri];if(!isDrawable(roi)||!editableRoi(roi))continue;for(let r=0;r<(roi.rings||[]).length;r++){const ring=roi.rings[r],limit=(typeof ringClosed==='function'&&ringClosed(ring))?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=multiViewSlideToCanvas(ring[j],pane);if(Math.hypot(q.x-c.x,q.y-c.y)<=9)return {roi:ri,ring:r,point:j};}}}return null;}\n",
    "function multiViewInsertVertexAt(pane,p,clientX,clientY){if(selectedRoi<0||!isDrawable(rois[selectedRoi])||!editableRoi(rois[selectedRoi])||!pointInsideSlide(p))return false;const rect=pane.overlay.getBoundingClientRect(),c={x:clientX-rect.left,y:clientY-rect.top},roi=rois[selectedRoi];let best=null;(roi.rings||[]).forEach((ring,r)=>{const limit=(typeof ringClosed==='function'&&ringClosed(ring))?ring.length-1:ring.length;for(let j=0;j<limit;j++){const a=multiViewSlideToCanvas(ring[j],pane),b=multiViewSlideToCanvas(ring[(j+1)%ring.length],pane),d=segmentDistance(c,a,b);if(!best||d<best.d)best={d:d,ring:r,after:j};}});if(!best||best.d>14)return false;pushAnnotationUndo('roi_vertex_inserted');const ring=roi.rings[best.ring];ring.splice(best.after+1,0,{x:Math.round(p.x),y:Math.round(p.y)});activeVertex={roi:selectedRoi,ring:best.ring,point:best.after+1};refreshRoiGeometry(roi);buildRoiList();draw();return true;}\n",
    "function multiViewAfterAction(save=true){if(save&&typeof saveActiveProjectAnnotations==='function')saveActiveProjectAnnotations();if(typeof buildRoiList==='function')buildRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof updateButtons==='function')updateButtons();drawMultiViewOverlays();}\n",
    "function multiViewCommitAnnotationAction(save=true){if(save&&typeof saveActiveProjectAnnotations==='function')saveActiveProjectAnnotations();if(typeof buildRoiList==='function')buildRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateButtons==='function')updateButtons();if(typeof updateAnnotationDirtyIndicator==='function')updateAnnotationDirtyIndicator();drawMultiViewOverlays();}\n",
    "function multiViewFinishBrush(){if(!brushing)return false;finishBrush();multiViewCommitAnnotationAction(true);return true;}\n",
    "function multiViewFinishDraft(){const before=rois.length;finishDraft();multiViewCommitAnnotationAction(rois.length!==before||selectedRoi>=0);return true;}\n",
    "function multiViewClearSelection(){clearSelectedAnnotation(false);clearSelectedTrajectory(false);clearSelectedLayerObject(false);clearSelectedMeasure(false);if(typeof buildRoiList==='function')buildRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();if(typeof buildLayerList==='function')buildLayerList();if(typeof updateMeasureList==='function')updateMeasureList();if(typeof updateButtons==='function')updateButtons();draw();}\n",
    "function multiViewPaneMouseDown(e,pane,index){if(e.button&&e.button!==0)return;multiViewStopEvent(e);multiViewPointerPane=pane;multiViewFocusPane(index,true);if(typeof updateMagnificationControls==='function')updateMagnificationControls();if(pane&&pane.blank){notify('Drop a project image or section into this empty custom view pane first.','info',1800);drawMultiViewOverlays();return;}multiViewLastCanvasPointer=multiViewCanvasPoint(e,pane);lastPointer=multiViewPointerToSlide(e,pane);updateCursorFeedback(e);if(mode==='screenshot'){multiViewStartScreenshotSelection(e,pane);return;}const editMode=['draw','trajectory','brush','edit','measure','select'].includes(mode);if(editMode&&!multiViewEnsureEditingContext(pane,index)){notify('Could not activate this pane for editing.','warning',2600);return;}if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);multiViewAfterAction(false);drawMultiViewOverlays();return;}if(mode==='trajectory'){if(e.detail===1)addTrajectoryPoint(lastPointer);multiViewAfterAction(false);return;}if(mode==='brush'){startBrush(lastPointer,e);multiViewAfterAction(false);drawMultiViewOverlays();return;}if(mode==='edit'){const vertex=multiViewFindVertexAt(pane,e.clientX,e.clientY);if(vertex){pushAnnotationUndo(isTrajectoryAreaRoi(rois[vertex.roi])?'trajectory_border_vertex_moved':'roi_vertex_moved');selectAnnotation(vertex.roi,false);activeVertex=prepareVertexDrag(vertex);draggingVertex=activeVertex;multiViewAfterAction(false);return;}if(startCurveEditStroke(multiViewLastCanvasPointer,lastPointer,p=>multiViewSlideToCanvas(p,pane))){multiViewAfterAction(false);return;}selectAnnotation(roiAt(lastPointer),true);multiViewAfterAction(true);return;}if(mode==='measure'){addMeasurePoint(lastPointer);multiViewAfterAction(true);return;}if(mode==='select'){if(!selectObjectAtPoint(lastPointer,'trajectory'))multiViewClearSelection();multiViewAfterAction(true);return;}multiViewPaneDragging=true;multiViewPaneMoved=false;multiViewPaneLastX=e.clientX;multiViewPaneLastY=e.clientY;multiViewPaneDragStartX=e.clientX;multiViewPaneDragStartY=e.clientY;drawMultiViewOverlays();}\n",
    "function multiViewPaneMouseMove(e){if(!multiViewPointerPane)return;multiViewStopEvent(e);const pane=multiViewPointerPane;multiViewLastCanvasPointer=multiViewCanvasPoint(e,pane);lastPointer=multiViewPointerToSlide(e,pane);updateCursorFeedback(e);if(screenshotSelecting&&multiViewScreenshotPane===pane){multiViewUpdateScreenshotSelection(e,pane);return;}if(brushing){addBrushPoint(lastPointer,e);multiViewAfterAction(false);drawMultiViewOverlays();return;}if(curveEditStroke){addCurveEditPoint(lastPointer);multiViewAfterAction(false);return;}if(draggingVertex){moveActiveVertex(lastPointer);multiViewAfterAction(false);return;}if(multiViewPaneDragging){const dx=e.clientX-multiViewPaneLastX,dy=e.clientY-multiViewPaneLastY;if(Math.hypot(e.clientX-multiViewPaneDragStartX,e.clientY-multiViewPaneDragStartY)>3)multiViewPaneMoved=true;if(multiViewPaneMoved)multiViewPanByPixels(pane,dx,dy);multiViewPaneLastX=e.clientX;multiViewPaneLastY=e.clientY;return;}drawMultiViewOverlays();}\n",
    "function multiViewPaneMouseUp(e){if(!multiViewPointerPane)return;multiViewStopEvent(e);const pane=multiViewPointerPane,wasDragging=multiViewPaneDragging,wasClick=wasDragging&&!multiViewPaneMoved;multiViewLastCanvasPointer=multiViewCanvasPoint(e,pane);lastPointer=multiViewPointerToSlide(e,pane);if(screenshotSelecting&&multiViewScreenshotPane===pane){multiViewFinishScreenshotSelection(e);multiViewPointerPane=null;return;}if(brushing)multiViewFinishBrush();if(curveEditStroke)finishCurveEditStroke();if(draggingVertex)finishActiveVertexDrag();if(wasDragging&&multiViewPaneMoved){const panes=multiViewSync?multiViewPanes:[pane];panes.forEach(p=>{try{if(p&&p.viewer&&p.viewer.viewport)p.viewer.viewport.applyConstraints(true);}catch(err){}});if(multiViewSync&&pane&&pane.viewer)syncMultiViewFrom(pane.viewer);}multiViewPaneDragging=false;multiViewPaneMoved=false;if(wasClick&&mode==='pan')selectObjectAtPoint(lastPointer,'trajectory');multiViewAfterAction(true);multiViewPointerPane=null;updateCursorFeedback(e);drawMultiViewOverlays();}\n",
    "function multiViewPaneDblClick(e,pane,index){multiViewStopEvent(e);multiViewPointerPane=pane;multiViewFocusPane(index,true);if(typeof updateMagnificationControls==='function')updateMagnificationControls();multiViewLastCanvasPointer=multiViewCanvasPoint(e,pane);lastPointer=multiViewPointerToSlide(e,pane);const editMode=['draw','trajectory','edit','select'].includes(mode);if(editMode&&!multiViewEnsureEditingContext(pane,index)){notify('Could not activate this pane for editing.','warning',2600);multiViewPointerPane=null;return;}if(mode==='draw'){multiViewFinishDraft();multiViewPointerPane=null;return;}if(mode==='trajectory'){finishTrajectory();multiViewAfterAction(true);multiViewPointerPane=null;return;}if(mode==='edit'){multiViewInsertVertexAt(pane,lastPointer,e.clientX,e.clientY);multiViewAfterAction(true);multiViewPointerPane=null;return;}if(!selectObjectAtPoint(lastPointer,'trajectory'))clearSelectionAndPan();multiViewAfterAction(true);multiViewPointerPane=null;}\n",
    "function bindMultiViewPaneInteractions(paneObj,index){const overlay=paneObj&&paneObj.overlay;if(!overlay)return;overlay.tabIndex=0;overlay.addEventListener('mousedown',e=>multiViewPaneMouseDown(e,paneObj,index),true);overlay.addEventListener('dblclick',e=>multiViewPaneDblClick(e,paneObj,index),true);overlay.addEventListener('wheel',e=>{multiViewStopEvent(e);multiViewFocusPane(index,true);multiViewZoomAtEvent(e,paneObj);},{capture:true,passive:false});overlay.addEventListener('mousemove',e=>{if(multiViewPointerPane)return;multiViewLastCanvasPointer=multiViewCanvasPoint(e,paneObj);lastPointer=multiViewPointerToSlide(e,paneObj);updateCursorFeedback(e);drawMultiViewOverlays();},true);}\n",
    "function scheduleMultiViewPaneRefresh(pane,goHome=true){if(!pane||!pane.viewer)return;requestAnimationFrame(()=>resizeMultiViewPaneViewer(pane,goHome));[80,240,700].forEach(delay=>setTimeout(()=>resizeMultiViewPaneViewer(pane,goHome),delay));}\n",
    "function multiViewPreviewSource(entry){if(!entry)return null;const item=entry.item||null,section=entry.section||null,source=(typeof projectDisplaySource==='function')?projectDisplaySource(item,section):(section||item),imageSource=(source&&source.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri)||(source&&source.navigator_image_data_uri)||(section&&section.navigator_image_data_uri)||(item&&item.navigator_image_data_uri);return imageSource?{type:'image',url:imageSource}:null;}\n",
    "function openMultiViewPanePreviewFallback(pane,entry,reason='tile source did not draw'){if(!pane||!pane.viewer||pane.previewFallbackUsed)return false;const fallback=multiViewPreviewSource(entry||pane.entry);if(!fallback)return false;pane.previewFallbackUsed=true;try{pane.viewer.open(fallback);scheduleMultiViewPaneRefresh(pane,true);recordViewerLog('Multi-view pane switched to preview fallback.','warning',{pane:(multiViewPanes.indexOf(pane)+1),reason:reason,label:(entry&&entry.label)||(pane.entry&&pane.entry.label)||''},'multi-view');notify('Multi-view pane is using preview fallback; tiled source did not draw.','warning',5200);return true;}catch(e){recordViewerLog('Multi-view preview fallback failed.','warning',{reason:reason,error:e.message},'multi-view');return false;}}\n",
    "function syncMultiViewFrom(source){if(!multiViewSync||multiViewApplying||multiViewLayout<=1||!source)return;if((typeof dragging!=='undefined'&&dragging)||(typeof multiViewPaneDragging!=='undefined'&&multiViewPaneDragging))return;multiViewApplying=true;multiViewPanes.forEach(pane=>{if(pane.viewer&&!pane.blank&&pane.viewer!==source)copyViewportBetween(source,pane.viewer,true);});if(typeof osdViewer!=='undefined'&&osdViewer&&source!==osdViewer){copyViewportBetween(source,osdViewer,true);if(typeof syncViewState==='function')syncViewState();if(typeof requestDraw==='function')requestDraw();}multiViewApplying=false;}\n",
    "function multiViewOsdOptions(element,index,count){const roundMode=(OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES&&OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES.ALWAYS)||undefined,loaderLimit=tileImageLoaderLimit();const options={element:element,showNavigationControl:false,showNavigator:false,blendTime:0,alwaysBlend:false,immediateRender:true,placeholderFillStyle:progressivePreviewEnabled()?'rgba(255,255,255,0)':'#fff',subPixelRoundingForTransparency:roundMode,minPixelRatio:1,maxImageCacheCount:Math.max(96,Math.floor(tileCacheCount()/Math.max(1,count))),timeout:tileTimeoutMs(),animationTime:.12,springStiffness:9,visibilityRatio:1,constrainDuringPan:true,minZoomImageRatio:1,maxZoomPixelRatio:16,gestureSettingsMouse:{clickToZoom:false,dblClickToZoom:false,scrollToZoom:true,dragToPan:false},gestureSettingsTouch:{pinchToZoom:true,dragToPan:true},tileSources:multiViewTileSource(index,count)};if(loaderLimit>0)options.imageLoaderLimit=Math.max(1,loaderLimit);return options;}\n",
    "function refreshOpenedMultiViewPane(pane){if(!pane||!pane.viewer)return;scheduleMultiViewPaneRefresh(pane,true);}\n",
    "function replaceMultiViewPane(index,entry,toast=true){index=Number(index);entry=entry||multiViewEntry(index);if(!entry||index<0||index>=multiViewPanes.length)return false;const key=multiViewProjectEntryKey(entry);if(multiViewAssignmentUsed(key,index)){notify('This image is already displayed in another multi-view pane. Choose a different image or section.','warning',4200);recordViewerLog('Duplicate multi-view image was not opened.','info',{pane:index+1,label:entry.label||'',key:key},'multi-view');return false;}const pane=multiViewPanes[index],src=entry.useActiveSource?tileSourceFromConfig():projectTileSourceFromItem(entry.item,entry.section);if(!pane||!pane.viewer||!src){notify('This project image/section cannot be shown in multi-view. Open it from R as a tiled or browser-readable source.','warning',5200);return false;}multiViewAssignments[index]=key;pane.entryKey=multiViewAssignments[index];pane.entry=entry;pane.blank=false;pane.previewFallbackUsed=false;pane.tileFailureNotified=false;multiViewClearChannelItems(pane);if(pane.element){pane.element.classList.remove('blank');const blank=pane.element.querySelector('.multiViewPaneBlank');if(blank)blank.remove();}const title=pane.element&&pane.element.querySelector('.multiViewPaneTitle');if(title)title.textContent=entry.label||('View '+(index+1));setMultiViewActive(index);try{if(typeof pane.viewer.addOnceHandler==='function'){pane.viewer.addOnceHandler('open',()=>{refreshOpenedMultiViewPane(pane);syncMultiViewChannelSourcesForPane(pane);});pane.viewer.addOnceHandler('open-failed',e=>openMultiViewPanePreviewFallback(pane,entry,(e&&e.message)||'open failed'));}pane.viewer.open(src);scheduleMultiViewPaneRefresh(pane,true);setTimeout(()=>{try{if(pane.viewer&&pane.viewer.world&&typeof pane.viewer.world.getItemCount==='function'&&pane.viewer.world.getItemCount()<1)openMultiViewPanePreviewFallback(pane,entry,'no tiled image item opened');else{refreshOpenedMultiViewPane(pane);syncMultiViewChannelSourcesForPane(pane);}}catch(e){}},900);}catch(e){notify('Could not replace multi-view pane: '+e.message,'warning',5200);openMultiViewPanePreviewFallback(pane,entry,e.message);return false;}if(toast){notify('Pane '+(index+1)+' replaced with '+(entry.label||'project image'),'success',1800);scheduleViewerStateSync('multi_view_pane_replaced',{pane:index+1,label:entry.label||null,key:multiViewAssignments[index],layout:multiViewLayout});}return true;}\n",
    "function multiViewDroppedFiles(dt){try{return Array.from((dt&&dt.files)||[]).filter(Boolean);}catch(e){return[];}}\n",
    "function multiViewHasProjectDrag(dt){if(typeof projectDragPayloadCache!=='undefined'&&projectDragPayloadCache)return true;if(typeof projectDragIndex!=='undefined'&&Number(projectDragIndex)>=0)return true;if(!dt)return false;try{const types=Array.from(dt.types||[]);return types.includes('Files')||types.includes('application/x-wsitools-project-entry')||types.includes('text/x-wsitools-project-entry')||types.includes('text/plain')||types.includes('application/x-wsitools-project-reorder');}catch(e){return true;}}\n",
    "function multiViewReadableFile(file){if(!file)return false;const ext=typeof projectFileExtension==='function'?projectFileExtension(file.name):String(file.name||'').toLowerCase().split('.').pop();return typeof projectBrowserReadableExtension==='function'?projectBrowserReadableExtension(ext):['png','jpg','jpeg','webp','gif','bmp','avif'].includes(ext);}\n",
    "function loadMultiViewDroppedFile(index,file){if(!file)return false;if(!multiViewReadableFile(file)){if(typeof addProjectFileReference==='function')addProjectFileReference(file.name,file.type,file.size);notify('Added file reference, but this format needs an R/tiled backend before it can be displayed in multi-view.','warning',5200);return false;}const reader=new FileReader();reader.onerror=()=>notify('Could not read dropped image file','error',4200);reader.onload=()=>{const dataUri=String(reader.result||''),img=new Image();img.onload=()=>{let added=null;if(typeof addProjectImageDataUri==='function')added=addProjectImageDataUri(dataUri,file.name,img.naturalWidth,img.naturalHeight,{activate:false,apply:false,refresh_multi_view:false});if(!added||!added.item){notify('Dropped image was read, but could not be added to the project.','warning',4200);return;}renderProjectPanel();const entry=multiViewEntryFromIndices(added.index,-1)||multiViewProjectEntries().find(x=>x.item===added.item)||multiViewProjectEntries().find(x=>x.itemIndex===added.index);if(entry)replaceMultiViewPane(index,entry,true);else notify('Dropped image was added, but no multi-view source was created.','warning',4200);};img.onerror=()=>notify('Could not decode dropped image in the browser','error',4200);img.src=dataUri;};reader.readAsDataURL(file);return true;}\n",
    "function bindMultiViewPaneDrop(pane,index){const over=e=>{if(!multiViewHasProjectDrag(e.dataTransfer))return;e.preventDefault();e.stopPropagation();pane.classList.add('dropTarget');try{e.dataTransfer.dropEffect='copy';}catch(err){}};const leave=e=>{if(!pane.contains(e.relatedTarget))pane.classList.remove('dropTarget');};const drop=e=>{e.preventDefault();e.stopPropagation();pane.classList.remove('dropTarget');const entry=multiViewDropPayload(e.dataTransfer);if(entry){if(!replaceMultiViewPane(index,entry,true))recordViewerLog('Multi-view pane drop could not open the selected project image/section.','warning',{pane:index+1,label:entry.label||'',item_index:entry.itemIndex,section_index:entry.sectionIndex,drag_cache:!!(typeof projectDragPayloadCache!=='undefined'&&projectDragPayloadCache)},'multi-view');return;}const files=multiViewDroppedFiles(e.dataTransfer);if(files.length){loadMultiViewDroppedFile(index,files[0]);return;}recordViewerLog('Multi-view pane drop did not contain a project image/section.','warning',{pane:index+1,types:e.dataTransfer?Array.from(e.dataTransfer.types||[]):[]},'multi-view');notify('Drop a project image, section, or browser-readable image file onto a multi-view pane.','warning',3600);};pane.addEventListener('dragenter',over,true);pane.addEventListener('dragover',over,true);pane.addEventListener('dragleave',leave,true);pane.addEventListener('drop',drop,true);}\n",
    "function makeMultiViewPane(index,count){const host=multiViewHost(),pane=document.createElement('div'),label=document.createElement('div'),viewerDiv=document.createElement('div'),overlay=document.createElement('canvas'),entry=multiViewEntry(index),blank=multiViewPaneIsBlank(index);pane.className='multiViewPane'+(blank?' blank':'');pane.dataset.index=String(index);label.className='multiViewPaneTitle';label.textContent=multiViewPaneLabel(index);viewerDiv.className='multiViewPaneViewer';overlay.className='multiViewPaneOverlay';pane.append(label,viewerDiv);if(blank){const empty=document.createElement('div');empty.className='multiViewPaneBlank';empty.textContent='Drop a project image or section here';pane.appendChild(empty);}pane.appendChild(overlay);pane.addEventListener('pointerdown',()=>setMultiViewActive(index,true));bindMultiViewPaneDrop(pane,index);host.appendChild(pane);let viewer=null;try{viewer=OpenSeadragon(multiViewOsdOptions(viewerDiv,index,count));}catch(e){notify('Could not create multi-view pane','warning',3600);return {element:pane,overlay:overlay,viewer:null,entryKey:multiViewAssignments[index]||'',entry:entry,blank:blank,tileFailureNotified:false,previewFallbackUsed:false,channelItems:new Map(),channelPendingItems:new Set()};}const paneObj={element:pane,overlay:overlay,viewer:viewer,entryKey:multiViewAssignments[index]||'',entry:entry,blank:blank,tileFailureNotified:false,previewFallbackUsed:false,channelItems:new Map(),channelPendingItems:new Set()};bindMultiViewPaneInteractions(paneObj,index);viewer.addHandler('open',()=>{scheduleMultiViewPaneRefresh(paneObj,true);if(!paneObj.blank){if(multiViewSync&&typeof osdViewer!=='undefined'&&osdViewer&&osdReady&&!multiViewUsesProjectSources())copyViewportBetween(osdViewer,viewer,true);else if(multiViewUsesProjectSources())viewer.viewport.goHome(true);else zoomPaneToSlideBounds(viewer,multiViewInitialBounds(index,count));}if(typeof applyMultiViewImageTransform==='function')applyMultiViewImageTransform();syncMultiViewChannelSourcesForPane(paneObj);drawMultiViewOverlays();});viewer.addHandler('open-failed',e=>openMultiViewPanePreviewFallback(paneObj,entry,(e&&e.message)||'open failed'));viewer.addHandler('tile-load-failed',e=>{if(paneObj.blank||paneObj.tileFailureNotified)return;paneObj.tileFailureNotified=true;const tile=e&&e.tile||{};recordViewerLog('Multi-view tile failed to load.','warning',{pane:index+1,label:entry&&entry.label||'',url:tile.url||'',message:e&&e.message||''},'multi-view');setTimeout(()=>openMultiViewPanePreviewFallback(paneObj,entry,'tile failed to load'),250);});['animation','animation-finish','tile-drawn','tile-loaded','resize'].forEach(name=>viewer.addHandler(name,()=>{if(name==='animation-finish'&&!paneObj.blank)syncMultiViewFrom(viewer);drawMultiViewOverlays();}));scheduleMultiViewPaneRefresh(paneObj,true);return paneObj;}\n",
    "function buildMultiViewPanes(count){const host=multiViewHost();if(!host)return;destroyMultiViewPanes();count=Math.max(1,Math.min(12,Math.round(Number(count)||1)));const cols=multiViewColumns(count),rows=Math.ceil(count/cols);host.className='multiViewGrid '+((!multiViewCustomMode&&[1,2,4,6].includes(count))?('layout'+count):'layoutCustom');host.style.setProperty('--multi-view-count',String(count));host.style.setProperty('--multi-view-cols',String(cols));host.style.setProperty('--multi-view-rows',String(rows));multiViewEnsureFractions(cols,rows);multiViewApplyGridTemplate();multiViewAssignments.length=count;multiViewNormalizeAssignments(count);for(let i=0;i<count;i++)multiViewPanes.push(makeMultiViewPane(i,count));setMultiViewActive(0);requestAnimationFrame(()=>{multiViewApplyGridTemplate();multiViewPanes.forEach(pane=>scheduleMultiViewPaneRefresh(pane,true));});}\n",
    "function setMultiViewLayout(count,silent=false){count=Math.max(1,Math.min(12,Math.round(Number(count)||1)));if(count<=1){if(typeof saveActiveProjectAnnotations==='function')saveActiveProjectAnnotations();destroyMultiViewPanes();multiViewLayout=1;multiViewCustomMode=false;document.body.classList.remove('multiViewActive');if(typeof projectItems!=='undefined'&&Array.isArray(projectItems)&&projectItems.length&&typeof applyProjectPreview==='function')applyProjectPreview(projectItems[activeProjectIndex]||null,typeof activeProjectSection==='function'?activeProjectSection():null);updateMultiViewControls();if(typeof updateScaleBar==='function')updateScaleBar();if(typeof requestDraw==='function')requestDraw();if(!silent)scheduleViewerStateSync('multi_view_layout_updated',{layout:1,sync:multiViewSync});return true;}if(!multiViewSupported()){notify('Multi-view tissue display needs the tiled OpenSeadragon viewer. Open the slide in tiled mode for side-by-side tissue panes.','warning',5200);multiViewLayout=1;multiViewCustomMode=false;updateMultiViewControls();if(typeof updateScaleBar==='function')updateScaleBar();return false;}multiViewLayout=count;document.body.classList.add('multiViewActive');buildMultiViewPanes(count);updateMultiViewControls();if(typeof updateScaleBar==='function')updateScaleBar();if(!silent){notify('Multi-view tissue display: '+count+' pane'+(count===1?'':'s'),'success',1800);scheduleViewerStateSync('multi_view_layout_updated',{layout:multiViewLayout,sync:multiViewSync,custom:multiViewCustomMode,assignments:multiViewAssignments.slice()});}return true;}\n",
    "function refreshMultiViewSources(){if(multiViewLayout>1)setTimeout(()=>setMultiViewLayout(multiViewLayout,true),0);}\n",
    "function activeMultiViewPane(){if(multiViewLayout<=1||!multiViewPanes.length)return null;const pinned=clamp(Number(multiViewControlPaneIndex)||0,0,Math.max(0,multiViewPanes.length-1)),pane=multiViewPanes[pinned];if(pane&&pane.viewer&&!pane.blank)return pane;const active=multiViewPanes[clamp(Number(multiViewActiveIndex)||0,0,Math.max(0,multiViewPanes.length-1))];if(active&&active.viewer&&!active.blank)return active;return multiViewPanes.find(p=>p&&p.viewer&&!p.blank)||null;}\n",
    "function activeMultiViewViewer(){const pane=activeMultiViewPane();return pane&&pane.viewer?pane.viewer:null;}\n",
    "function multiViewTargetPanes(){if(multiViewLayout<=1)return[];if(multiViewSync)return multiViewPanes.filter(p=>p&&p.viewer&&p.viewer.viewport&&!p.blank);const pane=activeMultiViewPane();return pane?[pane]:multiViewPanes.filter(p=>p&&p.viewer&&p.viewer.viewport&&!p.blank);}\n",
    "function multiViewTargets(){return multiViewTargetPanes().map(p=>p.viewer).filter(Boolean);}\n",
    "function multiViewZoomAt(factor){const panes=multiViewTargetPanes();if(!panes.length)return false;let changed=false;panes.forEach(pane=>{changed=multiViewZoomPaneAt(pane,factor)||changed;});if(changed&&multiViewSync&&panes[0]&&panes[0].viewer)syncMultiViewFrom(panes[0].viewer);if(changed){refreshMultiViewOverlaysSoon();if(typeof updateMagnificationControls==='function')updateMagnificationControls();if(typeof updateStatus==='function')updateStatus(lastPointer);}return changed;}\n",
    "function multiViewFitView(){const targets=multiViewTargets();if(!targets.length)return false;targets.forEach(viewer=>viewer.viewport.goHome(false));if(multiViewSync)syncMultiViewFrom(targets[0]);refreshMultiViewOverlaysSoon();if(typeof updateMagnificationControls==='function')updateMagnificationControls();return true;}\n",
    "function multiViewOneToOne(){const targets=multiViewTargets();if(!targets.length)return false;targets.forEach(viewer=>{try{const item=viewer.world&&viewer.world.getItemAt(0);const zoom=item&&typeof item.imageToViewportZoom==='function'?item.imageToViewportZoom(1):NaN;if(Number.isFinite(zoom)&&zoom>0)viewer.viewport.zoomTo(zoom,null,false);}catch(e){}});if(multiViewSync)syncMultiViewFrom(targets[0]);refreshMultiViewOverlaysSoon();if(typeof updateMagnificationControls==='function')updateMagnificationControls();return true;}\n",
    "function applyCustomMultiViewLayout(){const input=el('multiViewCustomCount'),count=Math.max(1,Math.min(12,Math.round(Number(input&&input.value?input.value:3)||3)));multiViewCustomMode=count>1;setMultiViewLayout(count);}\n",
    "function bindMultiViewControls(){if(!window.__wsiToolsMultiViewPointerBound){window.addEventListener('mousemove',e=>{if(multiViewResizeMove(e))return;multiViewPaneMouseMove(e);},true);window.addEventListener('mouseup',e=>{if(finishMultiViewResize(e))return;multiViewPaneMouseUp(e);},true);window.addEventListener('resize',()=>{updateMultiViewResizeHandles();multiViewPanes.forEach(pane=>scheduleMultiViewPaneRefresh(pane,false));});window.__wsiToolsMultiViewPointerBound=true;}document.querySelectorAll('.multiViewLayout').forEach(button=>{button.onclick=()=>{multiViewCustomMode=false;setMultiViewLayout(Number(button.dataset.layout||1));};});const custom=el('multiViewCustom'),customCount=el('multiViewCustomCount');if(custom)custom.onclick=applyCustomMultiViewLayout;if(customCount)customCount.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();applyCustomMultiViewLayout();}};const sync=el('multiViewSync');if(sync)sync.onchange=e=>{multiViewSync=!!e.target.checked;updateMultiViewControls();if(multiViewSync){const v=activeMultiViewViewer();if(v)syncMultiViewFrom(v);}scheduleViewerStateSync('multi_view_sync_updated',{layout:multiViewLayout,sync:multiViewSync});};updateMultiViewControls();}\n"
  )
}

wsi_viewer_trajectory_js <- function() {
  paste0(
    "function trajectoryResolution(){return 200;}\n",
    "function trajectoryAreaWidth(){const input=el('trajectoryAreaWidth'),label=el('trajectoryAreaWidthValue');const value=Math.max(1,Math.min(20000,Math.round(Number(input&&input.value?input.value:512))));if(label)label.textContent=String(value)+' px';return value;}\n",
    "function trajectoryAreaPreviewEnabled(){const input=el('trajectoryAreaPreview');return !input||!!input.checked;}\n",
    "function copySlidePoint(p){return {x:Number(p.x),y:Number(p.y)};}\n",
    "function trajectoryLength(points){let total=0;for(let i=1;i<(points||[]).length;i++){total+=Math.hypot(points[i].x-points[i-1].x,points[i].y-points[i-1].y);}return total;}\n",
    "function resamplePolyline(points,n){points=(points||[]).filter(p=>p&&Number.isFinite(Number(p.x))&&Number.isFinite(Number(p.y))).map(copySlidePoint);n=Math.max(2,Math.round(Number(n)||20));if(points.length<2)return points;const distances=[0];for(let i=1;i<points.length;i++)distances[i]=distances[i-1]+Math.hypot(points[i].x-points[i-1].x,points[i].y-points[i-1].y);const total=distances[distances.length-1];if(!Number.isFinite(total)||total<=0)return points.slice(0,1);const out=[];let seg=1;for(let k=0;k<n;k++){const target=total*(k/(n-1));while(seg<distances.length-1&&distances[seg]<target)seg++;const prev=seg-1,d0=distances[prev],d1=distances[seg],t=d1>d0?(target-d0)/(d1-d0):0,a=points[prev],b=points[seg];out.push({x:a.x+(b.x-a.x)*t,y:a.y+(b.y-a.y)*t});}return out;}\n",
    "function catmullRomPoint(p0,p1,p2,p3,t){const t2=t*t,t3=t2*t;return {x:0.5*((2*p1.x)+(-p0.x+p2.x)*t+(2*p0.x-5*p1.x+4*p2.x-p3.x)*t2+(-p0.x+3*p1.x-3*p2.x+p3.x)*t3),y:0.5*((2*p1.y)+(-p0.y+p2.y)*t+(2*p0.y-5*p1.y+4*p2.y-p3.y)*t2+(-p0.y+3*p1.y-3*p2.y+p3.y)*t3)};}\n",
    "function smoothTrajectoryPoints(points,n=20){const clean=(points||[]).filter(pointInsideSlide).map(copySlidePoint);if(clean.length<3)return resamplePolyline(clean,n);const dense=[];const steps=16;for(let i=0;i<clean.length-1;i++){const p0=clean[Math.max(0,i-1)],p1=clean[i],p2=clean[i+1],p3=clean[Math.min(clean.length-1,i+2)];for(let j=0;j<steps;j++)dense.push(catmullRomPoint(p0,p1,p2,p3,j/steps));}dense.push(copySlidePoint(clean[clean.length-1]));return resamplePolyline(dense,n);}\n",
    "function currentTrajectoryPreview(){return smoothTrajectoryPoints(trajectoryDraft,trajectoryResolution());}\n",
    "function trajectoryBounds(points){points=(points||[]).filter(p=>p&&Number.isFinite(Number(p.x))&&Number.isFinite(Number(p.y)));if(!points.length)return null;const xs=points.map(p=>Number(p.x)),ys=points.map(p=>Number(p.y));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function zoomToTrajectory(trajectory){const b=trajectoryBounds((trajectory&&trajectory.points)||[]);if(!b)return;if(typeof zoomToSlideBounds==='function'){zoomToSlideBounds(b,1.25);draw();return;}if(typeof image!=='undefined'&&image.naturalWidth&&typeof slideToImage==='function'){const corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(p=>(typeof slideToViewImagePoint==='function')?slideToViewImagePoint(p):slideToImage(p)),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys),pad=1.35,w=Math.max(1,(xmax-xmin)*pad),h=Math.max(1,(ymax-ymin)*pad);scale=clamp(Math.min(innerWidth/w,innerHeight/h),minScale*0.8,40);offsetX=innerWidth/2-((xmin+xmax)/2)*scale;offsetY=innerHeight/2-((ymin+ymax)/2)*scale;draw();}}\n",
    "function selectedTrajectoryRecord(){if(selectedTrajectory>=0&&trajectories[selectedTrajectory])return trajectories[selectedTrajectory];if(selectedRoi>=0&&rois[selectedRoi]&&rois[selectedRoi].trajectory_area){const tid=String(rois[selectedRoi].trajectory_id||''),found=trajectories.find(t=>String(t&&t.id||'')===tid);if(found)return found;}return trajectories.length?trajectories[trajectories.length-1]:null;}\n",
    "function trajectoryAreaSource(){if(trajectoryDraft.length>=2)return {draft:true,id:null,name:'Draft trajectory',points:currentTrajectoryPreview()};const rec=selectedTrajectoryRecord();return rec?{draft:false,id:rec.id,name:rec.name,points:(rec.points||[]).map(copySlidePoint)}:null;}\n",
    "function trajectoryFlatCapRing(points,width){const pts=(points||[]).filter(pointInsideSlide).map(copySlidePoint),radius=Math.max(1,Number(width||trajectoryAreaWidth())/2);if(pts.length<2)return [];if(typeof brushRingFromPoints==='function')return brushRingFromPoints(pts,radius);const left=[],right=[];for(let i=0;i<pts.length;i++){const prev=pts[Math.max(0,i-1)],next=pts[Math.min(pts.length-1,i+1)],dx=next.x-prev.x,dy=next.y-prev.y,len=Math.hypot(dx,dy)||1,nx=-dy/len,ny=dx/len;left.push({x:pts[i].x+nx*radius,y:pts[i].y+ny*radius});right.push({x:pts[i].x-nx*radius,y:pts[i].y-ny*radius});}return closedRing(left.concat(right.reverse()));}\n",
    "function trajectoryAreaPreviewRing(points,width){return trajectoryFlatCapRing(points,width);}\n",
    "function trajectoryAreaGeometry(points,width,className=null){const pts=(points||[]).filter(pointInsideSlide).map(copySlidePoint);if(pts.length<2)return null;const ring=trajectoryFlatCapRing(pts,Number(width||trajectoryAreaWidth()));return ring.length?{type:'Polygon',groups:[[ring]],rings:[ring],ring:ring,bbox:boundsFromRings([ring]),mask_contour:false,trajectory_flat_caps:true}:null;}\n",
    "function drawTrajectoryAreaPreview(){if(!trajectoryAreaPreviewEnabled())return;const src=trajectoryAreaSource();if(!src||!src.points||src.points.length<2)return;const ring=trajectoryAreaPreviewRing(src.points,trajectoryAreaWidth());if(!ring.length)return;const colour=classColour(currentRoiClass(),'#facc15');ctx.save();ctx.beginPath();drawPathRings([ring]);ctx.fillStyle=hexToRgba(colour,.16);ctx.strokeStyle=colour;ctx.lineWidth=2;ctx.setLineDash([8,5]);ctx.fill('evenodd');ctx.stroke();ctx.setLineDash([]);ctx.restore();}\n",
    "function trajectoryAreaRoiIndex(rec=null){rec=rec||selectedTrajectoryRecord();if(!rec)return -1;const areaId=String(rec.area_roi_id||'');if(areaId){const byId=rois.findIndex(roi=>String(roi&&roi.id||'')===areaId);if(byId>=0)return byId;}const tid=String(rec.id||'');if(!tid)return -1;return rois.findIndex(roi=>roi&&roi.trajectory_area&&String(roi.trajectory_id||'')===tid);}\n",
    "function applyTrajectoryAreaGeometry(roi,rec,geom,width,cls){const groups=(geom&&geom.groups)||[];if(!roi||!groups.length)return false;if(typeof setRoiPositiveGroups==='function')setRoiPositiveGroups(roi,groups);else{roi.rings=groups[0]||[];roi.add_groups=groups.slice(1);}roi.subtract_rings=[];roi.geometry_type=groups.length>1?'MultiPolygon':'Polygon';roi.source='trajectory';roi.trajectory_area=true;roi.trajectory_flat_caps=true;roi.brush_mask_contour=false;roi.trajectory_id=rec&&rec.id||null;roi.trajectory_width_px=width;roi.trajectory_point_count=(rec&&rec.points||[]).length;roi.edited=true;roi.properties=Object.assign({},roi.properties||{},{wsiToolsTrajectory:{id:rec&&rec.id||null,name:rec&&rec.name||null,width_px:width,point_count:(rec&&rec.points||[]).length,flat_caps:true}});if(typeof refreshRoiGeometry==='function')refreshRoiGeometry(roi);return true;}\n",
    "function selectTrajectoryAreaRoi(rec=null,switchToEdit=true){rec=rec||selectedTrajectoryRecord();let idx=trajectoryAreaRoiIndex(rec),prepared=false;if(idx<0){const created=createTrajectoryAreaRoi({toast:false,draw:false});if(!created)return false;rec=created.rec||rec;idx=Number.isFinite(Number(created.index))?Number(created.index):trajectoryAreaRoiIndex(rec);prepared=true;}if(idx<0||!rois[idx]){notify('Could not prepare trajectory border for editing','warning');return false;}selectedTrajectory=-1;if(typeof selectAnnotation==='function')selectAnnotation(idx,true);else selectedRoi=idx;if(typeof setRoiPanelOpen==='function')setRoiPanelOpen(true);if(switchToEdit&&typeof setMode==='function')setMode('edit');if(typeof centerRoi==='function')centerRoi(idx);else if(typeof draw==='function')draw();notify(prepared?'Trajectory border ready for editing':'Trajectory border selected for editing','success');return true;}\n",
    "function createTrajectoryAreaRoi(options={}){options=options||{};let rec=null;if(trajectoryDraft.length>=2)rec=finishTrajectory(false);else rec=selectedTrajectoryRecord();if(!rec||!rec.points||rec.points.length<2){notify('Draw or select a trajectory first','warning');return null;}const width=trajectoryAreaWidth(),cls=currentRoiClass(),geom=trajectoryAreaGeometry(rec.points,width,cls);if(!geom||!geom.groups||!geom.groups.length){notify('Could not prepare trajectory border','warning');return null;}const roi=addRoiFromBrushGroups(geom.groups,'trajectory','Trajectory border',cls);if(!roi)return null;applyTrajectoryAreaGeometry(roi,rec,geom,width,cls);rec.area_width_px=width;rec.area_roi_id=roi.id||null;const idx=rois.indexOf(roi);renderTrajectoryList();buildRoiList();updateButtons();recordAnnotationHistory('trajectory_area_created',{trajectory_id:rec.id||null,roi_id:roi.id||null,width_px:width,class:roi.class||cls},false);scheduleViewerStateSync('trajectory_area_created',{trajectory_id:rec.id||null,roi_id:roi.id||null,width_px:width,class:roi.class||cls});if(options.select){if(typeof selectAnnotation==='function')selectAnnotation(idx,true);else selectedRoi=idx;if(options.edit&&typeof setMode==='function')setMode('edit');}if(options.toast!==false)notify('Trajectory border ready: '+width+' px wide. Use Edit border to refine it.','success');if(options.draw!==false)draw();return {roi:roi,rec:rec,index:idx,width_px:width};}\n",
    "function updateTrajectoryAreaRoi(){const rec=selectedTrajectoryRecord();if(!rec||!rec.points||rec.points.length<2){notify('Select a trajectory first','warning');return;}const idx=trajectoryAreaRoiIndex(rec);if(idx<0){notify('Use Edit border first to make the trajectory border editable','warning');return;}const roi=rois[idx];if(!roi){notify('Trajectory border ROI was not found','warning');return;}if(typeof lockedRoi==='function'&&lockedRoi(roi)){notify('Unlock the trajectory border before updating it','warning');return;}const width=trajectoryAreaWidth(),cls=roi.class||currentRoiClass(),geom=trajectoryAreaGeometry(rec.points,width,cls);if(!geom||!geom.groups||!geom.groups.length){notify('Could not update trajectory border','warning');return;}pushAnnotationUndo('trajectory_area_updated');applyTrajectoryAreaGeometry(roi,rec,geom,width,cls);if(typeof enforceRoiNonOverlap==='function')enforceRoiNonOverlap(idx);rec.area_width_px=width;rec.area_roi_id=roi.id||rec.area_roi_id||null;if(typeof selectAnnotation==='function')selectAnnotation(idx,false);selectedTrajectory=-1;buildRoiList();renderTrajectoryList();updateButtons();recordAnnotationHistory('trajectory_area_updated',{trajectory_id:rec.id||null,roi_id:roi.id||null,width_px:width,class:roi.class||cls},false);scheduleViewerStateSync('trajectory_area_updated',{trajectory_id:rec.id||null,roi_id:roi.id||null,width_px:width,class:roi.class||cls});notify('Trajectory border updated: '+width+' px wide','success');draw();}\n",
    "function drawTrajectoryPath(points,trajectory=null,preview=false){points=(points||[]).filter(p=>p&&Number.isFinite(Number(p.x))&&Number.isFinite(Number(p.y)));if(points.length<2)return;ctx.save();ctx.lineCap='round';ctx.lineJoin='round';ctx.beginPath();points.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.strokeStyle='rgba(255,255,255,.92)';ctx.lineWidth=preview?5:6;if(preview)ctx.setLineDash([8,5]);ctx.stroke();ctx.setLineDash([]);ctx.beginPath();points.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.strokeStyle=preview?'#facc15':((trajectory&&trajectory.colour)||'#ef4444');ctx.lineWidth=preview?3:3.5;ctx.stroke();const controls=(trajectory&&trajectory.control_points)||trajectoryDraft;if(controls&&controls.length){controls.forEach((p,i)=>{const q=slideToCanvas(p);ctx.beginPath();if(i===0){ctx.rect(q.x-5,q.y-5,10,10);}else{ctx.arc(q.x,q.y,4,0,Math.PI*2);}ctx.fillStyle='#eeeeee';ctx.strokeStyle=preview?'#facc15':((trajectory&&trajectory.colour)||'#ef4444');ctx.lineWidth=2;ctx.fill();ctx.stroke();});}if(!preview&&trajectory&&trajectory.name){const q=slideToCanvas(points[Math.floor(points.length/2)]);ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';const text=trajectory.name,w=ctx.measureText(text).width+8,x=clamp(q.x-w/2,4,innerWidth-w-4),y=clamp(q.y-24,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.72)';ctx.fillRect(x,y,w,18);ctx.fillStyle=(trajectory&&trajectory.colour)||'#ef4444';ctx.fillText(text,x+4,y+3);}ctx.restore();}\n",
    "function drawTrajectories(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('trajectories'))return;drawTrajectoryAreaPreview();trajectories.forEach((t,i)=>drawTrajectoryPath(t.points,Object.assign({},t,{colour:i===selectedTrajectory?'#facc15':(t.colour||'#ef4444')}),false));if(mode==='trajectory'&&trajectoryDraft.length){const preview=currentTrajectoryPreview();drawTrajectoryPath(preview,{control_points:trajectoryDraft,colour:'#facc15'},true);}}\n",
    "function trajectoryPayload(){return trajectories.map(t=>({id:t.id,name:t.name,n:t.n,length_px:t.length_px,area_width_px:Number.isFinite(Number(t.area_width_px))?Number(t.area_width_px):null,area_roi_id:t.area_roi_id||null,control_points:t.control_points,points:t.points,created:t.created||null}));}\n",
    "function renderTrajectoryList(){if(typeof enforceSingleObjectSelection==='function')enforceSingleObjectSelection('trajectory');const summary=el('trajectorySummary'),list=el('trajectoryList');if(summary)summary.textContent=trajectories.length?(trajectories.length+' trajector'+(trajectories.length===1?'y':'ies')+' saved'):'No trajectories yet.';if(list){list.innerHTML='';trajectories.forEach((t,i)=>{const b=document.createElement('button');b.type='button';b.className='trajectoryItem';b.classList.toggle('active',i===selectedTrajectory);const len=Number.isFinite(Number(t.length_px))?fmt(t.length_px,1)+' px':'NA',area=Number.isFinite(Number(t.area_width_px))?(' | area '+fmt(Number(t.area_width_px),0)+' px'):'';b.append(document.createTextNode(t.name||('Trajectory '+(i+1))));b.append(document.createElement('br'));const code=document.createElement('code');code.textContent=(t.points?t.points.length:0)+' points | '+len+area;b.append(code);b.onclick=()=>{selectTrajectory(i,true);zoomToTrajectory(t);notify((t.name||('Trajectory '+(i+1)))+': '+len,'info',3600);draw();};list.appendChild(b);});}updateTrajectoryButtons();}\n",
    "function updateTrajectoryList(){renderTrajectoryList();}\n",
	    "function trajectoryHitTolerance(){try{return Math.max(8,12/Math.max(slideUnitScale(),1e-9));}catch(e){return 12;}}\n",
	    "function trajectoryAt(p){if(!p||typeof trajectories==='undefined')return -1;const tol=trajectoryHitTolerance();for(let i=trajectories.length-1;i>=0;i--){const pts=((trajectories[i]&&trajectories[i].points)||[]).filter(q=>q&&Number.isFinite(Number(q.x))&&Number.isFinite(Number(q.y)));for(let j=1;j<pts.length;j++){if(pointLineDistance(p,pts[j-1],pts[j])<=tol)return i;}}return -1;}\n",
	    "let trajectoryProfileRows=[],trajectoryProfilePointRows=[];\n",
	    "function trajectoryProfileStatus(msg,type='info',toast=false){const box=el('trajectoryProfileSummary');if(box)box.textContent=msg||'';if(msg&&toast)notify(msg,type);}\n",
	    "function trajectoryProfileHelpText(){return 'Draw or select a trajectory, choose a visible spot/cell layer and feature, then run the gradient profile. Results are synced to R with viewer$get_trajectory_profile().';}\n",
	    "function showTrajectoryProfileHelp(control=null){notify(trajectoryProfileHelpText(),'info',7600);if(control&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);}\n",
	    "function trajectoryProfileBins(){const input=el('trajectoryProfileBins');const n=Math.round(Number(input&&input.value?input.value:20));return clamp(Number.isFinite(n)?n:20,2,200);}\n",
	    "function trajectoryProfileWidth(){const input=el('trajectoryProfileWidth'),label=el('trajectoryProfileWidthValue');const fallback=(typeof trajectoryAreaWidth==='function')?trajectoryAreaWidth():512;const value=Math.max(1,Math.min(20000,Math.round(Number(input&&input.value?input.value:fallback))));if(label)label.textContent=String(value)+' px';return value;}\n",
	    "function trajectoryProfileLayerSourceId(layer,index){return String(layer&&layer.id||layer&&layer.name||('layer_'+(Number(index)+1)));}\n",
	    "function trajectoryProfilePointLike(item){return item&&Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y))&&(String(item.type||'point').toLowerCase()==='point'||Number.isFinite(Number(item.radius))||item.id||item.label||item.name);}\n",
	    "function trajectoryProfileCellSources(){const cells=[];(rois||[]).forEach((roi,i)=>{if(!roi||!visibleRoi(roi)||!isDrawable(roi))return;if(typeof roiIsCellLike==='function'&&!roiIsCellLike(roi))return;let p=null;if(typeof roiCentroidPoint==='function')p=roiCentroidPoint(roi);if(!p){const b=roiBounds(roi);if(b)p={x:(b.xmin+b.xmax)/2,y:(b.ymin+b.ymax)/2};}if(!p||!Number.isFinite(Number(p.x))||!Number.isFinite(Number(p.y)))return;cells.push({item:Object.assign({},roi,{id:roi.id||('cell_roi_'+(i+1)),label:roi.name||roi.label||roi.id||('cell '+(i+1)),measurements:roi.measurements||roi.properties&&roi.properties.measurements||null}),index:i,x:Number(p.x),y:Number(p.y)});});return cells.length?[{id:'annotation_cell_centroids',label:'Cell annotation centroids',layer:{id:'annotation_cell_centroids',name:'Cell annotation centroids'},items:cells}]:[];}\n",
	    "function trajectoryProfileSources(){const out=[];(layers||[]).forEach((layer,li)=>{if(String(layer&&layer.source_type||layer&&layer.id||'').toLowerCase().includes('trajectory_profile'))return;if(typeof layerVisible==='function'&&!layerVisible(layer))return;if(!Array.isArray(layer&&layer.items))return;const items=[];(layer.items||[]).forEach((item,ii)=>{if(!item||item.visible===false||!trajectoryProfilePointLike(item))return;if(typeof layerItemMatchesActiveProject==='function'&&!layerItemMatchesActiveProject(item,layer))return;const x=Number(item.x),y=Number(item.y);if(Number.isFinite(x)&&Number.isFinite(y))items.push({item:item,index:ii,x:x,y:y});});if(!items.length)return;out.push({id:trajectoryProfileLayerSourceId(layer,li),label:String(layer.name||layer.id||('Layer '+(li+1))),layer:layer,layer_index:li,items:items});});return out.concat(trajectoryProfileCellSources());}\n",
	    "function trajectoryProfileNestedValue(obj,key){if(!obj||!key)return undefined;if(Object.prototype.hasOwnProperty.call(obj,key))return obj[key];const needle=String(key).toLowerCase(),found=Object.keys(obj).find(k=>String(k).toLowerCase()===needle);return found?obj[found]:undefined;}\n",
	    "function trajectoryProfileFeatureValue(item,feature){feature=String(feature||'count');if(feature==='count')return 1;if(!item)return undefined;let v=trajectoryProfileNestedValue(item,feature);if(typeof v!=='undefined')return v;if(item.gene&&String(item.gene).toLowerCase()===feature.toLowerCase())return item.gene_value;if(item.cluster_field&&String(item.cluster_field).toLowerCase()===feature.toLowerCase())return item.cluster_value;for(const bagName of ['gene_values','measurements','cluster_values']){v=trajectoryProfileNestedValue(item[bagName]||{},feature);if(typeof v!=='undefined')return v;}const props=item.properties||{};v=trajectoryProfileNestedValue(props.measurements||{},feature);if(typeof v!=='undefined')return v;if(feature==='class')return item.class||props.class||(props.classification&&props.classification.name);if(feature==='label')return item.label||item.name||item.id;return undefined;}\n",
	    "function trajectoryProfileFeatureCandidatesForSource(source){const seen=new Set(['count']),features=[{id:'count',label:'Count',type:'count'}];const add=(id,label=null,type='auto')=>{id=String(id||'').trim();if(!id||seen.has(id))return;seen.add(id);features.push({id:id,label:label||id,type:type});};(source.items||[]).slice(0,3000).forEach(row=>{const item=row.item||{};if(item.gene)add(String(item.gene),String(item.gene),'numeric');if(item.cluster_field)add(String(item.cluster_field),String(item.cluster_field),'categorical');if(item.class)add('class','Class','categorical');['gene_value','cluster_value','intensity','value','score','probability'].forEach(key=>{if(typeof item[key]!=='undefined')add(key,key);});['gene_values','measurements','cluster_values'].forEach(bag=>{const obj=item[bag]||{};Object.keys(obj).slice(0,80).forEach(k=>add(k,k,bag==='cluster_values'?'categorical':'auto'));});const props=item.properties||{};Object.keys(props.measurements||{}).slice(0,80).forEach(k=>add(k,k));});return features;}\n",
	    "function trajectoryProfileCurrentSource(){const sources=trajectoryProfileSources(),select=el('trajectoryProfileSource'),value=select&&select.value;return sources.find(s=>String(s.id)===String(value))||sources[0]||null;}\n",
	    "function populateTrajectoryProfileFeatures(){const select=el('trajectoryProfileFeature'),source=trajectoryProfileCurrentSource();if(!select)return;const current=select.value,features=source?trajectoryProfileFeatureCandidatesForSource(source):[];select.innerHTML='';features.forEach(f=>{const opt=document.createElement('option');opt.value=f.id;opt.textContent=f.label||f.id;select.appendChild(opt);});if(current&&features.some(f=>f.id===current))select.value=current;}\n",
	    "function populateTrajectoryProfileSources(){const select=el('trajectoryProfileSource');if(!select)return;const current=select.value,sources=trajectoryProfileSources();select.innerHTML='';sources.forEach(src=>{const opt=document.createElement('option');opt.value=src.id;opt.textContent=src.label+' ('+src.items.length.toLocaleString()+')';select.appendChild(opt);});if(current&&sources.some(s=>s.id===current))select.value=current;populateTrajectoryProfileFeatures();}\n",
	    "function trajectoryProfileProjection(points,p){points=(points||[]).filter(q=>q&&Number.isFinite(Number(q.x))&&Number.isFinite(Number(q.y)));if(points.length<2||!p)return null;let total=0,best=null;for(let i=1;i<points.length;i++){const a=points[i-1],b=points[i],vx=b.x-a.x,vy=b.y-a.y,len2=vx*vx+vy*vy,len=Math.sqrt(len2);if(len<=0)continue;const t=clamp(((p.x-a.x)*vx+(p.y-a.y)*vy)/len2,0,1),x=a.x+vx*t,y=a.y+vy*t,d=Math.hypot(p.x-x,p.y-y),along=total+len*t;if(!best||d<best.distance_to_path)best={distance_along_px:along,distance_to_path:d,x:x,y:y,segment:i-1};total+=len;}if(!best)return null;best.total_length_px=total;best.distance_fraction=total>0?best.distance_along_px/total:0;return best;}\n",
	    "function trajectoryProfileMedian(values){const v=values.filter(Number.isFinite).sort((a,b)=>a-b),n=v.length;if(!n)return NaN;const mid=Math.floor(n/2);return n%2?v[mid]:(v[mid-1]+v[mid])/2;}\n",
	    "function trajectoryProfileSd(values,mean){const v=values.filter(Number.isFinite),n=v.length;if(n<2)return 0;const m=Number.isFinite(mean)?mean:v.reduce((a,b)=>a+b,0)/n;return Math.sqrt(v.reduce((s,x)=>s+(x-m)*(x-m),0)/(n-1));}\n",
	    "function trajectoryProfileFeatureType(values,feature){if(feature==='count')return 'count';const finite=values.map(v=>Number(v)).filter(Number.isFinite);return finite.length>=Math.max(1,Math.ceil(values.length*.6))?'numeric':'categorical';}\n",
	    "function trajectoryProfileDominant(values){const counts=new Map();values.forEach(v=>{const key=String(v??'').trim();if(!key)return;counts.set(key,(counts.get(key)||0)+1);});let best='',n=0;counts.forEach((v,k)=>{if(v>n){best=k;n=v;}});return {value:best,count:n};}\n",
	    "function trajectoryProfileProjectLabel(){return typeof annotationSpotProjectLabel==='function'?annotationSpotProjectLabel():{image:'',section:''};}\n",
	    "function trajectoryProfileResultRows(src,feature,bins,width){const tr=trajectoryAreaSource(),points=(tr&&tr.points)||[];if(!tr||points.length<2)throw new Error('Draw or select a trajectory first.');const totalLen=trajectoryLength(points);if(!Number.isFinite(totalLen)||totalLen<=0)throw new Error('Selected trajectory has no measurable length.');const radius=Math.max(1,Number(width)/2),bucket=Array.from({length:bins},()=>[]),included=[];(src.items||[]).forEach(row=>{const p={x:Number(row.x),y:Number(row.y)},proj=trajectoryProfileProjection(points,p);if(!proj||proj.distance_to_path>radius)return;const item=row.item||{},raw=trajectoryProfileFeatureValue(item,feature);if(feature!=='count'&&(raw===null||typeof raw==='undefined'||raw===''))return;const bin=clamp(Math.floor(proj.distance_fraction*bins),0,bins-1);const rec={item:item,id:String(item.id||item.label||item.name||('point_'+(Number(row.index)+1))),label:String(item.label||item.name||item.id||('point '+(Number(row.index)+1))),x:p.x,y:p.y,value:feature==='count'?1:raw,bin:bin,projection:proj};bucket[bin].push(rec);included.push(rec);});const project=trajectoryProfileProjectLabel(),rows=[],allVals=included.map(r=>r.value),globalType=trajectoryProfileFeatureType(allVals,feature);for(let i=0;i<bins;i++){const vals=bucket[i].map(r=>r.value),type=globalType,binStart=totalLen*i/bins,binEnd=totalLen*(i+1)/bins,base={trajectory_id:String(tr.id||''),trajectory_name:String(tr.name||'Draft trajectory'),source_id:String(src.id||''),source_name:String(src.label||src.id||''),feature:String(feature),feature_type:type,category:'',bin:i+1,bin_start_px:binStart,bin_end_px:binEnd,distance_px:(binStart+binEnd)/2,distance_fraction:(i+.5)/bins,width_px:Number(width),total_length_px:totalLen,count:vals.length,mean:null,median:null,min:null,max:null,sd:null,dominant:'',dominant_count:0,fraction:null,project_image:project.image,project_section:project.section};if(type==='numeric'||type==='count'){const nums=vals.map(Number).filter(Number.isFinite),mean=nums.length?nums.reduce((a,b)=>a+b,0)/nums.length:NaN,median=trajectoryProfileMedian(nums);base.mean=Number.isFinite(mean)?mean:null;base.median=Number.isFinite(median)?median:null;base.min=nums.length?Math.min(...nums):null;base.max=nums.length?Math.max(...nums):null;base.sd=Number.isFinite(trajectoryProfileSd(nums,mean))?trajectoryProfileSd(nums,mean):null;}else{const dom=trajectoryProfileDominant(vals);base.dominant=dom.value;base.dominant_count=dom.count;base.fraction=vals.length?dom.count/vals.length:null;base.category=dom.value;}rows.push(base);}return {rows:rows,included:included,trajectory:tr,total_length_px:totalLen,width_px:Number(width)};}\n",
	    "function trajectoryProfileHex(r,g,b){const h=v=>Math.max(0,Math.min(255,Math.round(v))).toString(16).padStart(2,'0');return '#'+h(r)+h(g)+h(b);}\n",
	    "function trajectoryProfileColour(t){t=clamp(Number(t),0,1);const r=59+196*t,g=130+90*Math.sin(Math.PI*t),b=246*(1-t)+40*t;return trajectoryProfileHex(r,g,b);}\n",
	    "function applyTrajectoryProfileLayer(result){const included=(result&&result.included)||[],total=Number(result&&result.total_length_px)||1,items=included.map(row=>{const t=total>0?row.projection.distance_along_px/total:0,colour=trajectoryProfileColour(t);return {id:'trajectory_profile_'+row.id,name:row.label,label:row.label,type:'point',x:row.x,y:row.y,radius:Math.max(4,Number(row.item&&row.item.radius||6)),colour:colour,fill:hexToRgba(colour,.38),source:'trajectory_profile',profile_distance_px:row.projection.distance_along_px,profile_distance_to_path_px:row.projection.distance_to_path,profile_bin:row.bin+1};});upsertViewerLayer({id:'wsi_trajectory_profile_points',name:'Trajectory profile points',type:'vector',source_type:'trajectory_profile',visible:true,opacity:.95,colour:'#22c55e',replace:true,count:items.length,items:items});}\n",
	    "function clearTrajectoryProfile(sync=true){trajectoryProfileRows=[];trajectoryProfilePointRows=[];if(typeof removeViewerLayer==='function')removeViewerLayer('wsi_trajectory_profile_points');trajectoryProfileStatus('Trajectory profile cleared.');if(sync)scheduleViewerStateSync('trajectory_profile_cleared',{});updateTrajectoryButtons();}\n",
	    "function runTrajectoryProfile(){try{populateTrajectoryProfileSources();const src=trajectoryProfileCurrentSource(),feature=String((el('trajectoryProfileFeature')||{}).value||'count'),bins=trajectoryProfileBins(),width=trajectoryProfileWidth();if(!src)throw new Error('No visible point/cell layer is available. Show spatial spots, CellPhenotyper cells, or add an R point layer.');scheduleViewerStateSync('trajectory_profile_started',{source_id:src.id,source_name:src.label,feature:feature,bins:bins,width_px:width});const result=trajectoryProfileResultRows(src,feature,bins,width);trajectoryProfileRows=result.rows;trajectoryProfilePointRows=result.included;applyTrajectoryProfileLayer(result);const pointCount=result.included.length,rowCount=result.rows.length;trajectoryProfileStatus('Profiled '+pointCount.toLocaleString()+' point'+(pointCount===1?'':'s')+' across '+rowCount.toLocaleString()+' bins. Results sync to R with viewer$get_trajectory_profile().');notify('Trajectory profile finished: '+pointCount.toLocaleString()+' point'+(pointCount===1?'':'s'),'success',3200);scheduleViewerStateSync('trajectory_profile_finished',{trajectory_id:String(result.trajectory.id||''),trajectory_name:String(result.trajectory.name||'Draft trajectory'),source_id:src.id,source_name:src.label,feature:feature,bins:bins,width_px:width,point_count:pointCount,trajectory_profile:result.rows});updateTrajectoryButtons();draw();}catch(e){trajectoryProfileStatus('Gradient profile failed: '+e.message,'warning',true);scheduleViewerStateSync('trajectory_profile_failed',{error:e.message});}}\n",
	    "function updateTrajectoryProfileControls(){populateTrajectoryProfileSources();const run=el('runTrajectoryProfile'),clear=el('clearTrajectoryProfile'),src=trajectoryProfileCurrentSource(),tr=trajectoryAreaSource();if(run){run.disabled=!(src&&tr&&tr.points&&tr.points.length>=2);run.title=!src?'No visible point/cell layer is available. Show spatial spots, CellPhenotyper cells, or add an R point layer.':(!tr||!tr.points||tr.points.length<2?'Draw or select a trajectory, then run a gradient profile.':'Profile the selected point/cell feature along the trajectory.');}if(clear)clear.disabled=!(trajectoryProfileRows.length||(typeof layerFindIndex==='function'&&layerFindIndex('wsi_trajectory_profile_points')>=0));trajectoryProfileWidth();}\n",
	    "function proximityConfig(){return cfg.proximity||{enabled:false,sources:[]};}\n",
    "function proximityEnabled(){return !!(proximityConfig().enabled&&Array.isArray(proximityConfig().sources)&&proximityConfig().sources.length);}\n",
    "function proximityUrl(){return String(cfg.proximity_url||'');}\n",
    "function proximityStatus(msg,type='info',toast=false){const box=el('proximitySummary');if(box)box.textContent=msg||'';if(msg&&toast)notify(msg,type);}\n",
    "function proximitySources(){return (proximityConfig().sources||[]).filter(s=>s&&s.id);}\n",
    "function proximityCurrentSource(){const sources=proximitySources(),select=el('proximityPointSource'),value=select&&select.value;return sources.find(src=>String(src.id)===String(value))||sources[0]||{};}\n",
    "function populateProximitySources(){const select=el('proximityPointSource');if(!select)return;const current=select.value;select.innerHTML='';proximitySources().forEach(src=>{const opt=document.createElement('option');opt.value=String(src.id);opt.textContent=String(src.label||src.id);select.appendChild(opt);});if(current&&Array.from(select.options).some(o=>o.value===current))select.value=current;}\n",
    "let proximityStatsRows=[];\n",
    "function proximityFeatureSources(){const pred=(cfg.prediction&&Array.isArray(cfg.prediction.sources))?cfg.prediction.sources:[],items=[{id:'auto',label:'Auto: raw expression / cell table'}];pred.forEach(src=>{if(src&&src.id)items.push({id:String(src.id),label:String(src.label||src.id)});});return items;}\n",
    "function populateProximityStatsSources(){const select=el('proximityStatsFeatureSource');if(!select)return;const current=select.value||'auto';select.innerHTML='';proximityFeatureSources().forEach(src=>{const opt=document.createElement('option');opt.value=src.id;opt.textContent=src.label;select.appendChild(opt);});select.value=Array.from(select.options).some(o=>o.value===current)?current:'auto';}\n",
    "let proximityRoiSignatureCache='';\n",
    "function proximitySelectableRois(){const source=(proximityCurrentSource()&&proximityCurrentSource().unit)||'point';return (rois||[]).map((roi,index)=>({roi:roi,index:index})).filter(entry=>entry.roi&&entry.roi.visible!==false&&isDrawable(entry.roi)&&!String(entry.roi.source||'').toLowerCase().includes('cellphenotyper')&&String(entry.roi.class||'').toLowerCase()!==String(source).toLowerCase());}\n",
    "function proximityAnnotationId(entry){const roi=entry&&entry.roi||{};return String(roi.id||roi.name||entry.index);}\n",
    "function proximitySelectorClassKey(value){return String(value||'').trim().toLowerCase();}\n",
    "function proximityCategoryOptions(entries){const map=new Map();entries.forEach(entry=>{const roi=entry.roi||{},cls=String(roi.class||'unclassified').trim()||'unclassified',key=proximitySelectorClassKey(cls);if(!map.has(key))map.set(key,{className:cls,count:0});map.get(key).count++;});return Array.from(map.values()).sort((a,b)=>a.className.localeCompare(b.className));}\n",
    "function currentProximityRoiSignature(){return proximitySelectableRois().map(entry=>{const roi=entry.roi||{},geom=(typeof geometryType==='function'?geometryType(roi):(roi.geometry_type||roi.geometryType||'')),points=(typeof pointCount==='function'?pointCount(roi):(Array.isArray(roi.rings)?roi.rings.reduce((n,r)=>n+(Array.isArray(r)?r.length:0),0):''));return [entry.index,proximityAnnotationId(entry),roi.name||'',roi.label||'',roi.class||'',roi.visible===false?'0':'1',roi.source||'',geom,points].join('\\u001f');}).join('\\u001e');}\n",
    "function populateProximityAnnotationSelect(id){const select=el(id);if(!select)return;const chosen=new Set(Array.from(select.selectedOptions||[]).map(o=>o.value)),entries=proximitySelectableRois();select.innerHTML='';const addOption=(parent,value,text,selected=false)=>{const opt=document.createElement('option');opt.value=value;opt.textContent=text;if(selected)opt.selected=true;parent.appendChild(opt);return opt;};const classGroup=document.createElement('optgroup');classGroup.label='Categories';proximityCategoryOptions(entries).forEach(cat=>{const value='class:'+cat.className;addOption(classGroup,value,'All '+cat.className+' annotations ('+cat.count+')',chosen.has(value)||chosen.has(cat.className));});if(classGroup.children.length)select.appendChild(classGroup);const roiGroup=document.createElement('optgroup');roiGroup.label='Geometries';entries.forEach(entry=>{const roi=entry.roi,rid=proximityAnnotationId(entry),value='roi_index:'+entry.index,legacy='roi:'+rid,text=(roi.name||rid)+' ['+(roi.class||'unclassified')+']',selected=chosen.has(value)||chosen.has(legacy)||chosen.has(rid)||(!chosen.size&&selectedRoi===entry.index&&id==='proximityQueryAnnotations');addOption(roiGroup,value,text,selected);});if(roiGroup.children.length)select.appendChild(roiGroup);}\n",
    "function populateProximityAnnotations(){proximityRoiSignatureCache=currentProximityRoiSignature();populateProximityAnnotationSelect('proximityQueryAnnotations');populateProximityAnnotationSelect('proximityTargetAnnotations');updateProximityControls();}\n",
    "function syncProximityAnnotations(force=false){if(!proximityEnabled())return;const signature=currentProximityRoiSignature();if(force||signature!==proximityRoiSignatureCache){proximityRoiSignatureCache=signature;populateProximityAnnotationSelect('proximityQueryAnnotations');populateProximityAnnotationSelect('proximityTargetAnnotations');}updateProximityControls();}\n",
    "function proximitySelectedValues(id){const select=el(id);return select?Array.from(select.selectedOptions||[]).map(o=>o.value).filter(Boolean):[];}\n",
    "function proximityPayload(){return {point_source:(el('proximityPointSource')&&el('proximityPointSource').value)||(proximityCurrentSource().id)||'spatial:points',query_annotations:proximitySelectedValues('proximityQueryAnnotations'),target_annotations:proximitySelectedValues('proximityTargetAnnotations'),rois:roiGeojsonObject()};}\n",
    "function proximityStatsPayload(){const payload=proximityPayload();payload.action='stats';payload.feature_source=(el('proximityStatsFeatureSource')&&el('proximityStatsFeatureSource').value)||'auto';payload.method=(el('proximityStatsMethod')&&el('proximityStatsMethod').value)||'spearman';payload.quantile_step=Number((el('proximityStatsQuantileStep')&&el('proximityStatsQuantileStep').value)||0.005);payload.max_features=Number((el('proximityStatsMaxFeatures')&&el('proximityStatsMaxFeatures').value)||5000);return payload;}\n",
    "function proximityHelpText(){return 'Choose points/cells, select Measure inside annotations and Distance from annotations, then run proximity. Run statistics ranks genes/features against binned distance in the live R session.';}\n",
    "function showProximityHelp(control=null){notify(proximityHelpText(),'info',8200);if(control&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);}\n",
    "function updateProximityControls(){if(!proximityEnabled())return;populateProximitySources();populateProximityStatsSources();const run=el('runProximityAnalysis'),clear=el('clearProximityLayer'),runStats=el('runProximityStats'),showStats=el('showProximityStats'),url=proximityUrl(),payload=proximityPayload(),ready=!!(url&&payload.query_annotations.length&&payload.target_annotations.length),hint=!url?'Proximity analysis needs a live R viewer.':(!payload.query_annotations.length||!payload.target_annotations.length?'Select Measure inside and Distance from annotations.':'Run nearest-neighbour proximity analysis in the live R session.');if(run){run.disabled=!ready;run.title=hint;}if(runStats){runStats.disabled=!ready;runStats.title=ready?'Bin proximity distances and rank genes/features in the live R session.':hint;}if(showStats)showStats.disabled=!proximityStatsRows.length;if(clear)clear.disabled=!(typeof layerFindIndex==='function'&&layerFindIndex('wsi_proximity_distance')>=0);}\n",
    "function clearProximityStats(sync=true){proximityStatsRows=[];renderProximityStatsTable([],{});if(sync)scheduleViewerStateSync('proximity_stats_cleared',{});updateProximityControls();}\n",
    "function clearProximityResult(sync=true){if(typeof removeViewerLayer==='function')removeViewerLayer('wsi_proximity_distance');clearProximityStats(false);if(sync)scheduleViewerStateSync('proximity_cleared',{});proximityStatus('Proximity result cleared.');updateProximityControls();}\n",
    "async function runProximityAnalysis(){if(!proximityEnabled())return;const url=proximityUrl();if(!url){proximityStatus('Proximity analysis needs a live R viewer.','warning',true);return;}const payload=proximityPayload();if(!payload.query_annotations.length||!payload.target_annotations.length){proximityStatus('Select query and reference annotations.','warning',true);return;}proximityStatus('Running proximity analysis in R...');notify('Running proximity analysis in R','info',1800);try{const response=await fetch(url,{method:'POST',headers:{'Accept':'application/json','Content-Type':'application/json'},body:JSON.stringify(payload)});let body=null;try{body=await response.json();}catch(e){}if(!response.ok||body&&body.ok===false)throw new Error((body&&body.error)||('HTTP '+response.status));handleViewerCommands(body);const info=body&&body.proximity||{},count=Number(info.count||0),queryCount=Number(info.query_count||0),targetCount=Number(info.target_count||0),median=Number(info.median_distance_um),unit=String(info.point_unit||'point').toLowerCase(),unitLabel=unit==='spot'?'spot':(unit==='cell'?'cell':'point');let msg='Calculated proximity for '+count.toLocaleString()+' '+unitLabel+(count===1?'':'s')+'.';if(Number.isFinite(queryCount)&&queryCount!==count)msg+=' Query '+queryCount.toLocaleString()+'.';if(Number.isFinite(targetCount))msg+=' Reference '+targetCount.toLocaleString()+'.';if(Number.isFinite(median))msg+=' Median '+fmt(median,1)+' um.';msg+=' Results are synced to R with viewer$get_proximity().';proximityStatus(msg);notify('Proximity finished: '+count.toLocaleString()+' '+unitLabel+(count===1?'':'s'),'success',3200);updateProximityControls();}catch(e){proximityStatus('Proximity failed: '+e.message);notify('Proximity failed: '+e.message,'error',6200);}}\n",
    "function proximityStatsNumber(value,digits=3){const n=Number(value);return Number.isFinite(n)?fmt(n,digits):'';}\n",
    "function openProximityStatsWindow(){const panel=el('proximityStatsWindow');if(!panel)return;panel.classList.add('open');panel.setAttribute('aria-hidden','false');}\n",
    "function closeProximityStatsWindow(){const panel=el('proximityStatsWindow');if(!panel)return;panel.classList.remove('open');panel.setAttribute('aria-hidden','true');}\n",
    "function proximityStatsPointSource(featureSource){return String(featureSource||'').startsWith('cellphenotyper:')?'cellphenotyper:cells':'spatial:points';}\n",
    "async function applyProximityStatsFeature(row){const feature=String(row&&row.feature||'').trim();if(!feature){notify('No feature name is available for this row','warning',2200);return false;}const source=String(row.feature_source||((el('proximityStatsFeatureSource')||{}).value)||'spatial:raw');return applySeuratGeneColour(feature,true,{feature_source:source,point_source:proximityStatsPointSource(source)});}\n",
    "function renderProximityStatsTable(rows,info={}){const tbody=el('proximityStatsTable')&&el('proximityStatsTable').querySelector('tbody'),subtitle=el('proximityStatsSubtitle'),summary=el('proximityStatsWindowSummary');if(!tbody)return;tbody.innerHTML='';(rows||[]).slice(0,250).forEach(row=>{const tr=document.createElement('tr'),cells=[{value:row.rank},{value:row.feature,feature:true},{value:row.method},{value:proximityStatsNumber(row.correlation,4)},{value:proximityStatsNumber(row.MIC,4)},{value:proximityStatsNumber(row.p_value,3)},{value:row.n_bins},{value:row.n_points}];cells.forEach(cell=>{const td=document.createElement('td');td.textContent=cell.value===null||typeof cell.value==='undefined'?'':String(cell.value);if(cell.feature&&td.textContent){td.className='proximityStatsFeature';td.title='Colour viewer by '+td.textContent;td.onclick=()=>applyProximityStatsFeature(row);}tr.appendChild(td);});tbody.appendChild(tr);});const count=(rows||[]).length,method=info.method||((rows&&rows[0]&&rows[0].method)||''),source=info.feature_source||((rows&&rows[0]&&rows[0].feature_source)||'');if(subtitle)subtitle.textContent=count?(count.toLocaleString()+' feature'+(count===1?'':'s')+' | '+method+' | '+source):'No statistics have been run.';if(summary)summary.textContent=count?('Showing top '+Math.min(250,count).toLocaleString()+' of '+count.toLocaleString()+' ranked features. Click a feature name to colour the viewer. Full table is synced to R with viewer$get_proximity_stats().'):'Run proximity statistics from the Trajectories menu.';updateProximityControls();}\n",
    "function proximityStatsRowsFromBody(body){const rows=body&&body.proximity_stats_rows;if(Array.isArray(rows))return rows;if(rows&&typeof rows==='object'){const keys=Object.keys(rows),n=Math.max(0,...keys.map(k=>Array.isArray(rows[k])?rows[k].length:0));return Array.from({length:n},(_,i)=>{const o={};keys.forEach(k=>{o[k]=Array.isArray(rows[k])?rows[k][i]:rows[k];});return o;});}return [];}\n",
    "async function runProximityStatistics(){if(!proximityEnabled())return;const url=proximityUrl();if(!url){proximityStatus('Proximity statistics need a live R viewer.','warning',true);return;}const payload=proximityStatsPayload();if(!payload.query_annotations.length||!payload.target_annotations.length){proximityStatus('Select query and reference annotations.','warning',true);return;}proximityStatus('Running proximity statistics in R...');notify('Running proximity statistics in R','info',1800);try{const response=await fetch(url,{method:'POST',headers:{'Accept':'application/json','Content-Type':'application/json'},body:JSON.stringify(payload)});let body=null;try{body=await response.json();}catch(e){}if(!response.ok||body&&body.ok===false)throw new Error((body&&body.error)||('HTTP '+response.status));handleViewerCommands(body);const info=body&&body.proximity_stats||{},rows=proximityStatsRowsFromBody(body);proximityStatsRows=rows;renderProximityStatsTable(rows,info);openProximityStatsWindow();const msg='Proximity statistics finished: '+rows.length.toLocaleString()+' feature'+(rows.length===1?'':'s')+'. Results are synced to R with viewer$get_proximity_stats().';proximityStatus(msg);notify('Proximity statistics finished','success',3200);scheduleViewerStateSync('proximity_stats_finished',{count:rows.length,method:info.method||payload.method,feature_source:info.feature_source||payload.feature_source,proximity_stats:rows});}catch(e){proximityStatus('Proximity statistics failed: '+e.message);notify('Proximity statistics failed: '+e.message,'error',7200);scheduleViewerStateSync('proximity_stats_failed',{error:e.message});}}\n",
    "function proximityStatsCsv(){if(!proximityStatsRows.length)return '';const cols=['rank','feature','method','statistic','correlation','p_value','MIC','n_bins','n_points','distance_unit','feature_source'];const lines=[cols.map(csvValue).join(',')];proximityStatsRows.forEach(row=>lines.push(cols.map(c=>csvValue(row[c])).join(',')));return lines.join('\\n');}\n",
    "function saveProximityStatsCsv(){if(!proximityStatsRows.length){notify('Run proximity statistics first','warning');return;}downloadCsvText(proximityStatsCsv(),'wsiTools_proximity_statistics.csv');scheduleViewerStateSync('proximity_stats_exported',{count:proximityStatsRows.length,format:'csv'});notify('Proximity statistics CSV saved','success');}\n",
	    "function updateTrajectoryButtons(){const finish=el('finishTrajectory'),undo=el('undoTrajectoryPoint'),clear=el('clearTrajectories'),editArea=el('editTrajectoryArea'),updateArea=el('updateTrajectoryArea'),rec=selectedTrajectoryRecord(),hasArea=trajectoryAreaRoiIndex(rec)>=0,canEditBorder=trajectoryDraft.length>=2||!!(rec&&rec.points&&rec.points.length>=2)||hasArea;if(finish)finish.disabled=trajectoryDraft.length<2;if(undo)undo.disabled=trajectoryDraft.length<1;if(clear)clear.disabled=!trajectoryDraft.length&&!trajectories.length;if(editArea)editArea.disabled=!canEditBorder;if(updateArea)updateArea.disabled=!hasArea;const tool=el('toolTrajectory');if(tool)tool.classList.toggle('active',mode==='trajectory');trajectoryAreaWidth();if(typeof updateTrajectoryProfileControls==='function')updateTrajectoryProfileControls();if(typeof updateProximityControls==='function')updateProximityControls();}\n",
    "function addTrajectoryPoint(p){if(!pointInsideSlide(p))return;trajectoryDraft.push(copySlidePoint(p));renderTrajectoryList();draw();}\n",
    "function undoTrajectoryPoint(){if(!trajectoryDraft.length)return;trajectoryDraft.pop();renderTrajectoryList();draw();}\n",
    "function finishTrajectory(toast=true){if(trajectoryDraft.length<2){notify('Add at least 2 trajectory points','warning');return null;}pushAnnotationUndo('trajectory_added');const n=trajectoryResolution(),points=smoothTrajectoryPoints(trajectoryDraft,n);trajectorySeq++;const rec={id:'trajectory_'+trajectorySeq,name:'Trajectory '+trajectorySeq,n:n,control_points:trajectoryDraft.map(copySlidePoint),points:points.map(copySlidePoint),length_px:trajectoryLength(points),area_width_px:null,area_roi_id:null,colour:'#ef4444',created:new Date().toISOString()};trajectories.push(rec);trajectoryDraft=[];selectTrajectory(trajectories.length-1,true);recordAnnotationHistory('trajectory_added',{id:rec.id,name:rec.name,control_count:rec.control_points.length,point_count:rec.points.length},false);scheduleViewerStateSync('trajectory_added',{id:rec.id,name:rec.name,control_count:rec.control_points.length,point_count:rec.points.length,length_px:rec.length_px});if(toast!==false){notify(rec.name+' saved; trajectory drawing off','success');setMode('pan');}if(typeof closeAllToolMenus==='function')closeAllToolMenus();draw();return rec;}\n",
    "function clearTrajectories(){if(!trajectoryDraft.length&&!trajectories.length)return;pushAnnotationUndo('trajectories_cleared');trajectoryDraft=[];trajectories=[];selectedTrajectory=-1;renderTrajectoryList();recordAnnotationHistory('trajectories_cleared',{},false);scheduleViewerStateSync('trajectories_cleared',{});notify('Trajectories cleared','success');draw();}\n",
    "function deleteTrajectory(index=selectedTrajectory){const i=Number(index);if(i<0||!trajectories[i]){notify('Select a trajectory','warning');return false;}const removed=trajectories[i],label=removed.name||('Trajectory '+(i+1));pushAnnotationUndo('trajectory_deleted');trajectories.splice(i,1);selectedTrajectory=trajectories.length?Math.min(i,trajectories.length-1):-1;trajectoryDraft=[];renderTrajectoryList();updateButtons();recordAnnotationHistory('trajectory_deleted',{id:removed.id||null,name:label},false);scheduleViewerStateSync('trajectory_deleted',{id:removed.id||null,name:label});notifyAction('Deleted '+label+'.','Undo',()=>restoreAnnotationUndo(),'success',7000);draw();return true;}\n",
    "function deleteSelectedTrajectory(){return deleteTrajectory(selectedTrajectory);}\n",
    "function trajectoryStatus(){if(mode==='trajectory'){if(trajectoryDraft.length)return ' | trajectory '+trajectoryDraft.length+' control point'+(trajectoryDraft.length===1?'':'s');return ' | click trajectory points';}return trajectories.length?(' | trajectories '+trajectories.length):'';}\n",
    "function trajectoryHelpText(){return 'Click points on the slide to sketch a trajectory. Double-click, Enter, or Finish saves a smoothed backbone and returns to pan mode. Click a saved trajectory path to select it, then press Delete to remove only that trajectory. Edit border turns the flat-ended trajectory width preview into editable vertices; drag a border dot and nearby dots move smoothly with it. Update border rebuilds it with the current width.';}\n",
    "function showTrajectoryHelp(control=null){notify(trajectoryHelpText(),'info',9000);if(control&&typeof closeMenuAfterToolAction==='function')closeMenuAfterToolAction(control);}\n",
    "function bindTrajectoryControls(){const help=el('trajectoryHelp'),profileHelp=el('trajectoryProfileHelp'),proximityHelp=el('proximityHelp'),tool=el('toolTrajectory'),finish=el('finishTrajectory'),undo=el('undoTrajectoryPoint'),clear=el('clearTrajectories'),width=el('trajectoryAreaWidth'),editArea=el('editTrajectoryArea'),updateArea=el('updateTrajectoryArea'),preview=el('trajectoryAreaPreview'),profileSource=el('trajectoryProfileSource'),profileFeature=el('trajectoryProfileFeature'),profileBins=el('trajectoryProfileBins'),profileWidth=el('trajectoryProfileWidth'),profileRun=el('runTrajectoryProfile'),profileClear=el('clearTrajectoryProfile');if(help)help.onclick=e=>showTrajectoryHelp(e.currentTarget);if(profileHelp)profileHelp.onclick=e=>showTrajectoryProfileHelp(e.currentTarget);if(proximityHelp)proximityHelp.onclick=e=>showProximityHelp(e.currentTarget);if(tool)tool.onclick=e=>{setMode('trajectory');closeMenuAfterToolAction(e.currentTarget);};if(finish)finish.onclick=e=>{finishTrajectory(true);closeMenuAfterToolAction(e.currentTarget);};if(undo)undo.onclick=undoTrajectoryPoint;if(clear)clear.onclick=e=>{clearTrajectories();closeMenuAfterToolAction(e.currentTarget);};if(editArea)editArea.onclick=e=>{selectTrajectoryAreaRoi(null,true);closeMenuAfterToolAction(e.currentTarget);};if(updateArea)updateArea.onclick=e=>{updateTrajectoryAreaRoi();closeMenuAfterToolAction(e.currentTarget);};if(width)width.oninput=()=>{trajectoryAreaWidth();draw();};if(preview)preview.onchange=draw;if(profileSource)profileSource.onchange=()=>{populateTrajectoryProfileFeatures();updateTrajectoryProfileControls();};if(profileFeature)profileFeature.onchange=updateTrajectoryProfileControls;if(profileBins)profileBins.onchange=updateTrajectoryProfileControls;if(profileWidth)profileWidth.oninput=()=>trajectoryProfileWidth();if(profileRun)profileRun.onclick=e=>{runTrajectoryProfile();closeMenuAfterToolAction(e.currentTarget);};if(profileClear)profileClear.onclick=e=>{clearTrajectoryProfile(true);closeMenuAfterToolAction(e.currentTarget);};if(proximityEnabled()){populateProximitySources();populateProximityAnnotations();populateProximityStatsSources();const run=el('runProximityAnalysis'),clearProx=el('clearProximityLayer'),source=el('proximityPointSource'),query=el('proximityQueryAnnotations'),target=el('proximityTargetAnnotations'),runStats=el('runProximityStats'),showStats=el('showProximityStats'),statsSource=el('proximityStatsFeatureSource'),statsClose=el('proximityStatsClose'),statsCsv=el('proximityStatsDownloadCsv'),statsClear=el('proximityStatsClear');if(run)run.onclick=runProximityAnalysis;if(clearProx)clearProx.onclick=()=>clearProximityResult(true);if(runStats)runStats.onclick=e=>{runProximityStatistics();closeMenuAfterToolAction(e.currentTarget);};if(showStats)showStats.onclick=e=>{openProximityStatsWindow();closeMenuAfterToolAction(e.currentTarget);};if(statsClose)statsClose.onclick=closeProximityStatsWindow;if(statsCsv)statsCsv.onclick=saveProximityStatsCsv;if(statsClear)statsClear.onclick=()=>clearProximityStats(true);if(source)source.onchange=()=>syncProximityAnnotations(true);if(query)query.onchange=updateProximityControls;if(target)target.onchange=updateProximityControls;if(statsSource)statsSource.onchange=updateProximityControls;}trajectoryAreaWidth();updateTrajectoryProfileControls();renderTrajectoryList();syncProximityAnnotations(false);}\n"
  )
}

wsi_viewer_segmentation_js <- function() {
  paste0(
    "function segmentationStatus(msg,type='info',toast=false){const box=el('segmentationSummary');if(box)box.textContent=msg||'';if(msg&&toast)notify(msg,type);}\n",
    "function segmentationConfig(){return cfg.segmentation||{};}\n",
    "function segmentationRunUrl(){const seg=segmentationConfig();return String(seg.run_url||cfg.segmentation_run_url||'');}\n",
    "function segmentationSelectedEngine(){const select=el('segmentationEngine'),seg=segmentationConfig();return String((select&&select.value)||seg.default_engine||'stardist_he');}\n",
    "function selectedRoiFeatureText(){if(selectedRoi<0||!rois[selectedRoi])return null;const feature=roiFeature(rois[selectedRoi],selectedRoi);if(!feature)return null;return JSON.stringify({type:'FeatureCollection',features:[feature]},null,2);}\n",
    "function exportSelectedRoiForSegmentation(){const text=selectedRoiFeatureText();if(!text){segmentationStatus('Select an ROI before exporting the selected region.','warning',true);return;}const roi=rois[selectedRoi],name=(roi.id||roi.name||'selected_roi').replace(/[^A-Za-z0-9_.-]+/g,'_');downloadText(text,name+'_roi.geojson');segmentationStatus('Exported selected ROI GeoJSON. Run cell segmentation outside wsiTools, then load the resulting GeoJSON or centroid table.');notify('GeoJSON exported','success');}\n",
    "function segmentationOffset(){const local=!!(el('segLocalCoords')&&el('segLocalCoords').checked),base=(local&&selectedRoi>=0&&rois[selectedRoi])?roiBounds(rois[selectedRoi]):null;return base?{x:base.xmin,y:base.ymin}:{x:0,y:0};}\n",
    "function segmentationCellRadius(){const input=el('segCellRadius'),label=el('segCellRadiusValue');const value=Math.max(1,Number(input&&input.value?input.value:8));if(label)label.textContent=Math.round(value)+' px';return value;}\n",
    "function coordPoint(coord,offset){if(!coord||coord.length<2)return null;const x=Number(coord[0]),y=Number(coord[1]);if(!Number.isFinite(x)||!Number.isFinite(y))return null;return {x:x+offset.x,y:y+offset.y};}\n",
    "function ringFromCoords(coords,offset){const ring=(coords||[]).map(c=>coordPoint(c,offset)).filter(Boolean);return closedRing(ring);}\n",
    "function ringsFromGeojsonGeometry(geometry,offset){if(!geometry)return [];const type=String(geometry.type||'').toLowerCase(),coords=geometry.coordinates||[];if(type==='polygon')return coords.map(r=>ringFromCoords(r,offset)).filter(r=>r.length>=4);if(type==='multipolygon'){let rings=[];coords.forEach(poly=>{rings=rings.concat((poly||[]).map(r=>ringFromCoords(r,offset)).filter(r=>r.length>=4));});return rings;}return [];}\n",
    "function featureClassName(properties){const cls=properties&&properties.classification;if(cls&&typeof cls==='object'&&cls.name)return cls.name;if(properties&&properties.class)return properties.class;if(typeof cls==='string')return cls;return 'cell';}\n",
    "function addSegmentationGeojson(obj,options={}){const features=geojsonFeatures(obj);if(!features.length){segmentationStatus('No GeoJSON features found in segmentation file.','warning',true);return;}const previousSelected=selectedRoi,useLocal=Object.prototype.hasOwnProperty.call(options,'local')?!!options.local:!!(el('segLocalCoords')&&el('segLocalCoords').checked),offset=useLocal?segmentationOffset():{x:0,y:0};let added=0;features.forEach((feature,i)=>{const rings=ringsFromGeojsonGeometry(feature.geometry||{},offset);if(!rings.length)return;const props=feature.properties||{},name=props.name||props.label||feature.id||('CellPhenotyper cell '+(i+1)),cls=featureClassName(props),colour=classColour(String(cls),'#38bdf8');const roi={id:String(feature.id||('cellphenotyper_cell_'+Date.now()+'_'+i)),name:String(name),label:String(name),class:String(cls),geometry_type:'Polygon',source:'cellphenotyper',drawable:true,point_count:pointCount({rings:rings}),area:polygonArea(rings),bbox:null,colour:colour,original_colour:colour,fill:hexToRgba(colour,0.12),rings:rings,measurements:props.measurements||null,centroid:props.centroid||props.center||null,edited:true};refreshRoiGeometry(roi);rois.push(roi);added++;});if(!added){segmentationStatus('Segmentation GeoJSON did not contain polygon or multipolygon cells.','warning',true);return;}if(options.keepSelection&&previousSelected>=0&&rois[previousSelected])selectedRoi=previousSelected;else selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();const detail=Object.assign({added:added,type:'geojson'},options.detail||{});recordAnnotationHistory('segmentation_imported',detail);scheduleViewerStateSync('segmentation_added',detail);segmentationStatus('Loaded '+countText(added)+' CellPhenotyper cell polygon'+(added===1?'':'s')+'.');notify('Cells loaded: '+countText(added)+' cell'+(added===1?'':'s'),'success',3600);}\n",
    "function parseDelimitedLine(line,delimiter){const out=[];let cur='',quoted=false;for(let i=0;i<line.length;i++){const ch=line[i];if(ch==='\"'){if(quoted&&line[i+1]==='\"'){cur+='\"';i++;}else quoted=!quoted;}else if(ch===delimiter&&!quoted){out.push(cur);cur='';}else cur+=ch;}out.push(cur);return out.map(x=>x.trim());}\n",
    "function parseDelimitedTable(text){const lines=String(text||'').split(/\\r?\\n/).filter(line=>line.trim().length);if(!lines.length)return {headers:[],rows:[]};const delimiter=lines[0].indexOf('\\t')>=0?'\\t':',';const headers=parseDelimitedLine(lines[0],delimiter);const rows=lines.slice(1).map(line=>parseDelimitedLine(line,delimiter));return {headers:headers,rows:rows};}\n",
    "function headerIndex(headers,names){const lower=headers.map(h=>String(h).toLowerCase().trim());for(const name of names){const idx=lower.indexOf(name);if(idx>=0)return idx;}return -1;}\n",
    "function cellRing(center,radius,steps=18){const pts=[];for(let i=0;i<steps;i++){const a=i/steps*Math.PI*2;pts.push({x:center.x+Math.cos(a)*radius,y:center.y+Math.sin(a)*radius});}return closedRing(pts);}\n",
    "function addSegmentationCentroidTable(text,fileName){const table=parseDelimitedTable(text),headers=table.headers,rows=table.rows,xi=headerIndex(headers,['x','centroid_x','center_x','centre_x']),yi=headerIndex(headers,['y','centroid_y','center_y','centre_y']);if(xi<0||yi<0){segmentationStatus('CSV/TSV must contain x/y or centroid_x/centroid_y columns.','warning',true);return;}const idIdx=headerIndex(headers,['cell_id','id','object_id','label']),offset=segmentationOffset(),radius=segmentationCellRadius(),colour=classColour('cell','#38bdf8');let added=0;rows.forEach((row,i)=>{const x=Number(row[xi]),y=Number(row[yi]);if(!Number.isFinite(x)||!Number.isFinite(y))return;const p={x:x+offset.x,y:y+offset.y},ring=cellRing(p,radius),id=String((idIdx>=0&&row[idIdx])?row[idIdx]:('cellphenotyper_cell_'+(i+1))),roi={id:id,name:id,label:id,class:'cell',geometry_type:'Polygon',source:'cellphenotyper',drawable:true,point_count:ring.length-1,area:polygonArea([ring]),bbox:boundsFromRing(ring),colour:colour,original_colour:colour,fill:hexToRgba(colour,0.12),rings:[ring],edited:true,centroid:{x:p.x,y:p.y},source_file:fileName||''};refreshRoiGeometry(roi);rois.push(roi);added++;});if(!added){segmentationStatus('No numeric cell centroids were found in the table.','warning',true);return;}selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();const detail={added:added,type:'centroids',file:fileName||null};recordAnnotationHistory('segmentation_imported',detail);scheduleViewerStateSync('segmentation_added',detail);segmentationStatus('Loaded '+countText(added)+' CellPhenotyper centroid cell marker'+(added===1?'':'s')+'.');notify('Cells loaded: '+countText(added)+' cell'+(added===1?'':'s'),'success',3600);}\n",
    "function segmentationResultDetail(result){if(!result||result.type==='FeatureCollection'||result.type==='Feature')return {};const keys=['message','crop','output','slide_output','roi_id','bbox','status','segmentation_type'];const detail={};keys.forEach(k=>{if(Object.prototype.hasOwnProperty.call(result,k))detail[k]=result[k];});return detail;}\n",
    "function copyViewerText(text){if(navigator.clipboard&&navigator.clipboard.writeText)return navigator.clipboard.writeText(text);return new Promise((resolve,reject)=>{try{const area=document.createElement('textarea');area.value=text;area.setAttribute('readonly','');area.style.position='fixed';area.style.left='-9999px';document.body.appendChild(area);area.select();const ok=document.execCommand('copy');document.body.removeChild(area);ok?resolve():reject(new Error('copy failed'));}catch(e){reject(e);}});}\n",
    "function showSegmentationNotConfiguredNotice(){const message='No live cell-segmentation endpoint is configured. Reopen with wsi_viewer_live(slide, stardist = TRUE), or load an existing cell GeoJSON/CSV/mask file.';segmentationStatus(message,'warning',true);notify(message,'warning',6200);}\n",
    "async function startSegmentationForSelectedRoi(){const url=segmentationRunUrl(),text=selectedRoiFeatureText(),engine=segmentationSelectedEngine();if(!url){showSegmentationNotConfiguredNotice();return;}if(!text){segmentationStatus('Select or draw one ROI before running cell segmentation.','warning',true);return;}let roiObj;try{roiObj=JSON.parse(text);}catch(e){segmentationStatus('Could not serialize the selected ROI: '+e.message,'warning',true);return;}segmentationStatus('Running '+engine+' on selected ROI...','info',true);scheduleViewerStateSync('segmentation_started',{engine:engine,async:false});try{const res=await fetch(url,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify({engine:engine,roi:roiObj})});let result=null;try{result=await res.json();}catch(e){result={error:await res.text()};}if(!res.ok||result.error)throw new Error(result.error||('HTTP '+res.status));const detail=segmentationResultDetail(result);detail.engine=result.engine||engine;if(result.geojson){addSegmentationGeojson(result.geojson,{local:false,keepSelection:true,detail:detail});segmentationStatus(result.message||('Finished '+engine+'.'),'success',true);scheduleViewerStateSync('segmentation_finished',detail);return;}if(result.type==='FeatureCollection'||result.type==='Feature'){addSegmentationGeojson(result,{local:false,keepSelection:true,detail:detail});segmentationStatus('Finished '+engine+'.','success',true);scheduleViewerStateSync('segmentation_finished',detail);return;}segmentationStatus(result.message||('Finished '+engine+', but no viewer overlay was returned.'),'warning',true);scheduleViewerStateSync('segmentation_finished',detail);}catch(e){const detail={engine:engine,message:e.message};segmentationStatus('Cell segmentation failed: '+e.message,'warning',true);scheduleViewerStateSync('segmentation_failed',detail);}}\n",
    "function maskComponentsFromCanvas(canvas,minArea=3){const ctx=canvas.getContext('2d',{willReadFrequently:true}),w=canvas.width,h=canvas.height,img=ctx.getImageData(0,0,w,h).data,seen=new Uint8Array(w*h),components=[];function fg(i){const r=img[i*4],g=img[i*4+1],b=img[i*4+2],a=img[i*4+3];return a>0&&(r+g+b)>12;}for(let p=0;p<w*h;p++){if(seen[p]||!fg(p)){seen[p]=1;continue;}let stack=[p],xmin=w,ymin=h,xmax=0,ymax=0,count=0;seen[p]=1;while(stack.length){const q=stack.pop(),x=q%w,y=Math.floor(q/w);count++;if(x<xmin)xmin=x;if(x>xmax)xmax=x;if(y<ymin)ymin=y;if(y>ymax)ymax=y;const ns=[q-1,q+1,q-w,q+w];for(const n of ns){if(n<0||n>=w*h||seen[n])continue;const nx=n%w;if((n===q-1&&nx>x)||(n===q+1&&nx<x))continue;if(fg(n)){seen[n]=1;stack.push(n);}else seen[n]=1;}}if(count>=minArea)components.push({xmin:xmin,ymin:ymin,xmax:xmax+1,ymax:ymax+1,count:count});}return components;}\n",
    "function addSegmentationMaskImage(img,fileName){const canvas=document.createElement('canvas');canvas.width=img.naturalWidth||img.width;canvas.height=img.naturalHeight||img.height;const ctx=canvas.getContext('2d',{willReadFrequently:true});ctx.drawImage(img,0,0,canvas.width,canvas.height);const comps=maskComponentsFromCanvas(canvas,3),scaleX=Number(cfg.slide_width||canvas.width)/canvas.width,scaleY=Number(cfg.slide_height||canvas.height)/canvas.height,features=[];comps.forEach((c,i)=>{const x0=c.xmin*scaleX,y0=c.ymin*scaleY,x1=c.xmax*scaleX,y1=c.ymax*scaleY;features.push({type:'Feature',id:'mask_cell_'+(i+1),properties:{name:'mask cell '+(i+1),class:'cell',classification:{name:'cell'},source:'external_segmentation',source_file:fileName||'',mask_area_px:c.count},geometry:{type:'Polygon',coordinates:[[[x0,y0],[x1,y0],[x1,y1],[x0,y1],[x0,y0]]]}});});if(!features.length){segmentationStatus('Mask image contained no non-background components.','warning',true);return;}addSegmentationGeojson({type:'FeatureCollection',features:features},{local:false,detail:{type:'mask',file:fileName||'',added:features.length}});}\n",
    "function loadSegmentationMaskFile(file){const ext=String(file&&file.name||'').toLowerCase().split('.').pop();if(ext==='tif'||ext==='tiff'){segmentationStatus('Browser TIFF mask decoding is not portable. Load TIFF masks from R with import_segmentation(file, mask_as_rois = TRUE) and viewer$add_segmentation().','warning',true);return;}const reader=new FileReader();reader.onload=()=>{const img=new Image();img.onload=()=>{try{addSegmentationMaskImage(img,file.name);}catch(e){segmentationStatus('Could not convert mask image: '+e.message,'warning',true);}};img.onerror=()=>segmentationStatus('Could not decode mask image in the browser. Try PNG/JPEG/WebP or load it from R.','warning',true);img.src=reader.result;};reader.readAsDataURL(file);}\n",
    "function clearSegmentationOverlays(){const before=rois.length;for(let i=rois.length-1;i>=0;i--){if(rois[i].source==='cellphenotyper'||rois[i].source==='external_segmentation')rois.splice(i,1);}if(selectedRoi>=rois.length)selectedRoi=rois.length-1;buildRoiList();updateButtons();draw();scheduleViewerStateSync('segmentation_cleared',{});segmentationStatus('Removed '+countText(before-rois.length)+' cell overlay'+(before-rois.length===1?'':'s')+'.');notify('Segmentation cleared','success');}\n",
    "function bindSegmentationControls(){const exportButton=el('exportSelectedRoi'),startButton=el('startSegmentation'),loadButton=el('loadSegmentation'),loadCsvButton=el('loadSegmentationCsv'),loadMaskButton=el('loadSegmentationMask'),clearButton=el('clearSegmentation'),file=el('segmentationFile'),tableFile=el('segmentationTableFile'),maskFile=el('segmentationMaskFile'),radius=el('segCellRadius');if(exportButton)exportButton.onclick=exportSelectedRoiForSegmentation;if(startButton)startButton.onclick=startSegmentationForSelectedRoi;if(loadButton&&file)loadButton.onclick=()=>{file.value='';file.click();};if(loadCsvButton&&tableFile)loadCsvButton.onclick=()=>{tableFile.value='';tableFile.click();};if(loadMaskButton&&maskFile)loadMaskButton.onclick=()=>{maskFile.value='';maskFile.click();};if(clearButton)clearButton.onclick=clearSegmentationOverlays;if(radius){radius.oninput=()=>segmentationCellRadius();segmentationCellRadius();}if(file){file.onchange=()=>{const picked=file.files&&file.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addSegmentationGeojson(JSON.parse(reader.result));}catch(e){segmentationStatus('Could not read cell GeoJSON: '+e.message);}};reader.readAsText(picked);};}if(tableFile){tableFile.onchange=()=>{const picked=tableFile.files&&tableFile.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addSegmentationCentroidTable(reader.result,picked.name);}catch(e){segmentationStatus('Could not read cell centroid table: '+e.message);}};reader.readAsText(picked);};}if(maskFile){maskFile.onchange=()=>{const picked=maskFile.files&&maskFile.files[0];if(!picked)return;loadSegmentationMaskFile(picked);};}segmentationStatus('');}\n"
  )
}

wsi_viewer_project_js <- function() {
  paste0(
    "const projectItems=(cfg.project&&cfg.project.items)||[];\n",
    "let activeProjectIndex=Number((cfg.project&&cfg.project.active_index)||0);\n",
    "let activeProjectSectionIndex=-1;\n",
    "let projectDragIndex=-1;\n",
    "let projectDragPayloadCache=null;\n",
    "const projectAnnotationStore=new Map();\n",
    "function cloneProjectValue(value){try{return JSON.parse(JSON.stringify(value));}catch(e){return value;}}\n",
    "function projectSectionIsPyramidLevel(section){if(!section)return false;const status=String(section.status||'').toLowerCase(),id=String(section.id||'').toLowerCase(),label=String(section.label||'').toLowerCase();return status==='pyramid level'||section.project_section_type==='pyramid_level'||/^level_[0-9]+$/.test(id)||/^level\\s+[0-9]+:/.test(label);}\n",
    "function projectSections(item){return (Array.isArray(item&&item.sections)?item.sections:[]).filter(section=>!projectSectionIsPyramidLevel(section));}\n",
    "function defaultProjectSectionIndex(item){const sections=projectSections(item);if(!sections.length)return -1;const source=String((item&&item.image_data_uri)||'');let idx=sections.findIndex(s=>s&&s.image_data_uri&&String(s.image_data_uri)===source);if(idx<0)idx=sections.findIndex(s=>s&&s.image_data_uri);return idx;}\n",
    "activeProjectSectionIndex=defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);\n",
    "function activeProjectSection(){const item=projectItems[activeProjectIndex]||null,sections=projectSections(item);return activeProjectSectionIndex>=0?sections[activeProjectSectionIndex]:null;}\n",
    "function projectSafeName(value){let x=String(value||'').replace(/\\.[^.]+$/,'').replace(/[^A-Za-z0-9_-]+/g,'_').replace(/^_+|_+$/g,'');return x||'annotations';}\n",
    "function projectItemBaseKey(item,index){return projectSafeName((item&&(item.id||item.path||item.label))||('image_'+(Number(index)+1)));}\n",
    "function projectItemUniqueKey(index){const item=projectItems[index]||{},base=projectItemBaseKey(item,index);let count=0;projectItems.forEach((other,j)=>{if(projectItemBaseKey(other,j)===base)count++;});return count>1?base+'__item_'+(Number(index)+1):base;}\n",
    "function projectAnnotationKey(index=activeProjectIndex,sectionIndex=activeProjectSectionIndex){const item=projectItems[index]||{},sections=projectSections(item),section=sectionIndex>=0?sections[sectionIndex]:null,itemId=projectItemUniqueKey(index),sectionId=section?projectSafeName(section.id||section.label||('section_'+(sectionIndex+1))):'image';return itemId+'::'+sectionId;}\n",
    "function projectAnnotationSnapshotForStore(){return {rois:cloneProjectValue(rois),selectedRoi:selectedRoi,newRoiCount:newRoiCount,dirty:annotationsDirty,dirty_reason:annotationDirtyReason||'',undo:cloneProjectValue(annotationUndo||[]),redo:cloneProjectValue(annotationRedo||[]),trajectories:cloneProjectValue(typeof trajectoryPayload==='function'?trajectoryPayload():trajectories),selectedTrajectory:selectedTrajectory,trajectorySeq:trajectorySeq,measures:cloneProjectValue(measures||[]),selectedMeasure:selectedMeasure};}\n",
    "function saveActiveProjectAnnotations(){if(!projectItems.length)return;projectAnnotationStore.set(projectAnnotationKey(),projectAnnotationSnapshotForStore());}\n",
    "let projectAnnotationStorePreloaded=false;\n",
    "const initialProjectLayers=cloneProjectValue(cfg.layers||[]);\n",
    "const initialProjectSeurat=cloneProjectValue(cfg.seurat||null);\n",
    "function preloadProjectAnnotations(){if(projectAnnotationStorePreloaded)return;projectAnnotationStorePreloaded=true;const sets=(cfg.project&&Array.isArray(cfg.project.annotation_sets))?cfg.project.annotation_sets:[];sets.forEach(set=>{if(!set)return;const key=String(set.key||'');if(!key)return;projectAnnotationStore.set(key,{rois:cloneProjectValue(set.rois||[]),selectedRoi:Number.isFinite(Number(set.selectedRoi))?Number(set.selectedRoi):-1,newRoiCount:Number.isFinite(Number(set.newRoiCount))?Number(set.newRoiCount):(set.rois||[]).length,dirty:!!set.dirty,dirty_reason:String(set.dirty_reason||''),undo:cloneProjectValue(set.undo||[]),redo:cloneProjectValue(set.redo||[]),trajectories:cloneProjectValue(set.trajectories||[]),selectedTrajectory:Number.isFinite(Number(set.selectedTrajectory))?Number(set.selectedTrajectory):-1,trajectorySeq:Number.isFinite(Number(set.trajectorySeq))?Number(set.trajectorySeq):(set.trajectories||[]).length,measures:cloneProjectValue(set.measures||[]),selectedMeasure:Number.isFinite(Number(set.selectedMeasure))?Number(set.selectedMeasure):-1});});}\n",
    "function loadProjectAnnotations(redraw=true){if(!projectItems.length)return;const key=projectAnnotationKey();let state=projectAnnotationStore.get(key);if(!state){state={rois:[],selectedRoi:-1,newRoiCount:0,dirty:false,dirty_reason:'',undo:[],redo:[],trajectories:[],selectedTrajectory:-1,trajectorySeq:0,measures:[],selectedMeasure:-1};projectAnnotationStore.set(key,cloneProjectValue(state));}rois.splice(0,rois.length);(cloneProjectValue(state.rois)||[]).forEach(roi=>rois.push(roi));selectedRoi=Math.min(Math.max(Number(state.selectedRoi||-1),-1),rois.length-1);if(!Number.isFinite(selectedRoi))selectedRoi=-1;newRoiCount=Number.isFinite(Number(state.newRoiCount))?Number(state.newRoiCount):0;annotationUndo.splice(0,annotationUndo.length);(cloneProjectValue(state.undo)||[]).forEach(x=>annotationUndo.push(x));annotationRedo.splice(0,annotationRedo.length);(cloneProjectValue(state.redo)||[]).forEach(x=>annotationRedo.push(x));trajectories.splice(0,trajectories.length);(cloneProjectValue(state.trajectories)||[]).forEach(x=>trajectories.push(x));selectedTrajectory=Math.min(Math.max(Number(state.selectedTrajectory||-1),-1),trajectories.length-1);if(!Number.isFinite(selectedTrajectory))selectedTrajectory=-1;trajectorySeq=Number.isFinite(Number(state.trajectorySeq))?Number(state.trajectorySeq):trajectories.length;if(Array.isArray(measures)){measures.splice(0,measures.length);(cloneProjectValue(state.measures)||[]).forEach(m=>measures.push(m));selectedMeasure=Math.min(Math.max(Number(state.selectedMeasure||-1),-1),measures.length-1);if(!Number.isFinite(selectedMeasure))selectedMeasure=-1;if(typeof updateMeasureList==='function')updateMeasureList();}draft=[];measureStart=null;trajectoryDraft=[];brushing=false;brushPoints=[];activeVertex=null;draggingVertex=null;setAnnotationsDirty(!!state.dirty,state.dirty_reason||'',false);if(typeof buildRoiList==='function')buildRoiList();if(typeof updateTrajectoryList==='function')updateTrajectoryList();if(typeof updateButtons==='function')updateButtons();if(redraw&&typeof draw==='function')draw();}\n",
    "function projectAnnotationCounts(){const counts=[];projectAnnotationStore.forEach((state,key)=>counts.push({key:key,count:(state.rois||[]).length,trajectory_count:(state.trajectories||[]).length,dirty:!!state.dirty}));return counts;}\n",
    "function projectAnnotationFilename(){if(!projectItems.length)return cfg.annotation_filename||'wsiTools_annotations.geojson';const item=projectItems[activeProjectIndex]||{},section=activeProjectSection(),base=projectSafeName(item.label||item.path||item.id||'wsiTools'),suffix=section?projectSafeName(section.label||section.id||('section_'+(activeProjectSectionIndex+1))):'image';return base+'_'+suffix+'_annotations.geojson';}\n",
    "function projectStatePayload(){saveActiveProjectAnnotations();const item=projectItems[activeProjectIndex]||null,section=activeProjectSection();return {active_index:activeProjectIndex,active_section_index:activeProjectSectionIndex,active_key:projectAnnotationKey(),active:item?{id:item.id||null,label:item.label||null,path:item.path||null,backend:item.backend||null,type:item.type||null,status:item.status||null}:null,section:section?{id:section.id||null,label:section.label||null,scene:section.scene||null,status:section.status||null}:null,count:projectItems.length,annotation_sets:projectAnnotationCounts()};}\n",
    "function projectAnnotationSetsFull(){saveActiveProjectAnnotations();const sets=[];projectAnnotationStore.forEach((state,key)=>sets.push(Object.assign({key:key},cloneProjectValue(state))));return sets;}\n",
    "function projectUndoSnapshot(action='project_changed'){saveActiveProjectAnnotations();return {action:action,project_items:cloneProjectValue(projectItems),active_project_index:activeProjectIndex,active_project_section_index:activeProjectSectionIndex,project_annotation_sets:projectAnnotationSetsFull()};}\n",
    "function restoreProjectUndoSnapshot(snapshot,eventName='project_undo'){if(!snapshot||!Array.isArray(snapshot.project_items))return false;const keepUndo=cloneProjectValue(annotationUndo||[]),keepRedo=cloneProjectValue(annotationRedo||[]);projectItems.splice(0,projectItems.length);(cloneProjectValue(snapshot.project_items)||[]).forEach(item=>projectItems.push(item));projectAnnotationStore.clear();(Array.isArray(snapshot.project_annotation_sets)?snapshot.project_annotation_sets:[]).forEach(set=>{if(set&&set.key)projectAnnotationStore.set(String(set.key),cloneProjectValue(set));});projectAnnotationStorePreloaded=true;activeProjectIndex=clamp(Number(snapshot.active_project_index||0),0,Math.max(0,projectItems.length-1));activeProjectSectionIndex=Number.isFinite(Number(snapshot.active_project_section_index))?Number(snapshot.active_project_section_index):defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);loadProjectAnnotations(false);annotationUndo.splice(0,annotationUndo.length);(keepUndo||[]).forEach(x=>annotationUndo.push(x));annotationRedo.splice(0,annotationRedo.length);(keepRedo||[]).forEach(x=>annotationRedo.push(x));if(typeof ensureProjectWorkspaceVisible==='function')ensureProjectWorkspaceVisible();if(typeof renderProjectPanel==='function')renderProjectPanel();if(typeof updateProjectPanelToggle==='function')updateProjectPanelToggle();const item=projectItems[activeProjectIndex]||null;if(item&&projectSwitchable(item))applyProjectPreview(item,activeProjectSection());else if(typeof draw==='function')draw();setProjectDirty(true,eventName||snapshot.action||'project_restored');projectMenuStatus('Restored closed image.');scheduleViewerStateSync(eventName||'project_undo',Object.assign({restored_project:true},projectStatePayload()));return true;}\n",
    "function projectItemsForSnapshot(includeImageData=true){return cloneProjectValue(projectItems).map(item=>{if(includeImageData)return item;const strip=obj=>{if(!obj)return obj;delete obj.image_data_uri;delete obj.navigator_image_data_uri;return obj;};strip(item);if(Array.isArray(item.sections))item.sections=item.sections.map(strip);return item;});}\n",
    "function projectBrowserSnapshot(includeImageData=true){saveActiveProjectAnnotations();return {schema:'wsiTools-viewer-project',schema_version:1,saved_at:new Date().toISOString(),title:cfg.title||'wsiTools project',viewer_mode:cfg.viewer_mode||'viewer',slide:{width:Number(cfg.slide_width||0),height:Number(cfg.slide_height||0),mpp:cfg.mpp||null,objective_power:cfg.objective_power||null},project:{items:projectItemsForSnapshot(includeImageData),active_index:activeProjectIndex,active_section_index:activeProjectSectionIndex,annotation_sets:projectAnnotationSetsFull()},rois:cloneProjectValue(rois),trajectories:(typeof trajectoryPayload==='function'?trajectoryPayload():cloneProjectValue(trajectories)),measurements:cloneProjectValue(measures||[]),layers:cloneProjectValue(layers||[]),channel_sources:cloneProjectValue(typeof channelSources!=='undefined'?channelSources:[]),channel_settings:(typeof currentChannelSettingsPayload==='function'?currentChannelSettingsPayload():[]),stain:(typeof currentStainPayload==='function'?currentStainPayload():null),view:{mode:mode,scale:scale,offset_x:offsetX,offset_y:offsetY,roi_opacity:roiOpacity,show_rois:showRois,show_labels:showLabels,image_transform:(typeof imageTransformPayload==='function'?imageTransformPayload():null),base_layer:(typeof baseImagePayload==='function'?baseImagePayload():null)},annotations:{dirty:!!(annotationsDirty||projectDirty),dirty_reason:annotationDirtyReason||projectDirtyReason||'',annotation_dirty:annotationsDirty,project_dirty:projectDirty},history:(typeof annotationHistoryPayload==='function'?annotationHistoryPayload():[]),logs:(typeof viewerLogPayload==='function'?viewerLogPayload():[])};}\n",
    "function projectSnapshotName(){return projectSafeName(cfg.title||'wsiTools_project')+'.wsiproject.json';}\n",
    "function setProjectDirty(value=true,reason='project_changed'){projectDirty=!!value;projectDirtyReason=reason||'';updateAnnotationDirtyIndicator();}\n",
    "function markProjectDirty(reason='project_changed'){setProjectDirty(true,reason);}\n",
    "function projectHasUnsavedChanges(){saveActiveProjectAnnotations();let dirty=!!(annotationsDirty||projectDirty);projectAnnotationStore.forEach(state=>{if(state&&state.dirty)dirty=true;});return dirty;}\n",
    "async function confirmProjectReplacement(actionLabel='opening a new project'){if(!projectHasUnsavedChanges())return true;const saveFirst=window.confirm('You have an unsaved project. Do you want to save it before '+actionLabel+'?');if(saveFirst){const saved=await saveProjectFile();if(saved)return true;notify('Opening cancelled; current project was not saved.','info',3200);return false;}const proceed=window.confirm('Open the new project without saving current changes?');if(!proceed)notify('Opening cancelled','info',1800);return proceed;}\n",
    "async function saveProjectFile(){const name=projectSnapshotName(),wasAnnotationDirty=annotationsDirty,wasProjectDirty=projectDirty,wasAnnotationReason=annotationDirtyReason,wasProjectReason=projectDirtyReason;try{const snapshot=projectBrowserSnapshot(true),text=JSON.stringify(snapshot,null,2),blob=new Blob([text],{type:'application/json'});const mode=typeof saveBlobWithLocation==='function'?await saveBlobWithLocation(blob,name,{description:'wsiTools viewer project',accept:{'application/json':['.json','.wsiproject.json']}}):'unsupported';if(mode==='cancelled'||mode==='unsupported')return false;setProjectDirty(false,'project_file_saved');markAnnotationsSaved('project_file_saved');saveActiveProjectAnnotations();recordAnnotationHistory('project_saved',{name:name,image_count:projectItems.length,annotation_sets:snapshot.project.annotation_sets.length},false);scheduleViewerStateSync('project_saved',{mode:'browser_file',project_snapshot:projectBrowserSnapshot(false)});projectMenuStatus('Saved '+name+'.');notify('Project saved','success',2200);return true;}catch(e){if(e&&e.name==='AbortError')return false;setAnnotationsDirty(wasAnnotationDirty,wasAnnotationReason||'project_save_failed',false);setProjectDirty(wasProjectDirty,wasProjectReason||'project_save_failed');projectMenuStatus('Could not save project: '+e.message);notify('Project save failed','error',4200);return false;}}\n",
    "function restoreBrowserProject(snapshot){if(!snapshot||snapshot.schema!=='wsiTools-viewer-project')throw new Error('This is not a wsiTools viewer project JSON.');saveActiveProjectAnnotations();const project=snapshot.project||{},items=Array.isArray(project.items)?project.items:[];if(!items.length)throw new Error('Project contains no images.');projectItems.splice(0,projectItems.length);items.forEach(item=>projectItems.push(item));activeProjectIndex=clamp(Number(project.active_index||0),0,Math.max(0,projectItems.length-1));activeProjectSectionIndex=Number.isFinite(Number(project.active_section_index))?Number(project.active_section_index):defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);projectAnnotationStore.clear();const sets=Array.isArray(project.annotation_sets)?project.annotation_sets:[];if(sets.length){sets.forEach(set=>{if(set&&set.key)projectAnnotationStore.set(String(set.key),cloneProjectValue(set));});}else{projectAnnotationStore.set(projectAnnotationKey(),{rois:cloneProjectValue(snapshot.rois||[]),selectedRoi:-1,newRoiCount:(snapshot.rois||[]).length,dirty:false,dirty_reason:'',undo:[],redo:[],trajectories:cloneProjectValue(snapshot.trajectories||[]),selectedTrajectory:-1,trajectorySeq:(snapshot.trajectories||[]).length,measures:cloneProjectValue(snapshot.measurements||[]),selectedMeasure:-1});}projectAnnotationStorePreloaded=true;setProjectDirty(false,'project_opened');loadProjectAnnotations(false);if(Array.isArray(snapshot.measurements)&&!sets.length){measures.splice(0,measures.length);snapshot.measurements.forEach(m=>measures.push(m));if(typeof updateMeasureList==='function')updateMeasureList();}if(Array.isArray(snapshot.layers)){layers.splice(0,layers.length);snapshot.layers.forEach(layer=>layers.push(layer));if(typeof buildLayerList==='function')buildLayerList();}if(Array.isArray(snapshot.channel_sources)&&typeof upsertChannelSource==='function')snapshot.channel_sources.forEach(upsertChannelSource);if(Array.isArray(snapshot.channel_settings)&&typeof setChannelSettings==='function')snapshot.channel_settings.forEach(s=>setChannelSettings(s.id,s));if(snapshot.stain&&typeof applyStainPreferences==='function')applyStainPreferences({stain:snapshot.stain});openProjectPanel();renderProjectPanel();const item=projectItems[activeProjectIndex]||null;applyProjectPreview(item,activeProjectSection());recordAnnotationHistory('project_opened',{image_count:projectItems.length,source:'file'},false);scheduleViewerStateSync('project_opened',{mode:'browser_file',project_snapshot:projectBrowserSnapshot(false)});projectMenuStatus('Opened saved project with '+projectItems.length+' image'+(projectItems.length===1?'':'s')+'.');notify('Project opened','success',2200);}\n",
    "function openProjectFile(file){if(!file)return;projectMenuStatus('Opening '+file.name+'...');const reader=new FileReader();reader.onerror=()=>{projectMenuStatus('Could not read project file.');notify('Could not read project file','error');};reader.onload=()=>{try{restoreBrowserProject(JSON.parse(String(reader.result||'')));}catch(e){projectMenuStatus('Could not open project: '+e.message);notify('Project open failed','error',5200);}};reader.readAsText(file);}\n",
    "function projectItemCanPreview(item){return !!(item&&item.image_data_uri);}\n",
    "function projectItemCanTile(item){return !!(item&&(item.tile_url_base||item.tile_url_template)&&item.tile_format&&Number.isFinite(Number(item.max_level))&&Number.isFinite(Number(item.tile_size||cfg.tile_size||0)));}\n",
    "const initialProjectSource={width:Number(cfg.slide_width||0),height:Number(cfg.slide_height||0),image_data_uri:String(cfg.image_data_uri||''),navigator_image_data_uri:String(cfg.navigator_image_data_uri||''),tile_size:Number(cfg.tile_size||0),tile_format:String(cfg.tile_format||''),tile_url_base:String(cfg.tile_url_base||''),tile_url_template:String(cfg.tile_url_template||''),tile_url_style:String(cfg.tile_url_style||'deepzoom'),tile_overlap:Number(cfg.tile_overlap||0),min_level:Number(cfg.min_level||0),max_level:Number(cfg.max_level||0)};\n",
    "function projectInitialSource(item){if(!item||!item.active)return null;const hasTiles=!!((initialProjectSource.tile_url_base||initialProjectSource.tile_url_template)&&initialProjectSource.tile_format&&Number.isFinite(Number(initialProjectSource.max_level))&&Number(initialProjectSource.max_level)>0&&Number.isFinite(Number(initialProjectSource.tile_size)));const hasImage=!!initialProjectSource.image_data_uri;if(!hasTiles&&!hasImage)return null;return Object.assign({id:'active_project_image',label:item.label||cfg.title||'Active image',status:'active'},initialProjectSource,{width:Number(item.width||initialProjectSource.width||cfg.slide_width),height:Number(item.height||initialProjectSource.height||cfg.slide_height)});}\n",
    "function projectDisplaySource(item,section=null){if(projectItemCanTile(section))return section;if(projectItemCanTile(item))return item;const active=projectInitialSource(item);if(active)return active;return section||item;}\n",
    "function projectPayloadSource(item,section=null){return projectDisplaySource(item,section)||section||item||{};}\n",
    "function projectPayloadValue(item,section,field){const src=projectPayloadSource(item,section);if(src&&src[field]!=null)return cloneProjectValue(src[field]);if(section&&section[field]!=null)return cloneProjectValue(section[field]);if(item&&item[field]!=null)return cloneProjectValue(item[field]);if(item&&item.active&&field==='layers')return cloneProjectValue(initialProjectLayers||[]);if(item&&item.active&&field==='seurat')return cloneProjectValue(initialProjectSeurat||null);return null;}\n",
    "function normalizeProjectMpp(value){if(!value)return null;let x=NaN,y=NaN;if(Array.isArray(value)){x=Number(value[0]);y=Number(value.length>1?value[1]:value[0]);}else{x=Number(value.x);y=Number(value.y!=null?value.y:value.x);}return Number.isFinite(x)&&Number.isFinite(y)&&x>0&&y>0?{x:x,y:y}:null;}\n",
    "function projectMppValue(item,section=null){return normalizeProjectMpp(projectPayloadValue(item,section,'mpp')||projectPayloadValue(item,section,'pixel_size'));}\n",
    "function projectObjectivePowerValue(item,section=null){const v=Number(projectPayloadValue(item,section,'objective_power'));return Number.isFinite(v)&&v>0?v:null;}\n",
    "function applyProjectScaleMetadata(item,section=null){cfg.mpp=projectMppValue(item,section);cfg.objective_power=projectObjectivePowerValue(item,section);if(typeof updateScaleBar==='function')updateScaleBar();}\n",
    "function markInitialProjectManagedLayers(){if(!Array.isArray(layers))return;layers.forEach(layer=>{if(layer&&(layer.project_scoped||String(layer.source_type||'')==='seurat_spots'))layer.project_managed=true;});}\n",
    "function removeProjectManagedLayers(){if(!Array.isArray(layers))return;for(let i=layers.length-1;i>=0;i--){if(layers[i]&&layers[i].project_managed)layers.splice(i,1);}}\n",
    "function applyProjectLayers(item,section=null){if(!Array.isArray(layers))return;markInitialProjectManagedLayers();removeProjectManagedLayers();const next=projectPayloadValue(item,section,'layers');if(Array.isArray(next)){next.forEach(layer=>{if(!layer)return;layer.project_managed=true;layers.push(layer);});}if(typeof buildLayerList==='function')buildLayerList();}\n",
    "function applyProjectSeurat(item,section=null){const next=projectPayloadValue(item,section,'seurat');cfg.seurat=next||{enabled:false,plots:[],spot_count:0};if(typeof clearSpatialTilePreview==='function')clearSpatialTilePreview(false,false);if(typeof refreshSeuratSelectionAfterProjectSwitch==='function')refreshSeuratSelectionAfterProjectSwitch();else{if(typeof seuratSelectedLabels!=='undefined'&&seuratSelectedLabels&&seuratSelectedLabels.clear)seuratSelectedLabels.clear();if(typeof removeSeuratSelectionLayer==='function')removeSeuratSelectionLayer();}if(typeof updateSeuratControls==='function')updateSeuratControls();const plotPanel=el('seuratPlotWindow');if(plotPanel&&plotPanel.classList.contains('open')&&typeof renderSeuratPlotWindow==='function')renderSeuratPlotWindow();else if(typeof drawSeuratPlot==='function')drawSeuratPlot();if(typeof updateSeuratSelectionStatus==='function')updateSeuratSelectionStatus();}\n",
    "function applyProjectPayloads(item,section=null){applyProjectScaleMetadata(item,section);applyProjectLayers(item,section);applyProjectSeurat(item,section);}\n",
    "function projectItemMessage(item){return (item&&String(item.message||'').trim())||'';}\n",
    "function projectItemStatus(item){return (item&&String(item.status||'').trim())||'ready';}\n",
    "function projectSwitchable(item){return projectItemCanTile(item)||projectItemCanPreview(item)||!!(item&&item.active);}\n",
    "function projectNavigatorSource(item,section=null,display=null){return (display&&display.navigator_image_data_uri)||(section&&section.navigator_image_data_uri)||(item&&item.navigator_image_data_uri)||(display&&display.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri)||'';}\n",
    "function projectNavigatorSourceKey(item,section=null){const itemId=String((item&&(item.id||item.path||item.label))||'image'),sectionId=section?String(section.id||section.label||'section'):'main';return itemId+'::'+sectionId+'::'+activeProjectIndex+'::'+activeProjectSectionIndex;}\n",
    "function clearProjectDragClasses(){document.querySelectorAll('.projectItem.dragging,.projectItem.dragOver').forEach(item=>item.classList.remove('dragging','dragOver'));}\n",
    "function projectDragSectionIndex(itemIndex,sectionIndex=-1){itemIndex=Number(itemIndex);sectionIndex=Number(sectionIndex);if(sectionIndex>=0)return sectionIndex;const item=projectItems[itemIndex]||null,sections=projectSections(item);if(!sections.length)return -1;if(itemIndex===activeProjectIndex&&activeProjectSectionIndex>=0)return activeProjectSectionIndex;return defaultProjectSectionIndex(item);}\n",
    "function projectEntryDragPayload(itemIndex,sectionIndex=-1){itemIndex=Number(itemIndex);sectionIndex=projectDragSectionIndex(itemIndex,sectionIndex);const item=projectItems[itemIndex]||null,section=sectionIndex>=0&&item?projectSections(item)[sectionIndex]||null:null,key=(typeof multiViewProjectEntryKey==='function')?multiViewProjectEntryKey({item:item,section:section,itemIndex:itemIndex,sectionIndex:sectionIndex}):String(itemIndex)+':'+String(sectionIndex);return JSON.stringify({type:'wsiTools.projectEntry',itemIndex:itemIndex,sectionIndex:sectionIndex,key:key,label:(section&&section.label)||(item&&item.label)||(item&&item.path)||''});}\n",
    "function setProjectDragPayloadCache(payload,itemIndex,sectionIndex=-1){projectDragPayloadCache={payload:payload,itemIndex:Number(itemIndex),sectionIndex:Number(sectionIndex),set_at:Date.now()};}\n",
    "function clearProjectDragPayloadSoon(){setTimeout(()=>{projectDragPayloadCache=null;projectDragIndex=-1;clearProjectDragClasses();},650);}\n",
    "function setProjectEntryDragData(e,itemIndex,sectionIndex=-1,effect='copyMove'){const payload=projectEntryDragPayload(itemIndex,sectionIndex);let parsed=null;try{parsed=JSON.parse(payload);}catch(err){}setProjectDragPayloadCache(payload,itemIndex,parsed&&Number.isFinite(Number(parsed.sectionIndex))?Number(parsed.sectionIndex):sectionIndex);if(!e||!e.dataTransfer)return;try{e.dataTransfer.effectAllowed=effect;e.dataTransfer.setData('application/x-wsitools-project-entry',payload);e.dataTransfer.setData('text/x-wsitools-project-entry',payload);e.dataTransfer.setData('text/plain',payload);}catch(err){}}\n",
    "function moveProjectItem(from,to){from=Number(from);to=Number(to);if(!Number.isInteger(from)||!Number.isInteger(to)||from<0||from>=projectItems.length)return false;to=clamp(to,0,Math.max(0,projectItems.length-1));if(from===to)return false;saveActiveProjectAnnotations();const activeItem=projectItems[activeProjectIndex]||null,activeSection=activeProjectSectionIndex,moved=projectItems.splice(from,1)[0];projectItems.splice(to,0,moved);activeProjectIndex=activeItem?projectItems.indexOf(activeItem):to;if(activeProjectIndex<0)activeProjectIndex=to;activeProjectSectionIndex=activeSection;renderProjectPanel();markProjectDirty('project_image_reordered');recordAnnotationHistory('project_image_reordered',{from:from+1,to:to+1,label:moved&&moved.label||null},false);scheduleViewerStateSync('project_image_reordered',projectStatePayload());projectMenuStatus('Project image order updated.');notify('Project image order updated','success',1400);return true;}\n",
    "function projectAnnotationKeysForItem(index){const keys=[];if(index<0||index>=projectItems.length)return keys;keys.push(projectAnnotationKey(index,-1));projectSections(projectItems[index]).forEach((section,i)=>keys.push(projectAnnotationKey(index,i)));return Array.from(new Set(keys));}\n",
    "function removeProjectItem(index){index=Number(index);if(!Number.isInteger(index)||index<0||index>=projectItems.length)return false;if(projectItems.length<=1){projectMenuStatus('At least one project image must stay open.');notify('At least one project image must stay open.','warning',2600);return false;}saveActiveProjectAnnotations();const undoSnapshot=annotationSnapshot();if(typeof projectUndoSnapshot==='function')undoSnapshot.project=projectUndoSnapshot('project_image_closed');pushHistory(annotationUndo,undoSnapshot);annotationRedo=[];const removed=projectItems[index]||{},removedLabel=removed.label||removed.path||('Image '+(index+1)),removedKeys=projectAnnotationKeysForItem(index),wasActive=index===activeProjectIndex;projectItems.splice(index,1);removedKeys.forEach(key=>projectAnnotationStore.delete(key));if(wasActive){activeProjectIndex=Math.min(index,projectItems.length-1);if(!projectSwitchable(projectItems[activeProjectIndex])){const alt=projectItems.findIndex(projectSwitchable);if(alt>=0)activeProjectIndex=alt;}activeProjectSectionIndex=defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);renderProjectPanel();const next=projectItems[activeProjectIndex]||null;if(next&&projectSwitchable(next))applyProjectPreview(next,activeProjectSection());else{loadProjectAnnotations(false);if(typeof draw==='function')draw();}}else{if(index<activeProjectIndex)activeProjectIndex-=1;renderProjectPanel();}markProjectDirty('project_image_closed');recordAnnotationHistory('project_image_closed',{index:index+1,label:removedLabel},false);scheduleViewerStateSync('project_image_closed',Object.assign({closed:{index:index+1,label:removedLabel}},projectStatePayload()));projectMenuStatus('Closed '+removedLabel+'. Press Ctrl+Z to undo.');notifyAction('Closed '+removedLabel+'.','Undo',()=>restoreAnnotationUndo(),'success',7000);return true;}\n",
    "function bindProjectItemDrag(button,index){button.draggable=projectItems.length>1||projectSwitchable(projectItems[index]);button.dataset.projectIndex=String(index);button.ondragstart=e=>{projectDragIndex=index;button.classList.add('dragging');setProjectEntryDragData(e,index,-1,'copyMove');try{e.dataTransfer.setData('application/x-wsitools-project-reorder',String(index));}catch(err){}};button.ondragover=e=>{if(projectDragIndex<0||projectItems.length<=1)return;e.preventDefault();button.classList.add('dragOver');try{e.dataTransfer.dropEffect='move';}catch(err){}};button.ondragleave=()=>button.classList.remove('dragOver');button.ondrop=e=>{if(projectItems.length<=1)return;const rawType=e.dataTransfer?e.dataTransfer.getData('application/x-wsitools-project-reorder'):'';if(!rawType&&!String(e.dataTransfer&&e.dataTransfer.getData('text/plain')||'').match(/^\\d+$/))return;e.preventDefault();let raw=rawType;if(!raw&&e.dataTransfer){const plain=e.dataTransfer.getData('text/plain');if(/^\\d+$/.test(String(plain||'')))raw=plain;}const from=Number.isFinite(Number(raw))?Number(raw):projectDragIndex;let to=index;const rect=button.getBoundingClientRect();if(e.clientY>rect.top+rect.height/2)to=index+1;if(from<to)to--;clearProjectDragClasses();projectDragIndex=-1;projectDragPayloadCache=null;moveProjectItem(from,to);};button.ondragend=()=>clearProjectDragPayloadSoon();}\n",
    "function bindProjectSectionDrag(button,itemIndex,sectionIndex){button.draggable=true;button.dataset.projectIndex=String(itemIndex);button.dataset.projectSectionIndex=String(sectionIndex);button.ondragstart=e=>{projectDragIndex=itemIndex;setProjectEntryDragData(e,itemIndex,sectionIndex,'copy');button.classList.add('dragging');};button.ondragend=()=>{button.classList.remove('dragging');clearProjectDragPayloadSoon();};}\n",
    "function projectMenuStatus(message=''){const box=el('projectMenuSummary');if(!box)return;if(message){box.textContent=message;box.style.display='';}else{box.textContent='';box.style.display='none';}}\n",
    "function projectPanelIsClosed(){const panel=el('projectPanel');return !!(panel&&(panel.classList.contains('closed')||panel.style.display==='none'));}\n",
    "function updateProjectPanelToggle(){const button=el('projectPanelToggle'),closed=projectPanelIsClosed();if(button)button.classList.toggle('active',!closed);}\n",
    "function ensureProjectWorkspaceVisible(options={}){const workspace=el('workspacePanel'),panel=el('projectPanel'),preserve=!!(options&&options.preserve_state);if(workspace){workspace.style.visibility='visible';workspace.style.opacity='1';workspace.style.pointerEvents='auto';workspace.removeAttribute('aria-hidden');const rect=workspace.getBoundingClientRect(),safeTop=(typeof workspacePanelSafeTop==='function')?workspacePanelSafeTop():72;if(rect.width<8||rect.height<8||rect.right<24||rect.bottom<72||rect.left>innerWidth-24||rect.top<safeTop-1||rect.top>innerHeight-24){workspace.style.left='12px';workspace.style.top=safeTop+'px';workspace.style.right='auto';}}if(panel&&!preserve){panel.style.display='';panel.classList.remove('closed','minimized');const header=el('projectPanelHeader'),state=el('projectPanelMinimizeState');if(header)header.setAttribute('aria-expanded','true');if(state)state.textContent='double-click to minimize';}}\n",
    "function openProjectPanel(){ensureProjectWorkspaceVisible();renderProjectPanel();updateProjectPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();projectMenuStatus('Project panel open.');notify('Project panel opened','success',1400);}\n",
    "function addProjectImageDataUri(dataUri,fileName,width,height,options={}){if(!dataUri)return null;options=options||{};saveActiveProjectAnnotations();const index=projectItems.length+1,item={id:'browser_image_'+Date.now()+'_'+index,label:String(fileName||('Browser image '+index)),path:String(fileName||'browser image'),backend:'browser',type:'image',status:'browser image',width:Number(width)||Number(cfg.slide_width)||1,height:Number(height)||Number(cfg.slide_height)||1,image_data_uri:String(dataUri),navigator_image_data_uri:String(dataUri),sections:[]};projectItems.push(item);const itemIndex=projectItems.length-1,activate=options.activate!==false;projectAnnotationStore.set(projectAnnotationKey(itemIndex,-1),{rois:[],selectedRoi:-1,newRoiCount:0,dirty:false,dirty_reason:'',undo:[],redo:[],trajectories:[],selectedTrajectory:-1,trajectorySeq:0,measures:[],selectedMeasure:-1});if(activate){activeProjectIndex=itemIndex;activeProjectSectionIndex=-1;}openProjectPanel();renderProjectPanel();if(activate&&options.apply!==false)applyProjectPreview(item,null);if(typeof refreshMultiViewSources==='function'&&options.refresh_multi_view!==false)refreshMultiViewSources();projectMenuStatus('Added '+item.label+' as a project image.');markProjectDirty('project_image_added');recordAnnotationHistory('project_image_added',{name:item.label,source:'browser'},false);scheduleViewerStateSync('project_image_added',projectStatePayload());return {item:item,index:itemIndex};}\n",
    "function projectFileExtension(fileName){const name=String(fileName||'').toLowerCase();if(name.endsWith('.ome.tif'))return 'ome.tif';if(name.endsWith('.ome.tiff'))return 'ome.tiff';if(name.endsWith('.ome.zarr'))return 'ome.zarr';const match=name.match(/\\.([^.]+)$/);return match?match[1]:'';}\n",
    "function projectBrowserReadableExtension(ext){return ['png','jpg','jpeg','webp','gif','bmp','avif'].includes(String(ext||'').toLowerCase());}\n",
    "function addProjectFileReference(fileName,fileType='',fileSize=null){const ext=projectFileExtension(fileName),label=String(fileName||('Project file '+(projectItems.length+1))),index=projectItems.length+1,msg='File reference added. Raw WSI/microscopy formats such as CZI, SVS, NDPI and OME-TIFF need R backends or a live/tiled project source for visualization.';saveActiveProjectAnnotations();const item={id:'browser_file_reference_'+Date.now()+'_'+index,label:label,path:label,backend:'browser-reference',type:ext||String(fileType||'file'),status:'needs backend',message:msg,width:Number(cfg.slide_width)||1,height:Number(cfg.slide_height)||1,sections:[],file_name:label,file_type:String(fileType||''),file_size:Number(fileSize)||null};projectItems.push(item);openProjectPanel();renderProjectPanel();projectMenuStatus('Added '+label+' as a project file reference. Open from R for full-resolution viewing.');markProjectDirty('project_image_added');recordAnnotationHistory('project_image_added',{name:label,source:'browser-reference',type:item.type},false);scheduleViewerStateSync('project_image_added',projectStatePayload());notify('Added file reference: '+label,'info',3600);}\n",
    "function loadProjectImageFile(file){if(!file)return;const ext=projectFileExtension(file.name);if(!projectBrowserReadableExtension(ext)){addProjectFileReference(file.name,file.type,file.size);return;}projectMenuStatus('Reading '+file.name+'...');const reader=new FileReader();reader.onerror=()=>{projectMenuStatus('Could not read image file.');notify('Could not read image file','error');};reader.onload=()=>{const dataUri=String(reader.result||''),img=new Image();img.onload=()=>addProjectImageDataUri(dataUri,file.name,img.naturalWidth,img.naturalHeight);img.onerror=()=>addProjectFileReference(file.name,file.type,file.size);img.src=dataUri;};reader.readAsDataURL(file);}\n",
    "function loadProjectImageFiles(fileList){const files=Array.from(fileList||[]);if(!files.length)return;projectMenuStatus('Adding '+files.length+' image'+(files.length===1?'':'s')+'...');files.forEach(loadProjectImageFile);}\n",
    "function setProjectPanelMinimized(minimized,save=true){const panel=el('projectPanel'),header=el('projectPanelHeader'),state=el('projectPanelMinimizeState');if(!panel)return;panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(state)state.textContent=minimized?'double-click to expand':'double-click to minimize';if(save&&typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleProjectPanelMinimized(){const panel=el('projectPanel');if(!panel)return;setProjectPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function setProjectPanelClosed(closed,save=true){const panel=el('projectPanel'),header=el('projectPanelHeader');if(!panel)return;closed=!!closed;panel.classList.toggle('closed',closed);panel.style.display=closed?'none':'';if(closed)panel.classList.remove('minimized');if(header)header.setAttribute('aria-expanded',closed?'false':'true');updateProjectPanelToggle();if(save&&typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function closeProjectPanel(e){if(e){e.preventDefault();e.stopPropagation();}setProjectPanelClosed(true);notify('Project panel closed','info',1600);return false;}\n",
    "function toggleProjectPanelClosed(){const panel=el('projectPanel');if(!panel)return;if(projectPanelIsClosed())openProjectPanel();else closeProjectPanel();}\n",
    "function bindProjectPanelControls(){const header=el('projectPanelHeader'),close=el('projectPanelClose'),toggle=el('projectPanelToggle'),openImage=el('projectOpenImage'),file=el('projectImageFile'),saveFile=el('projectSaveFile'),openFile=el('projectOpenFile'),projectFile=el('projectFile');if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.ondblclick=e=>{e.preventDefault();toggleProjectPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleProjectPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';['pointerdown','mousedown','dblclick'].forEach(name=>close.addEventListener(name,e=>{e.stopPropagation();}));close.onclick=e=>closeProjectPanel(e);}if(toggle&&toggle.dataset.bound!=='1'){toggle.dataset.bound='1';toggle.onclick=()=>{toggleProjectPanelClosed();};}if(openImage&&file&&openImage.dataset.bound!=='1'){openImage.dataset.bound='1';openImage.onclick=e=>{e.preventDefault();e.stopPropagation();file.value='';file.click();closeContainingToolMenu(e.currentTarget);};}if(saveFile&&saveFile.dataset.bound!=='1'){saveFile.dataset.bound='1';saveFile.onclick=e=>{e.preventDefault();e.stopPropagation();saveProjectFile();closeContainingToolMenu(e.currentTarget);};}if(openFile&&projectFile&&openFile.dataset.bound!=='1'){openFile.dataset.bound='1';openFile.onclick=async e=>{e.preventDefault();e.stopPropagation();const control=e.currentTarget;if(!(await confirmProjectReplacement('opening a new project')))return;projectFile.value='';projectFile.click();closeContainingToolMenu(control);};}if(file&&file.dataset.bound!=='1'){file.dataset.bound='1';file.onchange=()=>loadProjectImageFiles(file.files);}if(projectFile&&projectFile.dataset.bound!=='1'){projectFile.dataset.bound='1';projectFile.onchange=()=>{const picked=projectFile.files&&projectFile.files[0];if(picked)openProjectFile(picked);};}updateProjectPanelToggle();}\n",
    "function renderProjectPanel(){const panel=el('projectPanel'),summary=el('projectSummary'),list=el('projectImageList'),sections=el('projectSectionList');if(!panel||!summary||!list||!sections)return;if(!projectItems.length){summary.textContent='No project images configured.';list.innerHTML='';sections.innerHTML='';return;}summary.textContent=projectItems.length+' image'+(projectItems.length===1?'':'s')+' available. Annotations are stored separately for each image/section. Drag images to reorder them; annotations and trajectories stay with each image/section. Current section has '+rois.length+' ROI'+(rois.length===1?'':'s')+'.';list.innerHTML='';projectItems.forEach((item,i)=>{const row=document.createElement('div');row.className='projectItemRow';const b=document.createElement('button');b.type='button';b.className='projectItem';b.classList.toggle('active',i===activeProjectIndex);b.classList.toggle('unavailable',!projectSwitchable(item));b.setAttribute('aria-disabled',projectSwitchable(item)?'false':'true');b.title='Drag to reorder project images';bindProjectItemDrag(b,i);const name=document.createElement('span');name.className='projectName';name.textContent=item.label||item.path||('Image '+(i+1));const status=document.createElement('span');status.className='projectStatus';status.textContent=projectItemStatus(item);const path=document.createElement('span');path.className='projectPath';path.textContent=item.path||item.backend||'';b.append(name,status,path);const msg=projectItemMessage(item);if(msg){const m=document.createElement('span');m.className='projectMessage';m.textContent=msg;b.appendChild(m);}b.onclick=()=>switchProjectItem(i);const close=document.createElement('button');close.type='button';close.className='projectItemClose';close.textContent='X';close.title='Close this project image';close.setAttribute('aria-label','Close '+(item.label||item.path||('image '+(i+1))));close.disabled=projectItems.length<=1;close.onclick=e=>{e.preventDefault();e.stopPropagation();removeProjectItem(i);};row.append(b,close);list.appendChild(row);});renderProjectSections();}\n",
    "function renderProjectSections(){const sections=el('projectSectionList');if(!sections)return;sections.innerHTML='';const item=projectItems[activeProjectIndex]||null;const values=projectSections(item);if(!values.length)return;const title=document.createElement('div');title.className='sideMeta';title.textContent='Sections';sections.appendChild(title);values.forEach((section,i)=>{const b=document.createElement('button');b.type='button';b.className='projectSectionItem';b.classList.toggle('active',i===activeProjectSectionIndex);b.disabled=!section.image_data_uri&&!projectItemCanTile(section);if(!b.disabled)bindProjectSectionDrag(b,activeProjectIndex,i);const name=document.createElement('span');name.className='projectName';name.textContent=section.label||section.id||('Section '+(i+1));const status=document.createElement('span');status.className='projectStatus';const saved=projectAnnotationStore.get(projectAnnotationKey(activeProjectIndex,i));const n=saved&&saved.rois?saved.rois.length:(i===activeProjectSectionIndex?rois.length:0);status.textContent=(section.status||'')+(n?(' | '+n+' ROI'+(n===1?'':'s')):'');b.append(name,status);const msg=section.message||'';if(msg){const m=document.createElement('span');m.className='projectMessage';m.textContent=msg;b.appendChild(m);}b.onclick=()=>switchProjectSection(i);sections.appendChild(b);});}\n",
    "function projectTileSourceFromItem(item,section=null){const source=projectDisplaySource(item,section);if(projectItemCanTile(source)){const src=source,base=String(src.tile_url_base||''),template=String(src.tile_url_template||''),fmt=String(src.tile_format),style=String(src.tile_url_style||'deepzoom'),tileSize=Number(src.tile_size||cfg.tile_size),maxLevel=Number(src.max_level),out={width:Number(src.width||(item&&item.width)||cfg.slide_width),height:Number(src.height||(item&&item.height)||cfg.slide_height),tileSize:tileSize,tileOverlap:Number(src.tile_overlap||0),minLevel:Number(src.min_level||0),maxLevel:maxLevel,getTileUrl:(level,x,y)=>tileUrlFromParts(base,template,style,fmt,level,x,y)};return withTileCors(out,base,template);}const imageSource=(source&&source.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri);if(imageSource)return {type:'image',url:imageSource};return null;}\n",
    "function projectContentBounds(item,section=null){const source=projectDisplaySource(item,section),tile=projectItemCanTile(source);const b=(source&&source.content_bbox)||(!tile&&section&&section.content_bbox)||(item&&item.content_bbox)||null;if(!b)return null;const vals=['xmin','ymin','xmax','ymax'].map(k=>Number(b[k]));if(vals.some(v=>!Number.isFinite(v)))return null;if(vals[2]<=vals[0]||vals[3]<=vals[1])return null;return {xmin:vals[0],ymin:vals[1],xmax:vals[2],ymax:vals[3]};}\n",
    "function zoomToProjectContent(item,section=null){const b=projectContentBounds(item,section);if(!b||typeof zoomToSlideBounds!=='function')return false;zoomToSlideBounds(b,1.18);return true;}\n",
    "function finishProjectSwitch(item,section=null,redraw=true){loadProjectAnnotations(false);applyProjectPayloads(item,section);renderProjectPanel();if(typeof buildRoiList==='function')buildRoiList();if(typeof updateButtons==='function')updateButtons();if(typeof syncChannelSourcesForActiveImage==='function')syncChannelSourcesForActiveImage();if(redraw){if(!(typeof zoomToProjectContent==='function'&&zoomToProjectContent(item,section))&&typeof fitView==='function')fitView();}if(typeof draw==='function')draw();const label=(item.label||item.path||'image')+(section?(' / '+(section.label||section.id||'section')):'');notify('Project section selected: '+label+' | '+rois.length+' ROI'+(rois.length===1?'':'s'),'success',2200);scheduleViewerStateSync('project_section_selected',projectStatePayload());}\n",
    "function applyProjectOsd(item,section=null){if(typeof osdViewer==='undefined'||!osdViewer)return false;const display=projectDisplaySource(item,section),tileSource=projectTileSourceFromItem(item,section);if(!tileSource){notify(projectItemMessage(section||item)||'No tiled source or preview is available for this project item.','warning',5200);return true;}const dims=display||section||item||{};cfg.slide_width=Number(dims.width||(item&&item.width)||cfg.slide_width);cfg.slide_height=Number(dims.height||(item&&item.height)||cfg.slide_height);cfg.title=(item&&item.label)||cfg.title;if(projectItemCanTile(display)){const src=display;cfg.tile_url_base=String(src.tile_url_base||'');cfg.tile_url_template=String(src.tile_url_template||'');cfg.tile_url_style=String(src.tile_url_style||'deepzoom');cfg.tile_format=String(src.tile_format||'');cfg.tile_size=Number(src.tile_size||cfg.tile_size);cfg.min_level=Number(src.min_level||0);cfg.max_level=Number(src.max_level);cfg.tile_overlap=Number(src.tile_overlap||0);}else{cfg.tile_url_base='';cfg.tile_url_template='';cfg.tile_url_style='deepzoom';cfg.tile_format='';cfg.min_level=0;cfg.max_level=0;}if(prefetchCache&&typeof prefetchCache.clear==='function')prefetchCache.clear();if(typeof clearChannelItems==='function')clearChannelItems();const navSource=projectNavigatorSource(item,section,display);if(typeof setNavigatorImageSource==='function')setNavigatorImageSource(navSource,projectNavigatorSourceKey(item,section));else if(typeof navigatorImage!=='undefined')navigatorImage.src=navSource;installProgressivePreviewBackground();if(typeof setImageTransform==='function')setImageTransform(0,false,false,false);osdReady=false;if(typeof osdViewer.addOnceHandler==='function')osdViewer.addOnceHandler('open',()=>finishProjectSwitch(item,section,true));else setTimeout(()=>finishProjectSwitch(item,section,true),120);osdViewer.open(tileSource);return true;}\n",
    "function applyProjectPreview(item,section=null){if(applyProjectOsd(item,section))return true;const display=projectDisplaySource(item,section),source=(display&&display.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri);if(!source){notify(projectItemMessage(section||item)||'No preview is available for this project item','warning',5200);return false;}if(typeof image==='undefined'){notify('Project image switching needs a tiled source or a thumbnail/project viewer preview.','warning',5200);return false;}if(typeof clearChannelItems==='function')clearChannelItems();const dims=display||section||item||{};cfg.slide_width=Number(dims.width||(item&&item.width)||cfg.slide_width);cfg.slide_height=Number(dims.height||(item&&item.height)||cfg.slide_height);cfg.title=(item&&item.label)||cfg.title;if(typeof setImageTransform==='function')setImageTransform(0,false,false,false);image.onload=()=>{fitView();finishProjectSwitch(item,section,false);};image.src=source;const navSource=projectNavigatorSource(item,section,display)||source;if(typeof setNavigatorImageSource==='function')setNavigatorImageSource(navSource,projectNavigatorSourceKey(item,section));else if(typeof navigatorImage!=='undefined')navigatorImage.src=navSource;return true;}\n",
    "function switchProjectItem(index){if(index<0||index>=projectItems.length)return;const item=projectItems[index];if(!projectSwitchable(item)){notify(projectItemMessage(item)||'This project item has no preview or tiled source yet. Install/configure the required backend or convert it to a supported tiled image.','warning',6200);return;}saveActiveProjectAnnotations();activeProjectIndex=index;activeProjectSectionIndex=defaultProjectSectionIndex(item);renderProjectPanel();applyProjectPreview(item,activeProjectSection());}\n",
    "function switchProjectSection(index){const item=projectItems[activeProjectIndex]||null;const section=item&&projectSections(item)[index];if(!item||!section)return;if(!section.image_data_uri&&!projectItemCanTile(section)){notify(section.message||'This section is listed from metadata but has no preview or tiled source yet.','warning',5200);return;}saveActiveProjectAnnotations();activeProjectSectionIndex=index;renderProjectPanel();applyProjectPreview(item,section);}\n",
    "function bindProjectPanel(){bindProjectPanelControls();preloadProjectAnnotations();if(projectItems.length&&projectAnnotationStore.has(projectAnnotationKey()))loadProjectAnnotations(false);else if(projectItems.length&&!projectAnnotationStore.has(projectAnnotationKey()))saveActiveProjectAnnotations();if(projectItems.length)applyProjectPayloads(projectItems[activeProjectIndex]||null,activeProjectSection());renderProjectPanel();}\n"
  )
}

wsi_viewer_managed_project_item <- function(item) {
  if (!is.list(item)) {
    return(FALSE)
  }
  fields <- tolower(as.character(unlist(
    item[c("backend", "type", "status", "role", "stage", "id")],
    use.names = FALSE
  )))
  fields <- fields[!is.na(fields) & nzchar(fields)]
  has_managed_field <- any(grepl(
    "cellphenotyper|seurat_spatial|giotto|spatialexperiment",
    fields
  ))
  has_spatial_config <- isTRUE((item$seurat %||% list())$enabled)
  has_cellphenotyper_config <- isTRUE((item$cellphenotyper %||% list())$enabled)
  sections <- item$sections %||% list()
  has_managed_section <- length(sections) > 0L &&
    any(vapply(sections, wsi_viewer_managed_project_item, logical(1)))
  isTRUE(has_managed_field || has_spatial_config || has_cellphenotyper_config || has_managed_section)
}

wsi_viewer_managed_analysis_project <- function(seurat_config, cellphenotyper_config, project_config) {
  source_name <- tolower(as.character((seurat_config %||% list())$source_name %||% ""))
  has_spatial_object <- isTRUE((seurat_config %||% list())$enabled) &&
    source_name %in% c("seurat", "giotto", "spatialexperiment")
  has_cellphenotyper <- isTRUE((cellphenotyper_config %||% list())$enabled)
  items <- (project_config %||% list())$items %||% list()
  has_managed_items <- length(items) > 0L &&
    any(vapply(items, wsi_viewer_managed_project_item, logical(1)))
  isTRUE(has_spatial_object || has_cellphenotyper || has_managed_items)
}

wsi_viewer_html <- function(config) {
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, null = "null")
  paste0(
    "<!doctype html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", wsi_html_escape(config$title), "</title>\n",
    "<style>\n",
    wsi_viewer_styles("#111"),
    "</style>\n",
    "</head>\n",
    "<body>\n",
    wsi_viewer_chrome(config, "Loading thumbnail..."),
    "<script>\n",
    "const cfg = ", config_json, ";\n",
    "const canvas = document.getElementById('viewer');\n",
    "const ctx = canvas.getContext('2d');\n",
    "const status = document.getElementById('status');\n",
    "const image = new Image();\n",
    "const navigatorImage = new Image();\n",
    "const el=id=>document.getElementById(id), rois=cfg.rois||[], layers=cfg.layers||[];\n",
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,dragStartX=0,dragStartY=0,dragMoved=false,lastX=0,lastY=0,lastPointer=null,lastCanvasPointer=null,multiViewLastCanvasPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0,nextRoiClass='tumour',activeRoiClass='tumour',activeRoiName='',nextRoiNameDirty=false,measureStart=null,measures=[],selectedMeasure=-1,trajectoryDraft=[],trajectories=[],trajectorySeq=0,selectedTrajectory=-1,selectedLayerIndex=-1,selectedLayerItemIndex=-1,brushing=false,brushPoints=[],brushRadius=32,brushScreenRadius=32,brushOperation='new',brushTargetRoi=-1,brushClass='',brushAdditiveSelection=false,brushTouchedSelection=new Set(),brushAltDown=false,draggingVertex=null,activeVertex=null,annotationUndo=[],annotationRedo=[],highlightedRoiClassKeys=new Set(),highlightAllRoiClasses=false;\n",
    "function clamp(v,min,max){return Math.max(min,Math.min(max,v));}\n",
    wsi_viewer_menu_js(),
    wsi_viewer_toast_js(),
    wsi_viewer_preferences_js(),
    wsi_viewer_jobs_js(),
    wsi_viewer_stain_js(),
    wsi_viewer_base_image_js(),
    wsi_viewer_channel_js(),
    wsi_viewer_sync_js(),
    wsi_viewer_shortcuts_js(),
    wsi_viewer_command_palette_js(),
    wsi_viewer_artifact_js(),
    wsi_viewer_image_transform_js(),
    wsi_viewer_screenshot_js(),
    wsi_viewer_project_js(),
    wsi_viewer_navigator_js(),
    wsi_viewer_scale_bar_js(),
    wsi_viewer_multiview_js(),
    "function setMode(m){mode=m;if(m==='brush'&&typeof setRoiPanelOpen==='function')setRoiPanelOpen(true,{automatic:true});if(m!=='edit'){draggingVertex=null;activeVertex=null;if(typeof curveEditStroke!=='undefined')curveEditStroke=null;}canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');canvas.classList.toggle('brushing',m==='brush');canvas.classList.toggle('editing',m==='edit');canvas.classList.toggle('measuring',m==='measure');canvas.classList.toggle('trajectory',m==='trajectory');canvas.classList.toggle('screenshot',m==='screenshot');const setToolActive=(id,on)=>{const button=el(id);if(button)button.classList.toggle('active',!!on);};setToolActive('toolPan',m==='pan');setToolActive('toolSelect',m==='select');setToolActive('toolDraw',m==='draw');setToolActive('toolBrush',m==='brush');setToolActive('toolEdit',m==='edit');setToolActive('toolMeasure',m==='measure');setToolActive('toolTrajectory',m==='trajectory');setToolActive('screenshotTool',m==='screenshot');updateCursorFeedback();updateButtons();saveToolPreference();if(canvas.width&&((typeof image==='undefined')||image.complete))draw();}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);fitView();}\n",
    "function fitView(){if(!image.naturalWidth)return;const dims=viewImageSize();minScale=Math.min(innerWidth/dims.width,innerHeight/dims.height);scale=minScale;offsetX=(innerWidth-dims.width*scale)/2;offsetY=(innerHeight-dims.height*scale)/2;draw();}\n",
    "function oneToOne(){if(!image.naturalWidth)return;const dims=viewImageSize();scale=1;offsetX=(innerWidth-dims.width)/2;offsetY=(innerHeight-dims.height)/2;draw();}\n",
    "function slideToImage(p){return {x:p.x/cfg.slide_width*image.naturalWidth,y:p.y/cfg.slide_height*image.naturalHeight};}\n",
    "function imageToSlide(p){return {x:p.x/image.naturalWidth*cfg.slide_width,y:p.y/image.naturalHeight*cfg.slide_height};}\n",
    "function slideToCanvas(p){const q=slideToViewImagePoint(p);return {x:offsetX+q.x*scale,y:offsetY+q.y*scale};}\n",
    "function pointerToSlide(evt){const rect=canvas.getBoundingClientRect();const px=evt.clientX-rect.left;const py=evt.clientY-rect.top,out=imageToSlide(viewToImagePoint({x:(px-offsetX)/scale,y:(py-offsetY)/scale}));return (typeof normaliseSlidePoint==='function')?normaliseSlidePoint(out):out;}\n",
    "function pointInsideSlide(p){return p.x>=0&&p.y>=0&&p.x<=cfg.slide_width&&p.y<=cfg.slide_height;}\n",
    "function roiBounds(roi){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointInRing(p,ring){let inside=false;for(let i=0,j=ring.length-1;i<ring.length;j=i++){const xi=ring[i].x,yi=ring[i].y,xj=ring[j].x,yj=ring[j].y;const hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!rois.length)return;selectedRoi=(i+rois.length)%rois.length;const b=roiBounds(rois[selectedRoi]),corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(slideToViewImagePoint),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys);const pad=1.35;scale=clamp(Math.min(innerWidth/Math.max(1,(xmax-xmin)*pad),innerHeight/Math.max(1,(ymax-ymin)*pad)),minScale*0.8,40);offsetX=innerWidth/2-((xmin+xmax)/2)*scale;offsetY=innerHeight/2-((ymin+ymax)/2)*scale;updateRoiList();draw();}\n",
    "function zoomAt(factor,cx,cy){const beforeX=(cx-offsetX)/scale,beforeY=(cy-offsetY)/scale;scale=clamp(scale*factor,minScale*0.6,80);offsetX=cx-beforeX*scale;offsetY=cy-beforeY*scale;draw();}\n",
    "function keyboardPanStep(fast=false){const base=Math.max(60,Math.round(Math.min(innerWidth,innerHeight)*0.12));return fast?base*3:base;}\n",
    "function panByPixels(dx,dy){offsetX+=dx;offsetY+=dy;draw();}\n",
    "function panByKeyboard(key,fast=false){const step=keyboardPanStep(fast);if(key==='ArrowLeft')panByPixels(step,0);else if(key==='ArrowRight')panByPixels(-step,0);else if(key==='ArrowUp')panByPixels(0,step);else if(key==='ArrowDown')panByPixels(0,-step);else return false;return true;}\n",
    "function draw(){if(typeof syncBrushRadiusToZoom==='function')syncBrushRadiusToZoom();ctx.clearRect(0,0,innerWidth,innerHeight);ctx.imageSmoothingEnabled=true;drawTransformedImage(image);applyStainToCanvas();drawLayers();drawTileGrid();drawArtifactOverlays();drawRois();drawDraft();drawBrushPreview();drawEditHandles();drawMeasurements();drawTrajectories();drawCrosshair();drawScreenshotSelection();if(typeof drawMultiViewOverlays==='function')drawMultiViewOverlays();drawMiniNavigator();updateScaleBar();updateStatus(lastPointer);}\n",
    "function labelRectOverlaps(a,b,pad=10){return !(a.x+a.w+pad<b.x||b.x+b.w+pad<a.x||a.y+a.h+pad<b.y||b.y+b.h+pad<a.y);}\n",
    "function roiLabelCandidates(anchor,w,h){const gap=14,near=h+gap,far=h*2+gap,offsets=[[0,-h/2],[0,-near],[0,gap],[w/2+gap,-h/2],[-w/2-gap,-h/2],[w/2+gap,gap],[-w/2-gap,gap],[w/2+gap,-near],[-w/2-gap,-near],[0,-far],[0,h+gap],[w+gap,-h/2],[-w-gap,-h/2],[w+gap,gap],[-w-gap,gap],[w+gap,-near],[-w-gap,-near],[w/2+gap,-far],[-w/2-gap,-far],[w/2+gap,h+gap],[-w/2-gap,h+gap]];const seen=new Set();return offsets.map((o,rank)=>{const x=clamp(anchor.x+o[0]-w/2,6,Math.max(6,innerWidth-w-6)),y=clamp(anchor.y+o[1],6,Math.max(6,innerHeight-h-6)),key=Math.round(x)+'|'+Math.round(y);if(seen.has(key))return null;seen.add(key);return {x:x,y:y,w:w,h:h,rank:rank};}).filter(Boolean);}\n",
    "function labelAnchorDistanceScore(anchor,c){const cx=c.x+c.w/2,cy=c.y+c.h/2;return Math.hypot(cx-anchor.x,cy-anchor.y)+(c.rank||0)*4;}\n",
    "function placeRoiLabel(anchor,w,h,occupied){const candidates=roiLabelCandidates(anchor,w,h).sort((a,b)=>labelAnchorDistanceScore(anchor,a)-labelAnchorDistanceScore(anchor,b));for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,12)))return c;}for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,2)))return c;}return null;}\n",
    "function crispLabelRect(rect){return {x:Math.round(rect.x),y:Math.round(rect.y),w:Math.ceil(rect.w),h:Math.ceil(rect.h)};}\n",
    "function labelLeaderTarget(anchor,r){return {x:clamp(anchor.x,r.x,r.x+r.w),y:clamp(anchor.y,r.y,r.y+r.h)};}\n",
    "function drawLabelLeader(item,r,colour){if(!item.anchor)return;const t=labelLeaderTarget(item.anchor,r),dist=Math.hypot(t.x-item.anchor.x,t.y-item.anchor.y);if(dist<8)return;ctx.save();ctx.strokeStyle='rgba(0,0,0,.84)';ctx.lineWidth=3.5;ctx.beginPath();ctx.moveTo(item.anchor.x,item.anchor.y);ctx.lineTo(t.x,t.y);ctx.stroke();ctx.strokeStyle=colour;ctx.lineWidth=1.8;ctx.beginPath();ctx.moveTo(item.anchor.x,item.anchor.y);ctx.lineTo(t.x,t.y);ctx.stroke();ctx.fillStyle=colour;ctx.strokeStyle='rgba(0,0,0,.9)';ctx.lineWidth=1.2;ctx.beginPath();ctx.arc(item.anchor.x,item.anchor.y,3.2,0,Math.PI*2);ctx.fill();ctx.stroke();ctx.restore();}\n",
    "function drawPlacedRoiLabel(item,rect){const r=crispLabelRect(rect),colour=item.colour||'#5eead4';ctx.save();drawLabelLeader(item,r,colour);ctx.font='700 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='middle';ctx.shadowColor='rgba(0,0,0,.55)';ctx.shadowBlur=5;ctx.fillStyle='rgba(0,0,0,.92)';ctx.fillRect(r.x,r.y,r.w,r.h);ctx.shadowBlur=0;ctx.strokeStyle='rgba(255,255,255,.86)';ctx.lineWidth=2.5;ctx.strokeRect(r.x+.5,r.y+.5,Math.max(1,r.w-1),Math.max(1,r.h-1));ctx.strokeStyle=colour;ctx.lineWidth=1.6;ctx.strokeRect(r.x+2.5,r.y+2.5,Math.max(1,r.w-5),Math.max(1,r.h-5));ctx.fillStyle=colour;ctx.fillRect(r.x+1,r.y+1,6,Math.max(1,r.h-2));ctx.fillStyle='#ffffff';ctx.fillText(item.text,r.x+13,r.y+r.h/2);ctx.restore();}\n",
    "function drawRoiLabels(items){const occupied=[];items.sort((a,b)=>(b.priority||0)-(a.priority||0));items.forEach(item=>{const rect=placeRoiLabel(item.anchor,item.w,item.h,occupied);if(!rect)return;occupied.push(rect);drawPlacedRoiLabel(item,rect);});}\n",
    "function drawSimpleRoiPath(roi){ctx.beginPath();(roi.rings||[]).forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});}\n",
    "function strokeSimpleRoiBorder(roi,i){const selected=i===selectedRoi,highlighted=roiClassHighlighted(roi),dimmed=annotationHighlightActive()&&!highlighted,colour=selected?'#ffffff':(roi.colour||'#5eead4');ctx.save();ctx.lineJoin='round';ctx.lineCap='round';ctx.globalAlpha=dimmed?0.32:1;ctx.setLineDash([]);drawSimpleRoiPath(roi);ctx.strokeStyle=highlighted?'rgba(255,255,255,.96)':'rgba(0,0,0,.72)';ctx.lineWidth=highlighted?10:(selected?7:5);ctx.stroke();drawSimpleRoiPath(roi);ctx.strokeStyle=colour;ctx.lineWidth=highlighted?5:(selected?4:2);if(highlighted){ctx.shadowColor=colour;ctx.shadowBlur=7;}else if(!selected){ctx.setLineDash([7,4]);ctx.lineDashOffset=-(i%9)*2;}ctx.stroke();ctx.setLineDash([]);ctx.restore();}\n",
    "function roiCanvasMetrics(b){if(!b)return {w:0,h:0,cx:0,cy:0};const p0=slideToCanvas({x:b.xmin,y:b.ymin}),p1=slideToCanvas({x:b.xmax,y:b.ymax});return {x:Math.min(p0.x,p1.x),y:Math.min(p0.y,p1.y),w:Math.abs(p1.x-p0.x),h:Math.abs(p1.y-p0.y),cx:(p0.x+p1.x)/2,cy:(p0.y+p1.y)/2};}\n",
    "function roiVisibleSlideBounds(padFraction=.08){const b=(typeof visibleSlideBounds==='function')?visibleSlideBounds():{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height},pad=Math.max(b.xmax-b.xmin,b.ymax-b.ymin)*padFraction;return {xmin:Math.max(0,b.xmin-pad),ymin:Math.max(0,b.ymin-pad),xmax:Math.min(cfg.slide_width,b.xmax+pad),ymax:Math.min(cfg.slide_height,b.ymax+pad)};}\n",
    "function roiIntersectsViewport(roi,bounds){const b=roiBounds(roi);return !!(b&&bounds&&b.xmin<=bounds.xmax&&b.xmax>=bounds.xmin&&b.ymin<=bounds.ymax&&b.ymax>=bounds.ymin);}\n",
    "function roiLodMode(roi,i,b,metrics,highlighted){if(i===selectedRoi||highlighted||rois.length<350)return 'detail';const px=typeof slideUnitScale==='function'?slideUnitScale():scale,maxDim=Math.max(metrics.w,metrics.h),pts=Number(pointCount(roi)||0);if(px>.22&&maxDim>12)return 'detail';if(rois.length<1500&&px>.12&&pts<800&&maxDim>28)return 'detail';if(maxDim<5)return 'centroid';return 'bbox';}\n",
    "function drawRoiLodMarker(roi,i,b,metrics,mode,dimmed,highlighted){const selected=i===selectedRoi,colour=selected?'#ffffff':(roi.colour||'#5eead4');ctx.save();ctx.globalAlpha=dimmed?0.28:Math.max(.42,Math.min(1,roiOpacity));ctx.strokeStyle=highlighted?'#ffffff':colour;ctx.fillStyle=hexToRgba(colour,mode==='centroid'?.72:.12);ctx.lineWidth=highlighted?2.4:(selected?2.2:1.2);ctx.setLineDash(selected?[]:[4,3]);if(mode==='centroid'){const r=highlighted||selected?4:2.5;ctx.beginPath();ctx.arc(metrics.cx,metrics.cy,r,0,Math.PI*2);ctx.fill();ctx.stroke();}else{const x=Math.round(metrics.x)+.5,y=Math.round(metrics.y)+.5,w=Math.max(2,Math.round(metrics.w)),h=Math.max(2,Math.round(metrics.h));ctx.fillRect(x,y,w,h);ctx.strokeRect(x,y,w,h);}ctx.setLineDash([]);ctx.restore();}\n",
    "function drawRois(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('annotations'))return;if(!showRois||!rois.length||!image.naturalWidth)return;ctx.save();ctx.lineWidth=2;ctx.font='600 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';const labelItems=[],borderItems=[],highlightActive=annotationHighlightActive(),viewBounds=roiVisibleSlideBounds(.10);let detailed=0,lod=0,culled=0;rois.forEach((roi,i)=>{if(!visibleRoi(roi)||!isDrawable(roi)){culled++;return;}const b=roiBounds(roi);if(!b||!roiIntersectsViewport(roi,viewBounds)){culled++;return;}const metrics=roiCanvasMetrics(b),highlighted=roiClassHighlighted(roi),dimmed=highlightActive&&!highlighted,mode=roiLodMode(roi,i,b,metrics,highlighted);let label=null;if(mode==='detail'){drawSimpleRoiPath(roi);(roi.rings||[]).forEach(ring=>{if(!label&&ring&&ring[0])label=slideToCanvas(ring[0]);});ctx.globalAlpha=dimmed?Math.min(.12,roiOpacity*.35):roiOpacity;ctx.fillStyle=roi.fill;ctx.fill('evenodd');ctx.globalAlpha=1;borderItems.push({roi:roi,index:i});detailed++;}else{drawRoiLodMarker(roi,i,b,metrics,mode,dimmed,highlighted);label={x:metrics.cx,y:metrics.cy};lod++;}if(showLabels&&label&&!dimmed&&(mode==='detail'||highlighted||i===selectedRoi)){const text=roi.name||roi.id;if(text)labelItems.push({anchor:label,text:text,w:ctx.measureText(text).width+18,h:22,colour:roi.colour,priority:highlighted?20:(i===selectedRoi?10:0)});}});borderItems.forEach(item=>strokeSimpleRoiBorder(item.roi,item.index));if(showLabels)drawRoiLabels(labelItems);if(lod>0&&typeof recordViewerLog==='function'&&Date.now()-(drawRois._lastLodLog||0)>8000){drawRois._lastLodLog=Date.now();recordViewerLog('Annotation level-of-detail active: '+lod+' simplified, '+detailed+' detailed, '+culled+' outside view.','info',{simplified:lod,detailed:detailed,culled:culled},'annotations');}ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!lastPointer||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function updateStatus(p){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x';msg+=magnificationStatus();msg+=(typeof multiViewStatus==='function'?multiViewStatus():'');msg+=stainStatus();msg+=measureStatus();msg+=trajectoryStatus();msg+=imageTransformStatus();msg+=screenshotStatus();if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(p&&pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg;}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>{b.classList.toggle('active',i===selectedRoi);b.classList.toggle('highlighted',rois[i]&&roiClassHighlighted(rois[i]));});renderAnnotationLabelHighlights();if(typeof syncProximityAnnotations==='function')syncProximityAnnotations(false);}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';b.style.setProperty('--wsi-highlight-accent',roi.colour||classColour(roi.class||'annotation'));const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){notify('Add at least 3 points','warning');return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;markAnnotationsDirty('roi_added');recordAnnotationHistory('roi_added',{id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount});buildRoiList();updateButtons();setMode('select');notify('ROI saved','success');if(typeof closeAllToolMenus==='function')closeAllToolMenus();draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>typeof normaliseSlideRingCoordinates==='function'?normaliseSlideRingCoordinates(r):r.map(p=>[Math.round(p.x),Math.round(p.y)]));const cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:cls},class:cls,source:roi.source||null,wsiTools:{coordinate_space:'level0_slide_pixels',coordinateSpace:'level0_slide_pixels',slide_width:Number(cfg.slide_width||0),slide_height:Number(cfg.slide_height||0),display_transform_applied:false}},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(typeof openAnnotationExportDialog==='function'){openAnnotationExportDialog('all');return;}if(!rois.length&&draft.length<3){notify('Draw an ROI first','warning');return;}const text=geojsonText();const name=(typeof projectAnnotationFilename==='function'?projectAnnotationFilename():null)||cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();markAnnotationsSaved('geojson_saved');notify('ROI saved','success');return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);markAnnotationsSaved('geojson_exported');notify('GeoJSON exported','success');}\n",
    "function updateButtons(){const has=rois.length>0,spotsAvailable=typeof annotationSpotItems==='function'&&annotationSpotItems().length>0,setDisabled=(id,value)=>{const button=el(id);if(button)button.disabled=!!value;},setActive=(id,value)=>{const button=el(id);if(button)button.classList.toggle('active',!!value);};['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>setDisabled(id,!has));setDisabled('annotationPanelToggle',false);setDisabled('layerPanelToggle',false);setDisabled('finishRoi',draft.length<3);setDisabled('undoPoint',draft.length<1);setDisabled('saveGeojson',!has&&draft.length<3);setDisabled('saveAnnotationSpotsCsv',!has||!spotsAvailable);setDisabled('seuratAnnotationSpotsCsv',!has||!spotsAvailable);setDisabled('exportSelectedRoiTiff',selectedRoi<0);if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();setActive('roiToggle',showRois&&has);setActive('labelsToggle',showLabels&&has);setActive('crosshairToggle',showCrosshair);const panel=el('roiPanel'),open=!!(panel&&panel.classList.contains('open'));['layersToggle','annotationPanelToggle','layerPanelToggle'].forEach(id=>setActive(id,open));}\n",
    wsi_viewer_geometry_js(),
    wsi_viewer_layers_js(),
    wsi_viewer_cell_controls_js(),
    wsi_viewer_seurat_js(),
    wsi_viewer_prediction_js(),
    wsi_viewer_kodama_js(),
    wsi_viewer_measure_js(),
    wsi_viewer_trajectory_js(),
    wsi_viewer_segmentation_js(),
    "canvas.addEventListener('mousedown',e=>{lastCanvasPointer=canvasPoint(e.clientX,e.clientY);lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(mode==='screenshot'){startScreenshotSelection(e);return;}if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='trajectory'){if(e.detail===1)addTrajectoryPoint(lastPointer);return;}if(mode==='brush'){startBrush(lastPointer,e);return;}if(mode==='edit'){const vertex=findVertexAt(e.clientX,e.clientY);if(vertex){pushAnnotationUndo(isTrajectoryAreaRoi(rois[vertex.roi])?'trajectory_border_vertex_moved':'roi_vertex_moved');selectAnnotation(vertex.roi,false);activeVertex=prepareVertexDrag(vertex);draggingVertex=activeVertex;updateRoiList();draw();return;}if(startCurveEditStroke(lastCanvasPointer,lastPointer,slideToCanvas))return;selectAnnotation(roiAt(lastPointer),true);draw();return;}if(mode==='measure'){addMeasurePoint(lastPointer);return;}if(mode==='select'){if(!selectObjectAtPoint(lastPointer,'trajectory')){clearSelectedAnnotation(false);clearSelectedTrajectory(false);clearSelectedLayerObject(false);clearSelectedMeasure(false);updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();buildLayerList();updateMeasureList();updateButtons();draw();}return;}dragging=true;dragStartX=e.clientX;dragStartY=e.clientY;dragMoved=false;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',e=>{lastCanvasPointer=canvasPoint(e.clientX,e.clientY);lastPointer=pointerToSlide(e);if(screenshotSelecting){finishScreenshotSelection(e);return;}if(brushing)finishBrush();if(curveEditStroke)finishCurveEditStroke();if(draggingVertex)finishActiveVertexDrag();const wasDragging=dragging,wasClick=wasDragging&&!dragMoved;dragging=false;canvas.classList.remove('dragging');if(wasClick&&mode==='pan'){const p=pointerToSlide(e);if(selectObjectAtPoint(p,'trajectory'))e.preventDefault();}updateCursorFeedback(e);});\n",
    "window.addEventListener('mousemove',e=>{lastCanvasPointer=canvasPoint(e.clientX,e.clientY);lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(screenshotSelecting){updateScreenshotSelection(e);return;}if(brushing){addBrushPoint(lastPointer,e);return;}if(curveEditStroke){addCurveEditPoint(lastPointer);return;}if(draggingVertex){moveActiveVertex(lastPointer);return;}if(dragging){const dx=e.clientX-lastX,dy=e.clientY-lastY;if(Math.hypot(e.clientX-dragStartX,e.clientY-dragStartY)>3)dragMoved=true;if(dragMoved)panByPixels(dx,dy);lastX=e.clientX;lastY=e.clientY;}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.2:1/1.2,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}if(mode==='trajectory'){e.preventDefault();finishTrajectory();return;}if(mode==='edit'){e.preventDefault();insertVertexAt(pointerToSlide(e),e.clientX,e.clientY);return;}const p=pointerToSlide(e);if(selectObjectAtPoint(p)){e.preventDefault();return;}e.preventDefault();clearSelectionAndPan();});\n",
    "const bindButton=(id,handler)=>{const button=el(id);if(button)button.onclick=handler;};bindButton('toolPan',e=>{setMode('pan');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolSelect',e=>{setMode('select');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolDraw',e=>{setMode('draw');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolBrush',e=>{setMode('brush');closeMenuAfterToolAction(e.currentTarget);});bindButton('newRoi',e=>{startNewAnnotation(mode==='draw'?'draw':'brush');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolEdit',e=>{setMode('edit');closeMenuAfterToolAction(e.currentTarget);});bindButton('finishRoi',e=>{finishDraft();closeMenuAfterToolAction(e.currentTarget);});bindButton('undoPoint',()=>{if(mode==='brush'&&brushPoints.length){brushPoints.pop();draw();}else undoDraftPoint();});bindButton('saveGeojson',e=>{saveGeojson();closeMenuAfterToolAction(e.currentTarget);});bindButton('saveAnnotationSpotsCsv',e=>{saveAnnotationSpotsCsv();closeMenuAfterToolAction(e.currentTarget);});bindButton('seuratAnnotationSpotsCsv',e=>{saveAnnotationSpotsCsv();closeMenuAfterToolAction(e.currentTarget);});bindButton('exportSelectedRoiTiff',e=>{exportImageRegion('selected_roi','tiff');closeMenuAfterToolAction(e.currentTarget);});bindButton('exportViewTiff',e=>{exportImageRegion('viewport','tiff');closeMenuAfterToolAction(e.currentTarget);});bindButton('screenshotTool',beginScreenshotMode);bindButton('zoomIn',()=>zoomAt(1.25,innerWidth/2,innerHeight/2));bindButton('zoomOut',()=>zoomAt(1/1.25,innerWidth/2,innerHeight/2));bindButton('fit',fitView);bindButton('oneToOne',oneToOne);\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();saveDisplayPreference();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();saveDisplayPreference();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);['layersToggle','annotationPanelToggle','layerPanelToggle'].forEach(id=>{const button=el(id);if(button)button.onclick=e=>{toggleRoiPanel();updateButtons();if(e&&e.currentTarget&&['annotationPanelToggle','layerPanelToggle'].includes(e.currentTarget.id)&&typeof closeContainingToolMenu==='function')closeContainingToolMenu(e.currentTarget);};});el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);saveRoiOpacityPreference();draw();};const crosshairButton=el('crosshairToggle');if(crosshairButton)crosshairButton.onclick=()=>{showCrosshair=!showCrosshair;updateButtons();saveDisplayPreference();draw();};\n",
    "window.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase(),typing=e.target&&['INPUT','TEXTAREA','SELECT'].includes(e.target.tagName);if(brushSubtractKeyEvent(e)){brushAltDown=true;updateCursorFeedback(e);draw();}if((e.ctrlKey||e.metaKey)&&!typing&&((e.shiftKey&&key==='z')||key==='y')){e.preventDefault();restoreAnnotationRedo();return;}if((e.ctrlKey||e.metaKey)&&!e.shiftKey&&key==='z'&&!typing){e.preventDefault();if(mode==='trajectory'&&trajectoryDraft.length){undoTrajectoryPoint();return;}restoreAnnotationUndo();return;}if(!typing&&!e.ctrlKey&&!e.metaKey&&!e.altKey&&panByKeyboard(e.key,e.shiftKey)){e.preventDefault();return;}if(!typing&&!e.ctrlKey&&!e.metaKey&&!e.altKey&&!e.shiftKey&&key==='n'){e.preventDefault();startNewAnnotation(mode==='draw'?'draw':'brush');return;}if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='b')setMode('brush');if(e.key==='e')setMode('edit');if(e.key==='m')setMode('measure');if(e.key==='t')setMode('trajectory');if(e.key==='Enter'&&mode==='draw')finishDraft();if(e.key==='Enter'&&mode==='trajectory'){e.preventDefault();finishTrajectory();}if((e.key==='Backspace'||e.key==='Delete')&&!typing){if(mode==='draw'&&draft.length){e.preventDefault();undoDraftPoint();return;}if(mode==='trajectory'&&trajectoryDraft.length){e.preventDefault();undoTrajectoryPoint();return;}if(mode==='edit'&&activeVertex){e.preventDefault();deleteSelectedVertex();return;}e.preventDefault();deleteSelectedObject();return;}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape'){measureStart=null;trajectoryDraft=[];brushing=false;brushPoints=[];brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();draggingVertex=null;activeVertex=null;if(typeof curveEditStroke!=='undefined')curveEditStroke=null;cancelScreenshotSelection();setMode('pan');draw();}});\n",
    "window.addEventListener('keyup',e=>{if(e.key==='Alt'||e.key==='Meta'||mode==='brush'||mode==='edit'){brushAltDown=brushSubtractModifier(e);updateCursorFeedback(e);draw();}});\n",
    "bindExclusiveMenus();bindShortcutHelp();bindCommandPalette();bindUnsavedIndicator();bindUnsavedRefreshGuard();bindMiniNavigator();bindProjectPanel();bindRoiPanelControls();bindSelectionCardControls();bindJobControls();bindStainControls();bindBaseImageControls();bindRoiClassControls();bindAnnotationListControls();bindAnnotationHistoryControls();bindViewerLogControls();bindViewerLogCapture();bindAnnotationSectionMaximizeControls();bindMeasureControls();bindTrajectoryControls();bindImageTransformControls();bindGeojsonImportControls();bindSegmentationControls();bindArtifactControls();bindCellControls();bindSeuratControls();bindPredictionControls();bindKodamaControls();bindMagnificationControls();bindMultiViewControls();bindPreferenceControls();buildRoiList();buildLayerList();buildChannelList();const initialMode=applyViewerPreferences();updateButtons();updateAnnotationDirtyIndicator();syncMessage('');setMode(initialMode||'pan');startViewerStateSocket();scheduleViewerStateSync('viewer_loaded',{});startViewerCommandPolling();startViewerAutosave();\n",
    "window.addEventListener('resize',resize);\n",
    "image.onload=resize;\n",
    "image.src=cfg.image_data_uri;\n",
    "</script>\n",
    "</body>\n",
    "</html>\n"
  )
}

wsi_tiled_viewer_html <- function(config) {
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, null = "null")
  loading_message <- if (identical(config$viewer_mode %||% NULL, "project") &&
      is.null(config$tile_url_base) && is.null(config$tile_url_template)) {
    "Loading project previews..."
  } else {
    "Loading Deep Zoom tiles..."
  }
  paste0(
    "<!doctype html>\n",
    "<html lang=\"en\">\n",
    "<head>\n",
    "<meta charset=\"utf-8\">\n",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n",
    "<title>", wsi_html_escape(config$title), "</title>\n",
    "<script src=\"https://cdn.jsdelivr.net/npm/openseadragon@4.1.1/build/openseadragon/openseadragon.min.js\"></script>\n",
    "<style>\n",
    wsi_viewer_styles("#101010"),
    "</style>\n",
    "</head>\n",
    "<body>\n",
    wsi_viewer_chrome(config, loading_message, tiled = TRUE),
    "<script>\n",
    "const cfg = ", config_json, ";\n",
    "const viewerEl = document.getElementById('viewer');\n",
    "const canvas = document.getElementById('overlay');\n",
    "const ctx = canvas.getContext('2d');\n",
    "const status = document.getElementById('status');\n",
    "const navigatorImage = new Image();\n",
    "const el=id=>document.getElementById(id), rois=cfg.rois||[], layers=cfg.layers||[];\n",
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,dragStartX=0,dragStartY=0,dragMoved=false,lastX=0,lastY=0,lastPointer=null,lastCanvasPointer=null,multiViewLastCanvasPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0,nextRoiClass='tumour',activeRoiClass='tumour',activeRoiName='',nextRoiNameDirty=false,measureStart=null,measures=[],selectedMeasure=-1,trajectoryDraft=[],trajectories=[],trajectorySeq=0,selectedTrajectory=-1,selectedLayerIndex=-1,selectedLayerItemIndex=-1,brushing=false,brushPoints=[],brushRadius=32,brushScreenRadius=32,brushOperation='new',brushTargetRoi=-1,brushClass='',brushAdditiveSelection=false,brushTouchedSelection=new Set(),brushAltDown=false,draggingVertex=null,activeVertex=null,annotationUndo=[],annotationRedo=[],highlightedRoiClassKeys=new Set(),highlightAllRoiClasses=false;\n",
    "let osdViewer=null,osdReady=false,renderQueued=false,loadingTiles=0,baseImageDirty=true,tileFailureNotified=false;\n",
    "let stainOverlayCanvas=null,stainOverlayKey='';\n",
    "const prefetchCache=new Map();\n",
    "function clamp(v,min,max){return Math.max(min,Math.min(max,v));}\n",
    wsi_viewer_menu_js(),
    wsi_viewer_toast_js(),
    wsi_viewer_preferences_js(),
    wsi_viewer_jobs_js(),
    wsi_viewer_stain_js(),
    wsi_viewer_base_image_js(),
    wsi_viewer_channel_js(),
    wsi_viewer_sync_js(),
    wsi_viewer_shortcuts_js(),
    wsi_viewer_command_palette_js(),
    wsi_viewer_artifact_js(),
    wsi_viewer_image_transform_js(),
    wsi_viewer_screenshot_js(),
    wsi_viewer_project_js(),
    wsi_viewer_navigator_js(),
    wsi_viewer_scale_bar_js(),
    wsi_viewer_multiview_js(),
    "function setMode(m){mode=m;if(m==='brush'&&typeof setRoiPanelOpen==='function')setRoiPanelOpen(true,{automatic:true});if(m!=='edit'){draggingVertex=null;activeVertex=null;if(typeof curveEditStroke!=='undefined')curveEditStroke=null;}canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');canvas.classList.toggle('brushing',m==='brush');canvas.classList.toggle('editing',m==='edit');canvas.classList.toggle('measuring',m==='measure');canvas.classList.toggle('trajectory',m==='trajectory');canvas.classList.toggle('screenshot',m==='screenshot');const setToolActive=(id,on)=>{const button=el(id);if(button)button.classList.toggle('active',!!on);};setToolActive('toolPan',m==='pan');setToolActive('toolSelect',m==='select');setToolActive('toolDraw',m==='draw');setToolActive('toolBrush',m==='brush');setToolActive('toolEdit',m==='edit');setToolActive('toolMeasure',m==='measure');setToolActive('toolTrajectory',m==='trajectory');setToolActive('screenshotTool',m==='screenshot');updateCursorFeedback();updateButtons();saveToolPreference();if(canvas.width)draw();}\n",
    "function activeTileMode(){return !!((cfg.tile_url_base||cfg.tile_url_template)&&cfg.tile_format&&Number.isFinite(Number(cfg.max_level))&&Number(cfg.max_level)>0);}\n",
    "function tileUrlFromParts(base,template,style,fmt,level,col,row){if(template)return String(template).replace(/\\{level\\}/g,level).replace(/\\{x\\}/g,col).replace(/\\{y\\}/g,row).replace(/\\{format\\}/g,fmt);return String(base).replace(/\\/$/,'')+'/'+level+'/'+col+(style==='slash'?('/'+row):('_'+row))+'.'+fmt;}\n",
    "function tileUrl(level,col,row){return tileUrlFromParts(cfg.tile_url_base||'',cfg.tile_url_template||'',cfg.tile_url_style||'deepzoom',cfg.tile_format,level,col,row);}\n",
    "function tileNeedsCors(url){return /^https?:\\/\\//i.test(String(url||''));}\n",
    "function withTileCors(source,base,template){if(source&&(tileNeedsCors(base)||tileNeedsCors(template)))source.crossOriginPolicy='Anonymous';return source;}\n",
    "function tileCacheCount(){return Math.max(64,Number(cfg.tile_cache_count||768));}\n",
    "function tilePrefetchCacheCount(){return Math.max(0,Number(cfg.tile_prefetch_cache_count||512));}\n",
    "function tileTimeoutMs(){const value=Number(cfg.tile_timeout_ms||cfg.tile_timeout||30000);return Number.isFinite(value)&&value>0?value:30000;}\n",
    "function tileImageLoaderLimit(){const value=Number(cfg.tile_image_loader_limit||cfg.image_loader_limit||0);return Number.isFinite(value)&&value>=0?Math.floor(value):0;}\n",
    "function progressivePreviewEnabled(){return !!cfg.progressive_preview&&!!cfg.navigator_image_data_uri;}\n",
    "function installProgressivePreviewBackground(){if(!viewerEl)return;if(!progressivePreviewEnabled()||(typeof imageTransformHasDisplayTransform==='function'&&imageTransformHasDisplayTransform())){viewerEl.style.backgroundImage='';viewerEl.classList.remove('progressivePreview');return;}viewerEl.style.backgroundImage='url(\"'+String(cfg.navigator_image_data_uri).replace(/\"/g,'%22')+'\")';viewerEl.style.backgroundSize='contain';viewerEl.style.backgroundRepeat='no-repeat';viewerEl.style.backgroundPosition='center center';viewerEl.classList.add('progressivePreview');}\n",
    "function tileSourceFromConfig(){if(activeTileMode()){const out={width:cfg.slide_width,height:cfg.slide_height,tileSize:cfg.tile_size,tileOverlap:Number(cfg.tile_overlap||0),minLevel:Number(cfg.min_level||0),maxLevel:cfg.max_level,getTileUrl:(level,x,y)=>tileUrl(level,x,y)};return withTileCors(out,cfg.tile_url_base,cfg.tile_url_template);}const source=cfg.image_data_uri||cfg.navigator_image_data_uri;if(source)return {type:'image',url:source};return {width:cfg.slide_width,height:cfg.slide_height,tileSize:cfg.tile_size,tileOverlap:0,minLevel:0,maxLevel:0,getTileUrl:()=>''};}\n",
    "function requestDraw(){if(renderQueued)return;renderQueued=true;requestAnimationFrame(()=>{renderQueued=false;draw();});}\n",
    "function osdItem(){return osdViewer&&osdViewer.world&&osdViewer.world.getItemAt(0);}\n",
    "let stainCanvasWarningShown=false;\n",
    "function osdBaseCanvasCandidates(){if(!viewerEl||!viewerEl.querySelectorAll)return[];return Array.from(viewerEl.querySelectorAll('canvas')).filter(c=>c&&c.width>0&&c.height>0&&getComputedStyle(c).display!=='none').sort((a,b)=>(b.width*b.height)-(a.width*a.height));}\n",
    "function osdBaseCanvas(){const candidates=osdBaseCanvasCandidates();return candidates.length?candidates[0]:null;}\n",
    "function syncViewState(){if(!osdReady||!osdItem())return;const a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:1,y:0});scale=Math.max(1e-9,Math.hypot(b.x-a.x,b.y-a.y));minScale=Math.min(innerWidth/cfg.slide_width,innerHeight/cfg.slide_height);offsetX=a.x;offsetY=a.y;}\n",
    "function markBaseImageDirty(){baseImageDirty=true;}\n",
    "function invalidateBaseImage(){markBaseImageDirty();if(osdViewer&&typeof osdViewer.forceRedraw==='function')osdViewer.forceRedraw();requestDraw();}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);syncViewState();draw();}\n",
    "function settleOpenSeadragonHome(viewer,immediate=true){try{const vp=viewer&&viewer.viewport;if(!vp||typeof vp.getZoom!=='function'||typeof vp.getHomeZoom!=='function'||typeof vp.goHome!=='function')return false;const zoom=Number(vp.getZoom(true)),home=Number(vp.getHomeZoom());if(Number.isFinite(zoom)&&Number.isFinite(home)&&home>0&&zoom<=home*1.002){vp.goHome(immediate);vp.applyConstraints(immediate);return true;}}catch(e){}return false;}\n",
    "function fitView(){if(typeof multiViewFitView==='function'&&multiViewFitView()){draw();return;}if(osdViewer){osdViewer.viewport.goHome(true);osdViewer.viewport.applyConstraints(true);syncViewState();prefetchNeighborTiles();draw();}}\n",
    "function zoomAt(factor,cx,cy){if(typeof multiViewZoomAt==='function'&&multiViewZoomAt(factor)){draw();return;}if(!osdViewer||!osdViewer.viewport)return;const rect=viewerEl&&viewerEl.getBoundingClientRect?viewerEl.getBoundingClientRect():{left:0,top:0,width:innerWidth,height:innerHeight},w=Math.max(1,Number(rect.width)||innerWidth||1),h=Math.max(1,Number(rect.height)||innerHeight||1),px=Number.isFinite(Number(cx))?Number(cx)-Number(rect.left||0):w/2,py=Number.isFinite(Number(cy))?Number(cy)-Number(rect.top||0):h/2,point=osdViewer.viewport.pointFromPixel(new OpenSeadragon.Point(clamp(px,0,w),clamp(py,0,h)),true);osdViewer.viewport.zoomBy(factor,point,true);osdViewer.viewport.applyConstraints(true);settleOpenSeadragonHome(osdViewer,true);syncViewState();prefetchNeighborTiles();draw();}\n",
    "function oneToOne(){if(typeof multiViewOneToOne==='function'&&multiViewOneToOne()){draw();return;}syncViewState();zoomAt(1/Math.max(scale,1e-9),innerWidth/2,innerHeight/2);}\n",
    "function currentLevel(){syncViewState();if(!activeTileMode())return 0;return clamp(Math.ceil(cfg.max_level+Math.log2(Math.max(scale,1e-9))),0,cfg.max_level);}\n",
    "function visibleTileRange(level,margin=1){if(!activeTileMode()||!osdReady||!osdItem())return null;const item=osdItem(),bounds=osdViewer.viewport.getBounds(true),p0=item.viewportToImageCoordinates(new OpenSeadragon.Point(bounds.x,bounds.y)),p1=item.viewportToImageCoordinates(new OpenSeadragon.Point(bounds.x+bounds.width,bounds.y+bounds.height)),down=Math.pow(2,cfg.max_level-level),levelW=Math.ceil(cfg.slide_width/down),levelH=Math.ceil(cfg.slide_height/down),tileSlide=cfg.tile_size*down;const left=clamp(Math.min(p0.x,p1.x),0,cfg.slide_width),right=clamp(Math.max(p0.x,p1.x),0,cfg.slide_width),top=clamp(Math.min(p0.y,p1.y),0,cfg.slide_height),bottom=clamp(Math.max(p0.y,p1.y),0,cfg.slide_height);return {c0:clamp(Math.floor(left/tileSlide)-margin,0,Math.ceil(levelW/cfg.tile_size)-1),c1:clamp(Math.floor(right/tileSlide)+margin,0,Math.ceil(levelW/cfg.tile_size)-1),r0:clamp(Math.floor(top/tileSlide)-margin,0,Math.ceil(levelH/cfg.tile_size)-1),r1:clamp(Math.floor(bottom/tileSlide)+margin,0,Math.ceil(levelH/cfg.tile_size)-1)};}\n",
    "function prefetchTile(level,col,row){if(!activeTileMode())return;const limit=tilePrefetchCacheCount();if(limit<=0)return;const key=tileUrl(level,col,row);if(prefetchCache.has(key))return;const img=new Image();if(tileNeedsCors(key))img.crossOrigin='anonymous';img.decoding='async';img.src=key;prefetchCache.set(key,img);while(prefetchCache.size>limit){const first=prefetchCache.keys().next().value;prefetchCache.delete(first);}}\n",
    "function prefetchNeighborTiles(){const margin=Number(cfg.tile_prefetch_margin??-1);if(margin<0)return;const level=currentLevel(),range=visibleTileRange(level,margin);if(!range)return;if(window.requestIdleCallback){requestIdleCallback(()=>prefetchTileRange(level,range),{timeout:400});}else{setTimeout(()=>prefetchTileRange(level,range),50);}}\n",
    "function prefetchTileRange(level,range){for(let row=range.r0;row<=range.r1;row++){for(let col=range.c0;col<=range.c1;col++)prefetchTile(level,col,row);}}\n",
    "function stainOverlayCacheKey(){const prefs=(typeof currentStainPayload==='function'?currentStainPayload():null)||{};return [canvas.width,canvas.height,JSON.stringify(prefs)].join('|');}\n",
    "function ensureStainOverlayCanvas(){if(!stainOverlayCanvas)stainOverlayCanvas=document.createElement('canvas');if(stainOverlayCanvas.width!==canvas.width||stainOverlayCanvas.height!==canvas.height){stainOverlayCanvas.width=canvas.width;stainOverlayCanvas.height=canvas.height;stainOverlayKey='';}return stainOverlayCanvas;}\n",
    "function applyOpenSeadragonStain(){if(typeof hasTiledStainChannels==='function'&&hasTiledStainChannels()){stainOverlayCanvas=null;stainOverlayKey='';baseImageDirty=false;return false;}if(!stainEnabled||!stainOn){baseImageDirty=false;return false;}const key=stainOverlayCacheKey();if(!baseImageDirty&&stainOverlayCanvas&&stainOverlayKey===key){ctx.drawImage(stainOverlayCanvas,0,0,innerWidth,innerHeight);return true;}const bases=osdBaseCanvasCandidates();if(!bases.length)return false;const overlay=ensureStainOverlayCanvas(),overlayCtx=overlay.getContext('2d',{willReadFrequently:true});overlayCtx.clearRect(0,0,overlay.width,overlay.height);for(const base of bases){if(applyStainToCanvas(overlayCtx,overlay,base)){stainOverlayKey=key;baseImageDirty=false;ctx.drawImage(overlay,0,0,innerWidth,innerHeight);return true;}}if(!stainCanvasWarningShown){stainCanvasWarningShown=true;notify('Stain channel selection needs readable tiles. For full-resolution H/E/residual display, open with wsi_viewer_live(..., dynamic_tiles = TRUE) or serve the viewer through localhost.','warning',7000);}baseImageDirty=false;return false;}\n",
    "function channelTrimCache(map,maxSize){while(map&&map.size>maxSize){const first=map.keys().next().value;map.delete(first);}}\n",
    "function channelMaskSourceExtent(src){const meta=(src&&src.metadata)||{},extent=meta.extent||src.extent||{},sw=Math.max(1,Number(src&&src.width||cfg.slide_width||1)),sh=Math.max(1,Number(src&&src.height||cfg.slide_height||1));if(extent&&Number.isFinite(Number(extent.width))&&Number(extent.width)>0&&Number.isFinite(Number(extent.height))&&Number(extent.height)>0){return {x:Number(extent.x||0),y:Number(extent.y||0),width:Number(extent.width),height:Number(extent.height),source_width:sw,source_height:sh};}return {x:0,y:0,width:Number(cfg.slide_width||sw),height:Number(cfg.slide_height||sh),source_width:sw,source_height:sh};}\n",
    "function channelMaskLevelForScale(src,canvasScale){const ext=channelMaskSourceExtent(src),max=Number(src.max_level??cfg.max_level??0),min=Number(src.min_level??0),sx=Number(canvasScale||scale||1)*ext.width/ext.source_width,sy=Number(canvasScale||scale||1)*ext.height/ext.source_height,display=Math.max(1e-9,sx,sy);return clamp(Math.ceil(max+Math.log2(display)),min,max);}\n",
    "function channelMaskTileRange(src,level,bounds,margin=1){const ext=channelMaskSourceExtent(src),tileSize=Math.max(1,Number(src.tile_size||cfg.tile_size||512)),max=Number(src.max_level??cfg.max_level??0),down=Math.pow(2,Math.max(0,max-level)),tileFull=tileSize*down,levelW=Math.ceil(ext.source_width/down),levelH=Math.ceil(ext.source_height/down),maxCol=Math.max(0,Math.ceil(levelW/tileSize)-1),maxRow=Math.max(0,Math.ceil(levelH/tileSize)-1),xmin=(Number(bounds.xmin)-ext.x)/ext.width*ext.source_width,xmax=(Number(bounds.xmax)-ext.x)/ext.width*ext.source_width,ymin=(Number(bounds.ymin)-ext.y)/ext.height*ext.source_height,ymax=(Number(bounds.ymax)-ext.y)/ext.height*ext.source_height;return {c0:clamp(Math.floor(Math.min(xmin,xmax)/tileFull)-margin,0,maxCol),c1:clamp(Math.floor(Math.max(xmin,xmax)/tileFull)+margin,0,maxCol),r0:clamp(Math.floor(Math.min(ymin,ymax)/tileFull)-margin,0,maxRow),r1:clamp(Math.floor(Math.max(ymin,ymax)/tileFull)+margin,0,maxRow),down:down};}\n",
    "function channelMaskTileKey(src,level,col,row){return String(src.id||'channel')+'|'+level+'|'+col+'|'+row+'|'+String(src.cache_key||(src.metadata||{}).cache_key||src.created||'');}\n",
    "function channelLoadMaskTile(src,level,col,row){const key=channelMaskTileKey(src,level,col,row),cached=channelMaskTileCache.get(key);if(cached)return cached.complete&&cached.naturalWidth?cached:null;const url=channelTileUrl(src,level,col,row);if(!url)return null;const img=new Image();try{if(typeof tileNeedsCors==='function'&&tileNeedsCors(url))img.crossOrigin='anonymous';}catch(e){}img.decoding='async';img.onload=()=>{if(typeof requestDraw==='function')requestDraw();if(typeof drawMultiViewOverlays==='function')drawMultiViewOverlays();};img.onerror=()=>recordViewerLog('Mask legend tile failed to load.','warning',{id:src.id,url:url},'channels');img.src=url;channelMaskTileCache.set(key,img);channelTrimCache(channelMaskTileCache,512);return null;}\n",
    "function channelPixelMatchesSelectedColour(r,g,b,a,colours){if(a<8||!colours.length)return false;for(const col of colours){if(Math.max(Math.abs(r-col.r),Math.abs(g-col.g),Math.abs(b-col.b))<=30)return true;const s=r+g+b,cs=col.r+col.g+col.b;if(s>16&&cs>0){const d=Math.abs(r/s-col.r/cs)+Math.abs(g/s-col.g/cs)+Math.abs(b/s-col.b/cs);if(d<0.20&&Math.max(r,g,b)>18)return true;}}return false;}\n",
    "function channelProcessedMaskTile(src,img,level,col,row,colours){if(!img||!img.naturalWidth||!img.naturalHeight)return null;const selectedKey=channelMaskSelectedValues(src).join(','),key=channelMaskTileKey(src,level,col,row)+'|sel:'+selectedKey,cached=channelMaskProcessedCache.get(key);if(cached)return cached;const tileCanvas=document.createElement('canvas');tileCanvas.width=img.naturalWidth;tileCanvas.height=img.naturalHeight;const tileCtx=tileCanvas.getContext('2d',{willReadFrequently:true});try{tileCtx.drawImage(img,0,0);const data=tileCtx.getImageData(0,0,tileCanvas.width,tileCanvas.height);for(let p=0;p<data.data.length;p+=4){if(!channelPixelMatchesSelectedColour(data.data[p],data.data[p+1],data.data[p+2],data.data[p+3],colours))data.data[p+3]=0;}tileCtx.putImageData(data,0,0);}catch(e){if(!channelMaskFilterWarningShown){channelMaskFilterWarningShown=true;notify('Mask class selection needs readable mask tiles. Open through localhost/live mode if the browser blocks tile pixel access.','warning',7000);}return null;}channelMaskProcessedCache.set(key,tileCanvas);channelTrimCache(channelMaskProcessedCache,512);return tileCanvas;}\n",
    "function channelMaskTileSlideBounds(src,level,col,row,img){const ext=channelMaskSourceExtent(src),tileSize=Math.max(1,Number(src.tile_size||cfg.tile_size||512)),max=Number(src.max_level??cfg.max_level??0),down=Math.pow(2,Math.max(0,max-level)),sx0=col*tileSize*down,sy0=row*tileSize*down,sx1=Math.min(ext.source_width,sx0+Math.max(1,img&&img.naturalWidth||tileSize)*down),sy1=Math.min(ext.source_height,sy0+Math.max(1,img&&img.naturalHeight||tileSize)*down);return {x0:ext.x+sx0/ext.source_width*ext.width,y0:ext.y+sy0/ext.source_height*ext.height,x1:ext.x+sx1/ext.source_width*ext.width,y1:ext.y+sy1/ext.source_height*ext.height};}\n",
    "function drawFilteredMaskChannelOnContext(targetCtx,src,bounds,canvasScale,toCanvas){if(!channelMaskCanvasFilterActive(src))return false;const colours=channelMaskSelectedColours(src);if(!colours.length)return true;const level=channelMaskLevelForScale(src,canvasScale),range=channelMaskTileRange(src,level,bounds,1);targetCtx.save();const oldSmoothing=targetCtx.imageSmoothingEnabled;targetCtx.imageSmoothingEnabled=false;targetCtx.globalAlpha=clamp(Number(src.opacity??1),0,1);for(let row=range.r0;row<=range.r1;row++){for(let col=range.c0;col<=range.c1;col++){const img=channelLoadMaskTile(src,level,col,row);if(!img)continue;const filtered=channelProcessedMaskTile(src,img,level,col,row,colours);if(!filtered)continue;const b=channelMaskTileSlideBounds(src,level,col,row,img),p0=toCanvas({x:b.x0,y:b.y0}),p1=toCanvas({x:b.x1,y:b.y1}),x=Math.min(p0.x,p1.x),y=Math.min(p0.y,p1.y),w=Math.abs(p1.x-p0.x),h=Math.abs(p1.y-p0.y);if(!Number.isFinite(x)||!Number.isFinite(y)||x>w+innerWidth*2||y>h+innerHeight*2)continue;targetCtx.drawImage(filtered,Math.floor(x),Math.floor(y),Math.ceil(w)+1,Math.ceil(h)+1);}}targetCtx.globalAlpha=1;targetCtx.imageSmoothingEnabled=oldSmoothing;targetCtx.restore();return true;}\n",
    "function drawFilteredMaskChannels(){if(!Array.isArray(channelSources)||!channelSources.length)return;const bounds=(typeof visibleSlideBounds==='function')?visibleSlideBounds():{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};visibleChannelSources().filter(channelMaskCanvasFilterActive).forEach(src=>drawFilteredMaskChannelOnContext(ctx,src,bounds,scale,slideToCanvas));}\n",
    "function multiViewDrawFilteredMaskChannels(cx,pane){if(!pane||!Array.isArray(channelSources)||!channelSources.length)return;const bounds=(typeof multiViewVisibleSlideBounds==='function')?multiViewVisibleSlideBounds(pane):{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height},paneScale=(typeof multiViewCanvasUnitScale==='function')?multiViewCanvasUnitScale(pane):scale;channelSources.map(src=>typeof normaliseChannelSource==='function'?normaliseChannelSource(src):src).filter(Boolean).filter(src=>src.visible!==false&&channelMaskCanvasFilterActive(src)&&(typeof multiViewChannelSourceMatchesPane!=='function'||multiViewChannelSourceMatchesPane(src,pane))).forEach(src=>drawFilteredMaskChannelOnContext(cx,src,bounds,paneScale,p=>multiViewSlideToCanvas(p,pane)));}\n",
    "function draw(){ctx.clearRect(0,0,innerWidth,innerHeight);syncViewState();if(typeof syncBrushRadiusToZoom==='function')syncBrushRadiusToZoom();applyOpenSeadragonStain();drawFilteredMaskChannels();drawLayers();drawTileGrid();drawArtifactOverlays();drawRois();drawDraft();drawBrushPreview();drawEditHandles();drawMeasurements();drawTrajectories();drawCrosshair();drawScreenshotSelection();if(typeof drawMultiViewOverlays==='function')drawMultiViewOverlays();drawMiniNavigator();updateScaleBar();updateStatus(lastPointer,currentLevel());}\n",
    "function slideToCanvas(p){if(!osdReady||!osdItem()){if(typeof slideToViewImagePoint==='function'){const q=slideToViewImagePoint(p);return {x:offsetX+q.x*scale,y:offsetY+q.y*scale};}return {x:offsetX+p.x*scale,y:offsetY+p.y*scale};}const vp=osdItem().imageToViewportCoordinates(Number(p.x),Number(p.y));const px=osdViewer.viewport.pixelFromPoint(vp,true),q={x:px.x,y:px.y};return (typeof osdDisplayPixelForOverlay==='function')?osdDisplayPixelForOverlay(q):q;}\n",
    "function pointInsideSlide(p){return p&&p.x>=0&&p.y>=0&&p.x<=cfg.slide_width&&p.y<=cfg.slide_height;}\n",
    "function zoomToSlideBounds(b,pad=1.35){if(!osdReady||!osdItem()||!b)return;const w=Math.max(1,b.xmax-b.xmin),h=Math.max(1,b.ymax-b.ymin),cx=(b.xmin+b.xmax)/2,cy=(b.ymin+b.ymax)/2,x0=cx-w*pad/2,y0=cy-h*pad/2,x1=cx+w*pad/2,y1=cy+h*pad/2,p0=osdItem().imageToViewportCoordinates(x0,y0),p1=osdItem().imageToViewportCoordinates(x1,y1),vpRect=new OpenSeadragon.Rect(p0.x,p0.y,p1.x-p0.x,p1.y-p0.y);osdViewer.viewport.fitBoundsWithConstraints(vpRect,true);syncViewState();prefetchNeighborTiles();}\n",
    "function roiBounds(roi){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointInRing(p,ring){let inside=false;for(let i=0,j=ring.length-1;i<ring.length;j=i++){const xi=ring[i].x,yi=ring[i].y,xj=ring[j].x,yj=ring[j].y;const hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!rois.length)return;selectedRoi=(i+rois.length)%rois.length;const b=roiBounds(rois[selectedRoi]);zoomToSlideBounds(b,1.35);updateRoiList();draw();}\n",
    "function labelRectOverlaps(a,b,pad=10){return !(a.x+a.w+pad<b.x||b.x+b.w+pad<a.x||a.y+a.h+pad<b.y||b.y+b.h+pad<a.y);}\n",
    "function roiLabelCandidates(anchor,w,h){const gap=14,near=h+gap,far=h*2+gap,offsets=[[0,-h/2],[0,-near],[0,gap],[w/2+gap,-h/2],[-w/2-gap,-h/2],[w/2+gap,gap],[-w/2-gap,gap],[w/2+gap,-near],[-w/2-gap,-near],[0,-far],[0,h+gap],[w+gap,-h/2],[-w-gap,-h/2],[w+gap,gap],[-w-gap,gap],[w+gap,-near],[-w-gap,-near],[w/2+gap,-far],[-w/2-gap,-far],[w/2+gap,h+gap],[-w/2-gap,h+gap]];const seen=new Set();return offsets.map((o,rank)=>{const x=clamp(anchor.x+o[0]-w/2,6,Math.max(6,innerWidth-w-6)),y=clamp(anchor.y+o[1],6,Math.max(6,innerHeight-h-6)),key=Math.round(x)+'|'+Math.round(y);if(seen.has(key))return null;seen.add(key);return {x:x,y:y,w:w,h:h,rank:rank};}).filter(Boolean);}\n",
    "function labelAnchorDistanceScore(anchor,c){const cx=c.x+c.w/2,cy=c.y+c.h/2;return Math.hypot(cx-anchor.x,cy-anchor.y)+(c.rank||0)*4;}\n",
    "function placeRoiLabel(anchor,w,h,occupied){const candidates=roiLabelCandidates(anchor,w,h).sort((a,b)=>labelAnchorDistanceScore(anchor,a)-labelAnchorDistanceScore(anchor,b));for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,12)))return c;}for(const c of candidates){if(!occupied.some(r=>labelRectOverlaps(c,r,2)))return c;}return null;}\n",
    "function crispLabelRect(rect){return {x:Math.round(rect.x),y:Math.round(rect.y),w:Math.ceil(rect.w),h:Math.ceil(rect.h)};}\n",
    "function labelLeaderTarget(anchor,r){return {x:clamp(anchor.x,r.x,r.x+r.w),y:clamp(anchor.y,r.y,r.y+r.h)};}\n",
    "function drawLabelLeader(item,r,colour){if(!item.anchor)return;const t=labelLeaderTarget(item.anchor,r),dist=Math.hypot(t.x-item.anchor.x,t.y-item.anchor.y);if(dist<8)return;ctx.save();ctx.strokeStyle='rgba(0,0,0,.84)';ctx.lineWidth=3.5;ctx.beginPath();ctx.moveTo(item.anchor.x,item.anchor.y);ctx.lineTo(t.x,t.y);ctx.stroke();ctx.strokeStyle=colour;ctx.lineWidth=1.8;ctx.beginPath();ctx.moveTo(item.anchor.x,item.anchor.y);ctx.lineTo(t.x,t.y);ctx.stroke();ctx.fillStyle=colour;ctx.strokeStyle='rgba(0,0,0,.9)';ctx.lineWidth=1.2;ctx.beginPath();ctx.arc(item.anchor.x,item.anchor.y,3.2,0,Math.PI*2);ctx.fill();ctx.stroke();ctx.restore();}\n",
    "function drawPlacedRoiLabel(item,rect){const r=crispLabelRect(rect),colour=item.colour||'#5eead4';ctx.save();drawLabelLeader(item,r,colour);ctx.font='700 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='middle';ctx.shadowColor='rgba(0,0,0,.55)';ctx.shadowBlur=5;ctx.fillStyle='rgba(0,0,0,.92)';ctx.fillRect(r.x,r.y,r.w,r.h);ctx.shadowBlur=0;ctx.strokeStyle='rgba(255,255,255,.86)';ctx.lineWidth=2.5;ctx.strokeRect(r.x+.5,r.y+.5,Math.max(1,r.w-1),Math.max(1,r.h-1));ctx.strokeStyle=colour;ctx.lineWidth=1.6;ctx.strokeRect(r.x+2.5,r.y+2.5,Math.max(1,r.w-5),Math.max(1,r.h-5));ctx.fillStyle=colour;ctx.fillRect(r.x+1,r.y+1,6,Math.max(1,r.h-2));ctx.fillStyle='#ffffff';ctx.fillText(item.text,r.x+13,r.y+r.h/2);ctx.restore();}\n",
    "function drawRoiLabels(items){const occupied=[];items.sort((a,b)=>(b.priority||0)-(a.priority||0));items.forEach(item=>{const rect=placeRoiLabel(item.anchor,item.w,item.h,occupied);if(!rect)return;occupied.push(rect);drawPlacedRoiLabel(item,rect);});}\n",
    "function drawSimpleRoiPath(roi){ctx.beginPath();(roi.rings||[]).forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});}\n",
    "function strokeSimpleRoiBorder(roi,i){const selected=i===selectedRoi,highlighted=roiClassHighlighted(roi),dimmed=annotationHighlightActive()&&!highlighted,colour=selected?'#ffffff':(roi.colour||'#5eead4');ctx.save();ctx.lineJoin='round';ctx.lineCap='round';ctx.globalAlpha=dimmed?0.32:1;ctx.setLineDash([]);drawSimpleRoiPath(roi);ctx.strokeStyle=highlighted?'rgba(255,255,255,.96)':'rgba(0,0,0,.72)';ctx.lineWidth=highlighted?10:(selected?7:5);ctx.stroke();drawSimpleRoiPath(roi);ctx.strokeStyle=colour;ctx.lineWidth=highlighted?5:(selected?4:2);if(highlighted){ctx.shadowColor=colour;ctx.shadowBlur=7;}else if(!selected){ctx.setLineDash([7,4]);ctx.lineDashOffset=-(i%9)*2;}ctx.stroke();ctx.setLineDash([]);ctx.restore();}\n",
    "function roiCanvasMetrics(b){if(!b)return {w:0,h:0,cx:0,cy:0};const p0=slideToCanvas({x:b.xmin,y:b.ymin}),p1=slideToCanvas({x:b.xmax,y:b.ymax});return {x:Math.min(p0.x,p1.x),y:Math.min(p0.y,p1.y),w:Math.abs(p1.x-p0.x),h:Math.abs(p1.y-p0.y),cx:(p0.x+p1.x)/2,cy:(p0.y+p1.y)/2};}\n",
    "function roiVisibleSlideBounds(padFraction=.08){const b=(typeof visibleSlideBounds==='function')?visibleSlideBounds():{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height},pad=Math.max(b.xmax-b.xmin,b.ymax-b.ymin)*padFraction;return {xmin:Math.max(0,b.xmin-pad),ymin:Math.max(0,b.ymin-pad),xmax:Math.min(cfg.slide_width,b.xmax+pad),ymax:Math.min(cfg.slide_height,b.ymax+pad)};}\n",
    "function roiIntersectsViewport(roi,bounds){const b=roiBounds(roi);return !!(b&&bounds&&b.xmin<=bounds.xmax&&b.xmax>=bounds.xmin&&b.ymin<=bounds.ymax&&b.ymax>=bounds.ymin);}\n",
    "function roiLodMode(roi,i,b,metrics,highlighted){if(i===selectedRoi||highlighted||rois.length<350)return 'detail';const px=typeof slideUnitScale==='function'?slideUnitScale():scale,maxDim=Math.max(metrics.w,metrics.h),pts=Number(pointCount(roi)||0);if(px>.22&&maxDim>12)return 'detail';if(rois.length<1500&&px>.12&&pts<800&&maxDim>28)return 'detail';if(maxDim<5)return 'centroid';return 'bbox';}\n",
    "function drawRoiLodMarker(roi,i,b,metrics,mode,dimmed,highlighted){const selected=i===selectedRoi,colour=selected?'#ffffff':(roi.colour||'#5eead4');ctx.save();ctx.globalAlpha=dimmed?0.28:Math.max(.42,Math.min(1,roiOpacity));ctx.strokeStyle=highlighted?'#ffffff':colour;ctx.fillStyle=hexToRgba(colour,mode==='centroid'?.72:.12);ctx.lineWidth=highlighted?2.4:(selected?2.2:1.2);ctx.setLineDash(selected?[]:[4,3]);if(mode==='centroid'){const r=highlighted||selected?4:2.5;ctx.beginPath();ctx.arc(metrics.cx,metrics.cy,r,0,Math.PI*2);ctx.fill();ctx.stroke();}else{const x=Math.round(metrics.x)+.5,y=Math.round(metrics.y)+.5,w=Math.max(2,Math.round(metrics.w)),h=Math.max(2,Math.round(metrics.h));ctx.fillRect(x,y,w,h);ctx.strokeRect(x,y,w,h);}ctx.setLineDash([]);ctx.restore();}\n",
    "function drawRois(){if(typeof screenshotIncludeComponent==='function'&&!screenshotIncludeComponent('annotations'))return;if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='600 12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';const labelItems=[],borderItems=[],highlightActive=annotationHighlightActive(),viewBounds=roiVisibleSlideBounds(.10);let detailed=0,lod=0,culled=0;rois.forEach((roi,i)=>{if(!visibleRoi(roi)||!isDrawable(roi)){culled++;return;}const b=roiBounds(roi);if(!b||!roiIntersectsViewport(roi,viewBounds)){culled++;return;}const metrics=roiCanvasMetrics(b),highlighted=roiClassHighlighted(roi),dimmed=highlightActive&&!highlighted,mode=roiLodMode(roi,i,b,metrics,highlighted);let label=null;if(mode==='detail'){drawSimpleRoiPath(roi);(roi.rings||[]).forEach(ring=>{if(!label&&ring&&ring[0])label=slideToCanvas(ring[0]);});ctx.globalAlpha=dimmed?Math.min(.12,roiOpacity*.35):roiOpacity;ctx.fillStyle=roi.fill;ctx.fill('evenodd');ctx.globalAlpha=1;borderItems.push({roi:roi,index:i});detailed++;}else{drawRoiLodMarker(roi,i,b,metrics,mode,dimmed,highlighted);label={x:metrics.cx,y:metrics.cy};lod++;}if(showLabels&&label&&!dimmed&&(mode==='detail'||highlighted||i===selectedRoi)){const text=roi.name||roi.id;if(text)labelItems.push({anchor:label,text:text,w:ctx.measureText(text).width+18,h:22,colour:roi.colour,priority:highlighted?20:(i===selectedRoi?10:0)});}});borderItems.forEach(item=>strokeSimpleRoiBorder(item.roi,item.index));if(showLabels)drawRoiLabels(labelItems);if(lod>0&&typeof recordViewerLog==='function'&&Date.now()-(drawRois._lastLodLog||0)>8000){drawRois._lastLodLog=Date.now();recordViewerLog('Annotation level-of-detail active: '+lod+' simplified, '+detailed+' detailed, '+culled+' outside view.','info',{simplified:lod,detailed:detailed,culled:culled},'annotations');}ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function keyboardPanStep(fast=false){const base=Math.max(60,Math.round(Math.min(innerWidth,innerHeight)*0.12));return fast?base*3:base;}\n",
    "function panByPixels(dx,dy){if(typeof activeMultiViewPane==='function'&&typeof multiViewPanByPixels==='function'){const pane=activeMultiViewPane();if(pane){multiViewPanByPixels(pane,dx,dy);draw();return;}}if(!osdViewer)return;const delta=osdViewer.viewport.deltaPointsFromPixels(new OpenSeadragon.Point(-dx,-dy),true);osdViewer.viewport.panBy(delta,true);osdViewer.viewport.applyConstraints(false);syncViewState();prefetchNeighborTiles();draw();}\n",
    "function panByKeyboard(key,fast=false){const step=keyboardPanStep(fast);if(key==='ArrowLeft')panByPixels(step,0);else if(key==='ArrowRight')panByPixels(-step,0);else if(key==='ArrowUp')panByPixels(0,step);else if(key==='ArrowDown')panByPixels(0,-step);else return false;return true;}\n",
    "function pointerToSlide(evt){const rect=canvas.getBoundingClientRect();let px=evt.clientX-rect.left,py=evt.clientY-rect.top,out;if(osdReady&&osdItem()){const p=(typeof overlayPixelForOsdDisplay==='function')?overlayPixelForOsdDisplay({x:px,y:py}):{x:px,y:py},vp=osdViewer.viewport.pointFromPixel(new OpenSeadragon.Point(p.x,p.y),true),img=osdItem().viewportToImageCoordinates(vp);out={x:img.x,y:img.y};}else out={x:(px-offsetX)/scale,y:(py-offsetY)/scale};return (typeof normaliseSlidePoint==='function')?normaliseSlidePoint(out):out;}\n",
    "function updateStatus(p,level){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x';msg+=magnificationStatus();msg+=(typeof multiViewStatus==='function'?multiViewStatus():'');msg+=activeTileMode()?(' | Deep Zoom level '+level+'/'+cfg.max_level):' | preview image';msg+=stainStatus();msg+=measureStatus();msg+=trajectoryStatus();msg+=imageTransformStatus();msg+=screenshotStatus();if(loadingTiles)msg+=' | loading '+loadingTiles+' tile'+(loadingTiles===1?'':'s');if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg;}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>{b.classList.toggle('active',i===selectedRoi);b.classList.toggle('highlighted',rois[i]&&roiClassHighlighted(rois[i]));});renderAnnotationLabelHighlights();if(typeof syncProximityAnnotations==='function')syncProximityAnnotations(false);}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';b.style.setProperty('--wsi-highlight-accent',roi.colour||classColour(roi.class||'annotation'));const sw=document.createElement('span');sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){notify('Add at least 3 points','warning');return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;markAnnotationsDirty('roi_added');recordAnnotationHistory('roi_added',{id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount});buildRoiList();updateButtons();setMode('select');notify('ROI saved','success');if(typeof closeAllToolMenus==='function')closeAllToolMenus();draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>typeof normaliseSlideRingCoordinates==='function'?normaliseSlideRingCoordinates(r):r.map(p=>[Math.round(p.x),Math.round(p.y)]));const cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:cls},class:cls,source:roi.source||null,wsiTools:{coordinate_space:'level0_slide_pixels',coordinateSpace:'level0_slide_pixels',slide_width:Number(cfg.slide_width||0),slide_height:Number(cfg.slide_height||0),display_transform_applied:false}},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(typeof openAnnotationExportDialog==='function'){openAnnotationExportDialog('all');return;}if(!rois.length&&draft.length<3){notify('Draw an ROI first','warning');return;}const text=geojsonText();const name=(typeof projectAnnotationFilename==='function'?projectAnnotationFilename():null)||cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();markAnnotationsSaved('geojson_saved');notify('ROI saved','success');return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);markAnnotationsSaved('geojson_exported');notify('GeoJSON exported','success');}\n",
    "function updateButtons(){const has=rois.length>0,spotsAvailable=typeof annotationSpotItems==='function'&&annotationSpotItems().length>0,setDisabled=(id,value)=>{const button=el(id);if(button)button.disabled=!!value;},setActive=(id,value)=>{const button=el(id);if(button)button.classList.toggle('active',!!value);};['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>setDisabled(id,!has));setDisabled('annotationPanelToggle',false);setDisabled('layerPanelToggle',false);setDisabled('finishRoi',draft.length<3);setDisabled('undoPoint',draft.length<1);setDisabled('saveGeojson',!has&&draft.length<3);setDisabled('saveAnnotationSpotsCsv',!has||!spotsAvailable);setDisabled('seuratAnnotationSpotsCsv',!has||!spotsAvailable);setDisabled('exportSelectedRoiTiff',selectedRoi<0);if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();setActive('roiToggle',showRois&&has);setActive('labelsToggle',showLabels&&has);setActive('crosshairToggle',showCrosshair);const panel=el('roiPanel'),open=!!(panel&&panel.classList.contains('open'));['layersToggle','annotationPanelToggle','layerPanelToggle'].forEach(id=>setActive(id,open));}\n",
    wsi_viewer_geometry_js(),
    wsi_viewer_layers_js(),
    wsi_viewer_cell_controls_js(),
    wsi_viewer_seurat_js(),
    wsi_viewer_prediction_js(),
    wsi_viewer_kodama_js(),
    wsi_viewer_measure_js(),
    wsi_viewer_trajectory_js(),
    wsi_viewer_segmentation_js(),
    "canvas.addEventListener('mousedown',e=>{lastCanvasPointer=canvasPoint(e.clientX,e.clientY);lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(mode==='screenshot'){startScreenshotSelection(e);return;}if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='trajectory'){if(e.detail===1)addTrajectoryPoint(lastPointer);return;}if(mode==='brush'){startBrush(lastPointer,e);return;}if(mode==='edit'){const vertex=findVertexAt(e.clientX,e.clientY);if(vertex){pushAnnotationUndo(isTrajectoryAreaRoi(rois[vertex.roi])?'trajectory_border_vertex_moved':'roi_vertex_moved');selectAnnotation(vertex.roi,false);activeVertex=prepareVertexDrag(vertex);draggingVertex=activeVertex;updateRoiList();draw();return;}if(startCurveEditStroke(lastCanvasPointer,lastPointer,slideToCanvas))return;selectAnnotation(roiAt(lastPointer),true);draw();return;}if(mode==='measure'){addMeasurePoint(lastPointer);return;}if(mode==='select'){if(!selectObjectAtPoint(lastPointer,'trajectory')){clearSelectedAnnotation(false);clearSelectedTrajectory(false);clearSelectedLayerObject(false);clearSelectedMeasure(false);updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();buildLayerList();updateMeasureList();updateButtons();draw();}return;}dragging=true;dragStartX=e.clientX;dragStartY=e.clientY;dragMoved=false;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',e=>{lastCanvasPointer=canvasPoint(e.clientX,e.clientY);lastPointer=pointerToSlide(e);if(screenshotSelecting){finishScreenshotSelection(e);return;}if(brushing)finishBrush();if(curveEditStroke)finishCurveEditStroke();if(draggingVertex)finishActiveVertexDrag();const wasDragging=dragging,wasClick=wasDragging&&!dragMoved;dragging=false;canvas.classList.remove('dragging');if(wasDragging&&dragMoved&&osdViewer&&osdViewer.viewport){osdViewer.viewport.applyConstraints(true);syncViewState();if(typeof syncMultiViewFrom==='function')syncMultiViewFrom(osdViewer);scheduleViewerStateSync('viewport_changed',{scale:scale,offset_x:offsetX,offset_y:offsetY});}if(wasClick&&mode==='pan'){const p=pointerToSlide(e);if(selectObjectAtPoint(p,'trajectory'))e.preventDefault();}updateCursorFeedback(e);});\n",
    "window.addEventListener('mousemove',e=>{lastCanvasPointer=canvasPoint(e.clientX,e.clientY);lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(screenshotSelecting){updateScreenshotSelection(e);return;}if(brushing){addBrushPoint(lastPointer,e);return;}if(curveEditStroke){addCurveEditPoint(lastPointer);return;}if(draggingVertex){moveActiveVertex(lastPointer);return;}if(dragging){const dx=e.clientX-lastX,dy=e.clientY-lastY;if(Math.hypot(e.clientX-dragStartX,e.clientY-dragStartY)>3)dragMoved=true;if(dragMoved)panByPixels(dx,dy);lastX=e.clientX;lastY=e.clientY;}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.25:1/1.25,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}if(mode==='trajectory'){e.preventDefault();finishTrajectory();return;}if(mode==='edit'){e.preventDefault();insertVertexAt(pointerToSlide(e),e.clientX,e.clientY);return;}const p=pointerToSlide(e);if(selectObjectAtPoint(p)){e.preventDefault();return;}e.preventDefault();clearSelectionAndPan();});\n",
    "const bindButton=(id,handler)=>{const button=el(id);if(button)button.onclick=handler;};bindButton('toolPan',e=>{setMode('pan');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolSelect',e=>{setMode('select');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolDraw',e=>{setMode('draw');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolBrush',e=>{setMode('brush');closeMenuAfterToolAction(e.currentTarget);});bindButton('newRoi',e=>{startNewAnnotation(mode==='draw'?'draw':'brush');closeMenuAfterToolAction(e.currentTarget);});bindButton('toolEdit',e=>{setMode('edit');closeMenuAfterToolAction(e.currentTarget);});bindButton('finishRoi',e=>{finishDraft();closeMenuAfterToolAction(e.currentTarget);});bindButton('undoPoint',()=>{if(mode==='brush'&&brushPoints.length){brushPoints.pop();draw();}else undoDraftPoint();});bindButton('saveGeojson',e=>{saveGeojson();closeMenuAfterToolAction(e.currentTarget);});bindButton('saveAnnotationSpotsCsv',e=>{saveAnnotationSpotsCsv();closeMenuAfterToolAction(e.currentTarget);});bindButton('seuratAnnotationSpotsCsv',e=>{saveAnnotationSpotsCsv();closeMenuAfterToolAction(e.currentTarget);});bindButton('exportSelectedRoiTiff',e=>{exportImageRegion('selected_roi','tiff');closeMenuAfterToolAction(e.currentTarget);});bindButton('exportViewTiff',e=>{exportImageRegion('viewport','tiff');closeMenuAfterToolAction(e.currentTarget);});bindButton('screenshotTool',beginScreenshotMode);bindButton('zoomIn',()=>{if(typeof multiViewZoomAt==='function'&&multiViewZoomAt(1.5))return;zoomAt(1.5,innerWidth/2,innerHeight/2);});bindButton('zoomOut',()=>{if(typeof multiViewZoomAt==='function'&&multiViewZoomAt(1/1.5))return;zoomAt(1/1.5,innerWidth/2,innerHeight/2);});bindButton('fit',()=>{if(typeof multiViewFitView==='function'&&multiViewFitView()){draw();return;}fitView();});bindButton('oneToOne',()=>{if(typeof multiViewOneToOne==='function'&&multiViewOneToOne()){draw();return;}oneToOne();});\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();saveDisplayPreference();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();saveDisplayPreference();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);['layersToggle','annotationPanelToggle','layerPanelToggle'].forEach(id=>{const button=el(id);if(button)button.onclick=e=>{toggleRoiPanel();updateButtons();if(e&&e.currentTarget&&['annotationPanelToggle','layerPanelToggle'].includes(e.currentTarget.id)&&typeof closeContainingToolMenu==='function')closeContainingToolMenu(e.currentTarget);};});el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);saveRoiOpacityPreference();draw();};const crosshairButton=el('crosshairToggle');if(crosshairButton)crosshairButton.onclick=()=>{showCrosshair=!showCrosshair;updateButtons();saveDisplayPreference();draw();};\n",
    "window.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase(),typing=e.target&&['INPUT','TEXTAREA','SELECT'].includes(e.target.tagName);if(brushSubtractKeyEvent(e)){brushAltDown=true;updateCursorFeedback(e);draw();}if((e.ctrlKey||e.metaKey)&&!typing&&((e.shiftKey&&key==='z')||key==='y')){e.preventDefault();restoreAnnotationRedo();return;}if((e.ctrlKey||e.metaKey)&&!e.shiftKey&&key==='z'&&!typing){e.preventDefault();if(mode==='trajectory'&&trajectoryDraft.length){undoTrajectoryPoint();return;}restoreAnnotationUndo();return;}if(!typing&&!e.ctrlKey&&!e.metaKey&&!e.altKey&&panByKeyboard(e.key,e.shiftKey)){e.preventDefault();return;}if(!typing&&!e.ctrlKey&&!e.metaKey&&!e.altKey&&!e.shiftKey&&key==='n'){e.preventDefault();startNewAnnotation(mode==='draw'?'draw':'brush');return;}if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='b')setMode('brush');if(e.key==='e')setMode('edit');if(e.key==='m')setMode('measure');if(e.key==='t')setMode('trajectory');if(e.key==='Enter'&&mode==='draw')finishDraft();if(e.key==='Enter'&&mode==='trajectory'){e.preventDefault();finishTrajectory();}if((e.key==='Backspace'||e.key==='Delete')&&!typing){if(mode==='draw'&&draft.length){e.preventDefault();undoDraftPoint();return;}if(mode==='trajectory'&&trajectoryDraft.length){e.preventDefault();undoTrajectoryPoint();return;}if(mode==='edit'&&activeVertex){e.preventDefault();deleteSelectedVertex();return;}e.preventDefault();deleteSelectedObject();return;}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape'){measureStart=null;trajectoryDraft=[];brushing=false;brushPoints=[];brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();draggingVertex=null;activeVertex=null;if(typeof curveEditStroke!=='undefined')curveEditStroke=null;cancelScreenshotSelection();setMode('pan');draw();}});\n",
    "window.addEventListener('keyup',e=>{if(e.key==='Alt'||e.key==='Meta'||mode==='brush'||mode==='edit'){brushAltDown=brushSubtractModifier(e);updateCursorFeedback(e);draw();}});\n",
    "function initOpenSeadragon(){if(!window.OpenSeadragon){notify('OpenSeadragon failed to load','error',4200);return;}installProgressivePreviewBackground();const roundMode=(OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES&&OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES.ALWAYS)||undefined,loaderLimit=tileImageLoaderLimit();const options={element:viewerEl,showNavigationControl:false,showNavigator:false,blendTime:0,alwaysBlend:false,immediateRender:true,placeholderFillStyle:progressivePreviewEnabled()?'rgba(255,255,255,0)':'#fff',subPixelRoundingForTransparency:roundMode,minPixelRatio:1,maxImageCacheCount:tileCacheCount(),timeout:tileTimeoutMs(),animationTime:0.12,springStiffness:9,visibilityRatio:1,constrainDuringPan:true,minZoomImageRatio:1,maxZoomPixelRatio:16,gestureSettingsMouse:{clickToZoom:false,dblClickToZoom:false,scrollToZoom:false,dragToPan:false},gestureSettingsTouch:{pinchToZoom:true,dragToPan:false},tileSources:tileSourceFromConfig()};if(loaderLimit>0)options.imageLoaderLimit=Math.max(1,loaderLimit);osdViewer=OpenSeadragon(options);osdViewer.addHandler('open',()=>{osdReady=true;tileFailureNotified=false;installProgressivePreviewBackground();applyOpenSeadragonImageTransform();applyBaseImageDisplay();installInitialChannelSources();resize();if(typeof projectItems!=='undefined'&&projectItems.length&&typeof activeProjectSection==='function'&&typeof zoomToProjectContent==='function')zoomToProjectContent(projectItems[activeProjectIndex]||null,activeProjectSection());if(typeof refreshMultiViewSources==='function')refreshMultiViewSources();prefetchNeighborTiles();notify(activeTileMode()?'Tiled viewer ready':'Preview image ready',activeTileMode()?'success':'info');draw();});['animation','animation-finish','tile-drawn','tile-loaded','tile-load-failed','resize'].forEach(name=>osdViewer.addHandler(name,()=>{markBaseImageDirty();syncViewState();if(name==='animation-finish'){if(typeof syncMultiViewFrom==='function')syncMultiViewFrom(osdViewer);scheduleViewerStateSync('viewport_changed',{scale:scale,offset_x:offsetX,offset_y:offsetY});prefetchNeighborTiles();}else if(name==='tile-loaded')prefetchNeighborTiles();else if(name==='tile-load-failed'&&!tileFailureNotified){tileFailureNotified=true;notify('Tiles did not load. For live CZI viewing, keep the R session that created the viewer running.','warning',7200);}if(name==='animation')draw();else requestDraw();}));}\n",
    "bindExclusiveMenus();bindShortcutHelp();bindCommandPalette();bindUnsavedIndicator();bindUnsavedRefreshGuard();bindMiniNavigator();bindProjectPanel();bindRoiPanelControls();bindSelectionCardControls();bindJobControls();bindStainControls();bindBaseImageControls();bindRoiClassControls();bindAnnotationListControls();bindAnnotationHistoryControls();bindViewerLogControls();bindViewerLogCapture();bindAnnotationSectionMaximizeControls();bindMeasureControls();bindTrajectoryControls();bindImageTransformControls();bindGeojsonImportControls();bindSegmentationControls();bindArtifactControls();bindCellControls();bindSeuratControls();bindPredictionControls();bindKodamaControls();bindMagnificationControls();bindMultiViewControls();bindPreferenceControls();buildRoiList();buildLayerList();buildChannelList();const initialMode=applyViewerPreferences();updateButtons();updateAnnotationDirtyIndicator();syncMessage('');setMode(initialMode||'pan');startViewerStateSocket();scheduleViewerStateSync('viewer_loaded',{});startViewerCommandPolling();startViewerAutosave();\n",
    "window.addEventListener('resize',resize);\n",
    "resize();initOpenSeadragon();\n",
    "</script>\n",
    "</body>\n",
    "</html>\n"
  )
}

#' View a slide interactively
#'
#' Creates an HTML viewer for a slide without loading the full whole-slide image
#' into R memory. The default `mode = "thumbnail"` writes a self-contained
#' thumbnail preview. `mode = "tiles"` uses libvips to build Deep Zoom tiles on
#' disk so zooming can reveal full-resolution detail in the browser. When
#' `project_images` contains multiple images or sections and the caller has not
#' explicitly selected a mode, wsiTools opens in tiled/OpenSeadragon mode
#' automatically when a tile backend or explicit tile URL is available so the
#' View menu can compare project images side by side.
#'
#' The viewer includes lightweight pathology-viewer controls inspired by tools
#' such as QuPath and napari. Controls are grouped into menus for navigation,
#' annotations, GeoJSON overlays, view aids, and optional stains. These cover
#' pan and annotation modes, fit and 1:1 zoom, ROI visibility and label toggles, ROI
#' opacity, ROI previous/next navigation, a left-side annotation manager with
#' GeoJSON geometry listing, visibility toggles, lock/unlock, colour editing,
#' category editing, duplicate/delete, zoom-to-ROI, selected-ROI export,
#' browser-side GeoJSON import, crosshair display, polygon drawing,
#' brush-style annotation painting with a brush-size slider,
#' selected-ROI extension, platform-aware brush removal (`Alt` on Windows/Linux
#' and `Command` on macOS), hole filling, selected-ROI vertex editing,
#' category reassignment including custom annotation categories, 10-step
#' `Ctrl+Z` undo and `Ctrl+Shift+Z`/`Ctrl+Y` redo for annotations,
#' trajectories, and closed project images, CellPhenotyper cell overlays,
#' distance measurement in pixels and micrometres when MPP metadata is
#' available, browser-based GeoJSON export, and a `Ctrl+K` command palette for
#' common actions such as importing GeoJSON, exporting selected ROIs, showing
#' a coordinate-only tile grid, and requesting a project save
#' from a live R session. A `?` shortcut and Help menu show a compact viewer
#' guide plus the keyboard map for pan, draw, brush, edit, measure, undo, redo,
#' save, import, and export. The View menu can split tiled slides into 2, 4, or
#' 6 OpenSeadragon panes with linked or independent zoom/pan. In project
#' viewers, the panes use different project images/sections when available;
#' otherwise they compare separate tissue regions within the active image.
#' Optional `stain = "ihc"` adds an RGB optical-density deconvolution display
#' with simple original/all/show-only controls.
#' `stain = "he"` uses hematoxylin, eosin, and residual H&E channels. Use
#' `channels` to provide
#' multi-IHC or custom H&E channel definitions. `project_images` adds a
#' left-side Project section for related images or multi-scene microscopy files,
#' including CZI entries that can be activated through optional Bio-Formats
#' tooling.
#'
#' @param slide A `wsi_slide` object.
#' @param width Thumbnail width used for `mode = "thumbnail"`.
#' @param height Optional thumbnail height limit for `mode = "thumbnail"`.
#' @param output Optional HTML output path. Defaults to a temporary file.
#' @param open Whether to open the viewer with [utils::browseURL()].
#' @param title Optional viewer title.
#' @param overwrite Whether to overwrite `output` when it already exists.
#' @param mode Viewer mode. Use `"thumbnail"` for a small self-contained
#'   preview or `"tiles"` for a full-resolution Deep Zoom viewer.
#' @param tiled Optional logical shortcut for `mode = "tiles"` when `TRUE` and
#'   `mode = "thumbnail"` when `FALSE`.
#' @param tile_dir Directory for Deep Zoom files when `mode = "tiles"`.
#' @param tile_size Deep Zoom tile size in pixels.
#' @param tile_format Tile image format for `mode = "tiles"`.
#' @param quality JPEG quality for tiled viewers when `tile_format = "jpg"`.
#' @param rebuild Whether to rebuild existing Deep Zoom tiles.
#' @param tile_overlap Optional Deep Zoom tile overlap in pixels. Defaults to
#'   one-pixel overlap for generated static tiles and zero for external tile
#'   sources unless supplied.
#' @param tile_url_base,tile_url_template Optional externally served tile source
#'   for `mode = "tiles"`. These are normally set by [wsi_viewer_live()] when
#'   `dynamic_tiles = TRUE`; when supplied, static Deep Zoom generation is
#'   skipped.
#' @param tile_prefetch_margin Number of tile rows/columns beyond the visible
#'   viewport to prefetch in tiled viewers. Use `0` to prefetch only visible
#'   tiles, `1` or `2` for smoother panning, and `-1` to disable browser-side
#'   prefetching.
#' @param tile_cache_count Maximum number of decoded OpenSeadragon tiles kept
#'   in the browser cache.
#' @param tile_prefetch_cache_count Maximum number of browser-side speculative
#'   prefetch tile images kept by wsiTools.
#' @param progressive_preview Whether tiled viewers should keep the
#'   low-resolution navigator preview behind the tile canvas while sharper
#'   tiles stream in. This improves perceived opening speed for very large WSI
#'   files without loading the full image into R.
#' @param tile_url_style URL style for `tile_url_base`: `"deepzoom"` uses
#'   `/{level}/{x}_{y}.{format}`, while `"slash"` uses
#'   `/{level}/{x}/{y}.{format}`.
#' @param max_level Optional maximum Deep Zoom level for externally served
#'   tiles.
#' @param tile_source_label Optional text shown in the viewer subtitle for
#'   externally served tiles.
#' @param tile_sources Optional metadata list for project/session tile sources.
#' @param roi Optional ROI overlay. Supply a GeoJSON path or an object returned
#'   by [wsi_read_geojson()]. Coordinates are interpreted as level-0 slide
#'   pixel coordinates, matching QuPath-style GeoJSON exports.
#' @param roi_fill_alpha Fill transparency for ROI polygons.
#' @param roi_class_presets Editable ROI class presets from
#'   [wsi_roi_class_presets()]. These define dropdown classes, default class
#'   colours, and optional export rules.
#' @param stain Optional stain display mode. Use `"ihc"` for interactive
#'   stain-channel color deconvolution in the browser, or `"he"` for
#'   hematoxylin/eosin/residual H&E deconvolution.
#' @param channels Optional stain channels created by [wsi_stain_channels()].
#'   RGB brightfield deconvolution supports up to three independent channels at
#'   a time.
#' @param base_layer_name Display name for the base image layer in the Stains
#'   menu.
#' @param base_layer_visible,base_layer_opacity Initial visibility and opacity
#'   for the base tissue/image layer. These are useful when checking tiled
#'   masks or other overlays independently from the base image.
#' @param hematoxylin,hrp RGB optical-density vectors used when
#'   `stain = "ihc"` and `channels` is not supplied.
#' @param hematoxylin_colour,hrp_colour Initial display colours for the
#'   hematoxylin and HRP/DAB channels.
#' @param hematoxylin_strength,hrp_strength Initial display gains for the
#'   hematoxylin and HRP/DAB channels.
#' @param segmentation_run_url Optional live HTTP endpoint used by the Cells
#'   menu to run selected-ROI cell segmentation.
#' @param segmentation_engines,segmentation_default_engine Optional Cells-menu
#'   engine presets. Supported values are `"stardist_he"`, `"stardist_ihc"`,
#'   and `"mesmer_dapi"`.
#' @param viewer_state_url Optional HTTP endpoint used to sync annotations,
#'   measurements, segmentation overlays, and display state back to R. Use
#'   [wsi_viewer_live()] or [wsi_viewer_session()] to create this endpoint.
#' @param viewer_state_ws_url Optional WebSocket endpoint used by live viewers
#'   for lower-latency browser-to-R sync. The HTTP polling bridge remains the
#'   fallback when this is `NULL` or WebSockets are unavailable.
#' @param viewer_transport Browser-to-R live transport advertised to the
#'   viewer: `"auto"`, `"websocket"`, or `"polling"`. Static viewers ignore
#'   this value.
#' @param seurat_gene_url Optional live HTTP endpoint used by the Seurat menu
#'   to retrieve one selected gene from R at a time. Static viewers ignore this
#'   unless a compatible endpoint is supplied manually.
#' @param spatial_tile_export_url Optional live HTTP endpoint used by Seurat,
#'   Giotto, and SpatialExperiment menus to export spot-centered tiles from R.
#'   Static viewers can preview tile boxes but cannot write local image files.
#' @param image_export_url Optional live HTTP endpoint used by the viewer to
#'   export the current viewport or selected annotation bounding box as
#'   PNG/JPEG/TIFF through R.
#' @param prediction_url Optional live HTTP endpoint used by the Prediction menu
#'   to run optional `fastPLS` PLS-LDA from annotation-defined training/test
#'   sets in the active R session.
#' @param proximity_url Optional live HTTP endpoint used by the Trajectories
#'   menu to calculate nearest-neighbour proximity from spots/cells inside one
#'   annotation to spots/cells inside another annotation in the active R session.
#' @param autosave_enabled Whether the viewer should periodically sync state to
#'   the live R bridge for project autosave. This is normally set by
#'   [wsi_viewer_live()].
#' @param autosave_interval Seconds between autosave syncs.
#' @param autosave_path Optional project directory shown in autosave status
#'   messages.
#' @param project_images Optional paths, slide objects, or project records to
#'   list in the left Project section. CZI paths are listed without loading the
#'   full file into R; real CZI previews require the optional native libCZI
#'   bridge, or an explicitly enabled legacy Python bridge.
#' @param channel_sources Optional tiled channel sources created with
#'   [wsi_channel_source()].
#' @param layers Optional viewer overlay layers, for example point layers from
#'   CellPhenotyper outputs.
#' @param seurat Optional object returned by [wsi_link_seurat_image()] used to
#'   enable the top **Seurat** menu and any available reduction plots.
#' @param cellphenotyper Optional CellPhenotyper project metadata used to enable
#'   the top **Cells** menu.
#'
#' @return The HTML viewer path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' html <- wsi_viewer(slide, open = FALSE)
#' wsi_close(slide)
#' }
wsi_viewer <- function(slide, width = 1600, height = NULL, output = NULL,
                       open = interactive(), title = NULL, overwrite = FALSE,
                       mode = c("thumbnail", "tiles"), tiled = NULL,
                       tile_dir = NULL,
                       tile_size = 512, tile_format = c("jpg", "png"),
                       quality = 90, rebuild = FALSE,
                       tile_overlap = NULL,
                       tile_url_base = NULL,
                       tile_url_template = NULL,
                       tile_prefetch_margin = 1L,
                       tile_cache_count = 768L,
                       tile_prefetch_cache_count = 512L,
                       progressive_preview = TRUE,
                       tile_url_style = c("deepzoom", "slash"),
                       max_level = NULL,
                       tile_source_label = NULL,
                       tile_sources = NULL,
                       roi = NULL,
                       roi_fill_alpha = 0.15,
                       roi_class_presets = wsi_roi_class_presets(),
          stain = c("none", "ihc", "he"),
                       channels = NULL,
                       base_layer_name = NULL,
                       base_layer_visible = TRUE,
                       base_layer_opacity = 1,
                       hematoxylin = c(0.650, 0.704, 0.286),
                       hrp = c(0.268, 0.570, 0.776),
                       hematoxylin_colour = "#4b3f99",
                       hrp_colour = "#8b5a2b",
                       hematoxylin_strength = 1,
                       hrp_strength = 1,
                       segmentation_run_url = NULL,
                       segmentation_engines = c("stardist_he", "stardist_ihc", "mesmer_dapi"),
                       segmentation_default_engine = "stardist_he",
	                       viewer_state_url = NULL,
	                       viewer_state_ws_url = NULL,
	                       seurat_gene_url = NULL,
	                       spatial_tile_export_url = NULL,
	                       image_export_url = NULL,
	                       geojson_mask_url = NULL,
	                       prediction_url = NULL,
	                       proximity_url = NULL,
	                       autosave_enabled = FALSE,
                       autosave_interval = 5,
                       autosave_path = NULL,
                       project_images = NULL,
                       channel_sources = NULL,
                       layers = NULL,
                       seurat = NULL,
                       cellphenotyper = NULL,
                       viewer_transport = c("auto", "websocket", "polling")) {
  wsi_check_slide(slide)
  mode_missing <- missing(mode)
  mode <- match.arg(mode)
  if (!is.null(tiled)) {
    if (!is.logical(tiled) || length(tiled) != 1L || is.na(tiled)) {
      wsi_abort("`tiled` must be `NULL`, `TRUE`, or `FALSE`.")
    }
    mode <- if (isTRUE(tiled)) "tiles" else "thumbnail"
    mode_missing <- FALSE
  }
  tile_format <- match.arg(tile_format)
  tile_url_style <- match.arg(tile_url_style)
  viewer_transport <- match.arg(viewer_transport)
  if (!is.logical(base_layer_visible) || length(base_layer_visible) != 1L || is.na(base_layer_visible)) {
    wsi_abort("`base_layer_visible` must be `TRUE` or `FALSE`.")
  }
  base_layer_opacity <- wsi_check_scalar_number(base_layer_opacity, "base_layer_opacity")
  base_layer_opacity <- max(0, min(1, base_layer_opacity))
  stain <- match.arg(stain)
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  if (!is.null(tile_overlap)) {
    tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
    if (tile_overlap >= tile_size) {
      wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
    }
  }
  tile_prefetch_margin <- as.integer(wsi_check_scalar_number(tile_prefetch_margin, "tile_prefetch_margin"))
  tile_cache_count <- as.integer(wsi_check_scalar_number(tile_cache_count, "tile_cache_count", allow_zero = FALSE))
  tile_prefetch_cache_count <- as.integer(wsi_check_scalar_number(tile_prefetch_cache_count, "tile_prefetch_cache_count"))
  tile_cache_count <- max(64L, tile_cache_count)
  tile_prefetch_cache_count <- max(0L, tile_prefetch_cache_count)
  if (!is.logical(progressive_preview) || length(progressive_preview) != 1L || is.na(progressive_preview)) {
    wsi_abort("`progressive_preview` must be `TRUE` or `FALSE`.")
  }
  roi_fill_alpha <- wsi_check_scalar_number(roi_fill_alpha, "roi_fill_alpha")
  if (roi_fill_alpha > 1) {
    wsi_abort("`roi_fill_alpha` must be between 0 and 1.")
  }
  if (!is.null(height)) {
    height <- as.integer(wsi_check_scalar_number(height, "height", allow_zero = FALSE))
  }
  if (!is.null(segmentation_run_url)) {
    if (!is.character(segmentation_run_url) || length(segmentation_run_url) != 1L ||
        is.na(segmentation_run_url) || !nzchar(segmentation_run_url)) {
      wsi_abort("`segmentation_run_url` must be `NULL` or a single non-empty URL.")
    }
  }
  segmentation_engines <- unique(as.character(segmentation_engines %||% character()))
  segmentation_engines <- segmentation_engines[nzchar(segmentation_engines) & !is.na(segmentation_engines)]
  if (!length(segmentation_engines)) {
    segmentation_engines <- c("stardist_he")
  }
  segmentation_engines <- vapply(segmentation_engines, wsi_cell_segmentation_engine, character(1))
  segmentation_default_engine <- wsi_cell_segmentation_engine(segmentation_default_engine)
  if (!segmentation_default_engine %in% segmentation_engines) {
    segmentation_default_engine <- segmentation_engines[[1L]]
  }
  if (!is.null(viewer_state_url)) {
    if (!is.character(viewer_state_url) || length(viewer_state_url) != 1L ||
        is.na(viewer_state_url) || !nzchar(viewer_state_url)) {
      wsi_abort("`viewer_state_url` must be `NULL` or a single non-empty URL.")
    }
  }
  if (!is.null(viewer_state_ws_url)) {
    if (!is.character(viewer_state_ws_url) || length(viewer_state_ws_url) != 1L ||
        is.na(viewer_state_ws_url) || !nzchar(viewer_state_ws_url)) {
      wsi_abort("`viewer_state_ws_url` must be `NULL` or a single non-empty WebSocket URL.")
    }
    if (!grepl("^wss?://", viewer_state_ws_url)) {
      wsi_abort("`viewer_state_ws_url` must start with `ws://` or `wss://`.")
    }
  }
	  if (!is.null(seurat_gene_url)) {
	    if (!is.character(seurat_gene_url) || length(seurat_gene_url) != 1L ||
	        is.na(seurat_gene_url) || !nzchar(seurat_gene_url)) {
	      wsi_abort("`seurat_gene_url` must be `NULL` or a single non-empty URL.")
	    }
	  }
	  if (!is.null(spatial_tile_export_url)) {
	    if (!is.character(spatial_tile_export_url) || length(spatial_tile_export_url) != 1L ||
	        is.na(spatial_tile_export_url) || !nzchar(spatial_tile_export_url)) {
	      wsi_abort("`spatial_tile_export_url` must be `NULL` or a single non-empty URL.")
	    }
	  }
	  if (!is.null(image_export_url)) {
	    if (!is.character(image_export_url) || length(image_export_url) != 1L ||
	        is.na(image_export_url) || !nzchar(image_export_url)) {
	      wsi_abort("`image_export_url` must be `NULL` or a single non-empty URL.")
	    }
	  }
	  if (!is.null(geojson_mask_url)) {
	    if (!is.character(geojson_mask_url) || length(geojson_mask_url) != 1L ||
	        is.na(geojson_mask_url) || !nzchar(geojson_mask_url)) {
	      wsi_abort("`geojson_mask_url` must be `NULL` or a single non-empty URL.")
	    }
	  }
	  if (!is.null(prediction_url)) {
	    if (!is.character(prediction_url) || length(prediction_url) != 1L ||
	        is.na(prediction_url) || !nzchar(prediction_url)) {
	      wsi_abort("`prediction_url` must be `NULL` or a single non-empty URL.")
	    }
	  }
	  if (!is.null(proximity_url)) {
	    if (!is.character(proximity_url) || length(proximity_url) != 1L ||
	        is.na(proximity_url) || !nzchar(proximity_url)) {
	      wsi_abort("`proximity_url` must be `NULL` or a single non-empty URL.")
	    }
	  }
  if (!is.null(tile_url_base) &&
      (!is.character(tile_url_base) || length(tile_url_base) != 1L || is.na(tile_url_base) || !nzchar(tile_url_base))) {
    wsi_abort("`tile_url_base` must be `NULL` or a single non-empty URL/path string.")
  }
  if (!is.null(tile_url_template) &&
      (!is.character(tile_url_template) || length(tile_url_template) != 1L || is.na(tile_url_template) || !nzchar(tile_url_template))) {
    wsi_abort("`tile_url_template` must be `NULL` or a single non-empty URL template.")
  }
  if (!is.null(max_level)) {
    max_level <- as.integer(wsi_check_scalar_number(max_level, "max_level"))
  }
  if (!is.logical(autosave_enabled) || length(autosave_enabled) != 1L || is.na(autosave_enabled)) {
    wsi_abort("`autosave_enabled` must be `TRUE` or `FALSE`.")
  }
  autosave_interval <- as.numeric(wsi_check_scalar_number(autosave_interval, "autosave_interval", allow_zero = FALSE))
  if (!is.null(autosave_path) &&
      (!is.character(autosave_path) || length(autosave_path) != 1L || is.na(autosave_path) || !nzchar(autosave_path))) {
    wsi_abort("`autosave_path` must be `NULL` or a single non-empty project directory path.")
  }

  if (is.null(output)) {
    output <- tempfile(fileext = ".html")
    overwrite <- TRUE
  }
  output <- wsi_validate_output_path(output, overwrite = overwrite)

  title <- title %||% sprintf("wsiTools viewer: %s", basename(slide$path %||% "slide"))
  subtitle <- sprintf(
    "%s backend | %s x %s px | %s level%s",
    slide$backend,
    format(slide$dimensions[["width"]], scientific = FALSE, trim = TRUE),
    format(slide$dimensions[["height"]], scientific = FALSE, trim = TRUE),
    nrow(slide$levels),
    if (nrow(slide$levels) == 1L) "" else "s"
  )
  roi_class_presets <- wsi_normalize_roi_class_presets(roi_class_presets)
  rois <- wsi_viewer_roi_features(roi, fill_alpha = roi_fill_alpha, class_presets = roi_class_presets)
  mpp <- wsi_mpp(slide)
  mpp_config <- if (all(is.finite(mpp)) && all(mpp > 0)) {
    list(x = unname(mpp[["x"]]), y = unname(mpp[["y"]]))
  } else {
    NULL
  }
  objective_power <- suppressWarnings(wsi_objective_power(slide))
  objective_power_config <- if (length(objective_power) == 1L &&
      is.finite(objective_power) && objective_power > 0) {
    unname(objective_power)
  } else {
    NULL
  }
  annotation_filename <- paste0(tools::file_path_sans_ext(basename(output)), "_annotations.geojson")
  base_layer_config <- list(
    id = "base_image",
    name = as.character(base_layer_name %||% "Base image"),
    visible = isTRUE(base_layer_visible),
    opacity = base_layer_opacity
  )
  stain_config <- wsi_ihc_stain_config(
    stain = stain,
    channels = channels,
    hematoxylin = hematoxylin,
    hrp = hrp,
    hematoxylin_colour = hematoxylin_colour,
    hrp_colour = hrp_colour,
    hematoxylin_strength = hematoxylin_strength,
    hrp_strength = hrp_strength
  )
  project_config <- wsi_viewer_project_config(
    slide,
    project_images = project_images,
    width = min(width, 768L),
    height = height
  )
  project_item_count <- length(project_config$items %||% list())
  if (isTRUE(mode_missing) &&
      identical(mode, "thumbnail") &&
      project_item_count > 1L &&
      !identical(slide$backend, "mock") &&
      (wsi_has_vips() || !is.null(tile_url_base) || !is.null(tile_url_template))) {
    mode <- "tiles"
    tile_source_label <- tile_source_label %||% "auto tiled multi-view project"
  }
  layers_config <- wsi_viewer_layers_config(layers)
  seurat_config <- wsi_viewer_seurat_config(seurat)
  if (is.null(mpp_config) && isTRUE(seurat_config$enabled)) {
    mpp_config <- wsi_viewer_mpp_payload(seurat_config$mpp %||% seurat_config$pixel_size %||% NULL)
  }
  cellphenotyper_config <- wsi_viewer_cellphenotyper_config(cellphenotyper)
  segmentation_config <- list(
    enabled = !is.null(segmentation_run_url),
    run_url = segmentation_run_url,
    engines = lapply(segmentation_engines, function(engine) {
      list(
        engine = engine,
        label = switch(
          engine,
          stardist_he = "StarDist H&E",
          stardist_ihc = "StarDist IHC",
          mesmer_dapi = "Mesmer DAPI",
          engine
        )
      )
    }),
    default_engine = segmentation_default_engine
  )
  prediction_config <- wsi_prediction_config(seurat_config, cellphenotyper_config)
  proximity_config <- wsi_proximity_config(seurat_config, cellphenotyper_config)
  managed_analysis_project <- wsi_viewer_managed_analysis_project(
    seurat_config,
    cellphenotyper_config,
    project_config
  )

  if (identical(mode, "thumbnail")) {
    config <- list(
      title = title,
      subtitle = subtitle,
      viewer_mode = mode,
      preference_key = "wsiTools.viewer.preferences.v1",
      slide_width = unname(slide$dimensions[["width"]]),
      slide_height = unname(slide$dimensions[["height"]]),
      mpp = mpp_config,
      objective_power = objective_power_config,
      image_data_uri = wsi_viewer_thumbnail_data_uri(slide, width = width, height = height),
      navigator_image_data_uri = NULL,
      annotation_filename = annotation_filename,
      roi_class_presets = wsi_viewer_class_presets_payload(roi_class_presets),
      segmentation_run_url = segmentation_run_url,
      viewer_state_url = viewer_state_url,
	      viewer_state_ws_url = viewer_state_ws_url,
	      viewer_transport = viewer_transport,
	      seurat_gene_url = seurat_gene_url,
	      spatial_tile_export_url = spatial_tile_export_url,
	      image_export_url = image_export_url,
	      geojson_mask_url = geojson_mask_url,
	      prediction_url = prediction_url,
	      proximity_url = proximity_url,
	      managed_analysis_project = managed_analysis_project,
      autosave_enabled = isTRUE(autosave_enabled) && (!is.null(viewer_state_url) || !is.null(viewer_state_ws_url)),
      autosave_interval_ms = as.integer(max(1000, round(autosave_interval * 1000))),
      autosave_path = autosave_path,
      stain = stain_config,
      base_layer = base_layer_config,
      segmentation = segmentation_config,
      project = project_config,
      channel_sources = wsi_channel_sources_payload(channel_sources),
      tile_sources = tile_sources %||% list(),
      tile_prefetch_margin = as.integer(tile_prefetch_margin),
      tile_cache_count = as.integer(tile_cache_count),
      tile_prefetch_cache_count = as.integer(tile_prefetch_cache_count),
      progressive_preview = isTRUE(progressive_preview),
      rois = rois,
      layers = layers_config,
      seurat = seurat_config,
      cellphenotyper = cellphenotyper_config,
      prediction = prediction_config,
      proximity = proximity_config
    )
    writeLines(wsi_viewer_html(config), output, useBytes = TRUE)
  } else {
    external_tiles <- !is.null(tile_url_base) || !is.null(tile_url_template)
    requested_tile_overlap <- tile_overlap %||% if (isTRUE(external_tiles)) 0L else 1L
    tiles <- NULL
    if (!isTRUE(external_tiles)) {
      tile_dir <- tile_dir %||% wsi_default_tile_dir(output)
      tiles <- wsi_create_deepzoom_tiles(
        slide = slide,
        tile_dir = tile_dir,
        tile_size = tile_size,
        tile_overlap = requested_tile_overlap,
        tile_format = tile_format,
        quality = quality,
        rebuild = rebuild
      )
    }
    actual_tile_overlap <- if (!is.null(tiles$overlap)) tiles$overlap else requested_tile_overlap
    max_level <- max_level %||% wsi_dz_max_level(slide$dimensions[["width"]], slide$dimensions[["height"]])

    config <- list(
      title = title,
      subtitle = paste0(subtitle, " | ", tile_source_label %||% if (isTRUE(external_tiles)) "served tiles" else "Deep Zoom tiles"),
      viewer_mode = mode,
      preference_key = "wsiTools.viewer.preferences.v1",
      slide_width = unname(slide$dimensions[["width"]]),
      slide_height = unname(slide$dimensions[["height"]]),
      mpp = mpp_config,
      objective_power = objective_power_config,
      tile_size = as.integer(tile_size),
      tile_format = tile_format,
      tile_url_base = tile_url_base %||% if (!is.null(tile_url_template)) "" else wsi_tile_base_url(tile_dir, output),
      tile_url_template = tile_url_template,
      tile_url_style = tile_url_style,
      tile_overlap = actual_tile_overlap,
      navigator_image_data_uri = wsi_viewer_navigator_data_uri(slide, width = 512),
      dzi = if (!is.null(tiles)) basename(tiles$dzi) else NULL,
      max_level = max_level,
      annotation_filename = annotation_filename,
      roi_class_presets = wsi_viewer_class_presets_payload(roi_class_presets),
      segmentation_run_url = segmentation_run_url,
      viewer_state_url = viewer_state_url,
	      viewer_state_ws_url = viewer_state_ws_url,
	      viewer_transport = viewer_transport,
	      seurat_gene_url = seurat_gene_url,
	      spatial_tile_export_url = spatial_tile_export_url,
	      image_export_url = image_export_url,
	      geojson_mask_url = geojson_mask_url,
	      prediction_url = prediction_url,
	      proximity_url = proximity_url,
	      managed_analysis_project = managed_analysis_project,
      autosave_enabled = isTRUE(autosave_enabled) && (!is.null(viewer_state_url) || !is.null(viewer_state_ws_url)),
      autosave_interval_ms = as.integer(max(1000, round(autosave_interval * 1000))),
      autosave_path = autosave_path,
      stain = stain_config,
      base_layer = base_layer_config,
      segmentation = segmentation_config,
      project = project_config,
      channel_sources = wsi_channel_sources_payload(channel_sources),
      tile_sources = tile_sources %||% list(),
      tile_prefetch_margin = as.integer(tile_prefetch_margin),
      tile_cache_count = as.integer(tile_cache_count),
      tile_prefetch_cache_count = as.integer(tile_prefetch_cache_count),
      progressive_preview = isTRUE(progressive_preview),
      rois = rois,
      layers = layers_config,
      seurat = seurat_config,
      cellphenotyper = cellphenotyper_config,
      prediction = prediction_config,
      proximity = proximity_config
    )
    writeLines(wsi_tiled_viewer_html(config), output, useBytes = TRUE)
  }

  if (isTRUE(open)) {
    utils::browseURL(output)
  }
  invisible(output)
}

#' Create a viewer from non-interactive R scripts
#'
#' `wsi_viewer_noninteractive()` is a script-friendly wrapper around
#' [wsi_open()] and [wsi_viewer()]. It never opens a browser, returns the HTML
#' path visibly, and prints the path unless `quiet = TRUE`. This is useful from
#' `Rscript`, batch jobs, Quarto/knitr setup chunks, or remote sessions where
#' `utils::browseURL()` is not available.
#'
#' The function creates a static HTML viewer. For a live dynamic-tile viewer
#' from `Rscript`, call [wsi_viewer_live()] directly with `open = FALSE` and
#' `wait = TRUE` so the local tile/sync server remains alive.
#'
#' @param input A path to an image/slide or an existing `wsi_slide` object.
#' @param output HTML output path. If `NULL` and `input` is a path, a
#'   `*_wsiTools_viewer.html` file is created in the current working directory.
#'   If `input` is a slide object and `output = NULL`, a temporary file is used.
#' @param ... Additional arguments passed to [wsi_viewer()], such as `mode`,
#'   `width`, `stain`, `roi`, `project_images`, or `channel_sources`.
#' @param backend Backend passed to [wsi_open()] when `input` is a path.
#' @param overwrite Whether to overwrite `output` if it already exists.
#' @param close_slide Close the slide after writing the viewer when this
#'   function opened it from a path.
#' @param quiet If `FALSE`, print the generated HTML path.
#'
#' @return The normalized HTML path, visibly.
#' @export
#'
#' @examples
#' \dontrun{
#' html <- wsi_viewer_noninteractive(
#'   "sample.svs",
#'   output = "sample_viewer.html",
#'   mode = "tiles"
#' )
#'
#' # Equivalent command-line use:
#' # Rscript -e 'library(wsiTools); wsi_viewer_noninteractive("sample.svs")'
#' }
wsi_viewer_noninteractive <- function(input, output = NULL, ...,
                                      backend = c("auto", "openslide", "vips"),
                                      overwrite = FALSE,
                                      close_slide = TRUE,
                                      quiet = FALSE) {
  backend <- match.arg(backend)
  if (!is.logical(close_slide) || length(close_slide) != 1L || is.na(close_slide)) {
    wsi_abort("`close_slide` must be `TRUE` or `FALSE`.")
  }
  if (!is.logical(quiet) || length(quiet) != 1L || is.na(quiet)) {
    wsi_abort("`quiet` must be `TRUE` or `FALSE`.")
  }

  slide <- input
  if (!inherits(slide, "wsi_slide")) {
    if (!is.character(input) || length(input) != 1L || is.na(input) || !nzchar(input)) {
      wsi_abort("`input` must be a single image path or a `wsi_slide` object.")
    }
    if (is.null(output)) {
      output <- paste0(tools::file_path_sans_ext(basename(input)), "_wsiTools_viewer.html")
    }
    slide <- wsi_open(input, backend = backend)
    if (isTRUE(close_slide)) {
      on.exit(wsi_close(slide), add = TRUE)
    }
  }

  dots <- list(...)
  if (!is.null(dots$open)) {
    wsi_warn("`open` is ignored by `wsi_viewer_noninteractive()`; the viewer is always written without opening a browser.")
  }
  if (!is.null(dots$output) && is.null(output)) {
    output <- dots$output
  }
  if (!is.null(dots$overwrite)) {
    overwrite <- isTRUE(dots$overwrite)
  }
  dots$open <- FALSE
  dots$output <- output
  dots$overwrite <- overwrite
  dots$slide <- slide

  html <- do.call(wsi_viewer, dots)
  html <- normalizePath(html, winslash = "/", mustWork = FALSE)
  if (!isTRUE(quiet)) {
    message("wsiTools viewer written to: ", html)
  }
  html
}

#' View GeoJSON ROI annotations on a slide
#'
#' Convenience wrapper around [wsi_read_geojson()] and [wsi_viewer()]. The
#' GeoJSON is imported, converted to browser overlay coordinates, and drawn over
#' the interactive viewer. By default this uses full-resolution Deep Zoom tiled
#' mode so ROI outlines remain aligned while zooming. The viewer menus can
#' toggle ROI visibility, labels, opacity, ROI navigation, a GeoJSON geometry
#' side window, drawing, and GeoJSON export.
#'
#' @param slide A `wsi_slide` object.
#' @param geojson GeoJSON file path or a `wsi_roi` object from
#'   [wsi_read_geojson()].
#' @param mode Viewer mode passed to [wsi_viewer()].
#' @param ... Additional arguments passed to [wsi_viewer()], such as `output`,
#'   `tile_dir`, `tile_size`, `open`, or `overwrite`.
#'
#' @return The HTML viewer path, invisibly.
#' @export
#' @examples
#' \dontrun{
#' slide <- wsi_open("sample.svs")
#' html <- wsi_viewer_roi(slide, "annotations.geojson", output = "roi_viewer.html")
#' wsi_close(slide)
#' }
wsi_viewer_roi <- function(slide, geojson, mode = c("tiles", "thumbnail"), ...) {
  mode <- match.arg(mode)
  roi <- if (is.character(geojson) && length(geojson) == 1L) {
    wsi_read_geojson(geojson)
  } else {
    geojson
  }
  wsi_viewer(slide, mode = mode, roi = roi, ...)
}
