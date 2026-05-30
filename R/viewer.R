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
      "Interactive viewing requires libvips for real slides in this milestone. Install `vips` and `vipsheader`, then retry.",
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

wsi_viewer_navigator_data_uri <- function(slide, width = 512) {
  tryCatch(
    wsi_viewer_thumbnail_data_uri(slide, width = width, height = NULL),
    error = function(err) NULL
  )
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
      "Full-resolution tiled viewing requires libvips. Install `vips` and `vipsheader`, then retry.",
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
    return(list(dzi = dzi_file, tiles = tile_files, overlap = wsi_dzi_overlap(dzi_file, default = tile_overlap)))
  }

  if (isTRUE(rebuild)) {
    if (file.exists(dzi_file)) {
      unlink(dzi_file)
    }
    if (dir.exists(tile_files)) {
      unlink(tile_files, recursive = TRUE)
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
    ".multiViewPane{position:relative;overflow:hidden;background:#070707;border:1px solid rgba(255,255,255,.16);border-radius:4px;min-width:0;min-height:0;}\n",
    ".multiViewPane.active{border-color:#5eead4;box-shadow:inset 0 0 0 1px rgba(94,234,212,.75);}\n",
    ".multiViewPaneViewer{position:absolute;inset:0;}\n",
    ".multiViewPaneTitle{position:absolute;left:8px;top:8px;z-index:2;padding:4px 7px;border-radius:999px;background:rgba(0,0,0,.62);border:1px solid rgba(255,255,255,.20);font-size:11px;line-height:1;color:#e5e7eb;pointer-events:none;}\n",
    "#viewer.dragging,#overlay.dragging{cursor:grabbing;}\n",
    "#viewer.selecting,#viewer.drawing,#viewer.editing,#overlay.selecting,#overlay.drawing,#overlay.editing{cursor:crosshair;}\n",
    "#viewer.measuring,#viewer.trajectory,#overlay.measuring,#overlay.trajectory{cursor:crosshair;}\n",
    "#viewer.brushing,#overlay.brushing{cursor:copy;}\n",
    "#viewer.brush-add,#overlay.brush-add{cursor:copy;}\n",
    "#viewer.brush-subtract,#overlay.brush-subtract{cursor:alias;}\n",
    "#viewer.cursor-blocked,#overlay.cursor-blocked{cursor:not-allowed;}\n",
    ".bar{position:fixed;left:12px;right:12px;top:12px;display:flex;gap:8px;align-items:center;pointer-events:none;z-index:30;}\n",
    ".panel{background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;backdrop-filter:blur(6px);pointer-events:auto;}\n",
    ".titleLine{display:flex;align-items:center;gap:8px;min-width:0;}\n",
    ".title{font-weight:600;max-width:34vw;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".meta{font-size:12px;color:#d2d2d2;}\n",
    ".unsavedIndicator{display:none;align-items:center;gap:5px;flex:0 0 auto;border:1px solid rgba(250,204,21,.45);border-radius:999px;background:rgba(113,63,18,.72);color:#fde68a;font-size:11px;line-height:1;padding:3px 7px;}\n",
    ".unsavedIndicator::before{content:'';width:7px;height:7px;border-radius:50%;background:#facc15;box-shadow:0 0 0 2px rgba(250,204,21,.18);}\n",
    ".unsavedIndicator.dirty{display:inline-flex;}\n",
    ".spacer{flex:1;}\n",
    ".tools{display:flex;gap:6px;align-items:center;flex-wrap:wrap;justify-content:flex-end;position:relative;}\n",
    ".sep{width:1px;height:22px;background:rgba(255,255,255,.18);display:inline-block;}\n",
    ".navPanButton{width:30px;height:28px;padding:5px;display:inline-flex;align-items:center;justify-content:center;}\n",
    ".iconMove{width:17px;height:17px;display:block;}\n",
    ".navDock{position:fixed;right:14px;top:50%;transform:translateY(-50%);z-index:32;display:flex;flex-direction:column;gap:6px;padding:7px;}\n",
    ".navDock button{width:42px;height:36px;padding:0;display:flex;align-items:center;justify-content:center;font-weight:650;}\n",
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
    ".menuBody{position:absolute;right:0;top:calc(100% + 7px);z-index:20;min-width:230px;max-width:min(360px,calc(100vw - 24px));max-height:calc(100vh - 108px);overflow:auto;background:rgba(18,18,18,.96);border:1px solid rgba(255,255,255,.18);border-radius:6px;padding:8px;box-shadow:0 16px 36px rgba(0,0,0,.34);display:flex;flex-direction:column;gap:6px;}\n",
    ".menuGrid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px;}\n",
    ".menuBody button{width:100%;text-align:center;}\n",
    ".menuBody label.control{justify-content:space-between;gap:10px;min-height:28px;}\n",
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
    "#scaleBar{position:fixed;left:50%;bottom:18px;transform:translateX(-50%);z-index:31;pointer-events:none;color:#f8fafc;text-align:center;font-size:12px;line-height:1;text-shadow:0 1px 2px rgba(0,0,0,.9);min-width:120px;}\n",
    "#scaleBar.unavailable{display:none;}\n",
    "#scaleBarLine{height:6px;border-left:2px solid #fff;border-right:2px solid #fff;border-bottom:2px solid #fff;margin:0 auto 5px;box-shadow:0 1px 2px rgba(0,0,0,.85);}\n",
    "#scaleBarLabel{display:inline-block;background:rgba(18,18,18,.8);border:1px solid rgba(255,255,255,.18);border-radius:999px;padding:3px 8px;}\n",
    "#toastStack{position:fixed;right:12px;bottom:184px;z-index:42;display:flex;flex-direction:column;gap:8px;align-items:flex-end;pointer-events:none;max-width:min(360px,calc(100vw - 24px));}\n",
    ".toast{pointer-events:auto;max-width:100%;padding:9px 12px;border-radius:6px;border:1px solid rgba(255,255,255,.18);background:rgba(18,18,18,.94);color:#f8fafc;font-size:13px;line-height:1.25;box-shadow:0 14px 30px rgba(0,0,0,.36);opacity:0;transform:translateY(8px);transition:opacity .18s ease,transform .18s ease;cursor:pointer;}\n",
    ".toast-actionable{display:flex;align-items:center;gap:10px;}\n",
    ".toastMessage{min-width:0;}\n",
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
    "#shortcutHelpBackdrop{position:fixed;inset:0;z-index:46;background:rgba(0,0,0,.32);display:none;pointer-events:auto;}\n",
    "#shortcutHelpBackdrop.open{display:block;}\n",
    "#shortcutHelp{position:fixed;left:50%;top:8vh;transform:translateX(-50%);z-index:47;width:min(760px,calc(100vw - 28px));max-height:calc(100vh - 96px);display:none;pointer-events:auto;padding:12px;box-shadow:0 24px 60px rgba(0,0,0,.48);overflow:auto;}\n",
    "#shortcutHelp.open{display:block;}\n",
    ".shortcutHelpHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;}\n",
    ".shortcutHelpTitle{font-weight:650;}\n",
    ".shortcutHelpHint{font-size:11px;color:#b8b8b8;margin-top:2px;}\n",
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
    "#kodamaPlotWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#seuratPlotWindow::after{content:\"\";position:absolute;right:4px;bottom:4px;width:14px;height:14px;pointer-events:none;background:linear-gradient(135deg,transparent 0 45%,rgba(255,255,255,.34) 45% 54%,transparent 54% 64%,rgba(255,255,255,.34) 64% 73%,transparent 73%);opacity:.8;}\n",
    "#kodamaPlotWindow.moving{user-select:none;}\n",
    "#seuratPlotWindow.moving{user-select:none;}\n",
    "#kodamaPlotWindow.open{display:flex;flex-direction:column;}\n",
    "#seuratPlotWindow.open{display:flex;flex-direction:column;}\n",
    ".kodamaPlotHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    ".seuratPlotHead{display:flex;align-items:flex-start;justify-content:space-between;gap:12px;margin-bottom:8px;cursor:move;user-select:none;}\n",
    "#kodamaPlotTitle{font-weight:650;line-height:1.2;}\n",
    "#seuratPlotTitle{font-weight:650;line-height:1.2;}\n",
    "#kodamaPlotSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    "#seuratPlotSubtitle{font-size:11px;color:#b8b8b8;margin-top:2px;word-break:break-word;}\n",
    ".kodamaPlotTools{display:flex;gap:6px;flex-wrap:wrap;margin-bottom:8px;}\n",
    ".seuratPlotTools{display:flex;gap:6px;flex-wrap:wrap;margin-bottom:8px;}\n",
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
    "#projectPanel,#roiPanel,#annotationHistory{position:relative;box-sizing:border-box;}\n",
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
    "#projectPanel.resized,#annotationHistory.resized{display:flex;flex-direction:column;overflow:hidden;}\n",
    "#projectPanel.resized #projectPanelBody,#annotationHistory.resized #annotationHistoryBody{overflow:auto;max-height:none;flex:1 1 auto;min-height:0;padding-bottom:12px;}\n",
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
    ".projectPanel{margin:0;padding:8px 10px;border:1px solid rgba(255,255,255,.16);border-radius:6px;background:rgba(18,18,18,.86);}\n",
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
    ".annotationEditor .control{justify-content:space-between;}\n",
    ".annotationBrushControls{display:flex;flex-direction:column;gap:4px;margin:2px 0;padding:7px;border:1px solid rgba(255,255,255,.10);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".annotationBrushControls input[type=range]{width:170px;}\n",
    "#brushZoomHint{margin-bottom:0;}\n",
    ".annotationActions{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:5px;}\n",
    ".annotationActions button{padding:6px 5px;font-size:12px;}\n",
    "#annotationHistory{display:block;flex:0 0 auto;max-height:min(30vh,280px);overflow:auto;margin:0;padding:8px;border:1px solid rgba(255,255,255,.12);border-radius:5px;background:rgba(255,255,255,.03);}\n",
    "#annotationHistory.closed{display:none;}\n",
    "#annotationHistory.minimized{max-height:none;overflow:hidden;}\n",
    "#annotationHistory.minimized #annotationHistoryBody{display:none;}\n",
    "#projectPanel.minimized,#roiPanel.minimized,#annotationHistory.minimized{height:auto!important;flex:0 0 auto!important;}\n",
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
    ".historyItem{border:1px solid rgba(255,255,255,.12);border-radius:5px;background:rgba(255,255,255,.045);padding:6px;}\n",
    ".historyAction{font-size:11.5px;font-weight:650;color:#e7e7e7;line-height:1.25;}\n",
    ".historyMeta{font-size:10.5px;color:#a8a8a8;line-height:1.25;margin-top:2px;word-break:break-word;}\n",
    ".annotationSearch{display:grid;grid-template-columns:1fr auto auto auto;gap:5px;align-items:center;margin:6px 0;}\n",
    ".annotationSearch input[type=text]{width:100%;box-sizing:border-box;}\n",
    ".annotationSearch select{max-width:98px;}\n",
    ".annotationSearch button{padding:5px 7px;font-size:12px;}\n",
    ".annotationListTools{display:flex;gap:5px;align-items:center;margin:6px 0;}\n",
    ".annotationListTools button{padding:5px 7px;font-size:12px;}\n",
    ".roiListEmpty{padding:10px 8px;color:#b8b8b8;font-size:12px;border:1px dashed rgba(255,255,255,.18);border-radius:5px;background:rgba(255,255,255,.035);}\n",
    ".roiItem{display:block;width:100%;margin:6px 0;padding:8px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.04);color:#eee;text-align:left;}\n",
    ".roiItem.hidden{opacity:.55;}\n",
    ".layerItem.hidden{opacity:.55;}\n",
    ".roiItem.locked{border-color:rgba(250,204,21,.42);}\n",
    ".roiItem.active{border-color:#5eead4;background:rgba(20,184,166,.2);}\n",
    ".roiTop{display:flex;align-items:center;gap:8px;margin-bottom:5px;}\n",
    ".layerItem{display:block;width:100%;margin:6px 0;padding:8px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.04);color:#eee;text-align:left;}\n",
    ".layerTop{display:flex;align-items:center;gap:8px;margin-bottom:6px;}\n",
    ".layerControls{display:grid;grid-template-columns:auto 1fr auto;gap:7px;align-items:center;font-size:11px;color:#cfcfcf;}\n",
    ".layerControls button{padding:5px 6px;font-size:11px;}\n",
    ".swatch{width:10px;height:10px;border-radius:50%;display:inline-block;flex:0 0 auto;}\n",
    ".roiName{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;font-weight:600;}\n",
    ".roiClass{color:#aaa;font-size:11px;margin-left:auto;max-width:90px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".roiControls{display:grid;grid-template-columns:auto auto auto 1fr auto auto auto;gap:5px;align-items:center;margin-top:7px;}\n",
    ".roiControls button{padding:5px 6px;font-size:11px;}\n",
    ".roiControls input[type=color]{width:24px;height:22px;}\n",
    ".roiSelect{margin:0;}\n",
    ".roiDetails{display:grid;grid-template-columns:76px 1fr;gap:2px 8px;font-size:11px;color:#cfcfcf;line-height:1.25;}\n",
    ".roiDetails span:nth-child(odd){color:#8f8f8f;}\n",
    ".roiDetails code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;white-space:normal;word-break:break-word;}\n",
    ".roiBadge{display:inline-block;border:1px solid rgba(255,255,255,.16);border-radius:4px;padding:1px 5px;font-size:10px;color:#d7d7d7;background:rgba(255,255,255,.06);}\n",
    "#measureList{display:flex;flex-direction:column;gap:5px;max-height:180px;overflow:auto;}\n",
    ".measureItem{display:block;width:100%;text-align:left;line-height:1.25;}\n",
    ".measureItem code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;}\n",
    "#trajectoryResolutionValue,#trajectoryAreaWidthValue{min-width:54px;text-align:right;color:#d7d7d7;font-size:11px;}\n",
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
    "#jobSummary{font-size:11px;color:#b8b8b8;line-height:1.35;margin:0 2px 4px;}\n",
    "#jobList{display:flex;flex-direction:column;gap:6px;max-height:320px;overflow:auto;}\n",
    ".jobItem{border:1px solid rgba(255,255,255,.14);border-radius:5px;background:rgba(255,255,255,.045);padding:8px;}\n",
    ".jobTop{display:flex;align-items:flex-start;gap:8px;margin-bottom:6px;}\n",
    ".jobName{font-weight:650;line-height:1.2;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".jobStatus{margin-left:auto;font-size:10.5px;text-transform:uppercase;letter-spacing:.04em;color:#cbd5e1;border:1px solid rgba(255,255,255,.16);border-radius:999px;padding:2px 6px;background:rgba(255,255,255,.06);}\n",
    ".jobStatus.running{color:#bfdbfe;border-color:rgba(96,165,250,.42);background:rgba(30,64,175,.34);}\n",
    ".jobStatus.queued{color:#fde68a;border-color:rgba(250,204,21,.42);background:rgba(113,63,18,.34);}\n",
    ".jobStatus.completed{color:#99f6e4;border-color:rgba(45,212,191,.44);background:rgba(15,118,110,.34);}\n",
    ".jobStatus.failed{color:#fecaca;border-color:rgba(248,113,113,.52);background:rgba(127,29,29,.42);}\n",
    ".jobProgress{height:6px;border-radius:999px;background:rgba(255,255,255,.12);overflow:hidden;margin-bottom:6px;}\n",
    ".jobProgressFill{height:100%;width:0%;background:#5eead4;transition:width .2s ease;}\n",
    ".jobProgressFill.indeterminate{width:42%;animation:wsiJobPulse 1.2s ease-in-out infinite;background:#93c5fd;}\n",
    "@keyframes wsiJobPulse{0%{transform:translateX(-120%)}50%{transform:translateX(80%)}100%{transform:translateX(260%)}}\n",
    ".jobMeta{font-size:11px;color:#cbd5e1;line-height:1.3;word-break:break-word;}\n",
    ".jobLog{margin:6px 0 0;max-height:96px;overflow:auto;white-space:pre-wrap;font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:10.5px;line-height:1.25;color:#e5e7eb;background:rgba(0,0,0,.28);border-radius:4px;padding:6px;}\n",
    "#miniNavigator{position:fixed;right:12px;bottom:12px;width:220px;z-index:28;pointer-events:auto;padding:7px;}\n",
    "#miniNavigatorCanvas{display:block;width:100%;height:132px;border-radius:4px;background:#0b0b0b;border:1px solid rgba(255,255,255,.18);}\n",
    ".miniNavigatorMeta{display:flex;justify-content:space-between;gap:8px;margin-top:5px;font-size:10.5px;color:#cbd5e1;line-height:1.2;}\n",
    ".miniNavigatorMeta span{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    "@media(max-width:900px){.title{max-width:42vw}.bar{align-items:flex-start}.tools{max-width:56vw}.menuBody{right:auto;left:0}.navDock{right:12px;top:auto;bottom:118px;transform:none;}#workspacePanel{left:12px;right:12px;top:118px;width:auto;max-height:calc(100vh - 176px);}#workspaceResizeHandle{display:none;}#selectionCard{left:12px;right:12px;top:auto;bottom:58px;width:auto;}#annotationHistory.maximized{left:12px;right:12px;top:72px;bottom:12px;}#miniNavigator{display:none;}#toastStack{left:12px;right:12px;bottom:104px;align-items:stretch;}#commandPalette,#shortcutHelp{top:8vh;}.viewerGuideGrid,.viewerGuideTroubleshooting{grid-template-columns:1fr;}#multiViewGrid.layout2,#multiViewGrid.layout4,#multiViewGrid.layout6{grid-template-columns:1fr;grid-template-rows:repeat(var(--multi-view-count,1),minmax(160px,1fr));}}\n"
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

wsi_viewer_menu_js <- function() {
  paste0(
    "function closeContainingToolMenu(control){const menu=control&&control.closest?control.closest('.toolMenu'):null;if(menu)menu.open=false;}\n",
    "function bindExclusiveMenus(){const menus=Array.from(document.querySelectorAll('.toolMenu'));const closeOtherMenus=active=>menus.forEach(menu=>{if(menu!==active)menu.open=false;});const closeAllMenus=()=>menus.forEach(menu=>{menu.open=false;});menus.forEach(menu=>{const summary=menu.querySelector('summary');if(summary){summary.addEventListener('pointerdown',()=>closeOtherMenus(menu));summary.addEventListener('click',()=>setTimeout(()=>{if(menu.open)closeOtherMenus(menu);},0));}menu.addEventListener('toggle',()=>{if(menu.open)closeOtherMenus(menu);});});document.addEventListener('click',e=>{if(!e.target.closest('.toolMenu'))closeAllMenus();});document.addEventListener('keydown',e=>{if(e.key==='Escape')closeAllMenus();});}\n"
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
  has_geojson <- isTRUE(kodama$enabled) && length(geojsons) > 0L
  has_plots <- length(plots) > 0L
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
    "KODAMA",
    "Visualize KODAMA/MedSAM refined GeoJSON annotations and membership plots",
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
  grandqc <- config$cellphenotyper$grandqc %||% list(enabled = FALSE, geojsons = list())
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

wsi_viewer_seurat_controls <- function(config) {
  seurat <- config$seurat %||% list(enabled = FALSE, spot_count = 0L, plots = list())
  enabled <- isTRUE(seurat$enabled)
  plot_count <- length(seurat$plots %||% list())
  spot_count <- as.integer(seurat$displayed_spot_count %||% seurat$spot_count %||% 0L)
  wsi_viewer_menu(
    "Seurat",
    "Spatial transcriptomics spot overlays and PCA plots",
    paste0(
      "<div class=\"menuTitle\">Spatial spots</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"seuratSpotToggle\" title=\"Show or hide Seurat spatial spots\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      ">Spots</button>",
      "<button id=\"seuratSpotZoom\" title=\"Zoom to the Seurat spot extent\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      ">Zoom spots</button>",
      "<button id=\"seuratLayerPanel\" title=\"Open the layer list in the left panel\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      ">Layer panel</button>",
      "</div>",
      "<label class=\"control\" title=\"Seurat spot overlay opacity\">Opacity <input id=\"seuratSpotOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"0.85\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      "></label>",
      "<label class=\"control\" title=\"Seurat spot marker radius in slide pixels\">Spot size <input id=\"seuratSpotRadius\" type=\"range\" min=\"1\" max=\"120\" step=\"1\" value=\"",
      wsi_html_escape(as.character(round(as.numeric(seurat$spot_radius %||% 16)))),
      "\"",
      if (enabled && spot_count > 0L) "" else " disabled",
      "><span id=\"seuratSpotRadiusValue\">",
      wsi_html_escape(as.character(round(as.numeric(seurat$spot_radius %||% 16)))),
      " px</span></label>",
      "<div class=\"menuTitle\">Dimensional reduction</div>",
      "<div class=\"menuGrid\">",
      "<button id=\"seuratOpenPca\" title=\"Open the Seurat PCA scatter plot\"",
      if (enabled && plot_count > 0L) "" else " disabled",
      ">Open PCA</button>",
      "<button id=\"seuratClearSelection\" title=\"Clear PCA-selected spot highlights\"",
      if (enabled && plot_count > 0L) "" else " disabled",
      ">Clear selection</button>",
      "</div>",
      "<div id=\"seuratSummary\" class=\"menuHint\">",
      if (enabled) {
        paste0(
          format(spot_count, big.mark = ","),
          " Seurat spot", if (spot_count == 1L) "" else "s",
          " linked to ", wsi_html_escape(seurat$image_name %||% "spatial image"),
          " | ", wsi_html_escape(toupper(seurat$reduction %||% "PCA"))
        )
      } else {
        "No Seurat object is attached to this viewer."
      },
      "</div>"
    )
  )
}

wsi_viewer_chrome <- function(config, loading_message, tiled = FALSE) {
  class_options <- wsi_viewer_class_options(config$roi_class_presets)
  roi_panel_class <- "panel"
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
    "<span id=\"annotationDirtyIndicator\" class=\"unsavedIndicator\" title=\"Annotation changes have not been exported or saved to a project\">Unsaved</span></div><div class=\"meta\">",
    wsi_html_escape(config$subtitle), "</div></div>\n",
    "<div class=\"spacer\"></div>\n",
    "<div class=\"panel tools\" role=\"toolbar\" aria-label=\"Viewer tools\">",
    "<button id=\"toolPan\" class=\"navPanButton active\" title=\"Pan mode\" aria-label=\"Pan mode\"><svg class=\"iconMove\" viewBox=\"0 0 24 24\" aria-hidden=\"true\"><path d=\"M12 2l3 3h-2v6h6V9l3 3-3 3v-2h-6v6h2l-3 3-3-3h2v-6H5v2l-3-3 3-3v2h6V5H9l3-3z\" fill=\"currentColor\"/></svg></button>",
    wsi_viewer_menu(
      "Project",
      "Open the project panel or add images/file references",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"projectOpenPanel\" title=\"Open or restore the left Project panel\" onclick=\"event.preventDefault();event.stopPropagation();if(typeof openProjectPanel==='function'){openProjectPanel();}else{var w=document.getElementById('workspacePanel'),p=document.getElementById('projectPanel');if(w){w.style.visibility='visible';w.style.opacity='1';w.style.pointerEvents='auto';w.style.left='12px';w.style.top=(innerWidth<=900?'118px':'72px');w.style.right='auto';}if(p){p.classList.remove('closed','minimized');}}var m=this.closest('.toolMenu');if(m)m.open=false;return false;\">Open panel</button>",
        "<button id=\"projectOpenImage\" title=\"Add one or more browser-readable images or microscopy/WSI file references as new project items\">Add image</button>",
        "<button id=\"projectSaveFile\" title=\"Save this viewer project with project images, annotations, and trajectories\">Save project</button>",
        "<button id=\"projectOpenFile\" title=\"Open a saved wsiTools viewer project JSON file\">Open project</button>",
        "</div>",
        "<input id=\"projectImageFile\" type=\"file\" accept=\"image/*,.png,.jpg,.jpeg,.webp,.gif,.bmp,.tif,.tiff,.btf,.ome.tif,.ome.tiff,.qptiff,.svs,.ndpi,.scn,.mrxs,.bif,.czi,.lif,.vsi,.vms,.vmu,.zvi,.lsm,.oir,.isyntax,.jp2,.dicom,.dcm\" multiple style=\"display:none\">",
        "<input id=\"projectFile\" type=\"file\" accept=\"application/json,.json,.wsiproject,.wsiproject.json\" style=\"display:none\">",
        "<div id=\"projectMenuSummary\" class=\"menuHint\">Add image accepts browser-readable images plus WSI and microscopy formats such as CZI, SVS, NDPI, BTF, OME-TIFF, QPTIFF, MRXS, SCN, BIF and DICOM. Browser-readable images preview immediately; raw WSI/microscopy files are added as references without loading the whole file into memory and should be opened from R/backends for full-resolution tiles.</div>"
      )
    ),
    wsi_viewer_menu(
      "Annotations",
      "Draw, select, import, export, segment, and manage annotations",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"toolDraw\" title=\"Draw a polygon ROI\">Draw ROI</button>",
        "<button id=\"toolBrush\" title=\"Paint a new automatically named annotation; touching the same label merges, and Alt/Command removes from the selected annotation\">Brush</button>",
        "<button id=\"toolEdit\" title=\"Edit selected ROI vertices\">Edit</button>",
        "</div>",
        "<div class=\"menuTitle\">GeoJSON and display</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"roiToggle\" title=\"Toggle ROI overlays\">ROI</button>",
        "<button id=\"labelsToggle\" title=\"Toggle ROI labels\">Labels</button>",
        "<button id=\"prevRoi\" title=\"Previous ROI\">Prev</button>",
        "<button id=\"nextRoi\" title=\"Next ROI\">Next</button>",
        "<button id=\"importGeojson\" title=\"Import QuPath or wsiTools GeoJSON annotations\">Import GeoJSON</button>",
        "<button id=\"saveGeojson\" title=\"Save annotations as GeoJSON\">Save GeoJSON</button>",
        "<button id=\"layersToggle\" title=\"Show GeoJSON geometry list\">Geometry list</button>",
        "</div>",
        "<div class=\"menuTitle\">StarDist segmentation</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"exportSelectedRoi\" title=\"Export selected ROI as GeoJSON for StarDist crop analysis\">Export ROI</button>",
        "<button id=\"startSegmentation\" title=\"Crop the selected ROI, run the configured StarDist service, and import returned cells\">Run segmentation</button>",
        "<button id=\"loadSegmentation\" title=\"Import StarDist GeoJSON polygons as cell overlays\">Load GeoJSON</button>",
        "<button id=\"loadSegmentationCsv\" title=\"Import StarDist CSV/TSV centroid table as cell markers\">Load CSV</button>",
        "<button id=\"clearSegmentation\" title=\"Remove imported StarDist overlays\">Clear cells</button>",
        "</div>",
        "<label class=\"control\" title=\"Treat imported coordinates as crop-local and offset by the selected ROI bounding box\"><input id=\"segLocalCoords\" type=\"checkbox\" checked>crop coords</label>",
        "<label class=\"control\" title=\"Cell marker radius for CSV/TSV centroid imports\">cell radius <input id=\"segCellRadius\" type=\"range\" min=\"2\" max=\"80\" step=\"1\" value=\"8\"><span id=\"segCellRadiusValue\">8 px</span></label>",
        "<input id=\"segmentationFile\" type=\"file\" accept=\".geojson,.json,application/geo+json,application/json\" style=\"display:none\">",
        "<input id=\"segmentationTableFile\" type=\"file\" accept=\".csv,.tsv,.txt,text/csv,text/tab-separated-values,text/plain\" style=\"display:none\">",
        "<div id=\"segmentationSummary\" class=\"menuHint\"></div>",
        "<label class=\"control\" title=\"ROI opacity\">Opacity <input id=\"roiOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>",
        "<input id=\"geojsonImportFile\" type=\"file\" accept=\".geojson,.json,application/geo+json,application/json\" style=\"display:none\">",
        "<div id=\"geojsonImportSummary\" class=\"menuHint\"></div>",
        "<div class=\"menuTitle\">Undo / redo</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"undoAnnotation\" title=\"Undo the last annotation edit, up to 10 steps\">Undo edit</button>",
        "<button id=\"redoAnnotation\" title=\"Redo the last undone annotation edit, up to 10 steps\">Redo edit</button>",
        "</div>",
        "<div class=\"menuTitle\">Brush refinement</div>",
        "<label class=\"control\" title=\"Simplification tolerance in slide pixels\">Simplify <input id=\"simplifyTolerance\" type=\"range\" min=\"1\" max=\"100\" step=\"1\" value=\"12\"><span id=\"simplifyToleranceValue\">12 px</span></label>",
        "<div class=\"menuGrid\">",
        "<button id=\"fillRoiHoles\" title=\"Fill holes and remove brush-subtraction areas from the selected annotation\">Fill holes</button>",
        "</div>",
        "<label class=\"control\" title=\"Optional label for new polygon ROIs; brush annotations are named automatically\">Name <input id=\"roiLabelInput\" type=\"text\" maxlength=\"120\" placeholder=\"annotation label\"></label>",
        "<label class=\"control\" title=\"Pathology class for the next drawn or painted annotation\">Next class <select id=\"roiClassSelect\">",
        class_options,
        "</select></label>",
        "<label class=\"control\" title=\"Optional custom category for the next drawn or painted annotation\">Custom class <input id=\"roiClassCustom\" type=\"text\" maxlength=\"80\" placeholder=\"custom category\"></label>",
        "<button id=\"applyRoiClass\" title=\"Use this name and class for the next drawn or painted annotation\">Set next class</button>",
        "<button id=\"selectionCardToggle\" title=\"Show or hide the selected ROI summary\">ROI summary</button>",
        "<button id=\"deleteRoi\" title=\"Delete the selected ROI\">Delete selected</button>"
      )
    ),
    wsi_viewer_menu(
      "Cells",
      "CellPhenotyper and StarDist cell overlays",
      paste0(
        "<div class=\"menuTitle\">CellPhenotyper StarDist</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"cellToggle\" title=\"Show or hide the StarDist cell segmentation overlay\">StarDist cells</button>",
        "<button id=\"cellZoom\" title=\"Zoom to the StarDist cell extent\">Zoom cells</button>",
        "<button id=\"cellPanelToggle\" title=\"Open the layer list in the left panel\">Layer panel</button>",
        "</div>",
        "<label class=\"control\" title=\"Cell overlay opacity\">Opacity <input id=\"cellOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"0.75\"></label>",
        "<label class=\"control\" title=\"Cell marker radius in slide pixels\">Cell size <input id=\"cellRadius\" type=\"range\" min=\"1\" max=\"40\" step=\"1\" value=\"6\"><span id=\"cellRadiusValue\">6 px</span></label>",
        "<div id=\"cellSummary\" class=\"menuHint\">No CellPhenotyper cells loaded.</div>"
      )
    ),
    wsi_viewer_seurat_controls(config),
    wsi_viewer_kodama_controls(config),
    wsi_viewer_artifact_controls(config),
    wsi_viewer_menu(
      "Jobs",
      "Progress for StarDist, conversion, tile extraction, and pyramid jobs",
      paste0(
        "<div class=\"menuTitle\">Long Jobs</div>",
        "<div id=\"jobSummary\">No long-running jobs yet.</div>",
        "<div id=\"jobList\" aria-live=\"polite\"></div>",
        "<div class=\"menuHint\">Live R jobs update while the viewer sync service is running. Progress is shown when the backend reports it; logs are always kept when available.</div>"
      ),
      class = "jobMenu"
    ),
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
        "<div class=\"menuGrid\">",
        "<button id=\"toolTrajectory\" title=\"Click control points to sketch a trajectory\">Draw</button>",
        "<button id=\"finishTrajectory\" title=\"Finish and resample the current trajectory\">Finish</button>",
        "<button id=\"undoTrajectoryPoint\" title=\"Remove the last trajectory control point\">Undo point</button>",
        "<button id=\"trajectoryAreaRoi\" title=\"Create an annotation area using the selected trajectory as the backbone\">Create area</button>",
        "<button id=\"clearTrajectories\" title=\"Clear all trajectories\">Clear</button>",
        "</div>",
        "<label class=\"control\" title=\"Number of evenly spaced points saved along each trajectory\">points <input id=\"trajectoryResolution\" type=\"range\" min=\"5\" max=\"200\" step=\"1\" value=\"20\"><span id=\"trajectoryResolutionValue\">20</span></label>",
        "<label class=\"control\" title=\"Full width of the annotation corridor created around the trajectory backbone, in slide pixels\">area width <input id=\"trajectoryAreaWidth\" type=\"range\" min=\"16\" max=\"5000\" step=\"16\" value=\"512\"><span id=\"trajectoryAreaWidthValue\">512 px</span></label>",
        "<label class=\"control\" title=\"Preview the annotation corridor around the draft or selected trajectory\"><input id=\"trajectoryAreaPreview\" type=\"checkbox\" checked>preview area</label>",
        "<div class=\"menuHint\">Click points on the slide to sketch a trajectory. Double-click, Enter, or Finish saves a smoothed backbone and returns to pan mode; Create area converts the draft or selected trajectory into an annotation corridor with the chosen width.</div>",
        "<div id=\"trajectorySummary\" class=\"sideMeta\"></div>",
        "<div id=\"trajectoryList\"></div>"
      )
    ),
    wsi_viewer_menu(
      "Image",
      "Non-destructive image display transforms",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"rotateImageLeft\" title=\"Rotate the displayed image 90 degrees counter-clockwise\">Rotate left</button>",
        "<button id=\"rotateImageRight\" title=\"Rotate the displayed image 90 degrees clockwise\">Rotate right</button>",
        "<button id=\"flipImageHorizontal\" title=\"Flip the displayed image horizontally\">Flip H</button>",
        "<button id=\"flipImageVertical\" title=\"Flip the displayed image vertically\">Flip V</button>",
        "<button id=\"resetImageTransform\" title=\"Reset image rotation and flips\">Reset</button>",
        "</div>",
        "<div class=\"menuHint\">These controls change only the viewer orientation; the source slide is not modified.</div>",
        "<div id=\"imageTransformSummary\" class=\"menuHint\"></div>"
      )
    ),
    wsi_viewer_menu(
      "View",
      "Display aids and coordinates",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"crosshairToggle\" title=\"Toggle crosshair\">Crosshair</button>",
        "<button id=\"projectPanelToggle\" title=\"Show or hide the left Project panel\">Project panel</button>",
        "<button id=\"historyPanelToggle\" title=\"Show or hide the left History panel\">History panel</button>",
        "<button id=\"resetPreferences\" title=\"Reset persistent viewer preferences saved in this browser\">Reset prefs</button>",
        "</div>",
        "<div class=\"menuTitle\">Magnification</div>",
        "<div class=\"menuGrid magnificationGrid\">",
        "<button id=\"magnification5\" class=\"magnificationPreset\" data-magnification=\"5\" title=\"Zoom to approximately 5x scan-equivalent magnification\">5x</button>",
        "<button id=\"magnification10\" class=\"magnificationPreset\" data-magnification=\"10\" title=\"Zoom to approximately 10x scan-equivalent magnification\">10x</button>",
        "<button id=\"magnification20\" class=\"magnificationPreset\" data-magnification=\"20\" title=\"Zoom to approximately 20x scan-equivalent magnification\">20x</button>",
        "<button id=\"magnification40\" class=\"magnificationPreset\" data-magnification=\"40\" title=\"Zoom to approximately 40x scan-equivalent magnification\">40x</button>",
        "</div>",
        "<div id=\"magnificationSummary\" class=\"menuHint\">Magnification unavailable</div>",
        "<div class=\"menuTitle\">Multi-view tissue display</div>",
        "<div class=\"menuGrid multiViewControls\">",
        "<button id=\"multiView1\" class=\"multiViewLayout\" data-layout=\"1\" title=\"Return to one main tissue view\">1 view</button>",
        "<button id=\"multiView2\" class=\"multiViewLayout\" data-layout=\"2\" title=\"Split the workspace into two tissue views\">2 views</button>",
        "<button id=\"multiView4\" class=\"multiViewLayout\" data-layout=\"4\" title=\"Split the workspace into four tissue views\">4 views</button>",
        "<button id=\"multiView6\" class=\"multiViewLayout\" data-layout=\"6\" title=\"Split the workspace into six tissue views\">6 views</button>",
        "</div>",
        "<label class=\"control\" title=\"Synchronize zoom and pan across all multi-view panes\"><input id=\"multiViewSync\" type=\"checkbox\">link zoom/pan</label>",
        "<div id=\"multiViewSummary\" class=\"menuHint\">Single view. Use 2, 4, or 6 views to compare tissue regions side by side.</div>",
        "<div id=\"syncSummary\" class=\"menuHint\"></div>"
      )
    ),
    wsi_viewer_stain_controls(config),
    wsi_viewer_menu(
      "Help",
      "Viewer guide and shortcuts",
      paste0(
        "<button id=\"shortcutHelpButton\" title=\"Open viewer guide and keyboard shortcuts\">Viewer guide</button>",
        "<div class=\"menuTitle\">Quick help</div>",
        "<div class=\"menuHint\">Open images from Project, navigate with the right-side controls, draw ROIs from Annotations, and save GeoJSON or project state from the relevant menus.</div>",
        "<div class=\"menuHint\">Press ? anytime to open this guide.</div>"
      )
    ),
    "</div>\n",
    "</div>\n",
    "<div id=\"navDock\" class=\"panel navDock\" aria-label=\"Navigation controls\">",
    "<button id=\"zoomIn\" title=\"Zoom in\" aria-label=\"Zoom in\">+</button>",
    "<button id=\"zoomOut\" title=\"Zoom out\" aria-label=\"Zoom out\">-</button>",
    "<button id=\"fit\" title=\"Fit slide to window\" aria-label=\"Fit slide to window\">Fit</button>",
    "<button id=\"oneToOne\" title=\"Show image pixels at 1:1\" aria-label=\"Show image pixels at 1:1\">1:1</button>",
    "</div>\n",
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
    "<div class=\"annotationEditor\" aria-label=\"Selected annotation editor\">",
    "<label class=\"control\" title=\"Rename the selected annotation\">Name <input id=\"annotationNameInput\" type=\"text\" maxlength=\"120\" placeholder=\"annotation name\"></label>",
    "<label class=\"control\" title=\"Pathology class for the selected annotation\">Selected class <select id=\"annotationClassSelect\">",
    class_options,
    "</select></label>",
    "<label class=\"control\" title=\"Optional custom annotation category\">Custom <input id=\"annotationClassCustom\" type=\"text\" maxlength=\"80\" placeholder=\"custom category\"></label>",
    "<label class=\"control\" title=\"Annotation display color\">Color <input id=\"annotationColorInput\" type=\"color\" value=\"#00BFC4\"></label>",
    "<div class=\"annotationBrushControls\" aria-label=\"Brush controls\">",
    "<label class=\"control\" title=\"Brush screen radius; effective slide-pixel size adapts automatically with zoom\">Brush size <input id=\"brushSize\" type=\"range\" min=\"8\" max=\"240\" step=\"2\" value=\"32\"><span id=\"brushSizeValue\">32 px</span></label>",
    "<div id=\"brushZoomHint\" class=\"sideMeta\">effective 32 slide px</div>",
    "</div>",
    "<div class=\"annotationActions\">",
    "<button id=\"annotationApply\" title=\"Apply name, class, and color to the selected annotation\">Apply</button>",
    "<button id=\"annotationVisible\" title=\"Show or hide the selected annotation\">Hide</button>",
    "<button id=\"annotationLock\" title=\"Lock or unlock the selected annotation\">Lock</button>",
    "<button id=\"annotationZoom\" title=\"Zoom to the selected annotation\">Zoom</button>",
    "<button id=\"annotationDuplicate\" title=\"Duplicate the selected annotation\">Duplicate</button>",
    "<button id=\"annotationDelete\" title=\"Delete the selected annotation\">Delete</button>",
    "</div>",
    "<button id=\"annotationExportSelected\" title=\"Export checked annotations, or the selected annotation when none are checked\">Export selected ROIs</button>",
    "</div>",
    "<div class=\"sideTitle\">Layers</div><div class=\"sideMeta\">R-controlled overlays</div><div id=\"layerSummary\" class=\"sideMeta\"></div><div id=\"layerList\"></div>",
    "<div class=\"sideTitle\">ROIs</div>",
    "<div class=\"annotationSearch\" aria-label=\"ROI search and filter controls\">",
    "<input id=\"annotationSearchInput\" type=\"text\" maxlength=\"120\" placeholder=\"search name/class\" title=\"Search annotations by name, class, ID, or source\">",
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
    "<div class=\"annotationListTools\"><button id=\"annotationSelectAll\" title=\"Check shown annotations for export\">All</button><button id=\"annotationSelectNone\" title=\"Deselect annotations and clear export checks for shown annotations\">Deselect</button></div>",
    "<div id=\"roiList\"></div>",
    "</div><div class=\"sidePanelResizeHandle\" data-panel=\"roiPanel\" role=\"separator\" aria-orientation=\"horizontal\" title=\"Drag to resize Annotations vertically\"></div></div>",
    "<div id=\"annotationHistory\" class=\"panel historyPanel\" aria-label=\"Viewer history\" tabindex=\"-1\">",
    "<div id=\"historyPanelHeader\" class=\"projectPanelHeader\" role=\"button\" tabindex=\"0\" aria-expanded=\"true\" title=\"Double-click to minimize or restore the history panel\">",
    "<div><div class=\"sideTitle\">History</div><div class=\"sideMeta\">Viewer actions</div></div>",
    "<div class=\"panelHeaderActions\"><span class=\"historyActions\"><button id=\"maximizeAnnotationHistory\" title=\"Maximize history in the viewer window\" aria-expanded=\"false\">Maximize</button><button id=\"clearAnnotationHistory\" title=\"Clear the visible history\">Clear</button></span>",
    "<button id=\"historyPanelClose\" class=\"panelCloseButton\" type=\"button\" title=\"Close history panel\" aria-label=\"Close history panel\">x</button></div>",
    "</div>",
    "<div id=\"annotationHistoryBody\">",
    "<div id=\"annotationHistorySummary\" class=\"sideMeta\">No viewer actions yet.</div>",
    "<div id=\"annotationHistoryList\"></div>",
    "</div>",
    "<div class=\"sidePanelResizeHandle\" data-panel=\"annotationHistory\" role=\"separator\" aria-orientation=\"horizontal\" title=\"Drag to resize History vertically\"></div>",
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
    "<button id=\"selectionEdit\" title=\"Switch to vertex-edit mode for the selected ROI\">Edit</button>",
    "<button id=\"selectionDelete\" title=\"Delete the selected ROI\">Delete</button>",
    "<button id=\"selectionClose\" title=\"Hide the selected ROI summary\">Close</button>",
    "</div></div>\n",
    "<div id=\"miniNavigator\" class=\"panel\" aria-label=\"Mini navigator\">",
    "<canvas id=\"miniNavigatorCanvas\"></canvas>",
    "<div class=\"miniNavigatorMeta\"><span id=\"miniNavigatorViewport\">viewport</span><span id=\"miniNavigatorDensity\">density</span></div>",
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
    "<div class=\"seuratPlotHead\"><div><div id=\"seuratPlotTitle\">Seurat PCA plot</div>",
    "<div id=\"seuratPlotSubtitle\"></div></div><button id=\"seuratPlotClose\" type=\"button\" title=\"Close Seurat PCA plot window\">X</button></div>",
    "<div class=\"seuratPlotTools\">",
    "<button id=\"seuratPlotReset\" type=\"button\" title=\"Redraw the PCA plot\">Redraw</button>",
    "<button id=\"seuratPlotClearSelection\" type=\"button\" title=\"Clear selected spots\">Clear selection</button>",
    "</div>",
    "<div id=\"seuratPlotSelectionStatus\" class=\"menuHint\">Draw a lasso around PCA points to highlight matching spots on the slide.</div>",
    "<div id=\"seuratPlotViewport\"><canvas id=\"seuratPlotCanvas\"></canvas></div>",
    "<div id=\"seuratPlotLegend\"></div>",
    "</div>\n",
    "<div id=\"shortcutHelpBackdrop\" aria-hidden=\"true\"></div>\n",
    "<div id=\"shortcutHelp\" class=\"panel\" role=\"dialog\" aria-modal=\"true\" aria-labelledby=\"shortcutHelpTitle\" aria-hidden=\"true\">",
    "<div class=\"shortcutHelpHead\"><div><div id=\"shortcutHelpTitle\" class=\"shortcutHelpTitle\">Viewer Guide</div>",
    "<div class=\"shortcutHelpHint\">Common viewer operations, troubleshooting, and shortcuts. Press ? to open or close this guide.</div></div><span class=\"commandKbd\">?</span></div>",
    "<div class=\"viewerGuideGrid\">",
    "<section class=\"viewerGuideSection\"><h3>Open images</h3><ul class=\"viewerGuideList\">",
    "<li>Use Project > Add image to add ordinary browser-readable images to the current viewer.</li>",
    "<li>Open WSI, CZI, SVS, OME-TIFF, and tiled sources from R when possible, so wsiTools can prepare tiles without loading the full image into memory.</li>",
    "<li>The Project panel lists the current images and tissue sections. Select a row to switch the visible sample.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Zoom and pan</h3><ul class=\"viewerGuideList\">",
    "<li>Use the four-arrow Pan control, mouse drag, trackpad, or scroll wheel to move through the slide.</li>",
    "<li>The right-side controls provide zoom in, zoom out, fit, and 1:1 view.</li>",
    "<li>Use View for approximate 5x, 10x, 20x, or 40x magnification when pixel size metadata is available.</li>",
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
    "<li>Only one annotation or trajectory is selected at a time.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Stains, channels, and overlays</h3><ul class=\"viewerGuideList\">",
    "<li>The Stains menu shows only channels available for the current image.</li>",
    "<li>H&E deconvolution, IHC channels, and mIHC overlays are display layers and may be slower than the base tiled image.</li>",
    "<li>mIHC overlays are tied to their matching H&E image and are hidden when another tissue section is selected.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Projects and saved outputs</h3><ul class=\"viewerGuideList\">",
    "<li>Use Project to save or reopen viewer state, including images, annotations, trajectories, measurements, layers, and viewport position when supported.</li>",
    "<li>Browser downloads such as GeoJSON usually go to your browser Downloads folder unless the browser asks for a location.</li>",
    "<li>Live viewers can sync selected viewer objects back to R; static HTML viewers do not automatically update R objects.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection\"><h3>Analysis tools</h3><ul class=\"viewerGuideList\">",
    "<li>Artifacts imports GrandQC GeoJSON QC regions as editable annotations when a CellPhenotyper project provides them.</li>",
    "<li>Segmentation tools such as StarDist are optional. If no command is configured, import existing segmentation GeoJSON or CSV instead.</li>",
    "<li>Tile grids can be previewed before extraction to confirm which regions will be exported.</li>",
    "</ul></section>",
    "<section class=\"viewerGuideSection viewerGuideWide\"><h3>Troubleshooting</h3><div class=\"viewerGuideTroubleshooting\">",
    "<div class=\"viewerGuideIssue\"><strong>Images or tiles do not load</strong>Check that the file path exists, the backend is available with wsi_backends(), and the viewer is opened through http://127.0.0.1 or localhost when canvas pixel access is needed.</div>",
    "<div class=\"viewerGuideIssue\"><strong>Black or missing tiles</strong>Try refreshing the viewer, use prebuilt Deep Zoom tiles when available, and confirm OpenSlide/libvips can read the image. Dynamic tiles are a fallback and may be slower.</div>",
    "<div class=\"viewerGuideIssue\"><strong>Slow rendering</strong>Prefer static prebuilt tiles for SVS/TIFF/OME-TIFF, reduce visible overlays, and avoid enabling several deconvolved or mIHC channels at once on very large slides.</div>",
    "<div class=\"viewerGuideIssue\"><strong>Saved outputs are hard to find</strong>Browser-triggered exports usually appear in Downloads. R-side exports are written to the output path passed to the R function or project save call.</div>",
    "</div></section>",
    "</div>",
    "<div class=\"menuTitle\">Keyboard shortcuts</div>",
    "<dl class=\"shortcutList\">",
    "<dt><span class=\"commandKbd\">Space</span> / <span class=\"commandKbd\">P</span></dt><dd>Pan mode</dd>",
    "<dt><span class=\"commandKbd\">D</span></dt><dd>Draw polygon ROI</dd>",
    "<dt><span class=\"commandKbd\">B</span></dt><dd>Brush annotation editing; hold Alt on Windows/Linux or Command on Mac to subtract</dd>",
    "<dt><span class=\"commandKbd\">N</span></dt><dd>Deselect the current annotation and start a new ROI</dd>",
    "<dt><span class=\"commandKbd\">E</span></dt><dd>Edit selected ROI vertices</dd>",
    "<dt><span class=\"commandKbd\">M</span></dt><dd>Measure distance between two points</dd>",
    "<dt><span class=\"commandKbd\">T</span></dt><dd>Draw trajectory control points; Enter or double-click finishes and returns to pan</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+Z</span></dt><dd>Undo annotation or trajectory edit</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+Shift+Z</span> / <span class=\"commandKbd\">Ctrl+Y</span></dt><dd>Redo annotation or trajectory edit</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+S</span></dt><dd>Save annotations as GeoJSON</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+I</span></dt><dd>Import GeoJSON annotations</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+E</span></dt><dd>Export selected ROIs</dd>",
    "<dt><span class=\"commandKbd\">Ctrl+K</span></dt><dd>Open command palette</dd>",
    "<dt><span class=\"commandKbd\">Esc</span></dt><dd>Close help, command palette, or return to pan mode</dd>",
    "</dl>",
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
    "let annotationsDirty=false,annotationDirtyReason='';\n",
    "let annotationHistory=[],annotationHistorySeq=0;\n",
    "function annotationHistoryDetailText(detail={}){if(!detail||typeof detail!=='object')return '';const parts=[];['name','old_name','class','operation','file','id','source_id','count','added','type','job_id'].forEach(k=>{if(detail[k]!==null&&typeof detail[k]!=='undefined'&&String(detail[k]).length)parts.push(k.replace('_',' ')+': '+String(detail[k]));});if(Array.isArray(detail.selected_indices)&&detail.selected_indices.length)parts.push('selected: '+detail.selected_indices.length);return parts.join(' | ');}\n",
    "function annotationHistoryActionLabel(action,detail={}){const a=String(action||'annotation_changed');if(a==='roi_added')return 'Created ROI';if(a==='geojson_imported')return 'Imported GeoJSON';if(a==='roi_renamed')return 'Renamed '+(detail.name||detail.id||'ROI');if(a==='roi_metadata_updated'||a==='roi_updated')return 'Updated '+(detail.name||detail.id||'ROI');if(a==='roi_brush_extend')return 'Brush add';if(a==='roi_brush_subtract')return 'Brush subtract';if(a==='roi_deleted')return 'Deleted '+(detail.name||detail.id||'ROI');if(a==='roi_duplicated')return 'Duplicated ROI';if(a==='roi_color_updated')return 'Changed ROI color';if(a==='roi_visibility_updated')return detail.visible?'Showed ROI':'Hid ROI';if(a==='roi_lock_updated')return detail.locked?'Locked ROI':'Unlocked ROI';if(a==='roi_smoothed')return 'Smoothed ROI';if(a==='roi_simplified')return 'Simplified ROI';if(a==='roi_holes_filled')return 'Filled ROI holes';if(a==='rois_merged')return 'Merged ROIs';if(a==='roi_split')return 'Split ROI';if(a==='annotation_undo')return 'Undo annotation edit';if(a==='annotation_redo')return 'Redo annotation edit';if(a==='stardist_ran')return 'Ran StarDist';if(a==='segmentation_imported')return 'Imported segmentation';if(a==='measurement_added')return 'Measured distance';if(a==='trajectory_added')return 'Created trajectory';if(a==='trajectory_area_created')return 'Created trajectory area';if(a==='trajectories_cleared')return 'Cleared trajectories';if(a==='artifact_detected')return 'Detected artifacts';if(a==='artifact_flagged')return 'Flagged artifact';if(a==='artifacts_cleared')return 'Cleared artifacts';if(a==='grandqc_loaded')return 'Loaded GrandQC';if(a==='grandqc_cleared')return 'Cleared GrandQC';if(a==='annotation_history_cleared')return 'Cleared history';return a.replace(/_/g,' ').replace(/^./,c=>c.toUpperCase());}\n",
    "function renderAnnotationHistory(){const summary=el('annotationHistorySummary'),list=el('annotationHistoryList');if(!summary||!list)return;list.innerHTML='';if(!annotationHistory.length){summary.textContent='No viewer actions yet.';return;}summary.textContent=annotationHistory.length+' viewer action'+(annotationHistory.length===1?'':'s')+' in this session.';annotationHistory.slice(0,30).forEach(entry=>{const item=document.createElement('div');item.className='historyItem';const action=document.createElement('div');action.className='historyAction';action.textContent=entry.label||annotationHistoryActionLabel(entry.action,entry.detail);const meta=document.createElement('div');meta.className='historyMeta';const when=entry.time?new Date(entry.time):null,time=when&&!Number.isNaN(when.getTime())?when.toLocaleTimeString():'';const details=annotationHistoryDetailText(entry.detail||{});meta.textContent=[time,details].filter(Boolean).join(' | ');item.append(action,meta);list.appendChild(item);});}\n",
    "function annotationHistoryPayload(){return annotationHistory.slice().reverse().map(entry=>({id:entry.id,time:entry.time,action:entry.action,label:entry.label,detail:entry.detail||{}}));}\n",
    "function recordAnnotationHistory(action,detail={},sync=false){const entry={id:'history_'+(++annotationHistorySeq),time:new Date().toISOString(),action:String(action||'annotation_changed'),label:annotationHistoryActionLabel(action,detail||{}),detail:detail||{}};annotationHistory.unshift(entry);if(annotationHistory.length>120)annotationHistory.length=120;renderAnnotationHistory();if(sync&&typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('annotation_history_updated',{action:entry.action,label:entry.label});return entry;}\n",
    "function clearAnnotationHistory(){annotationHistory=[];renderAnnotationHistory();if(typeof scheduleViewerStateSync==='function')scheduleViewerStateSync('annotation_history_cleared',{});notify('History cleared','success');}\n",
    "function updateHistoryPanelToggle(){const button=el('historyPanelToggle'),panel=el('annotationHistory');if(button&&panel)button.classList.toggle('active',!panel.classList.contains('closed'));}\n",
    "function setHistoryPanelMinimized(minimized){const panel=el('annotationHistory'),header=el('historyPanelHeader');if(!panel)return;panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleHistoryPanelMinimized(){const panel=el('annotationHistory');if(!panel)return;setHistoryPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function setHistoryPanelClosed(closed){const panel=el('annotationHistory');if(!panel)return;if(closed&&maximizedAnnotationSection==='annotationHistory'&&typeof closeMaximizedAnnotationSection==='function')closeMaximizedAnnotationSection();panel.classList.toggle('closed',!!closed);updateHistoryPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleHistoryPanelClosed(){const panel=el('annotationHistory');if(!panel)return;setHistoryPanelClosed(!panel.classList.contains('closed'));}\n",
    "function bindAnnotationHistoryControls(){const clear=el('clearAnnotationHistory'),header=el('historyPanelHeader'),close=el('historyPanelClose'),toggle=el('historyPanelToggle');if(clear)clear.onclick=clearAnnotationHistory;if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.ondblclick=e=>{e.preventDefault();toggleHistoryPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleHistoryPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';close.onclick=e=>{e.preventDefault();e.stopPropagation();setHistoryPanelClosed(true);notify('History panel closed','info',1600);};}if(toggle&&toggle.dataset.bound!=='1'){toggle.dataset.bound='1';toggle.onclick=()=>toggleHistoryPanelClosed();}updateHistoryPanelToggle();renderAnnotationHistory();}\n",
    "let maximizedAnnotationSection=null;\n",
    "function annotationSectionButton(sectionId){return sectionId==='annotationHistory'?el('maximizeAnnotationHistory'):null;}\n",
    "function annotationSectionTitle(sectionId){return sectionId==='annotationHistory'?'History':'';}\n",
    "function setAnnotationSectionMaximized(sectionId,maximize=true){const ids=['annotationHistory'];if(!ids.includes(sectionId))return;const active=maximize?sectionId:null;if(active==='annotationHistory'){if(typeof setHistoryPanelClosed==='function')setHistoryPanelClosed(false);if(typeof setHistoryPanelMinimized==='function')setHistoryPanelMinimized(false);}ids.forEach(id=>{const section=el(id),button=annotationSectionButton(id),on=active===id;if(section)section.classList.toggle('maximized',on);if(button){button.textContent=on?'Restore':'Maximize';button.setAttribute('aria-expanded',on?'true':'false');}});const backdrop=el('annotationSectionBackdrop');if(backdrop)backdrop.classList.toggle('open',!!active);maximizedAnnotationSection=active;if(active){const section=el(active);if(section){section.scrollTop=0;setTimeout(()=>section.focus&&section.focus(),0);}notify(annotationSectionTitle(active)+' maximized','info',1800);}}\n",
    "function closeMaximizedAnnotationSection(){if(maximizedAnnotationSection)setAnnotationSectionMaximized(maximizedAnnotationSection,false);}\n",
    "function toggleAnnotationSectionMaximized(sectionId){setAnnotationSectionMaximized(sectionId,maximizedAnnotationSection!==sectionId);}\n",
    "function bindAnnotationSectionMaximizeControls(){const history=el('maximizeAnnotationHistory'),backdrop=el('annotationSectionBackdrop');if(history)history.onclick=()=>toggleAnnotationSectionMaximized('annotationHistory');if(backdrop)backdrop.onclick=closeMaximizedAnnotationSection;window.addEventListener('keydown',e=>{if(e.key==='Escape'&&maximizedAnnotationSection){e.preventDefault();e.stopPropagation();closeMaximizedAnnotationSection();}},true);}\n",
    "function dismissToast(toast){if(!toast)return;toast.classList.remove('visible');toast.classList.add('leaving');setTimeout(()=>toast.remove(),220);}\n",
    "function showToast(message,type='info',timeout=2600,action=null){const stack=el('toastStack');if(!stack||!message)return null;const safeType=String(type||'info').replace(/[^A-Za-z0-9_-]+/g,'').toLowerCase()||'info';const toast=document.createElement('div');toast.className='toast toast-'+safeType;toast.setAttribute('role',safeType==='error'?'alert':'status');const text=document.createElement('span');text.className='toastMessage';text.textContent=String(message);toast.appendChild(text);if(action&&action.label&&typeof action.run==='function'){toast.classList.add('toast-actionable');const button=document.createElement('button');button.type='button';button.className='toastAction';button.textContent=String(action.label);button.onclick=e=>{e.stopPropagation();dismissToast(toast);action.run();};toast.appendChild(button);}toast.onclick=e=>{if(e.target===toast||e.target===text)dismissToast(toast);};stack.appendChild(toast);requestAnimationFrame(()=>toast.classList.add('visible'));const ttl=Number(timeout);if(!Number.isFinite(ttl)||ttl>0)setTimeout(()=>dismissToast(toast),Number.isFinite(ttl)?ttl:2600);return toast;}\n",
    "function notify(message,type='info',timeout=2600){return showToast(message,type,timeout);}\n",
    "function notifyAction(message,actionLabel,actionRun,type='info',timeout=6000){return showToast(message,type,timeout,{label:actionLabel,run:actionRun});}\n",
    "function countText(value){return Number(value).toLocaleString();}\n",
    "function updateAnnotationDirtyIndicator(){const node=el('annotationDirtyIndicator');if(!node)return;node.classList.toggle('dirty',annotationsDirty);node.setAttribute('aria-hidden',annotationsDirty?'false':'true');node.title=annotationsDirty?'Annotation changes have not been exported or saved to a project':'Annotations are exported or saved';}\n",
    "function setAnnotationsDirty(value=true,reason='annotation_changed',sync=false){annotationsDirty=!!value;annotationDirtyReason=reason||'';updateAnnotationDirtyIndicator();if(sync&&typeof scheduleViewerStateSync==='function')scheduleViewerStateSync(annotationsDirty?'annotations_dirty':'annotations_saved',{dirty:annotationsDirty,reason:annotationDirtyReason});}\n",
    "function markAnnotationsDirty(reason='annotation_changed'){setAnnotationsDirty(true,reason,false);}\n",
    "function markAnnotationsSaved(reason='annotations_saved'){setAnnotationsDirty(false,reason,true);}\n"
  )
}

wsi_viewer_preferences_js <- function() {
  paste0(
    "const viewerPreferenceKey=cfg.preference_key||'wsiTools.viewer.preferences.v1';\n",
    "let viewerPreferencesReady=false,viewerPreferencesCache=null,roiPanelDragState=null,workspaceResizeState=null,sidePanelResizeState=null,selectionCardVisible=false;\n",
    "function loadViewerPreferences(){if(viewerPreferencesCache)return viewerPreferencesCache;try{const raw=window.localStorage&&localStorage.getItem(viewerPreferenceKey);viewerPreferencesCache=raw?JSON.parse(raw):{};}catch(e){viewerPreferencesCache={};}return viewerPreferencesCache||{};}\n",
    "function saveViewerPreferences(patch={}){if(!viewerPreferencesReady&&!patch.__initial)return;try{const current=Object.assign({},loadViewerPreferences());delete patch.__initial;const next=Object.assign(current,patch,{viewer_mode:cfg.viewer_mode||current.viewer_mode||'',updated_at:new Date().toISOString()});viewerPreferencesCache=next;if(window.localStorage)localStorage.setItem(viewerPreferenceKey,JSON.stringify(next));return next;}catch(e){return loadViewerPreferences();}}\n",
    "function validToolMode(value){const modeName=String(value||'');return ['pan','select','draw','brush','edit','measure','trajectory'].includes(modeName)?modeName:null;}\n",
    "function preferenceNumber(value,min,max){const n=Number(value);return Number.isFinite(n)?clamp(n,min,max):NaN;}\n",
    "function setPreferenceInput(id,value){const input=el(id);if(input&&value!==null&&typeof value!=='undefined'&&!Number.isNaN(value))input.value=String(value);}\n",
    "function leftWorkspacePanel(){return el('workspacePanel')||el('roiPanel');}\n",
    "function applyPanelPreferences(prefs){const panel=leftWorkspacePanel(),roi=el('roiPanel');if(!panel||!roi)return;const panelPrefs=prefs.panel||{};if(Number.isFinite(Number(panelPrefs.width)))setWorkspacePanelWidth(Number(panelPrefs.width),false);if(Number.isFinite(Number(panelPrefs.left))&&Number.isFinite(Number(panelPrefs.top)))setRoiPanelPosition(Number(panelPrefs.left),Number(panelPrefs.top),false);if(typeof panelPrefs.minimized==='boolean')setRoiPanelMinimized(panelPrefs.minimized);if(typeof setProjectPanelClosed==='function')setProjectPanelClosed(false);if(typeof setProjectPanelMinimized==='function')setProjectPanelMinimized(false);if(typeof ensureProjectWorkspaceVisible==='function')ensureProjectWorkspaceVisible();if(Number.isFinite(Number(panelPrefs.project_height)))setSidePanelHeight('projectPanel',Number(panelPrefs.project_height),false);if(Number.isFinite(Number(panelPrefs.roi_height)))setSidePanelHeight('roiPanel',Number(panelPrefs.roi_height),false);if(Number.isFinite(Number(panelPrefs.history_height)))setSidePanelHeight('annotationHistory',Number(panelPrefs.history_height),false);if(typeof panelPrefs.history_minimized==='boolean'&&typeof setHistoryPanelMinimized==='function')setHistoryPanelMinimized(panelPrefs.history_minimized);if(typeof panelPrefs.history_closed==='boolean'&&typeof setHistoryPanelClosed==='function')setHistoryPanelClosed(panelPrefs.history_closed);}\n",
    "function applyStainPreferences(prefs){if(!stainEnabled||!prefs.stain)return;stainOn=typeof prefs.stain.enabled==='boolean'?prefs.stain.enabled:stainOn;const saved=Array.isArray(prefs.stain.channels)?prefs.stain.channels:[];stainChannels.forEach((ch,i)=>{const pref=saved.find(s=>String(s.id||'')===String(ch.id))||saved[i]||{};if(!stainState[i])stainState[i]={visible:true,color:'#666666',strength:1,opacity:1,contrast_min:0,contrast_max:1};if(typeof pref.visible==='boolean')stainState[i].visible=pref.visible;if(pref.color)stainState[i].color=pref.color;const strength=Number(pref.strength??pref.gain),opacity=Number(pref.opacity),cmin=Number(pref.contrast_min),cmax=Number(pref.contrast_max);if(Number.isFinite(strength))stainState[i].strength=strength;if(Number.isFinite(opacity))stainState[i].opacity=opacity;if(Number.isFinite(cmin))stainState[i].contrast_min=cmin;if(Number.isFinite(cmax))stainState[i].contrast_max=cmax;const vis=el('stainVisible_'+ch.id),color=el('stainColor_'+ch.id),gain=el('stainStrength_'+ch.id),op=el('stainOpacity_'+ch.id),lo=el('stainContrastMin_'+ch.id),hi=el('stainContrastMax_'+ch.id);if(vis)vis.checked=!!stainState[i].visible;if(color)color.value=stainState[i].color;if(gain)gain.value=String(stainState[i].strength);if(op)op.value=String(stainState[i].opacity);if(lo)lo.value=String(stainState[i].contrast_min);if(hi)hi.value=String(stainState[i].contrast_max);});if(typeof updateStainControls==='function')updateStainControls();if(typeof syncTiledStainChannels==='function'&&syncTiledStainChannels())return;if(typeof invalidateBaseImage==='function')invalidateBaseImage();}\n",
    "function applyViewerPreferences(){const prefs=loadViewerPreferences();const brush=preferenceNumber(prefs.brush_size,8,240);if(Number.isFinite(brush)){brushScreenRadius=brush;setPreferenceInput('brushSize',brush);}const opacity=preferenceNumber(prefs.roi_opacity,0,1);if(Number.isFinite(opacity)){roiOpacity=opacity;setPreferenceInput('roiOpacity',opacity);}const cls=String(prefs.selected_class||'').trim();if(cls){nextRoiClass=cls;activeRoiClass=cls;if(typeof ensureRoiClassOption==='function')ensureRoiClassOption(cls);setPreferenceInput('roiClassSelect',cls);}if(prefs.custom_class){setPreferenceInput('roiClassCustom',prefs.custom_class);}if(typeof updateBrushControls==='function')updateBrushControls();applyStainPreferences(prefs);applyPanelPreferences(prefs);viewerPreferencesReady=true;saveViewerPreferences({__initial:true,viewer_mode:cfg.viewer_mode||'',last_opened_at:new Date().toISOString()});return validToolMode(prefs.tool_mode)||'pan';}\n",
    "function saveToolPreference(){saveViewerPreferences({tool_mode:mode});}\n",
    "function saveBrushPreference(){saveViewerPreferences({brush_size:brushScreenRadius});}\n",
    "function saveRoiOpacityPreference(){saveViewerPreferences({roi_opacity:roiOpacity});}\n",
    "function currentClassPreference(){const custom=el('roiClassCustom');const customValue=String((custom&&custom.value)||'').trim();return {selected_class:nextRoiClass||activeRoiClass||currentRoiClass(),custom_class:customValue};}\n",
    "function saveRoiClassPreference(){saveViewerPreferences(currentClassPreference());}\n",
    "function currentStainPreferences(){if(!stainEnabled)return null;syncStainStateFromControls();return {enabled:stainOn,channels:stainChannels.map((ch,i)=>({id:ch.id,name:ch.name,visible:!!(stainState[i]&&stainState[i].visible),color:stainState[i]?stainState[i].color:ch.colour,strength:stainState[i]?stainState[i].strength:ch.strength,gain:stainState[i]?stainState[i].strength:ch.strength,opacity:stainState[i]?stainState[i].opacity:(ch.opacity??1),contrast_min:stainState[i]?stainState[i].contrast_min:(ch.contrast_min??0),contrast_max:stainState[i]?stainState[i].contrast_max:(ch.contrast_max??1)}))};}\n",
    "function saveStainPreferences(){const stain=currentStainPreferences();if(stain)saveViewerPreferences({stain:stain});}\n",
    "function sidePanelIds(){return ['projectPanel','roiPanel','annotationHistory'];}\n",
    "function sidePanelHeightForPrefs(id){const panel=el(id);if(!panel||(!panel.classList.contains('resized')&&!panel.style.height))return null;const rect=panel.getBoundingClientRect(),h=Math.round(rect.height);return Number.isFinite(h)&&h>0?h:null;}\n",
    "function roiPanelPosition(){const panel=leftWorkspacePanel(),roi=el('roiPanel'),project=el('projectPanel'),history=el('annotationHistory');if(!panel||!roi)return null;const rect=panel.getBoundingClientRect(),out={left:Math.round(rect.left),top:Math.round(rect.top),width:Math.round(rect.width),open:roi.classList.contains('open'),minimized:roi.classList.contains('minimized'),project_minimized:!!(project&&project.classList.contains('minimized')),project_closed:!!(project&&project.classList.contains('closed')),history_minimized:!!(history&&history.classList.contains('minimized')),history_closed:!!(history&&history.classList.contains('closed'))};const ph=sidePanelHeightForPrefs('projectPanel'),rh=sidePanelHeightForPrefs('roiPanel'),hh=sidePanelHeightForPrefs('annotationHistory');if(ph!==null)out.project_height=ph;if(rh!==null)out.roi_height=rh;if(hh!==null)out.history_height=hh;return out;}\n",
    "function savePanelPreferences(){const panel=roiPanelPosition();if(panel)saveViewerPreferences({panel:panel});}\n",
    "function setRoiPanelPosition(left,top,save=true){const panel=leftWorkspacePanel();if(!panel)return;const rect=panel.getBoundingClientRect(),maxLeft=Math.max(0,innerWidth-Math.min(rect.width||420,innerWidth)),maxTop=Math.max(0,innerHeight-Math.min(rect.height||80,innerHeight));panel.style.left=Math.round(clamp(left,0,maxLeft))+'px';panel.style.top=Math.round(clamp(top,0,maxTop))+'px';panel.style.right='auto';if(save)savePanelPreferences();}\n",
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
    "function bindPreferenceControls(){bindWorkspaceResizeControls();bindSidePanelResizeControls();const reset=el('resetPreferences');if(reset)reset.onclick=()=>{try{if(window.localStorage)localStorage.removeItem(viewerPreferenceKey);}catch(e){}viewerPreferencesCache={};notify('Viewer preferences reset','success');};}\n"
  )
}

wsi_viewer_jobs_js <- function() {
  paste0(
    "const viewerJobs=new Map();\n",
    "function jobStatusLabel(status){const s=String(status||'queued').toLowerCase();if(s==='finished'||s==='complete'||s==='success')return 'completed';return s;}\n",
    "function jobLogLines(job){const log=job&&job.log;if(Array.isArray(log))return log.map(String).filter(Boolean);if(typeof log==='string'&&log.length)return log.split(/\\r?\\n/).filter(Boolean);return [];}\n",
    "function normaliseViewerJob(job){if(!job)return null;const id=String(job.id||job.job_id||'');if(!id)return null;const status=jobStatusLabel(job.status||job.display_status||job.raw_status);const progress=Number(job.progress);return {id:id,name:String(job.name||job.job_name||'wsiTools job'),status:status,progress:Number.isFinite(progress)?Math.max(0,Math.min(100,progress)):NaN,progress_available:!!job.progress_available&&Number.isFinite(progress),message:String(job.message||''),log:jobLogLines(job),updated:String(job.updated||new Date().toISOString()),started:String(job.started||''),finished:String(job.finished||'')};}\n",
    "function renderJobList(){const summary=el('jobSummary'),list=el('jobList');if(!summary||!list)return;const jobs=Array.from(viewerJobs.values()).sort((a,b)=>String(b.updated||'').localeCompare(String(a.updated||'')));list.innerHTML='';if(!jobs.length){summary.textContent='No long-running jobs yet.';return;}const counts=jobs.reduce((acc,j)=>{acc[j.status]=(acc[j.status]||0)+1;return acc;},{});summary.textContent=jobs.length+' job'+(jobs.length===1?'':'s')+' | '+Object.keys(counts).map(k=>k+' '+counts[k]).join(', ');jobs.slice(0,20).forEach(job=>{const item=document.createElement('div');item.className='jobItem';const top=document.createElement('div');top.className='jobTop';const name=document.createElement('div');name.className='jobName';name.textContent=job.name;const status=document.createElement('span');status.className='jobStatus '+job.status.replace(/[^A-Za-z0-9_-]+/g,'');status.textContent=job.status;top.append(name,status);const bar=document.createElement('div');bar.className='jobProgress';const fill=document.createElement('div');fill.className='jobProgressFill';if(Number.isFinite(job.progress)){fill.style.width=job.progress+'%';}else if(job.status==='running'||job.status==='queued'){fill.classList.add('indeterminate');}else{fill.style.width='0%';}bar.appendChild(fill);const meta=document.createElement('div');meta.className='jobMeta';const pct=Number.isFinite(job.progress)?(' | '+Math.round(job.progress)+'%'):'';meta.textContent=(job.message||job.updated||'')+pct;item.append(top,bar,meta);const lines=job.log||[];if(lines.length){const pre=document.createElement('pre');pre.className='jobLog';pre.textContent=lines.slice(-8).join('\\n');item.appendChild(pre);}list.appendChild(item);});}\n",
    "function upsertViewerJob(job){const rec=normaliseViewerJob(job);if(!rec)return;const old=viewerJobs.get(rec.id);viewerJobs.set(rec.id,Object.assign({},old||{},rec));renderJobList();if(old&&old.status!==rec.status){if(rec.status==='completed')notify(rec.name+' completed','success',3200);else if(rec.status==='failed')notify(rec.name+' failed','error',4800);}}\n",
    "function updateViewerJob(id,patch={}){const old=viewerJobs.get(id)||{id:id,name:patch.name||'wsiTools job',status:'queued'};upsertViewerJob(Object.assign({},old,patch,{id:id,updated:new Date().toISOString()}));}\n",
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
    "let stainInv=null,stainBasis=[],heGramInv=null;\n",
    "let stainError='';\n",
    "function stainIsHE(){return /^H&E$/i.test(String((cfg.stain||{}).type||''));}\n",
    "function setStainInputState(i,visible){const ch=stainChannels[i];if(!ch)return;if(!stainState[i])stainState[i]={visible:true,color:ch.colour||'#666666',strength:Number(ch.strength||ch.gain||1),opacity:Number(ch.opacity??1),contrast_min:Number(ch.contrast_min??0),contrast_max:Number(ch.contrast_max??1)};stainState[i].visible=!!visible;const input=el('stainVisible_'+ch.id);if(input)input.checked=!!visible;}\n",
    "function setExclusiveTiledHEChannel(index){stainChannels.forEach((ch,i)=>setStainInputState(i,i===index));}\n",
    "function rgbHex(hex){const h=String(hex||'#000000').replace('#','');const s=h.length===3?h.split('').map(c=>c+c).join(''):h;const n=parseInt(s,16);return {r:(n>>16)&255,g:(n>>8)&255,b:n&255};}\n",
    "function norm3(v){const n=Math.hypot(Number(v[0]),Number(v[1]),Number(v[2]));return n>0?[Number(v[0])/n,Number(v[1])/n,Number(v[2])/n]:[0,0,0];}\n",
    "function cross3(a,b){return [a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]];}\n",
    "function inv3(m){const a=m[0][0],b=m[0][1],c=m[0][2],d=m[1][0],e=m[1][1],f=m[1][2],g=m[2][0],h=m[2][1],i=m[2][2];const A=e*i-f*h,B=-(d*i-f*g),C=d*h-e*g,D=-(b*i-c*h),E=a*i-c*g,F=-(a*h-b*g),G=b*f-c*e,H=-(a*f-c*d),I=a*e-b*d;const det=a*A+b*B+c*C;if(Math.abs(det)<1e-8)return null;return [[A/det,D/det,G/det],[B/det,E/det,H/det],[C/det,F/det,I/det]];}\n",
    "function initStain(){if(!stainEnabled)return;const b=(cfg.stain.basis||[]).map(norm3);stainBasis=b;heGramInv=null;if(b.length!==3){notify('IHC stain basis incomplete','error',4200);return;}stainInv=inv3([[b[0][0],b[1][0],b[2][0]],[b[0][1],b[1][1],b[2][1]],[b[0][2],b[1][2],b[2][2]]]);if(/^H&E$/i.test(String(cfg.stain.type||''))&&b.length>=2){const h=b[0],e=b[1],hh=h[0]*h[0]+h[1]*h[1]+h[2]*h[2],he=h[0]*e[0]+h[1]*e[1]+h[2]*e[2],ee=e[0]*e[0]+e[1]*e[1]+e[2]*e[2],det=hh*ee-he*he;if(Math.abs(det)>1e-8)heGramInv=[[ee/det,-he/det],[-he/det,hh/det]];}if(!stainInv)notify('IHC stain vectors invalid','error',4200);}\n",
    "function syncStainStateFromControls(){if(!stainEnabled)return;stainChannels.forEach((ch,i)=>{if(!stainState[i])stainState[i]={visible:true,color:'#666666',strength:1,opacity:1,contrast_min:0,contrast_max:1};const vis=el('stainVisible_'+ch.id),color=el('stainColor_'+ch.id),strength=el('stainStrength_'+ch.id),opacity=el('stainOpacity_'+ch.id),cmin=el('stainContrastMin_'+ch.id),cmax=el('stainContrastMax_'+ch.id);if(vis)stainState[i].visible=!!vis.checked;if(color)stainState[i].color=color.value;if(strength)stainState[i].strength=Number(strength.value);if(opacity)stainState[i].opacity=Number(opacity.value);if(cmin)stainState[i].contrast_min=Number(cmin.value);if(cmax)stainState[i].contrast_max=Number(cmax.value);});}\n",
    "function activeStainNames(){syncStainStateFromControls();return stainChannels.filter((ch,i)=>stainState[i]&&stainState[i].visible).map(ch=>ch.name||ch.id).join(', ');}\n",
    "function stainChannelSourceFor(ch){if(typeof channelSources==='undefined'||!Array.isArray(channelSources)||!ch)return null;const wanted=String(ch.id||'');return channelSources.find(src=>{const meta=(src&&src.metadata)||{};return (String(meta.kind||'')==='stain_channel'||String(meta.source_type||'')==='stain_deconvolution')&&String(meta.stain_channel_id||src.stain_channel_id||'')===wanted;})||null;}\n",
    "function hasTiledStainChannels(){return stainEnabled&&stainChannels.length&&stainChannels.some(ch=>!!stainChannelSourceFor(ch));}\n",
    "function syncTiledStainChannels(){if(!hasTiledStainChannels())return false;syncStainStateFromControls();let active=[];stainState.forEach((s,i)=>{if(s&&s.visible)active.push(i);});if(stainOn&&stainIsHE()&&active.length>1){setExclusiveTiledHEChannel(active[0]);active=[active[0]];}const activeNameList=active.map(i=>stainChannels[i]&&(stainChannels[i].name||stainChannels[i].id)).filter(Boolean).join(', ');stainChannels.forEach((ch,i)=>{const src=stainChannelSourceFor(ch),state=stainState[i]||{};if(!src)return;const visible=!!(stainOn&&state.visible);if(typeof setChannelSettings==='function')setChannelSettings(src.id,{visible:visible,colour:state.color||ch.colour||src.colour,gain:Number(state.strength??ch.strength??src.gain??1),opacity:Number(state.opacity??ch.opacity??src.opacity??.9),contrast_min:Number(state.contrast_min??ch.contrast_min??src.contrast_min??0),contrast_max:Number(state.contrast_max??ch.contrast_max??src.contrast_max??1)});else src.visible=visible;});if(typeof setBaseImageVisible==='function')setBaseImageVisible(!(stainOn&&active.length));if(stainOn&&stainIsHE()&&active.length===1)setStainMessage('Showing tiled '+activeNameList+' channel. H&E tiled deconvolution displays one channel at a time for smooth zoom.');else setStainMessage(stainOn?(activeNameList?('Showing tiled '+activeNameList+' channel layer'+(activeNameList.includes(',')?'s':'')):('No stain channel visible')):'Showing original RGB image.');return true;}\n",
    "function setStainMessage(msg){const box=el('stainMessage');if(box)box.textContent=msg||'';}\n",
    "function stainStatus(){if(!stainEnabled)return '';if(stainError)return ' | stains unavailable: '+stainError;if(!stainOn)return ' | original RGB';const active=activeStainNames();return ' | '+(cfg.stain.label||'IHC channels')+(active?' '+active:' no channels');}\n",
    "function stainInputImageData(targetCtx,targetCanvas,sourceCanvas=null){if(!sourceCanvas)return targetCtx.getImageData(0,0,targetCanvas.width,targetCanvas.height);let readCanvas=sourceCanvas,readCtx=sourceCanvas.getContext('2d',{willReadFrequently:true});if(readCanvas.width!==targetCanvas.width||readCanvas.height!==targetCanvas.height){const tmp=document.createElement('canvas');tmp.width=targetCanvas.width;tmp.height=targetCanvas.height;const tctx=tmp.getContext('2d',{willReadFrequently:true});tctx.drawImage(readCanvas,0,0,tmp.width,tmp.height);readCanvas=tmp;readCtx=tctx;}return readCtx.getImageData(0,0,targetCanvas.width,targetCanvas.height);}\n",
    "function stainConcentrations(odR,odG,odB){if(heGramInv&&stainBasis.length>=2){const h=stainBasis[0],e=stainBasis[1],dh=h[0]*odR+h[1]*odG+h[2]*odB,de=e[0]*odR+e[1]*odG+e[2]*odB,ch=Math.max(0,heGramInv[0][0]*dh+heGramInv[0][1]*de),ce=Math.max(0,heGramInv[1][0]*dh+heGramInv[1][1]*de),rr=odR-h[0]*ch-e[0]*ce,gg=odG-h[1]*ch-e[1]*ce,bb=odB-h[2]*ch-e[2]*ce;return [ch,ce,Math.max(0,Math.hypot(rr,gg,bb))];}return [Math.max(0,stainInv[0][0]*odR+stainInv[0][1]*odG+stainInv[0][2]*odB),Math.max(0,stainInv[1][0]*odR+stainInv[1][1]*odG+stainInv[1][2]*odB),Math.max(0,stainInv[2][0]*odR+stainInv[2][1]*odG+stainInv[2][2]*odB)];}\n",
    "function activeStainIndices(){syncStainStateFromControls();const idx=[];stainState.forEach((s,i)=>{if(s&&s.visible)idx.push(i);});return idx;}\n",
    "function stainAutoScales(data,active){const out=stainChannels.map(()=>1);if(!active.length)return out;const samples=active.map(()=>[]),pixels=data.length/4,step=Math.max(1,Math.floor(pixels/45000));for(let px=0;px<pixels;px+=step){const p=px*4,r=data[p],g=data[p+1],b=data[p+2];if(data[p+3]===0||(r>246&&g>246&&b>246)||(r<28&&g<28&&b<28))continue;const c=stainConcentrations(-Math.log((r+1)/256),-Math.log((g+1)/256),-Math.log((b+1)/256));active.forEach((idx,j)=>{const v=Number(c[idx]);if(Number.isFinite(v)&&v>0)samples[j].push(v);});}active.forEach((idx,j)=>{const s=samples[j];if(!s.length){out[idx]=1;return;}s.sort((a,b)=>a-b);out[idx]=Math.max(0.08,s[Math.max(0,Math.floor(s.length*0.985)-1)]||s[s.length-1]||1);});return out;}\n",
    "function stainIntensity(c,state,scale){const lo=Number.isFinite(state.contrast_min)?state.contrast_min:0,manual=!!(state&&state.manual_contrast),hi=Math.max(lo+1e-6,manual&&Number.isFinite(state.contrast_max)?state.contrast_max:scale),ci=clamp((c-lo)/(hi-lo),0,1),strength=Number.isFinite(state.strength)?state.strength:1,opacity=Number.isFinite(state.opacity)?state.opacity:1;return clamp((1-Math.exp(-ci*Math.max(1.2,strength*2.4)))*opacity,0,1);}\n",
    "function applyStainToCanvas(targetCtx=ctx,targetCanvas=canvas,sourceCanvas=null){if(!stainEnabled||!stainOn||!stainInv||!stainChannels.length)return false;const active=activeStainIndices();if(!active.length)return false;let img;try{img=stainInputImageData(targetCtx,targetCanvas,sourceCanvas);stainError='';setStainMessage(active.length===1?'Showing '+(stainChannels[active[0]].name||stainChannels[active[0]].id)+' deconvolution channel.':'Showing deconvolved stain composite.');}catch(e){stainError=(location.protocol==='file:')?'open the viewer through localhost/http, not file://':'canvas pixel access blocked';setStainMessage('Stain selection needs readable canvas pixels. Use wsi_viewer_live(..., dynamic_tiles = TRUE), or serve the viewer from http://127.0.0.1:<port>/ instead of opening it as file://.');return false;}const data=img.data,colors=stainState.map(s=>rgbHex(s.color)),scales=stainAutoScales(data,active);for(let p=0;p<data.length;p+=4){const r=data[p],g=data[p+1],b=data[p+2];if(data[p+3]===0||(r<28&&g<28&&b<28))continue;const c=stainConcentrations(-Math.log((r+1)/256),-Math.log((g+1)/256),-Math.log((b+1)/256));let rr=255,gg=255,bb=255;for(const i of active){if(i>=c.length)continue;const state=stainState[i]||{},t=stainIntensity(c[i],state,scales[i]),col=colors[i]||{r:102,g:102,b:102};rr=rr*(1-t)+col.r*t;gg=gg*(1-t)+col.g*t;bb=bb*(1-t)+col.b*t;}data[p]=rr;data[p+1]=gg;data[p+2]=bb;}targetCtx.putImageData(img,0,0);return true;}\n",
    "function stainDisplayChanged(event='stain_updated'){updateStainControls();saveStainPreferences();if(syncTiledStainChannels()){scheduleViewerStateSync(event,{tiled_stain_channels:true});if(typeof requestDraw==='function')requestDraw();else draw();return;}scheduleViewerStateSync(event,{});if(typeof invalidateBaseImage==='function')invalidateBaseImage();else if(typeof requestDraw==='function')requestDraw();else draw();}\n",
    "function setStainVisible(indices){if(hasTiledStainChannels()&&stainIsHE()&&Array.isArray(indices)&&indices.length>1){stainOn=false;stainDisplayChanged('stain_updated');setStainMessage('H&E tiled deconvolution is shown one channel at a time. Use Hematoxylin, Eosin, or Residual; Original shows the combined RGB image.');return;}const keep=new Set(indices);stainOn=true;stainChannels.forEach((ch,i)=>setStainInputState(i,keep.has(i)));stainDisplayChanged('stain_updated');}\n",
    "function showOriginalStain(){stainOn=false;stainDisplayChanged('stain_updated');}\n",
    "function updateStainControls(){if(!stainEnabled)return;const toggle=el('stainToggle');if(toggle)toggle.classList.toggle('active',stainOn);const original=el('stainShowOriginal'),all=el('stainShowAll');if(original)original.classList.toggle('active',!stainOn);if(all)all.classList.toggle('active',stainOn&&stainState.every(s=>s&&s.visible));document.querySelectorAll('.stainOnly').forEach(button=>{const idx=Number(button.dataset.stainIndex);button.classList.toggle('active',stainOn&&stainState[idx]&&stainState[idx].visible&&stainState.filter(s=>s&&s.visible).length===1);});stainChannels.forEach(ch=>['Visible_','Color_','Strength_','Opacity_','ContrastMin_','ContrastMax_'].forEach(prefix=>{const input=el('stain'+prefix+ch.id);if(input)input.disabled=!stainOn;}));}\n",
    "function bindStainControls(){if(!stainEnabled)return;initStain();syncStainStateFromControls();if(hasTiledStainChannels())stainOn=false;const toggle=el('stainToggle');if(toggle)toggle.onclick=()=>{stainOn=!stainOn;stainDisplayChanged('stain_updated');};const original=el('stainShowOriginal'),all=el('stainShowAll');if(original)original.onclick=showOriginalStain;if(all)all.onclick=()=>setStainVisible(stainChannels.map((_,i)=>i));document.querySelectorAll('.stainOnly').forEach(button=>{button.onclick=()=>setStainVisible([Number(button.dataset.stainIndex)]);});const redraw=()=>{stainOn=true;syncStainStateFromControls();stainDisplayChanged('stain_updated');};stainChannels.forEach(ch=>{['Visible_','Color_','Strength_','Opacity_','ContrastMin_','ContrastMax_'].forEach(prefix=>{const input=el('stain'+prefix+ch.id);if(input){input.addEventListener('input',redraw);input.addEventListener('change',redraw);}});});updateStainControls();syncTiledStainChannels();}\n"
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
    "function setChannelItemSettings(src){const item=channelItems.get(src.id);if(item&&typeof item.setOpacity==='function'){const active=channelSourceMatchesActive(src);item.setOpacity((!active||src.visible===false)?0:clamp(Number(src.opacity??1),0,1));}}\n",
    "function channelPlacementOptions(src){const meta=src.metadata||{},extent=meta.extent||src.extent||null,out={};if(extent&&Number.isFinite(Number(extent.x))&&Number.isFinite(Number(extent.width))){out.x=Number(extent.x)/Math.max(Number(cfg.slide_width)||1,1);out.y=Number(extent.y||0)/Math.max(Number(cfg.slide_width)||1,1);out.width=Number(extent.width)/Math.max(Number(cfg.slide_width)||1,1);}else{out.x=0;out.y=0;out.width=1;}return out;}\n",
    "function upsertChannelSource(src){src=normaliseChannelSource(src);if(!src)return;const idx=channelSources.findIndex(x=>String(x.id)===src.id);if(idx>=0)channelSources[idx]=Object.assign({},channelSources[idx],src);else channelSources.push(src);src=channelSources.find(x=>String(x.id)===src.id)||src;const existing=channelItems.get(src.id);if(existing){setChannelItemSettings(src);buildChannelList();return;}if(channelPendingItems.has(src.id)){buildChannelList();return;}const tileSource=channelTileSource(src);if(!tileSource||!osdViewer||typeof osdViewer.addTiledImage!=='function'){buildChannelList();return;}channelPendingItems.add(src.id);const opts=Object.assign({tileSource:tileSource,opacity:(!channelSourceMatchesActive(src)||src.visible===false)?0:clamp(Number(src.opacity??1),0,1),success:event=>{channelPendingItems.delete(src.id);channelItems.set(src.id,event.item);setChannelItemSettings(src);buildChannelList();},error:()=>{channelPendingItems.delete(src.id);notify('Channel '+(src.name||src.id)+' failed to load','warning',3600);}},channelPlacementOptions(src));osdViewer.addTiledImage(opts);buildChannelList();}\n",
    "function removeChannelItem(id){id=String(id||'');channelPendingItems.delete(id);const item=channelItems.get(id);if(item&&osdViewer&&osdViewer.world&&typeof osdViewer.world.removeItem==='function'){try{osdViewer.world.removeItem(item);}catch(e){}}channelItems.delete(id);}\n",
    "function clearChannelItems(){Array.from(channelItems.keys()).forEach(removeChannelItem);channelPendingItems.clear();}\n",
    "function removeChannelSource(id){id=String(id||'');removeChannelItem(id);channelSources=channelSources.filter(src=>String(src.id)!==id);buildChannelList();scheduleViewerStateSync('channel_source_removed',{id:id});}\n",
    "function channelNeedsReload(src,settings){if(!src||!settings)return false;const nextColour=settings.colour||settings.color;if(nextColour&&String(nextColour)!==String(src.colour||src.color||'#ffffff'))return true;const numericChanged=(key,current)=>typeof settings[key]!=='undefined'&&Number(settings[key])!==Number(current);return numericChanged('gain',src.gain??src.strength??1)||numericChanged('contrast_min',src.contrast_min??0)||numericChanged('contrast_max',src.contrast_max??1);}\n",
    "function reloadChannelSource(src){const id=String(src.id||'');removeChannelItem(id);upsertChannelSource(src);}\n",
    "function setChannelSettings(id,settings={}){id=String(id||'');const src=channelSources.find(x=>String(x.id)===id);if(!src)return;const reloadDynamic=src.type==='dynamic'&&channelNeedsReload(src,settings);Object.assign(src,settings);if(typeof settings.visible==='boolean')src.visible=settings.visible;if(typeof settings.opacity!=='undefined')src.opacity=Number(settings.opacity);if(settings.colour||settings.color)src.colour=String(settings.colour||settings.color);if(typeof settings.gain!=='undefined')src.gain=Number(settings.gain);if(typeof settings.contrast_min!=='undefined')src.contrast_min=Number(settings.contrast_min);if(typeof settings.contrast_max!=='undefined')src.contrast_max=Number(settings.contrast_max);if(reloadDynamic)reloadChannelSource(src);else setChannelItemSettings(src);if(src.type==='stain'&&stainEnabled){const idx=stainChannels.findIndex(ch=>String(ch.id)===id);if(idx>=0){if(typeof src.visible==='boolean')stainState[idx].visible=src.visible;if(src.colour)stainState[idx].color=src.colour;if(Number.isFinite(src.gain))stainState[idx].strength=src.gain;if(Number.isFinite(src.opacity))stainState[idx].opacity=src.opacity;if(Number.isFinite(src.contrast_min))stainState[idx].contrast_min=src.contrast_min;if(Number.isFinite(src.contrast_max))stainState[idx].contrast_max=src.contrast_max;applyStainPreferences({stain:{enabled:stainOn,channels:stainState.map((s,i)=>Object.assign({id:stainChannels[i].id,name:stainChannels[i].name},s,{color:s.color,strength:s.strength,gain:s.strength}))}});invalidateBaseImage();}}buildChannelList();scheduleViewerStateSync('channel_updated',{id:id});}\n",
    "function syncChannelSourcesForActiveImage(){const activeIds=new Set();channelSources.map(normaliseChannelSource).filter(Boolean).forEach(src=>{const active=channelSourceMatchesActive(src);if(active){activeIds.add(src.id);if(channelItems.has(src.id))setChannelItemSettings(src);else upsertChannelSource(src);}else{removeChannelItem(src.id);}});Array.from(channelItems.keys()).forEach(id=>{if(!activeIds.has(id))removeChannelItem(id);});buildChannelList();}\n",
    "function installInitialChannelSources(){clearChannelItems();syncChannelSourcesForActiveImage();}\n",
    "function currentChannelSettingsPayload(){const tileSettings=channelSources.map(src=>({id:String(src.id||''),name:String(src.name||src.id||''),type:String(src.type||'deepzoom'),visible:src.visible!==false,opacity:Number(src.opacity??1),colour:String(src.colour||src.color||'#ffffff'),gain:Number(src.gain??src.strength??1),contrast_min:Number(src.contrast_min??0),contrast_max:Number(src.contrast_max??1)}));if(typeof currentStainPayload==='function'){const stain=currentStainPayload();if(stain&&Array.isArray(stain.channels))stain.channels.forEach(ch=>{if(!tileSettings.some(x=>x.id===String(ch.id)))tileSettings.push({id:String(ch.id),name:String(ch.name||ch.id),type:'stain',visible:ch.visible!==false,opacity:Number(ch.opacity??1),colour:String(ch.colour||ch.color||'#ffffff'),gain:Number(ch.gain??ch.strength??1),contrast_min:Number(ch.contrast_min??0),contrast_max:Number(ch.contrast_max??1)});});}return tileSettings;}\n",
    "function channelSourceLabel(src){return String(src.name||src.id||'channel');}\n",
    "function channelControlRow(src,compact=false){src=normaliseChannelSource(src);const row=document.createElement('div');row.className='layerItem channelItem';if(src.visible===false)row.classList.add('hidden');const top=document.createElement('div');top.className='layerTop';const box=document.createElement('input');box.type='checkbox';box.checked=src.visible!==false;box.title='Toggle channel visibility';box.onchange=e=>setChannelSettings(src.id,{visible:!!e.target.checked});const sw=document.createElement('input');sw.type='color';sw.value=src.colour||src.color||'#ffffff';sw.title='Channel colour';sw.onchange=e=>setChannelSettings(src.id,{colour:e.target.value});const nm=document.createElement('span');nm.className='roiName';nm.textContent=channelSourceLabel(src);const meta=document.createElement('span');meta.className='roiClass';meta.textContent=src.type||'channel';top.append(box,sw,nm,meta);const controls=document.createElement('div');controls.className='layerControls';const op=document.createElement('input');op.type='range';op.min='0';op.max='1';op.step='0.05';op.value=String(Number(src.opacity??1));op.title='Channel opacity';op.oninput=e=>setChannelSettings(src.id,{opacity:Number(e.target.value)});controls.append(document.createTextNode('opacity'),op);if(!compact){const gain=document.createElement('input');gain.type='range';gain.min='0';gain.max='5';gain.step='0.05';gain.value=String(Number(src.gain??1));gain.title='Channel gain';gain.onchange=e=>setChannelSettings(src.id,{gain:Number(e.target.value)});const cmin=document.createElement('input');cmin.type='range';cmin.min='0';cmin.max='1';cmin.step='0.01';cmin.value=String(Number(src.contrast_min??0));cmin.title='Contrast minimum';cmin.onchange=e=>setChannelSettings(src.id,{contrast_min:Number(e.target.value)});const cmax=document.createElement('input');cmax.type='range';cmax.min='0.01';cmax.max='1';cmax.step='0.01';cmax.value=String(Number(src.contrast_max??1));cmax.title='Contrast maximum';cmax.onchange=e=>setChannelSettings(src.id,{contrast_max:Number(e.target.value)});controls.append(document.createTextNode(' gain'),gain,document.createTextNode(' min'),cmin,document.createTextNode(' max'),cmax);}row.append(top,controls);return row;}\n",
    "function buildChannelList(){const list=el('channelList'),summary=el('channelSummary'),menuList=el('channelMenuList'),menuSummary=el('channelMenuSummary'),sources=visibleChannelSources();const count=sources.length,visible=sources.filter(s=>s.visible!==false).length,total=channelSources.length,hidden=total-count;if(summary)summary.textContent=count?(visible+'/'+count+' channel overlays visible'):(hidden?'No image channels for the current image.':'No image channel overlays.');if(menuSummary)menuSummary.textContent=count?(visible+'/'+count+' overlay channels visible'+(hidden?(' | '+hidden+' hidden for this image'):'')):(hidden?'No image channels for the current image.':'No tiled mIHC/image channels configured.');if(list){list.innerHTML='';sources.forEach(src=>list.appendChild(channelControlRow(src,false)));}if(menuList){menuList.innerHTML='';sources.forEach(src=>menuList.appendChild(channelControlRow(src,true)));}}\n"
  )
}

wsi_viewer_sync_js <- function() {
  paste0(
    "let stateSyncTimer=null,stateSyncEvent='viewer_state',stateSyncDetail={},stateSyncSeq=0,lastSyncedSelectedRoi=-2;\n",
    "let stateSocket=null,stateSocketReady=false,stateSocketReconnectTimer=null,stateSocketFallbackNotified=false;\n",
    "function liveSyncAvailable(){return !!(cfg.viewer_state_url||cfg.viewer_state_ws_url||'');}\n",
    "function syncMessage(msg){const box=el('syncSummary');if(box)box.textContent=msg||(liveSyncAvailable()?'R sync ready':'R sync off');}\n",
    "function roiGeojsonObject(filterFn=null){const features=[];rois.forEach((roi,i)=>{if(filterFn&&!filterFn(roi,i))return;const feature=roiFeature(roi,i);if(feature)features.push(feature);});return {type:'FeatureCollection',features:features};}\n",
    "function selectedRoiFeatureObject(){if(selectedRoi<0||!rois[selectedRoi])return null;return roiFeature(rois[selectedRoi],selectedRoi);}\n",
    "function selectedRoisGeojsonObject(){const features=[];const indices=(typeof roiExportIndices==='function')?roiExportIndices():[];indices.forEach(i=>{if(i>=0&&rois[i]){const feature=roiFeature(rois[i],i);if(feature)features.push(feature);}});return {type:'FeatureCollection',features:features};}\n",
    "function segmentationGeojsonObject(){return roiGeojsonObject(roi=>{const source=String(roi.source||'').toLowerCase(),cls=String(roi.class||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells';});}\n",
    "function currentStainPayload(){if(!stainEnabled)return null;syncStainStateFromControls();return {enabled:stainOn,channels:stainChannels.map((ch,i)=>({id:ch.id,name:ch.name,type:'stain',visible:!!(stainState[i]&&stainState[i].visible),color:stainState[i]?stainState[i].color:ch.colour,colour:stainState[i]?stainState[i].color:ch.colour,strength:stainState[i]?stainState[i].strength:ch.strength,gain:stainState[i]?stainState[i].strength:ch.strength,opacity:stainState[i]?stainState[i].opacity:(ch.opacity??1),contrast_min:stainState[i]?stainState[i].contrast_min:(ch.contrast_min??0),contrast_max:stainState[i]?stainState[i].contrast_max:(ch.contrast_max??1)}))};}\n",
    "function viewerStatePayload(event,detail={}){return {event:event||'viewer_state',time:new Date().toISOString(),sequence:++stateSyncSeq,slide:{title:cfg.title,width:cfg.slide_width,height:cfg.slide_height},project:(typeof projectStatePayload==='function'?projectStatePayload():null),selected_index:selectedRoi,selected_roi:selectedRoiFeatureObject(),selected_rois:selectedRoisGeojsonObject(),rois:roiGeojsonObject(),segmentation:segmentationGeojsonObject(),layers:layerStatePayload(),measurements:measures,trajectories:(typeof trajectoryPayload==='function'?trajectoryPayload():[]),artifacts:(typeof artifactPayload==='function'?artifactPayload():[]),view:{mode:mode,scale:scale,offset_x:offsetX,offset_y:offsetY,roi_opacity:roiOpacity,show_rois:showRois,show_labels:showLabels,image_transform:(typeof imageTransformPayload==='function'?imageTransformPayload():null),base_layer:baseImagePayload()},annotations:{dirty:annotationsDirty,dirty_reason:annotationDirtyReason},history:annotationHistoryPayload(),stain:currentStainPayload(),channel_sources:channelSources,channel_settings:(typeof currentChannelSettingsPayload==='function'?currentChannelSettingsPayload():[]),tile_sources:cfg.tile_sources||[],kodama_selection:(typeof kodamaSelectionPayload==='function'?kodamaSelectionPayload():null),seurat_selection:(typeof seuratSelectionPayload==='function'?seuratSelectionPayload():null),detail:detail};}\n",
    "let stateCommandPollTimer=null,stateCommandSeen=new Set(),viewerAutosaveTimer=null,viewerAutosaveLastError='';\n",
    "function handleViewerCommand(command){if(!command||!command.id||stateCommandSeen.has(command.id))return;stateCommandSeen.add(command.id);const payload=command.payload||{},geojson=payload.geojson||payload;if(command.type==='job_update'){if(typeof upsertViewerJob==='function')upsertViewerJob(payload.job||payload);syncMessage('R command: job update');return;}if(command.type==='add_rois'){if(typeof addImportedGeojson==='function')addImportedGeojson(geojson,payload.name||'R session');syncMessage('R command: added ROIs');return;}if(command.type==='add_segmentation'){if(typeof addSegmentationGeojson==='function')addSegmentationGeojson(geojson,{local:false,detail:{source:payload.name||'R session'}});syncMessage('R command: added segmentation');return;}if(command.type==='add_layer'){if(typeof upsertViewerLayer==='function')upsertViewerLayer(payload.layer||payload);syncMessage('R command: added layer');return;}if(command.type==='set_layer_visible'){if(typeof setViewerLayerVisible==='function')setViewerLayerVisible(payload.id||payload.name,payload.visible);syncMessage('R command: layer visibility');return;}if(command.type==='remove_layer'){if(typeof removeViewerLayer==='function')removeViewerLayer(payload.id||payload.name);syncMessage('R command: removed layer');return;}if(command.type==='add_channel_source'){if(typeof upsertChannelSource==='function')upsertChannelSource(payload.source||payload);syncMessage('R command: added channel');return;}if(command.type==='remove_channel_source'){if(typeof removeChannelSource==='function')removeChannelSource(payload.id);syncMessage('R command: removed channel');return;}if(command.type==='set_channel_settings'){if(typeof setChannelSettings==='function')setChannelSettings(payload.id,payload.settings||payload);syncMessage('R command: channel settings');return;}if(command.type==='restore_project_state'){if(payload.rois&&typeof addImportedGeojson==='function')addImportedGeojson(payload.rois,'restored project');if(Array.isArray(payload.trajectories)){trajectories.splice(0,trajectories.length);payload.trajectories.forEach(t=>trajectories.push(t));selectedTrajectory=trajectories.length?0:-1;if(typeof updateTrajectoryList==='function')updateTrajectoryList();}if(payload.segmentation&&typeof addSegmentationGeojson==='function')addSegmentationGeojson(payload.segmentation,{local:false,detail:{source:'restored project'}});if(Array.isArray(payload.channel_sources)&&typeof upsertChannelSource==='function')payload.channel_sources.forEach(upsertChannelSource);if(Array.isArray(payload.channel_settings)&&typeof setChannelSettings==='function')payload.channel_settings.forEach(s=>setChannelSettings(s.id,s));if(payload.stain&&typeof applyStainPreferences==='function')applyStainPreferences({stain:payload.stain});syncMessage('R command: project restored');draw();return;}if(command.type==='annotations_saved'){markAnnotationsSaved(payload.reason||'project_saved');syncMessage('R command: annotations saved');return;}console.warn('Unknown wsiTools viewer command',command.type);}\n",
    "function handleViewerCommands(body){if(typeof handleViewerJobs==='function')handleViewerJobs(body);const commands=(body&&body.commands)||[];if(Array.isArray(commands))commands.forEach(handleViewerCommand);}\n",
    "function viewerAutosaveMessage(body){const autosave=body&&body.autosave;if(!autosave||!autosave.enabled)return '';if(autosave.last_error){const msg='Autosave failed: '+autosave.last_error;if(viewerAutosaveLastError!==autosave.last_error){viewerAutosaveLastError=autosave.last_error;notify(msg,'error',5200);}return msg;}viewerAutosaveLastError='';if(autosave.last_save&&autosave.count>0){const t=new Date(autosave.last_save);const stamp=Number.isNaN(t.getTime())?autosave.last_save:t.toLocaleTimeString();return 'Autosaved '+stamp;}return 'Autosave ready';}\n",
    "function viewerSocketSend(event='viewer_state',detail={}){if(!stateSocketReady||!stateSocket||stateSocket.readyState!==WebSocket.OPEN)return false;try{stateSocket.send(JSON.stringify(viewerStatePayload(event,detail)));syncMessage('R sync: '+event+' via WebSocket');return true;}catch(e){stateSocketReady=false;return false;}}\n",
    "function scheduleViewerSocketReconnect(){const url=cfg.viewer_state_ws_url||'';if(!url||stateSocketReconnectTimer)return;stateSocketReconnectTimer=setTimeout(()=>{stateSocketReconnectTimer=null;startViewerStateSocket();},2000);}\n",
    "function startViewerStateSocket(){const url=cfg.viewer_state_ws_url||'';if(!url||typeof WebSocket==='undefined'||stateSocket||stateSocketReconnectTimer)return false;try{stateSocket=new WebSocket(url);}catch(e){stateSocket=null;startViewerCommandPolling();return false;}stateSocket.onopen=()=>{stateSocketReady=true;stateSocketFallbackNotified=false;syncMessage('R sync: WebSocket connected');};stateSocket.onmessage=event=>{try{const body=JSON.parse(event.data);if(body&&body.ok===false&&body.error){syncMessage('R sync failed: '+body.error);notify('R sync failed: '+body.error,'error',4200);return;}handleViewerCommands(body);syncMessage(viewerAutosaveMessage(body)||'R sync: WebSocket');}catch(e){console.warn('Could not parse wsiTools WebSocket message',e);}};stateSocket.onclose=()=>{stateSocketReady=false;stateSocket=null;if(!stateSocketFallbackNotified){stateSocketFallbackNotified=true;syncMessage('R sync: WebSocket unavailable; polling fallback active');}startViewerCommandPolling();scheduleViewerSocketReconnect();};stateSocket.onerror=()=>{stateSocketReady=false;try{if(stateSocket)stateSocket.close();}catch(e){}};return true;}\n",
    "async function syncViewerState(event='viewer_state',detail={}){if(viewerSocketSend(event,detail))return true;const url=cfg.viewer_state_url||'';if(!url)return false;try{const response=await fetch(url,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(viewerStatePayload(event,detail))});if(!response.ok){const text=await response.text();throw new Error(text||('HTTP '+response.status));}let body=null;try{body=await response.json();}catch(e){}handleViewerCommands(body);syncMessage(viewerAutosaveMessage(body)||('R sync: '+event));return true;}catch(e){syncMessage('R sync failed: '+e.message);return false;}}\n",
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
    "function openShortcutHelp(){if(typeof closeCommandPalette==='function')closeCommandPalette();document.querySelectorAll('.toolMenu').forEach(menu=>menu.open=false);const help=el('shortcutHelp'),backdrop=el('shortcutHelpBackdrop');if(!help)return;shortcutHelpOpen=true;help.classList.add('open');help.setAttribute('aria-hidden','false');if(backdrop)backdrop.classList.add('open');}\n",
    "function closeShortcutHelp(){const help=el('shortcutHelp'),backdrop=el('shortcutHelpBackdrop');shortcutHelpOpen=false;if(help){help.classList.remove('open');help.setAttribute('aria-hidden','true');}if(backdrop)backdrop.classList.remove('open');}\n",
    "function toggleShortcutHelp(){if(shortcutHelpOpen)closeShortcutHelp();else openShortcutHelp();}\n",
    "function shortcutImportGeojson(){const b=el('importGeojson'),f=el('geojsonImportFile');if(b)b.click();else if(f){f.value='';f.click();}}\n",
    "function bindShortcutHelp(){const button=el('shortcutHelpButton'),close=el('shortcutHelpClose'),backdrop=el('shortcutHelpBackdrop');if(button)button.onclick=openShortcutHelp;if(close)close.onclick=closeShortcutHelp;if(backdrop)backdrop.onclick=closeShortcutHelp;document.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase(),typing=shortcutTypingTarget(e.target),modifier=e.ctrlKey||e.metaKey;if(shortcutHelpOpen){if(e.key==='Escape'||key==='?'){e.preventDefault();e.stopPropagation();closeShortcutHelp();}return;}if(typing)return;if((e.key==='?'||(e.shiftKey&&key==='/'))&&!modifier&&!e.altKey){e.preventDefault();e.stopPropagation();openShortcutHelp();return;}if(modifier&&!e.shiftKey&&key==='s'){e.preventDefault();e.stopPropagation();if(typeof saveGeojson==='function')saveGeojson();return;}if(modifier&&!e.shiftKey&&key==='i'){e.preventDefault();e.stopPropagation();shortcutImportGeojson();return;}if(modifier&&!e.shiftKey&&key==='e'){e.preventDefault();e.stopPropagation();if(typeof exportSelectedAnnotations==='function')exportSelectedAnnotations();return;}if(typeof commandPaletteOpen!=='undefined'&&commandPaletteOpen)return;if(!modifier&&!e.altKey&&!e.shiftKey&&(key==='p'||e.code==='Space')){e.preventDefault();e.stopPropagation();setMode('pan');return;}},true);}\n"
  )
}

wsi_viewer_command_palette_js <- function() {
  paste0(
    "let commandPaletteOpen=false,commandPaletteActive=0,tileGridVisible=false;\n",
    "function tileGridSize(){return Math.max(1,Number(cfg.tile_size||512));}\n",
    "function canvasToSlidePoint(x,y){if(typeof osdReady!=='undefined'&&osdReady&&typeof osdItem==='function'&&osdItem()&&typeof OpenSeadragon!=='undefined'){const vp=osdViewer.viewport.pointFromPixel(new OpenSeadragon.Point(x,y),true),img=osdItem().viewportToImageCoordinates(vp);return {x:img.x,y:img.y};}if(typeof image!=='undefined'&&image.naturalWidth&&typeof imageToSlide==='function'){const p={x:(x-offsetX)/scale,y:(y-offsetY)/scale};return imageToSlide((typeof viewToImagePoint==='function')?viewToImagePoint(p):p);}return {x:(x-offsetX)/scale,y:(y-offsetY)/scale};}\n",
    "function visibleSlideBounds(){const pts=[canvasToSlidePoint(0,0),canvasToSlidePoint(innerWidth,0),canvasToSlidePoint(0,innerHeight),canvasToSlidePoint(innerWidth,innerHeight)].filter(p=>p&&Number.isFinite(p.x)&&Number.isFinite(p.y));if(!pts.length)return {xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};return {xmin:clamp(Math.min(...pts.map(p=>p.x)),0,cfg.slide_width),ymin:clamp(Math.min(...pts.map(p=>p.y)),0,cfg.slide_height),xmax:clamp(Math.max(...pts.map(p=>p.x)),0,cfg.slide_width),ymax:clamp(Math.max(...pts.map(p=>p.y)),0,cfg.slide_height)};}\n",
    "function drawTileGrid(){if(!tileGridVisible)return;const b=visibleSlideBounds();let step=tileGridSize(),base=step,a=slideToCanvas({x:0,y:0}),c=slideToCanvas({x:step,y:0}),spacing=Math.abs(c.x-a.x);while(Number.isFinite(spacing)&&spacing>0&&spacing<22&&step<base*64){step*=2;spacing*=2;}const x0=Math.floor(b.xmin/step)*step,y0=Math.floor(b.ymin/step)*step;ctx.save();ctx.strokeStyle='rgba(250,204,21,.55)';ctx.lineWidth=1;ctx.setLineDash([4,5]);ctx.beginPath();for(let x=x0;x<=b.xmax+step;x+=step){const p0=slideToCanvas({x:x,y:b.ymin}),p1=slideToCanvas({x:x,y:b.ymax});ctx.moveTo(p0.x,p0.y);ctx.lineTo(p1.x,p1.y);}for(let y=y0;y<=b.ymax+step;y+=step){const p0=slideToCanvas({x:b.xmin,y:y}),p1=slideToCanvas({x:b.xmax,y:y});ctx.moveTo(p0.x,p0.y);ctx.lineTo(p1.x,p1.y);}ctx.stroke();ctx.setLineDash([]);ctx.fillStyle='rgba(250,204,21,.9)';ctx.font='11px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.fillText('tile grid '+Math.round(tileGridSize())+' px',12,innerHeight-18);ctx.restore();}\n",
    "function toggleTileGrid(){tileGridVisible=!tileGridVisible;scheduleViewerStateSync('tile_grid_toggled',{visible:tileGridVisible,tile_size:tileGridSize()});notify(tileGridVisible?'Tile grid shown':'Tile grid hidden','success');draw();}\n",
    "async function requestProjectSave(){if(!liveSyncAvailable()){notify('Live R sync is off; use Save project in the Project menu to download a project file, or use viewer$save_project(...) in R.','warning',5200);return;}const snapshot=(typeof projectBrowserSnapshot==='function')?projectBrowserSnapshot(false):null;const ok=await syncViewerState('project_save_requested',{dirty:annotationsDirty,reason:annotationDirtyReason,project_snapshot:snapshot});notify(ok?'Project save requested in R':'Project save request failed',ok?'success':'error',3600);}\n",
    "function commandPaletteDefinitions(){return [{id:'open_project_panel',label:'Open project panel',hint:'Show the left Project panel',kbd:'Project',enabled:()=>typeof openProjectPanel==='function',run:()=>openProjectPanel()},{id:'add_project_image',label:'Add project image',hint:'Add browser-readable images or WSI/microscopy file references to this viewer project',kbd:'file',enabled:()=>!!el('projectImageFile'),run:()=>{const b=el('projectOpenImage'),f=el('projectImageFile');if(b)b.click();else if(f)f.click();}},{id:'new_roi',label:'New ROI',hint:'Deselect current annotation and start painting a separate ROI',kbd:'N',enabled:()=>true,run:()=>startNewAnnotation('brush')},{id:'draw_trajectory',label:'Draw trajectory',hint:'Click control points to create a smoothed path synced to R',kbd:'T',enabled:()=>true,run:()=>setMode('trajectory')},{id:'trajectory_area',label:'Create trajectory area',hint:'Convert the draft or selected trajectory into a width-adjustable ROI corridor',kbd:'ROI',enabled:()=>typeof createTrajectoryAreaRoi==='function'&&(trajectoryDraft.length>=2||trajectories.length>0),run:()=>createTrajectoryAreaRoi()},{id:'import_geojson',label:'Import GeoJSON',hint:'Load QuPath or wsiTools annotations',kbd:'file',enabled:()=>!!el('geojsonImportFile'),run:()=>{const b=el('importGeojson'),f=el('geojsonImportFile');if(b)b.click();else if(f)f.click();}},{id:'export_selected_rois',label:'Export selected ROIs',hint:'Download checked or selected ROI annotations',kbd:'GeoJSON',enabled:()=>typeof roiExportIndices==='function'&&roiExportIndices().length>0,run:()=>exportSelectedAnnotations()},{id:'run_stardist',label:'Run StarDist',hint:'Segment the selected ROI through the live R service',kbd:'ROI',enabled:()=>!!((cfg.segmentation_run_url||'')&&typeof selectedRoiFeatureText==='function'&&selectedRoiFeatureText()),run:()=>startSegmentationForSelectedRoi()},{id:'load_grandqc_artifacts',label:'Load GrandQC artifacts',hint:'Import GrandQC artifact GeoJSON annotations from the CellPhenotyper project',kbd:'QC',enabled:()=>typeof loadAllGrandqcGeojsons==='function'&&grandqcItems().length>0,run:()=>loadAllGrandqcGeojsons()},{id:'show_tile_grid',label:(tileGridVisible?'Hide tile grid':'Show tile grid'),hint:'Overlay a coordinate-only tile grid; no pixels are read',kbd:Math.round(tileGridSize())+' px',enabled:()=>true,run:()=>toggleTileGrid()},{id:'save_project',label:'Save project',hint:'Sync viewer state and request project saving in R',kbd:'R',enabled:()=>liveSyncAvailable(),run:()=>requestProjectSave()}];}\n",
    "function commandPaletteQuery(){const input=el('commandPaletteSearch');return String(input&&input.value||'').trim().toLowerCase();}\n",
    "function commandPaletteItems(){const q=commandPaletteQuery();return commandPaletteDefinitions().filter(item=>!q||[item.label,item.hint,item.id].join(' ').toLowerCase().includes(q));}\n",
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
    "function drawArtifactOverlays(){}\n",
    "function bindArtifactControls(){const items=grandqcItems(),loadAll=el('grandqcLoadAll'),clear=el('grandqcClear');document.querySelectorAll('.grandqcLoad').forEach(button=>{button.onclick=()=>loadGrandqcGeojson(button.dataset.grandqcIndex);button.disabled=!items.length;});if(loadAll){loadAll.onclick=loadAllGrandqcGeojsons;loadAll.disabled=!items.length;}if(clear){clear.onclick=()=>clearGrandqcRois(true);clear.disabled=!items.length;}artifactStatus(items.length?(items.length+' GrandQC GeoJSON file'+(items.length===1?'':'s')+' available.'):'No GrandQC GeoJSON was found for this project.');}\n"
  )
}

wsi_viewer_image_transform_js <- function() {
  paste0(
    "let imageRotation=0,imageFlipX=false,imageFlipY=false;\n",
    "function normalizedImageRotation(value=imageRotation){let r=Number(value)||0;r=((Math.round(r/90)*90)%360+360)%360;return r;}\n",
    "function imageTransformPayload(){return {rotation:normalizedImageRotation(),flip_x:!!imageFlipX,flip_y:!!imageFlipY};}\n",
    "function imageTransformLabel(){const parts=[];const r=normalizedImageRotation();if(r)parts.push('rot '+r+' deg');if(imageFlipX)parts.push('flip H');if(imageFlipY)parts.push('flip V');return parts.length?parts.join(' + '):'original orientation';}\n",
    "function imageTransformStatus(){const label=imageTransformLabel();return label==='original orientation'?'':' | image '+label;}\n",
    "function updateImageTransformSummary(){const box=el('imageTransformSummary');if(box)box.textContent=imageTransformLabel();const reset=el('resetImageTransform');if(reset)reset.disabled=normalizedImageRotation()===0&&!imageFlipX&&!imageFlipY;}\n",
    "function sourceImageSize(){if(typeof image!=='undefined'&&image&&image.naturalWidth)return {width:image.naturalWidth,height:image.naturalHeight};return {width:Number(cfg.slide_width)||1,height:Number(cfg.slide_height)||1};}\n",
    "function viewImageSize(){const size=sourceImageSize(),r=normalizedImageRotation();return r===90||r===270?{width:size.height,height:size.width}:size;}\n",
    "function imageToViewPoint(p){const size=sourceImageSize(),w=size.width,h=size.height,r=normalizedImageRotation();let x=Number(p.x),y=Number(p.y);if(imageFlipX)x=w-x;if(imageFlipY)y=h-y;if(r===90)return {x:h-y,y:x};if(r===180)return {x:w-x,y:h-y};if(r===270)return {x:y,y:w-x};return {x:x,y:y};}\n",
    "function viewToImagePoint(p){const size=sourceImageSize(),w=size.width,h=size.height,r=normalizedImageRotation();let x=Number(p.x),y=Number(p.y),q;if(r===90)q={x:y,y:h-x};else if(r===180)q={x:w-x,y:h-y};else if(r===270)q={x:w-y,y:x};else q={x:x,y:y};if(imageFlipX)q.x=w-q.x;if(imageFlipY)q.y=h-q.y;return q;}\n",
    "function slideToViewImagePoint(p){return imageToViewPoint(slideToImage(p));}\n",
    "function applyCanvasImageTransform(img){const w=img.naturalWidth,h=img.naturalHeight,r=normalizedImageRotation();if(r===90){ctx.translate(h,0);ctx.rotate(Math.PI/2);}else if(r===180){ctx.translate(w,h);ctx.rotate(Math.PI);}else if(r===270){ctx.translate(0,w);ctx.rotate(-Math.PI/2);}if(imageFlipX||imageFlipY){ctx.translate(imageFlipX?w:0,imageFlipY?h:0);ctx.scale(imageFlipX?-1:1,imageFlipY?-1:1);}}\n",
    "function drawTransformedImage(img){ctx.save();ctx.globalAlpha=baseImageOpacityValue();ctx.translate(offsetX,offsetY);ctx.scale(scale,scale);applyCanvasImageTransform(img);if(baseImageOpacityValue()>0)ctx.drawImage(img,0,0);ctx.restore();}\n",
    "function effectiveOpenSeadragonTransform(){let rotation=normalizedImageRotation(),flip=!!imageFlipX;if(imageFlipY){rotation=(rotation+180)%360;flip=!flip;}return {rotation:rotation,flip:flip};}\n",
    "function applyOpenSeadragonImageTransform(){if(typeof osdViewer==='undefined'||!osdViewer||!osdViewer.viewport)return false;const t=effectiveOpenSeadragonTransform();if(typeof osdViewer.viewport.setRotation==='function')osdViewer.viewport.setRotation(t.rotation,false);if(typeof osdViewer.viewport.setFlip==='function')osdViewer.viewport.setFlip(t.flip);if(typeof osdViewer.forceRedraw==='function')osdViewer.forceRedraw();return true;}\n",
    "function applyImageTransform(refit=false){updateImageTransformSummary();if(typeof osdViewer!=='undefined'&&osdViewer){applyOpenSeadragonImageTransform();syncViewState();prefetchNeighborTiles();draw();}else if(refit&&typeof fitView==='function'){fitView();}else if(typeof draw==='function'){draw();}scheduleViewerStateSync('image_transform_updated',imageTransformPayload());}\n",
    "function rotateImageDisplay(delta){const before=viewImageSize();imageRotation=normalizedImageRotation(imageRotation+delta);const after=viewImageSize();const refit=before.width!==after.width||before.height!==after.height;applyImageTransform(refit);notify('Image '+imageTransformLabel(),'success',1800);}\n",
    "function flipImageDisplay(axis){if(axis==='x')imageFlipX=!imageFlipX;else imageFlipY=!imageFlipY;applyImageTransform(false);notify('Image '+imageTransformLabel(),'success',1800);}\n",
    "function resetImageDisplayTransform(){imageRotation=0;imageFlipX=false;imageFlipY=false;applyImageTransform(true);notify('Image orientation reset','success',1800);}\n",
    "function bindImageTransformControls(){const left=el('rotateImageLeft'),right=el('rotateImageRight'),flipH=el('flipImageHorizontal'),flipV=el('flipImageVertical'),reset=el('resetImageTransform');if(left)left.onclick=()=>rotateImageDisplay(-90);if(right)right.onclick=()=>rotateImageDisplay(90);if(flipH)flipH.onclick=()=>flipImageDisplay('x');if(flipV)flipV.onclick=()=>flipImageDisplay('y');if(reset)reset.onclick=resetImageDisplayTransform;updateImageTransformSummary();}\n"
  )
}

wsi_viewer_navigator_js <- function() {
  paste0(
    "let miniNavigatorDragging=false;\n",
    "function miniNavigatorCanvas(){return el('miniNavigatorCanvas');}\n",
    "function miniNavigatorLayout(){const canvas=miniNavigatorCanvas();if(!canvas)return null;const rect=canvas.getBoundingClientRect(),dpr=window.devicePixelRatio||1,w=Math.max(1,Math.floor(rect.width*dpr)),h=Math.max(1,Math.floor(rect.height*dpr));if(canvas.width!==w||canvas.height!==h){canvas.width=w;canvas.height=h;}const nctx=canvas.getContext('2d');nctx.setTransform(dpr,0,0,dpr,0,0);const pad=8,cw=Math.max(1,rect.width),ch=Math.max(1,rect.height),sw=Math.max(1,Number(cfg.slide_width)||1),sh=Math.max(1,Number(cfg.slide_height)||1),s=Math.min((cw-pad*2)/sw,(ch-pad*2)/sh),rw=sw*s,rh=sh*s,ox=(cw-rw)/2,oy=(ch-rh)/2;return {canvas:canvas,ctx:nctx,width:cw,height:ch,scale:s,ox:ox,oy:oy,sw:sw,sh:sh};}\n",
    "function miniNavigatorPoint(p,l){return {x:l.ox+Number(p.x)*l.scale,y:l.oy+Number(p.y)*l.scale};}\n",
    "function miniNavigatorBounds(b,l){if(!b)return null;const p0=miniNavigatorPoint({x:b.xmin,y:b.ymin},l),p1=miniNavigatorPoint({x:b.xmax,y:b.ymax},l);return {x:p0.x,y:p0.y,w:p1.x-p0.x,h:p1.y-p0.y};}\n",
    "function miniNavigatorSlidePoint(clientX,clientY,l){const rect=l.canvas.getBoundingClientRect(),x=clamp((clientX-rect.left-l.ox)/Math.max(l.scale,1e-9),0,l.sw),y=clamp((clientY-rect.top-l.oy)/Math.max(l.scale,1e-9),0,l.sh);return {x:x,y:y};}\n",
    "function miniNavigatorOverviewImage(){if(typeof navigatorImage!=='undefined'&&navigatorImage.complete&&navigatorImage.naturalWidth)return navigatorImage;if(typeof image!=='undefined'&&image.complete&&image.naturalWidth)return image;return null;}\n",
    "function drawMiniNavigatorOverview(nctx,l){const img=miniNavigatorOverviewImage(),w=l.sw*l.scale,h=l.sh*l.scale;nctx.save();nctx.beginPath();nctx.rect(l.ox,l.oy,w,h);nctx.clip();if(img){nctx.imageSmoothingEnabled=true;nctx.drawImage(img,l.ox,l.oy,w,h);nctx.restore();return true;}nctx.fillStyle='rgba(15,23,42,.96)';nctx.fillRect(l.ox,l.oy,w,h);nctx.restore();return false;}\n",
    "function navigatorRoiBounds(roi){try{return typeof roiBounds==='function'?roiBounds(roi):null;}catch(e){return null;}}\n",
    "function navigatorCellLikeRoi(roi){const source=String((roi&&roi.source)||'').toLowerCase(),cls=String((roi&&roi.class)||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells'||cls==='detection';}\n",
    "function navigatorTissueLikeRoi(roi){const cls=String((roi&&roi.class)||'').toLowerCase();return !navigatorCellLikeRoi(roi)&&cls!=='exclusion'&&cls!=='artefact';}\n",
    "function drawMiniNavigatorLayerDensity(nctx,l){let drawn=false;(layers||[]).forEach(layer=>{if(typeof normaliseViewerLayer==='function')normaliseViewerLayer(layer);if(typeof layerVisible==='function'&&!layerVisible(layer))return;const type=String(layer.type||'').toLowerCase(),values=layer.values;if(!(type==='heatmap'||type==='mask')||!Array.isArray(values)||!values.length)return;const rows=values.length,cols=Array.isArray(values[0])?values[0].length:0;if(!cols)return;const ext=layer.extent||{xmin:0,ymin:0,xmax:l.sw,ymax:l.sh},b=miniNavigatorBounds(ext,l);if(!b)return;const min=Number.isFinite(Number(layer.min))?Number(layer.min):0,max=Number.isFinite(Number(layer.max))?Number(layer.max):1,den=Math.max(1e-9,max-min),cellW=b.w/cols,cellH=b.h/rows,alpha=0.08+0.26*Math.max(0,Math.min(1,Number(layer.opacity||0.35)));nctx.save();for(let r=0;r<rows;r++){const row=values[r]||[];for(let c=0;c<cols;c++){const v=Number(row[c]);if(!Number.isFinite(v))continue;const t=clamp((v-min)/den,0,1);if(t<=0)continue;nctx.fillStyle='rgba(34,197,94,'+(alpha*t).toFixed(3)+')';nctx.fillRect(b.x+c*cellW,b.y+r*cellH,Math.max(.75,cellW),Math.max(.75,cellH));drawn=true;}}nctx.restore();});return drawn;}\n",
    "function drawMiniNavigatorRoiDensity(nctx,l){const cols=18,rows=clamp(Math.round(cols*l.sh/l.sw),5,18),ann=new Array(cols*rows).fill(0),tissue=new Array(cols*rows).fill(0);let annTouched=0,tissueTouched=0;rois.forEach((roi,i)=>{if(typeof visibleRoi==='function'&&!visibleRoi(roi))return;if(typeof isDrawable==='function'&&!isDrawable(roi))return;const b=navigatorRoiBounds(roi);if(!b)return;const x0=clamp(Math.floor(b.xmin/l.sw*cols),0,cols-1),x1=clamp(Math.floor(b.xmax/l.sw*cols),0,cols-1),y0=clamp(Math.floor(b.ymin/l.sh*rows),0,rows-1),y1=clamp(Math.floor(b.ymax/l.sh*rows),0,rows-1),tissueLike=navigatorTissueLikeRoi(roi);for(let y=y0;y<=y1;y++){for(let x=x0;x<=x1;x++){const idx=y*cols+x;ann[idx]+=1;annTouched++;if(tissueLike){tissue[idx]+=1;tissueTouched++;}}}});const maxAnn=Math.max(1,...ann),maxTissue=Math.max(1,...tissue),cellW=l.sw*l.scale/cols,cellH=l.sh*l.scale/rows;nctx.save();for(let y=0;y<rows;y++){for(let x=0;x<cols;x++){const idx=y*cols+x,px=l.ox+x*cellW,py=l.oy+y*cellH;if(tissue[idx]>0){const a=0.08+0.24*(tissue[idx]/maxTissue);nctx.fillStyle='rgba(34,197,94,'+a.toFixed(3)+')';nctx.fillRect(px,py,cellW,cellH);}if(ann[idx]>0){const a=0.06+0.22*(ann[idx]/maxAnn);nctx.fillStyle='rgba(245,158,11,'+a.toFixed(3)+')';nctx.fillRect(px,py,cellW,cellH);}}}nctx.restore();return {annotation:annTouched,tissue:tissueTouched};}\n",
    "function drawMiniNavigatorMarkers(nctx,l){const marked=new Set();function mark(i,primary=false){if(i<0||!rois[i]||marked.has(i))return;marked.add(i);const b=navigatorRoiBounds(rois[i]);if(!b)return;const cx=(b.xmin+b.xmax)/2,cy=(b.ymin+b.ymax)/2,p=miniNavigatorPoint({x:cx,y:cy},l),r=primary?4.5:3;const rect=miniNavigatorBounds(b,l);nctx.save();if(rect){nctx.strokeStyle=primary?'#ffffff':'#5eead4';nctx.globalAlpha=primary ? .95 : .55;nctx.lineWidth=primary?1.5:1;nctx.strokeRect(rect.x,rect.y,Math.max(2,rect.w),Math.max(2,rect.h));}nctx.globalAlpha=1;nctx.beginPath();nctx.arc(p.x,p.y,r,0,Math.PI*2);nctx.fillStyle=primary?'#ffffff':'#5eead4';nctx.strokeStyle='#0b0b0b';nctx.lineWidth=1.5;nctx.fill();nctx.stroke();nctx.restore();}\n",
    "if(selectedRoi>=0)mark(selectedRoi,true);rois.forEach((roi,i)=>{if(roi&&roi.export_selected)mark(i,false);});}\n",
    "function drawMiniNavigatorViewport(nctx,l){if(typeof visibleSlideBounds!=='function')return null;const b=visibleSlideBounds(),r=miniNavigatorBounds(b,l);if(!r)return null;nctx.save();nctx.strokeStyle='#67e8f9';nctx.lineWidth=2;nctx.shadowColor='rgba(103,232,249,.45)';nctx.shadowBlur=5;nctx.strokeRect(r.x,r.y,Math.max(2,r.w),Math.max(2,r.h));nctx.shadowBlur=0;nctx.strokeStyle='rgba(255,255,255,.88)';nctx.lineWidth=1;nctx.strokeRect(r.x+.5,r.y+.5,Math.max(1,r.w-1),Math.max(1,r.h-1));nctx.restore();return b;}\n",
    "function navigatorDensitySummary(roiDensity,layerDensity){const visible=(rois||[]).filter(roi=>!(typeof visibleRoi==='function')||visibleRoi(roi)).length,selected=(selectedRoi>=0?1:0)+(rois||[]).filter(roi=>roi&&roi.export_selected).length;let text=(visible?visible+' ROI'+(visible===1?'':'s'):'no ROIs');if(selected)text+=' | '+selected+' marked';if(layerDensity)text+=' | tissue map';else if(roiDensity&&roiDensity.tissue)text+=' | tissue density';return text;}\n",
    "function drawMiniNavigator(){const l=miniNavigatorLayout();if(!l)return;const nctx=l.ctx;nctx.clearRect(0,0,l.width,l.height);nctx.save();nctx.fillStyle='rgba(3,7,18,.82)';nctx.fillRect(0,0,l.width,l.height);drawMiniNavigatorOverview(nctx,l);nctx.strokeStyle='rgba(255,255,255,.3)';nctx.lineWidth=1;nctx.strokeRect(l.ox,l.oy,l.sw*l.scale,l.sh*l.scale);const layerDensity=drawMiniNavigatorLayerDensity(nctx,l),roiDensity=drawMiniNavigatorRoiDensity(nctx,l);drawMiniNavigatorMarkers(nctx,l);const b=drawMiniNavigatorViewport(nctx,l);nctx.restore();const viewport=el('miniNavigatorViewport'),density=el('miniNavigatorDensity');if(viewport&&b){viewport.textContent='x '+Math.round(b.xmin)+'-'+Math.round(b.xmax)+' y '+Math.round(b.ymin)+'-'+Math.round(b.ymax);}if(density)density.textContent=navigatorDensitySummary(roiDensity,layerDensity);}\n",
    "function panMiniNavigatorTo(clientX,clientY){const l=miniNavigatorLayout();if(!l)return;const p=miniNavigatorSlidePoint(clientX,clientY,l);if(typeof osdReady!=='undefined'&&osdReady&&typeof osdItem==='function'&&osdItem()&&typeof OpenSeadragon!=='undefined'){const vp=osdItem().imageToViewportCoordinates(p.x,p.y);osdViewer.viewport.panTo(vp,false);osdViewer.viewport.applyConstraints(false);if(typeof syncViewState==='function')syncViewState();if(typeof prefetchNeighborTiles==='function')prefetchNeighborTiles();draw();return;}if(typeof image!=='undefined'&&image.naturalWidth&&typeof slideToImage==='function'){const q=(typeof slideToViewImagePoint==='function')?slideToViewImagePoint(p):slideToImage(p);offsetX=innerWidth/2-q.x*scale;offsetY=innerHeight/2-q.y*scale;draw();}}\n",
    "function bindMiniNavigator(){const canvas=miniNavigatorCanvas();if(!canvas)return;if(typeof navigatorImage!=='undefined'&&cfg.navigator_image_data_uri&&!navigatorImage.src){navigatorImage.onload=()=>drawMiniNavigator();navigatorImage.onerror=()=>drawMiniNavigator();navigatorImage.src=cfg.navigator_image_data_uri;}canvas.addEventListener('mousedown',e=>{miniNavigatorDragging=true;panMiniNavigatorTo(e.clientX,e.clientY);});window.addEventListener('mousemove',e=>{if(miniNavigatorDragging)panMiniNavigatorTo(e.clientX,e.clientY);});window.addEventListener('mouseup',()=>{miniNavigatorDragging=false;});canvas.addEventListener('dblclick',e=>{e.preventDefault();panMiniNavigatorTo(e.clientX,e.clientY);if(typeof fitView==='function'&&e.altKey)fitView();});drawMiniNavigator();}\n"
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
    "function cursorTargets(){const targets=[canvas];if(typeof viewerEl!=='undefined'&&viewerEl)targets.push(viewerEl);return targets;}\n",
    "function brushBlockedAt(p){const hit=roiAt(p);return hit>=0&&lockedRoi(rois[hit])?hit:-1;}\n",
    "function cursorBlocked(){return (mode==='brush'||mode==='edit')&&lastPointer&&pointInsideSlide(lastPointer)&&brushBlockedAt(lastPointer)>=0;}\n",
    "function viewerIsMac(){const platform=String(navigator.platform||navigator.userAgent||'');return /Mac|iPhone|iPad|iPod/i.test(platform);}\n",
    "function brushSubtractModifier(e={}){return !!(e&&(viewerIsMac()?e.metaKey:e.altKey));}\n",
    "function brushSubtractKeyEvent(e={}){return viewerIsMac()?e.key==='Meta':e.key==='Alt';}\n",
    "function brushCursorState(){const blocked=cursorBlocked(),subtract=mode==='brush'&&!blocked&&(brushing?brushOperation==='subtract':brushAltDown),add=mode==='brush'&&!blocked&&!subtract;return {blocked:blocked,subtract:subtract,add:add};}\n",
    "function updateCursorFeedback(e={}){if(e&&(typeof e.altKey==='boolean'||typeof e.metaKey==='boolean')){brushAltDown=brushSubtractModifier(e);if(mode==='brush'&&brushing&&brushTargetRoi>=0)brushOperation=brushAltDown?'subtract':'new';}const state=brushCursorState();cursorTargets().forEach(target=>{target.classList.toggle('brush-add',state.add);target.classList.toggle('brush-subtract',state.subtract);target.classList.toggle('cursor-blocked',state.blocked);});return state;}\n",
    "function clearSelectedTrajectory(refresh=true){if(typeof selectedTrajectory==='undefined'||selectedTrajectory<0)return false;selectedTrajectory=-1;if(refresh){if(typeof renderTrajectoryList==='function')renderTrajectoryList();else if(typeof updateTrajectoryList==='function')updateTrajectoryList();}return true;}\n",
    "function clearSelectedAnnotation(refresh=true){const changed=!(typeof selectedRoi==='undefined'||selectedRoi<0);selectedRoi=-1;activeVertex=null;draggingVertex=null;brushTargetRoi=-1;if(typeof selectionCardVisible!=='undefined')selectionCardVisible=false;if(refresh){if(typeof updateRoiList==='function')updateRoiList();else if(typeof buildRoiList==='function')buildRoiList();}return changed;}\n",
    "function selectAnnotation(index,refresh=true){const idx=Number(index);selectedRoi=Number.isFinite(idx)?idx:-1;if(selectedRoi>=0)clearSelectedTrajectory(false);activeVertex=null;draggingVertex=null;if(refresh){if(typeof updateRoiList==='function')updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof updateButtons==='function')updateButtons();}return selectedRoi;}\n",
    "function selectTrajectory(index,refresh=true){const idx=Number(index);selectedTrajectory=Number.isFinite(idx)?idx:-1;if(selectedTrajectory>=0)clearSelectedAnnotation(false);if(refresh){if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof updateRoiList==='function')updateRoiList();if(typeof updateButtons==='function')updateButtons();}return selectedTrajectory;}\n",
    "function enforceSingleObjectSelection(prefer='annotation'){const hasRoi=Number.isFinite(Number(selectedRoi))&&selectedRoi>=0&&!!rois[selectedRoi],hasTrajectory=typeof selectedTrajectory!=='undefined'&&Number.isFinite(Number(selectedTrajectory))&&selectedTrajectory>=0&&typeof trajectories!=='undefined'&&!!trajectories[selectedTrajectory];if(!hasRoi||!hasTrajectory)return false;if(prefer==='trajectory')selectedRoi=-1;else selectedTrajectory=-1;return true;}\n",
    "function clearSelectionAndPan(){clearSelectedAnnotation(false);clearSelectedTrajectory(false);setMode('pan');if(typeof updateRoiList==='function')updateRoiList();if(typeof renderTrajectoryList==='function')renderTrajectoryList();if(typeof updateButtons==='function')updateButtons();if(typeof draw==='function')draw();}\n",
    "function selectObjectAtPoint(p){const hit=roiAt(p);if(hit>=0){selectAnnotation(hit,true);setMode('select');notify('Selected '+(rois[hit].name||rois[hit].id||'ROI'),'info',1400);draw();return true;}if(typeof trajectoryAt==='function'){const t=trajectoryAt(p);if(t>=0){selectTrajectory(t,true);setMode('select');notify('Selected '+((trajectories[t]&&trajectories[t].name)||('Trajectory '+(t+1))),'info',1400);draw();return true;}}return false;}\n",
    "function centerRoi(i){if(!hasDrawable())return;let idx=-1;for(let k=0;k<rois.length;k++){const candidate=(i+rois.length+k)%rois.length;if(isDrawable(rois[candidate])){idx=candidate;break;}}if(idx<0)return;selectAnnotation(idx,false);const b=roiBounds(rois[selectedRoi]);if(!b){notify('No drawable bounds','warning');updateRoiList();draw();return;}const pad=1.35;if(typeof zoomToSlideBounds==='function'){zoomToSlideBounds(b,pad);updateRoiList();draw();return;}let viewW=b.xmax-b.xmin,viewH=b.ymax-b.ymin,centerX=(b.xmin+b.xmax)/2,centerY=(b.ymin+b.ymax)/2;if(typeof slideToViewImagePoint==='function'){const corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(slideToViewImagePoint),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys);viewW=xmax-xmin;viewH=ymax-ymin;centerX=(xmin+xmax)/2;centerY=(ymin+ymax)/2;}else if(typeof slideToImage==='function'){const p0=slideToImage({x:b.xmin,y:b.ymin}),p1=slideToImage({x:b.xmax,y:b.ymax});viewW=p1.x-p0.x;viewH=p1.y-p0.y;centerX=(p0.x+p1.x)/2;centerY=(p0.y+p1.y)/2;}const maxScale=(typeof image!=='undefined')?40:4;scale=clamp(Math.min(innerWidth/Math.max(1,viewW*pad),innerHeight/Math.max(1,viewH*pad)),minScale*0.8,maxScale);offsetX=innerWidth/2-centerX*scale;offsetY=innerHeight/2-centerY*scale;updateRoiList();draw();}\n",
    "function roiLabelText(roi,i){return roi.label||roi.name||roi.id||('geometry '+(i+1));}\n",
    "function roiLabelPoint(roi){const b=roiBounds(roi);if(b)return slideToCanvas({x:(b.xmin+b.xmax)/2,y:(b.ymin+b.ymax)/2});if(isDrawable(roi)&&roi.rings[0]&&roi.rings[0][0])return slideToCanvas(roi.rings[0][0]);return null;}\n",
    "function drawPathRings(rings){(rings||[]).forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});}\n",
    "function drawRois(){if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{if(!visibleRoi(roi)||!isDrawable(roi))return;const selected=(i===selectedRoi)||!!roi.export_selected,groups=roiDrawGroups(roi);groups.forEach(group=>{ctx.beginPath();drawPathRings(group.rings);drawPathRings(group.holes);ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=selected?'#ffffff':(lockedRoi(roi)?'#facc15':roi.colour);ctx.lineWidth=selected?4:(lockedRoi(roi)?3:2);ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;});if(showLabels){const label=roiLabelPoint(roi),text=roiLabelText(roi,i);if(label&&text){const w=ctx.measureText(text).width+8;const x=clamp(label.x-w/2,4,innerWidth-w-4),y=clamp(label.y-9,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.72)';ctx.fillRect(x,y,w,18);ctx.fillStyle=roi.colour||'#5eead4';ctx.fillText(text,x+4,y+3);}}});ctx.restore();}\n",
    "function annotationSnapshot(){return {rois:JSON.parse(JSON.stringify(rois)),selectedRoi:selectedRoi,newRoiCount:newRoiCount,trajectories:JSON.parse(JSON.stringify(typeof trajectories==='undefined'?[]:trajectories)),selectedTrajectory:typeof selectedTrajectory==='undefined'?-1:selectedTrajectory,trajectorySeq:typeof trajectorySeq==='undefined'?0:trajectorySeq};}\n",
    "function pushHistory(stack,state){stack.push(state);if(stack.length>10)stack.shift();}\n",
    "function pushAnnotationUndo(action='annotation_edit'){try{pushHistory(annotationUndo,annotationSnapshot());annotationRedo=[];markAnnotationsDirty(action);updateButtons();}catch(e){console.warn('Could not record annotation undo state',e);}}\n",
    "function restoreAnnotationState(state,eventName){rois.splice(0,rois.length);(state.rois||[]).forEach(roi=>rois.push(roi));selectedRoi=Math.min(Math.max(Number(state.selectedRoi),-1),rois.length-1);if(!Number.isFinite(selectedRoi))selectedRoi=-1;if(Number.isFinite(Number(state.newRoiCount)))newRoiCount=Number(state.newRoiCount);if(Object.prototype.hasOwnProperty.call(state,'trajectories')&&typeof trajectories!=='undefined'){trajectories.splice(0,trajectories.length);(state.trajectories||[]).forEach(t=>trajectories.push(t));selectedTrajectory=Math.min(Math.max(Number(state.selectedTrajectory),-1),trajectories.length-1);if(!Number.isFinite(selectedTrajectory))selectedTrajectory=-1;if(Number.isFinite(Number(state.trajectorySeq)))trajectorySeq=Number(state.trajectorySeq);enforceSingleObjectSelection('annotation');if(typeof renderTrajectoryList==='function')renderTrajectoryList();}activeVertex=null;draggingVertex=null;brushing=false;brushPoints=[];brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();draft=[];if(typeof trajectoryDraft!=='undefined')trajectoryDraft=[];markAnnotationsDirty(eventName||'annotation_history');buildRoiList();updateButtons();draw();scheduleViewerStateSync(eventName||'annotation_history',{undo:annotationUndo.length,redo:annotationRedo.length,dirty:annotationsDirty,trajectory_count:typeof trajectories==='undefined'?0:trajectories.length});}\n",
    "function restoreAnnotationUndo(){if(!annotationUndo.length){notify('Nothing to undo','warning');return false;}pushHistory(annotationRedo,annotationSnapshot());const state=annotationUndo.pop();restoreAnnotationState(state,'annotation_undo');recordAnnotationHistory('annotation_undo',{undo:annotationUndo.length,redo:annotationRedo.length});notify('Undo applied','success');return true;}\n",
    "function restoreAnnotationRedo(){if(!annotationRedo.length){notify('Nothing to redo','warning');return false;}pushHistory(annotationUndo,annotationSnapshot());const state=annotationRedo.pop();restoreAnnotationState(state,'annotation_redo');recordAnnotationHistory('annotation_redo',{undo:annotationUndo.length,redo:annotationRedo.length});notify('Redo applied','success');return true;}\n",
    "function annotationLabelValue(){const input=el('roiLabelInput');return (nextRoiNameDirty&&input)?input.value.trim():'';}\n",
    "function clearNextAnnotationName(){const input=el('roiLabelInput');if(input)input.value='';activeRoiName='';nextRoiNameDirty=false;}\n",
    "function customCategoryValue(){const input=el('roiClassCustom');return input?input.value.trim():'';}\n",
    "function currentRoiClass(){const custom=customCategoryValue();if(custom)return custom;const select=el('roiClassSelect');return select&&select.value?select.value:(nextRoiClass||activeRoiClass||'annotation');}\n",
    "function setNextRoiClass(cls){const value=String(cls||'annotation').trim()||'annotation';nextRoiClass=value;activeRoiClass=value;ensureRoiClassOption(value);const select=el('roiClassSelect');if(select)setSelectValue(select,value);const panel=el('annotationClassSelect');if(panel&&(selectedRoi<0||mode==='brush'||brushing))setSelectValue(panel,value);return value;}\n",
    "function escapeRegExp(value){return String(value||'').replace(/[.*+?^${}()|[\\]\\\\]/g,'\\\\$&');}\n",
    "function automaticAnnotationName(className,labelPrefix='ROI',excludeIndex=-1){const raw=String(className||'').trim()||String(labelPrefix||'ROI').trim()||'ROI',base=raw.replace(/\\s+/g,' '),used=new Set(rois.map((roi,i)=>i===excludeIndex?'':String(roi.name||roi.label||'').toLowerCase()).filter(Boolean));let n=1,name=base+' '+n;while(used.has(name.toLowerCase())){n++;name=base+' '+n;}return name;}\n",
    "function nameMatchesClassPattern(name,className){const n=String(name||'').trim(),cls=String(className||'').trim().replace(/\\s+/g,' ');if(!n||!cls)return false;return new RegExp('^'+escapeRegExp(cls)+'\\\\s+\\\\d+$','i').test(n);}\n",
    "function annotationNameLooksAutomatic(name,className){const n=String(name||'').trim();return !n||nameMatchesClassPattern(n,className)||/^(roi|drawn roi|painted roi|annotation)\\s+\\d+$/i.test(n);}\n",
    "function roiUsesAutomaticName(roi,oldClass){const name=String((roi&&(roi.name||roi.label))||'').trim();return !name||roi&&roi.automatic_name===true||annotationNameLooksAutomatic(name,oldClass||roiClassName(roi));}\n",
    "function setRoiNameForClassAssignment(roi,selectedName,oldClass,newClass,index=selectedRoi){const supplied=String(selectedName||'').trim(),current=String((roi&&(roi.name||roi.label))||'').trim(),typed=supplied&&supplied!==current;if(typed){roi.name=supplied;roi.label=supplied;roi.automatic_name=false;return supplied;}if(!supplied||roiUsesAutomaticName(roi,oldClass)){const autoName=automaticAnnotationName(newClass,'ROI',index);roi.name=autoName;roi.label=autoName;roi.automatic_name=true;return autoName;}roi.name=supplied;roi.label=supplied;return supplied;}\n",
    "function ensureRoiClassOption(value){if(!value)return;const selects=(typeof classSelects==='function')?classSelects():[el('roiClassSelect')].filter(Boolean);selects.forEach(select=>addSelectOption(select,value));}\n",
    "function setRoiPanelOpen(open){const panel=el('roiPanel'),button=el('layersToggle');if(panel)panel.classList.toggle('open',!!open);if(button)button.classList.toggle('active',!!(panel&&panel.classList.contains('open')));if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleRoiPanel(){const panel=el('roiPanel');setRoiPanelOpen(!(panel&&panel.classList.contains('open')));}\n",
    "function setRoiPanelMinimized(minimized){const panel=el('roiPanel'),header=el('roiPanelHeader'),state=el('roiPanelMinimizeState');if(!panel)return;panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(state)state.textContent=minimized?'double-click to expand':'double-click to minimize';if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleRoiPanelMinimized(){const panel=el('roiPanel');if(!panel)return;if(!panel.classList.contains('open'))setRoiPanelOpen(true);setRoiPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function bindRoiPanelControls(){const header=el('roiPanelHeader'),close=el('roiPanelClose');if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.addEventListener('mousedown',startRoiPanelDrag);header.ondblclick=e=>{e.preventDefault();toggleRoiPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleRoiPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';close.onclick=e=>{e.preventDefault();e.stopPropagation();setRoiPanelOpen(false);notify('Annotation panel closed','info',1600);};}}\n",
    "function setSelectionText(id,value){const node=el(id);if(node)node.textContent=value;}\n",
    "function selectedRoiAreaValue(roi){if(!roi)return NaN;if(Number.isFinite(Number(roi.area)))return Number(roi.area);if(isDrawable(roi))return roiDrawGroups(roi).reduce((sum,g)=>sum+polygonArea(g.rings)-g.holes.reduce((s,h)=>s+ringArea(h),0),0);return NaN;}\n",
    "function formatSelectionArea(area){if(!Number.isFinite(area)||area<=0)return 'NA';const px=(typeof measurePixelSize==='function')?measurePixelSize():null;if(px){const mm2=area*px.x*px.y/1e6;return fmt(mm2,3)+' mm2';}return fmt(area,0)+' px2';}\n",
    "function roiIsCellLike(roi){if(!roi)return false;const source=String(roi.source||'').toLowerCase(),cls=String(roi.class||'').toLowerCase(),props=roi.properties||{},obj=String(props.objectType||props.object_type||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells'||obj==='detection';}\n",
    "function roiCentroidPoint(roi){if(!roi)return null;const c=roi.centroid||roi.center;if(c){const x=Number(c.x!=null?c.x:c[0]),y=Number(c.y!=null?c.y:c[1]);if(Number.isFinite(x)&&Number.isFinite(y))return {x:x,y:y};}const b=roiBounds(roi);return b?{x:(b.xmin+b.xmax)/2,y:(b.ymin+b.ymax)/2}:null;}\n",
    "function selectionCellCount(roi,index){if(!roi||!isDrawable(roi))return NaN;let n=0;rois.forEach((candidate,i)=>{if(i===index||!visibleRoi(candidate)||!roiIsCellLike(candidate))return;const p=roiCentroidPoint(candidate);if(p&&roiContainsPoint(roi,p))n++;});return n;}\n",
    "function formatSelectionDensity(cells,area){if(!Number.isFinite(cells)||!Number.isFinite(area)||area<=0)return 'NA';const px=(typeof measurePixelSize==='function')?measurePixelSize():null;if(px){const mm2=area*px.x*px.y/1e6;return mm2>0?(fmt(cells/mm2,1)+' cells/mm2'):'NA';}return fmt(cells/area*1e6,1)+' cells/Mpx';}\n",
    "function setSelectionCardVisible(show){const card=el('selectionCard'),button=el('selectionCardToggle');selectionCardVisible=!!(show&&selectedRoi>=0&&rois[selectedRoi]);if(card){card.classList.toggle('open',selectionCardVisible);card.setAttribute('aria-hidden',selectionCardVisible?'false':'true');}if(button)button.classList.toggle('active',selectionCardVisible);}\n",
    "function toggleSelectionCard(){if(selectedRoi<0||!rois[selectedRoi]){setSelectionCardVisible(false);notify('Select an ROI','warning');return;}setSelectionCardVisible(!selectionCardVisible);updateSelectionCard();}\n",
    "function updateSelectionCard(){const card=el('selectionCard'),roi=selectedRoi>=0?rois[selectedRoi]:null;if(!card)return;if(!roi){setSelectionCardVisible(false);return;}const area=selectedRoiAreaValue(roi),cells=selectionCellCount(roi,selectedRoi),sw=el('selectionCardSwatch'),del=el('selectionDelete'),edit=el('selectionEdit');if(sw)sw.style.background=roi.colour||'#cccccc';setSelectionText('selectionCardName',roiLabelText(roi,selectedRoi));setSelectionText('selectionCardClass',lockedRoi(roi)?((roi.class||'annotation')+' | locked'):(roi.class||'annotation'));setSelectionText('selectionCardArea',formatSelectionArea(area));setSelectionText('selectionCardCells',Number.isFinite(cells)?String(cells):'NA');setSelectionText('selectionCardDensity',formatSelectionDensity(cells,area));if(del)del.disabled=!editableRoi(roi);if(edit)edit.disabled=!editableRoi(roi)||!isDrawable(roi);setSelectionCardVisible(selectionCardVisible);}\n",
    "function bindSelectionCardControls(){const zoom=el('selectionZoom'),edit=el('selectionEdit'),del=el('selectionDelete'),close=el('selectionClose'),toggle=el('selectionCardToggle');if(zoom)zoom.onclick=()=>{if(selectedRoi>=0)centerRoi(selectedRoi);};if(edit)edit.onclick=()=>{if(selectedRoi>=0&&isDrawable(rois[selectedRoi])&&editableRoi(rois[selectedRoi]))setMode('edit');};if(del)del.onclick=()=>deleteSelectedRoi();if(close)close.onclick=()=>setSelectionCardVisible(false);if(toggle)toggle.onclick=toggleSelectionCard;}\n",
    "function classSelects(){return ['roiClassSelect','annotationClassSelect'].map(id=>el(id)).filter(Boolean);}\n",
    "function roiClassSelects(){return classSelects();}\n",
    "function addSelectOption(select,value){if(!select)return;if(value&&!Array.from(select.options).some(o=>o.value===value)){const opt=document.createElement('option');opt.value=value;opt.textContent=value;select.appendChild(opt);}}\n",
    "function setSelectValue(select,value){if(!select)return;addSelectOption(select,value);if(value)select.value=value;}\n",
    "function populateRoiClassSelects(){classSelects().forEach(select=>{const current=select.value||activeRoiClass||'';select.innerHTML='';roiClassPresets.forEach(p=>{const opt=document.createElement('option');opt.value=p.class;opt.textContent=p.label||p.class;select.appendChild(opt);});if(current)setSelectValue(select,current);else if(roiClassPresets.length)select.value=roiClassPresets[0].class;});}\n",
    "function recolourRoisForClass(cls,colour,markEdited=true){const key=classPresetKey(cls),c=normaliseHexColour(colour,'');if(!key||!c)return;rois.forEach(roi=>{if(classPresetKey(roi.class)===key)setRoiColour(roi,c,markEdited);});}\n",
    "function setClassColour(cls,colour,markEdited=true){const preset=ensureClassPreset(cls,colour);const c=normaliseHexColour(colour,preset.color||stableClassColour(cls));preset.color=c;recolourRoisForClass(preset.class,c,markEdited);return c;}\n",
    "function applyClassPresetColoursToRois(markEdited=false){rois.forEach(roi=>{if(!roi)return;const cls=roi.class||'annotation',seed=normaliseHexColour(roi.colour||roi.original_colour||'','');const colour=classColour(cls,seed);setRoiColour(roi,colour,markEdited);if(markEdited===false)roi.original_colour=colour;});}\n",
    "function setRoiControlsFromSelection(){const roi=selectedRoi>=0?rois[selectedRoi]:null;const has=!!roi;const panelName=el('annotationNameInput'),panelClass=el('annotationClassSelect'),panelCustom=el('annotationClassCustom'),panelColor=el('annotationColorInput'),vis=el('annotationVisible'),lock=el('annotationLock');if(!has){[panelName,panelCustom].forEach(x=>{if(x)x.value='';});if(panelClass&&(mode==='brush'||brushing))setSelectValue(panelClass,brushClass||nextRoiClass||activeRoiClass||'annotation');if(vis)vis.textContent='Hide';if(lock)lock.textContent='Lock';return;}const name=roi.name||roi.label||'',cls=roi.class||'annotation',displayCls=(brushing&&brushOperation!=='subtract')?(brushClass||nextRoiClass||activeRoiClass||cls):cls;if(panelName)panelName.value=name;if(panelCustom)panelCustom.value='';if(panelClass)setSelectValue(panelClass,displayCls);if(panelColor)panelColor.value=normaliseHexColour((displayCls===cls?roi.colour:'')||classColour(displayCls)||'#00BFC4');if(vis)vis.textContent=visibleRoi(roi)?'Hide':'Show';if(lock)lock.textContent=lockedRoi(roi)?'Unlock':'Lock';}\n",
    "function updateRoiList(){if(typeof enforceSingleObjectSelection==='function')enforceSingleObjectSelection('annotation');document.querySelectorAll('.roiItem').forEach(b=>{const i=Number(b.dataset.index),roi=rois[i],selected=roi&&!!roi.export_selected;b.classList.toggle('active',i===selectedRoi||selected);b.classList.toggle('hidden',roi&&roi.visible===false);b.classList.toggle('locked',lockedRoi(roi));const box=b.querySelector('.roiSelect');if(box)box.checked=!!selected;});setRoiControlsFromSelection();updateSelectionCard();updateButtons();syncRoiSelection();}\n",
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
    "function bindAnnotationListControls(){const search=el('annotationSearchInput'),filter=el('annotationFilter'),sort=el('annotationSort'),clear=el('annotationFilterClear');const rebuild=()=>buildRoiList();if(search){search.oninput=rebuild;search.onkeydown=e=>{if(e.key==='Escape'){e.preventDefault();clearAnnotationListFilters();}};}if(filter)filter.onchange=rebuild;if(sort)sort.onchange=rebuild;if(clear)clear.onclick=clearAnnotationListFilters;}\n",
    "function buildRoiList(){const list=el('roiList'),summary=el('roiSummary');if(!list||!summary)return;list.innerHTML='';const entries=currentRoiListEntries();summary.textContent=annotationListSummary(entries);entries.forEach(entry=>{const roi=entry.roi,i=entry.index;const item=document.createElement('div');item.className='roiItem';item.dataset.index=String(i);const top=document.createElement('div');top.className='roiTop';const exportBox=document.createElement('input');exportBox.type='checkbox';exportBox.className='roiSelect';exportBox.title='Select this annotation for export';exportBox.checked=!!roi.export_selected;exportBox.onclick=e=>e.stopPropagation();exportBox.onchange=e=>{roi.export_selected=!!e.target.checked;updateButtons();scheduleViewerStateSync('roi_export_selection_updated',{id:roi.id||null,selected:roi.export_selected});};const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour||'#cccccc';const nm=document.createElement('span');nm.className='roiName';nm.textContent=roiLabelText(roi,i);const cl=document.createElement('span');cl.className='roiClass';cl.textContent=(lockedRoi(roi)?'locked ':'')+(roi.class||'');top.append(exportBox,sw,nm,cl);const details=document.createElement('div');details.className='roiDetails';const bb=roiBounds(roi);addDetail(details,'Geometry',geometryType(roi));addDetail(details,'Bounds',formatBounds(bb),true);addDetail(details,'Points',String(pointCount(roi)));const area=entry.area;addDetail(details,'Area',Number.isFinite(area)?fmt(area,1):'NA');addDetail(details,'Source',roi.source||'geojson');addDetail(details,'ID',String(roi.id||i+1),true);if(!isDrawable(roi)){const badge=document.createElement('span');badge.className='roiBadge';badge.textContent='listed only';details.append(document.createElement('span'),badge);}const controls=document.createElement('div');controls.className='roiControls';const vis=document.createElement('button');vis.type='button';vis.textContent=visibleRoi(roi)?'Hide':'Show';vis.title='Toggle annotation visibility';vis.onclick=e=>{e.stopPropagation();toggleRoiVisibility(i);};const lock=document.createElement('button');lock.type='button';lock.textContent=lockedRoi(roi)?'Unlock':'Lock';lock.title='Lock or unlock annotation editing';lock.onclick=e=>{e.stopPropagation();toggleRoiLock(i);};const colour=document.createElement('input');colour.type='color';colour.value=normaliseHexColour(roi.colour||'#00BFC4');colour.title='Annotation color';colour.onclick=e=>e.stopPropagation();colour.onchange=e=>{e.stopPropagation();updateRoiColor(i,e.target.value);};const zoom=document.createElement('button');zoom.type='button';zoom.textContent='Zoom';zoom.title='Zoom to ROI';zoom.onclick=e=>{e.stopPropagation();centerRoi(i);};const dup=document.createElement('button');dup.type='button';dup.textContent='Dup';dup.title='Duplicate ROI';dup.onclick=e=>{e.stopPropagation();duplicateRoi(i);};const del=document.createElement('button');del.type='button';del.textContent='Del';del.title='Delete ROI';del.onclick=e=>{e.stopPropagation();deleteRoi(i);};controls.append(vis,lock,colour,document.createElement('span'),zoom,dup,del);item.append(top,details,controls);item.onclick=()=>{selectAnnotation(i,false);if(isDrawable(roi)){updateRoiList();draw();}else{updateRoiList();draw();notify('Geometry listed only','warning');}};list.appendChild(item);});if(!entries.length){const empty=document.createElement('div');empty.className='roiListEmpty';empty.textContent=rois.length?'No annotations match the current search or filter.':'No annotations yet.';list.appendChild(empty);}if(rois.length)setRoiPanelOpen(true);updateRoiList();}\n",
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
    "function addImportedGeojson(obj,fileName){const features=geojsonFeatures(obj);if(!features.length){geojsonImportStatus('No GeoJSON features found.');return;}pushAnnotationUndo('geojson_imported');let added=0,listed=0;features.forEach((feature,fi)=>{const parts=geojsonGeometryParts(feature.geometry||{});parts.forEach((part,pi)=>{const roi=importedRoiFromFeature(feature,part,added,fileName,parts.length);if(!roi.drawable&&(!roi.bbox||!Number.isFinite(Number(roi.bbox.xmin))))return;rois.push(roi);if(roi.drawable)added++;else listed++;});});if(!added&&!listed){geojsonImportStatus('No supported GeoJSON geometries found.');return;}selectAnnotation(rois.length-1,false);showRois=true;buildRoiList();updateButtons();draw();recordAnnotationHistory('geojson_imported',{file:fileName||null,added:added,listed:listed});scheduleViewerStateSync('geojson_imported',{file:fileName||null,added:added,listed:listed});geojsonImportStatus('Imported '+added+' drawable ROI'+(added===1?'':'s')+(listed?(' and listed '+listed+' other geometr'+(listed===1?'y':'ies')):'' )+'.');}\n",
    "function bindGeojsonImportControls(){const button=el('importGeojson'),file=el('geojsonImportFile');if(button&&file)button.onclick=()=>{file.value='';file.click();};if(file){file.onchange=()=>{const picked=file.files&&file.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addImportedGeojson(JSON.parse(reader.result),picked.name);}catch(e){geojsonImportStatus('Could not import GeoJSON: '+e.message);}};reader.readAsText(picked);};}geojsonImportStatus('');}\n",
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
    "function simplifyTolerance(){const input=el('simplifyTolerance');return input?Number(input.value||12):12;}\n",
    "function checkedAnnotationIndices(){const out=[];rois.forEach((roi,i)=>{if(roi.export_selected)out.push(i);});return out;}\n",
    "function selectedEditableDrawableRoi(){return selectedRoi>=0&&rois[selectedRoi]&&isDrawable(rois[selectedRoi])&&editableRoi(rois[selectedRoi])?rois[selectedRoi]:null;}\n",
    "function smoothSelectedRoi(){const roi=selectedEditableDrawableRoi();if(!roi){notify('Select unlocked ROI','warning');return;}pushAnnotationUndo('roi_smoothed');transformRoiRings(roi,pts=>chaikinSmoothPoints(pts,2));enforceRoiNonOverlap(selectedRoi);buildRoiList();draw();recordAnnotationHistory('roi_smoothed',{id:roi.id||null,name:roiLabelText(roi,selectedRoi),non_overlapping:true});scheduleViewerStateSync('roi_smoothed',{id:roi.id||null,non_overlapping:true});notify('ROI smoothed','success');}\n",
    "function simplifySelectedRoi(){const roi=selectedEditableDrawableRoi();if(!roi){notify('Select unlocked ROI','warning');return;}const tol=simplifyTolerance();pushAnnotationUndo('roi_simplified');transformRoiRings(roi,pts=>simplifyClosedPoints(pts,tol));enforceRoiNonOverlap(selectedRoi);buildRoiList();draw();recordAnnotationHistory('roi_simplified',{id:roi.id||null,name:roiLabelText(roi,selectedRoi),tolerance:tol,non_overlapping:true});scheduleViewerStateSync('roi_simplified',{id:roi.id||null,tolerance:tol,non_overlapping:true});notify('ROI simplified','success');}\n",
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
    "function addRoiFromBrushGroups(groups,source,labelPrefix,className=null){activeRoiClass=String(className||currentRoiClass()||'annotation').trim()||'annotation';nextRoiClass=activeRoiClass;activeRoiName=source==='brush'?'':annotationLabelValue();const clipped=clipBrushGroupsAgainstAnnotations(groups,-1,activeRoiClass),merged=unionBrushGroupsWithSameLabel(clipped.groups,activeRoiClass,-1),clean=merged.groups,all=brushGroupRings(clean);if(!clean.length||!all.length){notify('Annotation overlaps a different label; no new area to add','warning');return null;}const colour=classColour(activeRoiClass);pushAnnotationUndo(merged.merged?'roi_same_label_merged':'roi_added');if(merged.merged&&merged.merged_indices.length){const target=rois[merged.merged_indices[0]];setRoiPositiveGroups(target,clean);target.subtract_rings=[];target.class=activeRoiClass;if(activeRoiName){target.name=activeRoiName;target.label=activeRoiName;}else if(!(target.name||target.label)){const autoName=automaticAnnotationName(activeRoiClass,labelPrefix);target.name=autoName;target.label=autoName;}setRoiColour(target,colour,true);target.same_label_merged=true;target.non_overlapping=true;target.overlap_clipped=!!clipped.clipped;refreshRoiGeometry(target);removeMergedSameLabelAnnotations(merged.merged_indices,target);showRois=true;buildRoiList();updateButtons();recordAnnotationHistory('roi_same_label_merged',{source:source,id:target.id||null,name:roiLabelText(target,selectedRoi),class:target.class,color:colour,merged_count:merged.merged_indices.length,overlap_clipped:!!clipped.clipped,non_overlapping:true});scheduleViewerStateSync('roi_same_label_merged',{id:target.id||null,class:target.class,merged_count:merged.merged_indices.length,overlap_clipped:!!clipped.clipped,non_overlapping:true});notify(clipped.clipped?'ROI merged with same label; different-label overlap clipped':'ROI merged with same label','success');draw();return target;}newRoiCount++;const roiName=activeRoiName||automaticAnnotationName(activeRoiClass,labelPrefix);const roi={id:source+'_roi_'+newRoiCount,name:roiName,label:roiName,class:activeRoiClass,visible:true,locked:false,isLocked:false,geometry_type:clean.length>1?'MultiPolygon':'Polygon',source:source,drawable:true,point_count:all.reduce((n,r)=>n+r.length-1,0),area:clean.reduce((s,g)=>s+polygonArea(g),0),bbox:boundsFromRings(all),colour:colour,original_colour:colour,fill:hexToRgba(colour,0.18),rings:clean[0],add_groups:clean.slice(1),add_rings:[],subtract_rings:[],drawn:source==='drawn',brushed:source==='brush',brush_mask_contour:true,brush_ring_count:all.length,non_overlapping:true,overlap_clipped:!!clipped.clipped,automatic_name:!activeRoiName};rois.push(roi);selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();recordAnnotationHistory('roi_added',{source:source,id:roi.id,name:roi.name,class:roi.class,color:colour,brush_ring_count:all.length,brush_mask_contour:true,non_overlapping:true,overlap_clipped:!!clipped.clipped,automatic_name:!activeRoiName});scheduleViewerStateSync('roi_added',{source:source,id:roi.id,class:roi.class,name:roi.name,color:colour,brush_ring_count:all.length,brush_mask_contour:true,non_overlapping:true,overlap_clipped:!!clipped.clipped,automatic_name:!activeRoiName});notify(clipped.clipped?'ROI saved; different-label overlap clipped':'ROI saved','success');return roi;}\n",
    "function addRoiFromBrushRings(rings,source,labelPrefix,className=null){return addRoiFromBrushGroups(ringsToBrushGroups(rings),source,labelPrefix,className);}\n",
    "function finishDraft(){if(draft.length<3){notify('Add at least 3 points','warning');return;}const ring=closedRing(draft);addRoiFromRing(ring,'drawn','Drawn ROI',currentRoiClass());draft=[];setMode('select');draw();}\n",
    "function brushPointSpacing(radius){return Math.max(1,Math.min(8,radius*.08));}\n",
    "function densifyBrushPoints(points,radius){const raw=points.filter(pointInsideSlide);if(raw.length<2)return raw;const spacing=brushPointSpacing(radius),out=[raw[0]];for(let i=1;i<raw.length;i++){const from=out[out.length-1],to=raw[i],dx=to.x-from.x,dy=to.y-from.y,dist=Math.hypot(dx,dy);if(!dist)continue;const steps=Math.max(1,Math.ceil(dist/spacing));for(let s=1;s<=steps;s++){out.push({x:from.x+dx*s/steps,y:from.y+dy*s/steps});}}return out;}\n",
    "function brushCircleRing(p,radius,steps=96){const ring=[];for(let i=0;i<steps;i++){const a=i/steps*Math.PI*2;ring.push({x:p.x+Math.cos(a)*radius,y:p.y+Math.sin(a)*radius});}return closedRing(ring);}\n",
    "function brushRingFromPoints(points,radius){const pts=densifyBrushPoints(points,radius);if(!pts.length)return [];if(pts.length===1)return brushCircleRing(pts[0],radius);const left=[],right=[];for(let i=0;i<pts.length;i++){const prev=pts[Math.max(0,i-1)],next=pts[Math.min(pts.length-1,i+1)],dx=next.x-prev.x,dy=next.y-prev.y,len=Math.hypot(dx,dy)||1,nx=-dy/len,ny=dx/len;left.push({x:pts[i].x+nx*radius,y:pts[i].y+ny*radius});right.push({x:pts[i].x-nx*radius,y:pts[i].y-ny*radius});}return closedRing(left.concat(right.reverse()));}\n",
    "function brushGeometryPathPoints(points,radius){const raw=(points||[]).filter(pointInsideSlide);if(raw.length<2)return raw;const spacing=Math.max(2,Math.min(32,Number(radius||0)*.25)),out=[raw[0]];for(let i=1;i<raw.length;i++){const last=out[out.length-1],p=raw[i];if(Math.hypot(p.x-last.x,p.y-last.y)>=spacing)out.push(p);}const final=raw[raw.length-1],last=out[out.length-1];if(final&&last&&Math.hypot(final.x-last.x,final.y-last.y)>1e-6)out.push(final);return out;}\n",
    "function brushCapsuleRing(a,b,radius,steps=12){const dx=b.x-a.x,dy=b.y-a.y,len=Math.hypot(dx,dy);if(!Number.isFinite(len)||len<1e-6)return brushCircleRing(a,radius);const theta=Math.atan2(dy,dx),ring=[];ring.push({x:a.x+Math.cos(theta+Math.PI/2)*radius,y:a.y+Math.sin(theta+Math.PI/2)*radius});for(let i=0;i<=steps;i++){const ang=theta+Math.PI/2-i*Math.PI/steps;ring.push({x:b.x+Math.cos(ang)*radius,y:b.y+Math.sin(ang)*radius});}ring.push({x:a.x+Math.cos(theta-Math.PI/2)*radius,y:a.y+Math.sin(theta-Math.PI/2)*radius});for(let i=0;i<=steps;i++){const ang=theta-Math.PI/2-i*Math.PI/steps;ring.push({x:a.x+Math.cos(ang)*radius,y:a.y+Math.sin(ang)*radius});}return closedRing(ring);}\n",
    "function additiveBrushRingsFromPoints(points,radius){const geom=brushMaskGeometry(points,radius,null,'new');return geom?geom.groups.map(g=>g[0]).filter(Boolean):[];}\n",
    "function brushSlideUnitScale(){try{const px=(typeof slideUnitScale==='function')?slideUnitScale():1;return Number.isFinite(px)&&px>.00011?px:1;}catch(e){return 1;}}\n",
    "function brushEffectiveRadius(screenRadius=brushScreenRadius){const base=Number(screenRadius);const size=Number.isFinite(base)?base:80;return clamp(size/brushSlideUnitScale(),1,Math.max(cfg.slide_width,cfg.slide_height));}\n",
    "function syncBrushRadiusToZoom(){brushScreenRadius=clamp(Number(brushScreenRadius)||80,8,240);brushRadius=brushEffectiveRadius(brushScreenRadius);const label=el('brushSizeValue'),hint=el('brushZoomHint');if(label)label.textContent=Math.round(brushScreenRadius)+' px';if(hint)hint.textContent='effective '+Math.round(brushRadius)+' slide px at current zoom';return brushRadius;}\n",
    "function brushRadiusValue(){return syncBrushRadiusToZoom();}\n",
    "function updateBrushControls(){const input=el('brushSize'),simp=el('simplifyTolerance'),simpLabel=el('simplifyToleranceValue');if(input)brushScreenRadius=Number(input.value||80);syncBrushRadiusToZoom();if(simp&&simpLabel)simpLabel.textContent=Math.round(Number(simp.value||12))+' px';}\n",
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
    "function brushMaskPixelSize(bounds,radius){if(!bounds)return 1;const maxDim=Math.max(1,bounds.xmax-bounds.xmin,bounds.ymax-bounds.ymin);let px=Math.max(1,Math.ceil(maxDim/2200));if(maxDim<12000&&Number(radius)>0)px=Math.min(px,Math.max(1,Math.ceil(Number(radius)/12)));return Math.max(1,Math.min(128,px));}\n",
    "function brushMaskFrame(bounds,radius){let px=brushMaskPixelSize(bounds,radius),origin,xmax,ymax,w,h;for(let guard=0;guard<8;guard++){origin={x:Math.max(0,Math.floor(bounds.xmin)-px),y:Math.max(0,Math.floor(bounds.ymin)-px)};xmax=Math.min(cfg.slide_width,Math.ceil(bounds.xmax)+px);ymax=Math.min(cfg.slide_height,Math.ceil(bounds.ymax)+px);w=Math.max(2,Math.ceil((xmax-origin.x)/px)+2);h=Math.max(2,Math.ceil((ymax-origin.y)/px)+2);if(w*h<=6000000||px>=128)break;px*=2;}return {origin:origin,pixel:px,width:w,height:h};}\n",
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
    "function maskRingToSlide(ring,frame){const pts=ring.map(p=>({x:frame.origin.x+p.x*frame.pixel,y:frame.origin.y+p.y*frame.pixel}));let clean=removeCollinearRingPoints(closedRing(pts));const tol=Math.max(.5,Math.min(8,frame.pixel*1.25));clean=simplifyClosedPoints(clean,tol);return closedRing(clean);}\n",
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
    "function finishBrush(){if(!brushing)return;syncBrushRadiusToZoom();updateBrushSelectionFromStroke();brushing=false;const pts=brushPoints.slice(),op=brushOperation,target=brushTargetRoi,cls=brushClass||currentRoiClass();let geometry=null;if(op==='subtract'&&target>=0)geometry=brushMaskGeometry(pts,brushRadius,rois[target],'subtract',[]);else geometry=brushMaskGeometry(pts,brushRadius,null,'new',brushProtectionForClass(cls,-1));brushPoints=[];brushOperation='new';brushTargetRoi=-1;if(geometry&&geometry.groups&&geometry.groups.length){if(op==='subtract'&&target>=0&&subtractSelectedRoiWithBrush(geometry.groups,target)){brushClass='';brushTouchedSelection=new Set();brushAdditiveSelection=false;updateCursorFeedback();draw();return;}addRoiFromBrushGroups(geometry.groups,'brush','Painted ROI',cls);brushClass='';updateRoiList();updateButtons();}else if(op==='subtract'&&target>=0){brushClass='';notify('Brush removed all contour pixels; ROI left unchanged','warning');}brushClass='';brushTouchedSelection=new Set();brushAdditiveSelection=false;updateCursorFeedback();draw();}\n",
    "function slideUnitScale(){const a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:1,y:0});return Math.max(.0001,Math.hypot(b.x-a.x,b.y-a.y));}\n",
    "function drawBrushGlyph(q,r,state){ctx.save();ctx.lineWidth=2;ctx.lineCap='round';ctx.strokeStyle=state.blocked?'#ef4444':(state.subtract?'#ef4444':'#22c55e');ctx.beginPath();ctx.moveTo(q.x-r,q.y);ctx.lineTo(q.x+r,q.y);ctx.stroke();if(state.blocked){ctx.beginPath();ctx.moveTo(q.x-r*.72,q.y-r*.72);ctx.lineTo(q.x+r*.72,q.y+r*.72);ctx.moveTo(q.x+r*.72,q.y-r*.72);ctx.lineTo(q.x-r*.72,q.y+r*.72);ctx.stroke();}else if(!state.subtract){ctx.beginPath();ctx.moveTo(q.x,q.y-r);ctx.lineTo(q.x,q.y+r);ctx.stroke();}ctx.restore();}\n",
    "function drawBrushPreview(){if(mode!=='brush')return;syncBrushRadiusToZoom();const px=slideUnitScale(),state=brushCursorState(),remove=state.subtract,blocked=state.blocked;ctx.save();ctx.strokeStyle=blocked?'rgba(239,68,68,.95)':(remove?'rgba(248,113,113,.9)':'rgba(34,197,94,.88)');ctx.fillStyle=blocked?'rgba(239,68,68,.10)':(remove?'rgba(248,113,113,.2)':'rgba(34,197,94,.16)');ctx.lineWidth=Math.max(1,brushRadius*2*px);ctx.lineCap='round';ctx.lineJoin='round';if(brushPoints.length&&!blocked){ctx.beginPath();brushPoints.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.stroke();}if(lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer),glyph=Math.max(6,Math.min(14,brushRadius*px*.45));ctx.beginPath();ctx.arc(q.x,q.y,Math.max(2,brushRadius*px),0,Math.PI*2);ctx.fill();ctx.strokeStyle=blocked?'#ef4444':(remove?'#ef4444':'#22c55e');ctx.lineWidth=1.5;ctx.stroke();drawBrushGlyph(q,glyph,state);}ctx.restore();}\n",
    "function canvasPoint(clientX,clientY){const rect=canvas.getBoundingClientRect();return {x:clientX-rect.left,y:clientY-rect.top};}\n",
    "function ringClosed(ring){if(!ring||ring.length<2)return false;const f=ring[0],l=ring[ring.length-1];return f.x===l.x&&f.y===l.y;}\n",
    "function findVertexAt(clientX,clientY){const c=canvasPoint(clientX,clientY),order=selectedRoi>=0?[selectedRoi]:rois.map((_,i)=>i);for(const ri of order){const roi=rois[ri];if(!isDrawable(roi)||!editableRoi(roi))continue;for(let r=0;r<roi.rings.length;r++){const ring=roi.rings[r],limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=slideToCanvas(ring[j]);if(Math.hypot(q.x-c.x,q.y-c.y)<=9)return {roi:ri,ring:r,point:j};}}}return null;}\n",
    "function moveActiveVertex(p){if(!activeVertex||!pointInsideSlide(p))return;const roi=rois[activeVertex.roi],ring=roi&&roi.rings?roi.rings[activeVertex.ring]:null;if(!ring||!editableRoi(roi))return;const closed=ringClosed(ring),pt={x:Math.round(p.x),y:Math.round(p.y)};ring[activeVertex.point]=pt;if(activeVertex.point===0&&closed)ring[ring.length-1]={x:pt.x,y:pt.y};selectAnnotation(activeVertex.roi,false);refreshRoiGeometry(roi);scheduleViewerStateSync('roi_edited',{id:roi.id||null});draw();}\n",
    "function drawEditHandles(){if(mode!=='edit'||selectedRoi<0||!isDrawable(rois[selectedRoi])||!editableRoi(rois[selectedRoi]))return;const roi=rois[selectedRoi];ctx.save();roi.rings.forEach((ring,r)=>{const limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=slideToCanvas(ring[j]),active=activeVertex&&activeVertex.roi===selectedRoi&&activeVertex.ring===r&&activeVertex.point===j;ctx.beginPath();ctx.arc(q.x,q.y,active?6:4,0,Math.PI*2);ctx.fillStyle=active?'#facc15':'#ffffff';ctx.strokeStyle='#111';ctx.lineWidth=2;ctx.fill();ctx.stroke();}});ctx.restore();}\n",
    "function segmentDistance(c,a,b){const dx=b.x-a.x,dy=b.y-a.y,len2=dx*dx+dy*dy;if(!len2)return Math.hypot(c.x-a.x,c.y-a.y);let t=((c.x-a.x)*dx+(c.y-a.y)*dy)/len2;t=clamp(t,0,1);return Math.hypot(c.x-(a.x+t*dx),c.y-(a.y+t*dy));}\n",
    "function insertVertexAt(p,clientX,clientY){if(selectedRoi<0||!isDrawable(rois[selectedRoi])||!editableRoi(rois[selectedRoi])||!pointInsideSlide(p))return false;const c=canvasPoint(clientX,clientY),roi=rois[selectedRoi];let best=null;roi.rings.forEach((ring,r)=>{const limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const a=slideToCanvas(ring[j]),b=slideToCanvas(ring[(j+1)%ring.length]),d=segmentDistance(c,a,b);if(!best||d<best.d)best={d:d,ring:r,after:j};}});if(!best||best.d>14)return false;pushAnnotationUndo('roi_vertex_inserted');const ring=roi.rings[best.ring];ring.splice(best.after+1,0,{x:Math.round(p.x),y:Math.round(p.y)});activeVertex={roi:selectedRoi,ring:best.ring,point:best.after+1};refreshRoiGeometry(roi);buildRoiList();draw();return true;}\n",
    "function deleteSelectedVertex(){if(!activeVertex)return false;const roi=rois[activeVertex.roi],ring=roi&&roi.rings?roi.rings[activeVertex.ring]:null;if(!ring||!editableRoi(roi)){notify('ROI locked','warning');return false;}const closed=ringClosed(ring),limit=closed?ring.length-1:ring.length;if(limit<=3){notify('Keep at least 3 vertices','warning');return false;}pushAnnotationUndo('roi_vertex_deleted');ring.splice(activeVertex.point,1);if(activeVertex.point===0&&closed)ring[ring.length-1]={x:ring[0].x,y:ring[0].y};activeVertex=null;refreshRoiGeometry(roi);buildRoiList();scheduleViewerStateSync('roi_edited',{id:roi.id||null});draw();return true;}\n",
    "function deleteRoi(index=selectedRoi){const i=Number(index);if(i<0||!rois[i]){notify('Select an ROI','warning');return;}if(lockedRoi(rois[i])){notify('ROI locked','warning');return;}const deleteLabel=roiLabelText(rois[i],i);pushAnnotationUndo('roi_deleted');const removed=rois.splice(i,1)[0];selectedRoi=Math.min(i,rois.length-1);activeVertex=null;buildRoiList();updateButtons();draw();recordAnnotationHistory('roi_deleted',{id:removed.id||null,name:deleteLabel});scheduleViewerStateSync('roi_deleted',{id:removed.id||null});notifyAction('Deleted '+deleteLabel+'.','Undo',()=>restoreAnnotationUndo(),'success',7000);}\n",
    "function deleteSelectedRoi(){deleteRoi(selectedRoi);}\n",
    "function updateRoiColor(index,colour){const i=Number(index),roi=rois[i];if(!roi)return;if(lockedRoi(roi)){notify('ROI locked','warning');buildRoiList();return;}pushAnnotationUndo('roi_color_updated');const cls=roi.class||'annotation',c=setClassColour(cls,colour,true);selectedRoi=i;buildRoiList();draw();recordAnnotationHistory('roi_color_updated',{id:roi.id||null,name:roiLabelText(roi,i),class:cls,color:c});scheduleViewerStateSync('roi_color_updated',{id:roi.id||null,class:cls,color:c,class_presets:roiClassPresets});notify('Class color updated','success');}\n",
    "function toggleRoiVisibility(index){const i=Number(index),roi=rois[i];if(!roi)return;pushAnnotationUndo('roi_visibility_updated');roi.visible=!visibleRoi(roi);selectedRoi=i;buildRoiList();draw();recordAnnotationHistory('roi_visibility_updated',{id:roi.id||null,name:roiLabelText(roi,i),visible:visibleRoi(roi)});scheduleViewerStateSync('roi_visibility_updated',{id:roi.id||null,visible:visibleRoi(roi)});notify(visibleRoi(roi)?'ROI shown':'ROI hidden','success');}\n",
    "function toggleRoiLock(index){const i=Number(index),roi=rois[i];if(!roi)return;pushAnnotationUndo('roi_lock_updated');roi.locked=!lockedRoi(roi);roi.isLocked=roi.locked;selectedRoi=i;buildRoiList();draw();recordAnnotationHistory('roi_lock_updated',{id:roi.id||null,name:roiLabelText(roi,i),locked:lockedRoi(roi)});scheduleViewerStateSync('roi_lock_updated',{id:roi.id||null,locked:lockedRoi(roi)});notify(lockedRoi(roi)?'ROI locked':'ROI unlocked','success');}\n",
    "function duplicateRoi(index=selectedRoi){const i=Number(index),roi=rois[i];if(!roi){notify('Select an ROI','warning');return;}pushAnnotationUndo('roi_duplicated');newRoiCount++;const clone=JSON.parse(JSON.stringify(roi));clone.id=String(roi.id||('roi_'+(i+1)))+'_copy_'+newRoiCount;clone.name=(roi.name||roi.label||clone.id)+' copy';clone.label=clone.name;clone.locked=false;clone.isLocked=false;clone.visible=visibleRoi(roi);clone.export_selected=false;clone.edited=true;rois.splice(i+1,0,clone);selectedRoi=i+1;buildRoiList();draw();recordAnnotationHistory('roi_duplicated',{source_id:roi.id||null,id:clone.id,name:clone.name});scheduleViewerStateSync('roi_duplicated',{source_id:roi.id||null,id:clone.id});notify('ROI duplicated','success');}\n",
    "function exportSelectedAnnotations(){const indices=roiExportIndices();if(!indices.length){notify(respectClassExportRules()?'No selected ROIs pass class export rules':'Select ROIs to export','warning');return;}const features=indices.map(i=>roiFeature(rois[i],i)).filter(Boolean);if(!features.length){notify('No exportable ROI geometry','warning');return;}const allFeatures=exportableRoiFeatures();const name=(cfg.annotation_filename||'wsiTools_annotations.geojson').replace(/\\.geojson$/i,'')+'_selected.geojson';downloadText(JSON.stringify({type:'FeatureCollection',features:features},null,2),name);if(features.length>=allFeatures.length)markAnnotationsSaved('geojson_exported');scheduleViewerStateSync('roi_exported',{count:features.length,dirty:annotationsDirty,respect_export_rules:respectClassExportRules()});notify('GeoJSON exported','success');}\n",
    "function selectAllAnnotations(){const entries=(typeof currentRoiListEntries==='function')?currentRoiListEntries():rois.map((roi,i)=>({roi:roi,index:i}));entries.forEach(entry=>{if(rois[entry.index])rois[entry.index].export_selected=true;});buildRoiList();updateButtons();}\n",
    "function selectNoAnnotations(){const entries=(typeof currentRoiListEntries==='function')?currentRoiListEntries():rois.map((roi,i)=>({roi:roi,index:i}));entries.forEach(entry=>{if(rois[entry.index])rois[entry.index].export_selected=false;});selectedRoi=-1;activeVertex=null;draggingVertex=null;brushTargetRoi=-1;brushClass='';brushOperation='new';brushTouchedSelection=new Set();buildRoiList();updateButtons();scheduleViewerStateSync('roi_deselected',{reason:'annotation_panel'});notify('Annotations deselected','info');draw();}\n",
    "function ringCoordinates(r){const ring=(r||[]).map(p=>[Math.round(p.x),Math.round(p.y)]);if(ring.length){const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);}return ring;}\n",
    "function roiCompositeGeometry(roi){const groups=roiDrawGroups(roi).map(g=>g.rings.concat(g.holes).map(ringCoordinates)).filter(g=>g.length&&g[0].length>=4);if(!groups.length)return null;if(groups.length===1)return {type:'Polygon',coordinates:groups[0]};return {type:'MultiPolygon',coordinates:groups};}\n",
    "function roiFeature(roi,i){let geometry=null;if(isDrawable(roi)&&(roi.edited||roi.drawn||roi.brushed||roi.brush_edited||roi.add_rings||roi.subtract_rings||!roi.coordinates)){geometry=roiCompositeGeometry(roi);}else if(roi.coordinates){geometry={type:geometryType(roi),coordinates:roi.coordinates};}if(!geometry)return null;const name=roiLabelText(roi,i),cls=roi.class||'annotation',preset=ensureClassPreset(cls,roi.colour||''),colour=classColour(cls,roi.colour||'#00BFC4'),originalColour=normaliseHexColour(roi.original_colour||'',''),props=clonePlain(roi.properties||{});let classification=(props.classification&&typeof props.classification==='object'&&!Array.isArray(props.classification))?clonePlain(props.classification):{};classification.name=cls;const colourChanged=!!(originalColour&&colour&&colour!==originalColour);if(colourChanged){classification.color=colour;delete classification.colorRGB;delete classification.color_rgb;delete classification.colour;}else if(colour&&!classification.color&&!classification.colorRGB)classification.color=colour;else if(colour)classification.color=colour;props.objectType=props.objectType||(roi.source==='stardist'?'detection':'annotation');props.name=name;props.label=name;props.classification=classification;props.class=cls;props.isLocked=lockedRoi(roi);props.visible=visibleRoi(roi);props.wsiTools=Object.assign({},clonePlain(props.wsiTools||{}),{classPreset:preset?preset.class:cls,export:classPresetExportable(cls),exportRule:classPresetExportRule(cls)});if(roi.measurements)props.measurements=roi.measurements;if(roi.centroid)props.centroid=roi.centroid;if(roi.source_file)props.source_file=roi.source_file;delete props.export_selected;const feature=clonePlain(roi.feature||{});feature.type='Feature';feature.id=roi.id||('roi_'+(i+1));feature.properties=props;feature.geometry=geometry;const b=roiBounds(roi);if(b&&Number.isFinite(Number(b.xmin)))feature.bbox=[b.xmin,b.ymin,b.xmax,b.ymax];return feature;}\n",
    "function exportableRoiFeatures(){return rois.map((roi,i)=>roiAllowedByExportRules(roi)?roiFeature(roi,i):null).filter(Boolean);}\n",
    "function geojsonText(){if(brushing)finishBrush();if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:exportableRoiFeatures()},null,2);}\n",
    "function annotationPanelNameValue(){const input=el('annotationNameInput');return input?input.value.trim():'';}\n",
    "function annotationPanelClassValue(){const custom=el('annotationClassCustom');if(custom&&custom.value.trim())return custom.value.trim();const select=el('annotationClassSelect');return select&&select.value?select.value:(activeRoiClass||'annotation');}\n",
    "function applySelectedRoiMetadata(name,cls,colour){if(selectedRoi<0||!rois[selectedRoi]){notify('Select an ROI','warning');return;}const roi=rois[selectedRoi];const oldName=roi.name||roi.label||roi.id||'ROI',oldClass=roi.class||'annotation',selectedClass=cls||'annotation',selectedName=name||'';pushAnnotationUndo('roi_metadata_updated');ensureRoiClassOption(selectedClass);setSelectValue(el('annotationClassSelect'),selectedClass);roi.class=selectedClass;const newName=setRoiNameForClassAssignment(roi,selectedName,oldClass,selectedClass,selectedRoi);const explicitColour=normaliseHexColour(colour||'',''),classColor=explicitColour?setClassColour(selectedClass,explicitColour,true):classColour(selectedClass);setRoiColour(roi,classColor,true);roi.edited=true;buildRoiList();recordAnnotationHistory(newName!==oldName?'roi_renamed':'roi_metadata_updated',{id:roi.id||null,name:roi.name||null,old_name:oldName,class:roi.class||null,old_class:oldClass,color:roi.colour||null,automatic_name:!!roi.automatic_name,locked:lockedRoi(roi)});scheduleViewerStateSync('roi_updated',{id:roi.id||null,class:roi.class||null,name:roi.name||null,color:roi.colour||null,automatic_name:!!roi.automatic_name,locked:lockedRoi(roi),class_presets:roiClassPresets});notify(lockedRoi(roi)?'ROI metadata updated; shape remains locked':'ROI updated','success');draw();}\n",
    "function applySelectedRoiClass(){const cls=setNextRoiClass(currentRoiClass());activeRoiName=annotationLabelValue();saveRoiClassPreference();notify('Next annotation class: '+cls,'success',1800);}\n",
    "function applyAnnotationPanelMetadata(){applySelectedRoiMetadata(annotationPanelNameValue(),annotationPanelClassValue(),el('annotationColorInput')?el('annotationColorInput').value:null);}\n",
    "function bindRoiClassControls(){applyClassPresetColoursToRois(false);populateRoiClassSelects();const select=el('roiClassSelect'),input=el('roiLabelInput'),custom=el('roiClassCustom'),brush=el('brushSize'),simplify=el('simplifyTolerance'),panelSelect=el('annotationClassSelect'),panelName=el('annotationNameInput'),panelCustom=el('annotationClassCustom'),panelColor=el('annotationColorInput');const syncPanelColour=cls=>{const c=classColour(cls);if(c&&panelColor)panelColor.value=c;};if(select){setNextRoiClass(select.value||nextRoiClass||activeRoiClass);select.onchange=e=>{const cls=setNextRoiClass(e.target.value);if(custom)custom.value='';clearNextAnnotationName();saveRoiClassPreference();notify('Next annotation class: '+cls,'info',1600);};}if(panelSelect){panelSelect.onchange=e=>{if(panelCustom)panelCustom.value='';if(custom)custom.value='';const cls=setNextRoiClass(annotationPanelClassValue());syncPanelColour(cls);clearNextAnnotationName();saveRoiClassPreference();notify('Next annotation class: '+cls+'. Click Apply only to update selected ROI.','info',2200);};}if(input){activeRoiName=input.value;nextRoiNameDirty=!!input.value.trim();input.oninput=e=>{activeRoiName=e.target.value;nextRoiNameDirty=!!e.target.value.trim();};input.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();nextRoiNameDirty=!!input.value.trim();notify(nextRoiNameDirty?'Name ready for next drawn ROI':'Next ROI will use automatic class name','info',1600);}};}if(panelName){panelName.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();notify('Click Apply to update selected annotation','info',1600);}};}if(custom){custom.oninput=e=>{const cls=e.target.value.trim();if(cls){nextRoiClass=cls;activeRoiClass=cls;clearNextAnnotationName();}saveRoiClassPreference();};custom.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();const cls=setNextRoiClass(currentRoiClass());notify('Next annotation class: '+cls,'info',1600);}};}if(panelCustom){panelCustom.oninput=e=>{const cls=annotationPanelClassValue();if(cls){nextRoiClass=cls;activeRoiClass=cls;if(custom)custom.value=cls;syncPanelColour(cls);saveRoiClassPreference();}};panelCustom.onkeydown=e=>{if(e.key==='Enter'){e.preventDefault();const cls=setNextRoiClass(annotationPanelClassValue());notify('Next annotation class: '+cls+'. Click Apply only to update selected ROI.','info',2200);}};}if(panelColor){panelColor.onchange=()=>notify('Click Apply to update selected annotation','info',1600);}if(brush){brush.oninput=()=>{updateBrushControls();saveBrushPreference();draw();};}if(simplify){simplify.oninput=()=>updateBrushControls();}updateBrushControls();const apply=el('applyRoiClass');if(apply)apply.onclick=applySelectedRoiClass;const panelApply=el('annotationApply');if(panelApply)panelApply.onclick=applyAnnotationPanelMetadata;const del=el('deleteRoi');if(del)del.onclick=deleteSelectedRoi;const panelDelete=el('annotationDelete');if(panelDelete)panelDelete.onclick=deleteSelectedRoi;const panelVisible=el('annotationVisible');if(panelVisible)panelVisible.onclick=()=>{if(selectedRoi>=0)toggleRoiVisibility(selectedRoi);};const panelLock=el('annotationLock');if(panelLock)panelLock.onclick=()=>{if(selectedRoi>=0)toggleRoiLock(selectedRoi);};const panelZoom=el('annotationZoom');if(panelZoom)panelZoom.onclick=()=>{if(selectedRoi>=0)centerRoi(selectedRoi);};const panelDuplicate=el('annotationDuplicate');if(panelDuplicate)panelDuplicate.onclick=()=>duplicateRoi(selectedRoi);const panelExport=el('annotationExportSelected');if(panelExport)panelExport.onclick=exportSelectedAnnotations;const undo=el('undoAnnotation'),redo=el('redoAnnotation'),smooth=el('smoothRoi'),simp=el('simplifyRoi'),fill=el('fillRoiHoles'),merge=el('mergeRois'),split=el('splitRoi');if(undo)undo.onclick=restoreAnnotationUndo;if(redo)redo.onclick=restoreAnnotationRedo;if(smooth)smooth.onclick=smoothSelectedRoi;if(simp)simp.onclick=simplifySelectedRoi;if(fill)fill.onclick=fillSelectedRoiHoles;if(merge)merge.onclick=mergeSelectedAnnotations;if(split)split.onclick=splitSelectedAnnotation;const all=el('annotationSelectAll'),none=el('annotationSelectNone');if(all)all.onclick=selectAllAnnotations;if(none)none.onclick=selectNoAnnotations;}\n",
    "function updateButtons(){const has=rois.length>0,hasLayers=layers.length>0,drawable=hasDrawable(),selected=selectedRoi>=0&&!!rois[selectedRoi],editable=selected&&editableRoi(rois[selectedRoi]),editableDrawable=selected&&editable&&isDrawable(rois[selectedRoi]),exportable=roiExportIndices().length>0,mergeable=mergeCandidateIndices().length>=2,setDisabled=(id,value)=>{const button=el(id);if(button)button.disabled=!!value;},setActive=(id,value)=>{const button=el(id);if(button)button.classList.toggle('active',!!value);};['roiToggle','labelsToggle','prevRoi','nextRoi'].forEach(id=>setDisabled(id,!drawable));setDisabled('layersToggle',!has&&!hasLayers);setDisabled('finishRoi',draft.length<3);setDisabled('undoPoint',draft.length<1&&!brushing);setDisabled('saveGeojson',!has&&draft.length<3&&brushPoints.length<2);if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();['selectionCardToggle','deleteRoi','annotationApply','annotationDelete','annotationDuplicate','annotationZoom','annotationVisible','annotationLock'].forEach(id=>setDisabled(id,!selected));const nextClassButton=el('applyRoiClass');if(nextClassButton)nextClassButton.disabled=false;['smoothRoi','simplifyRoi','fillRoiHoles','splitRoi'].forEach(id=>setDisabled(id,!editableDrawable));setDisabled('mergeRois',!mergeable);const undoButton=el('undoAnnotation'),redoButton=el('redoAnnotation');if(undoButton)undoButton.disabled=annotationUndo.length<1;if(redoButton)redoButton.disabled=annotationRedo.length<1;['annotationNameInput','annotationClassSelect','annotationClassCustom','annotationColorInput'].forEach(id=>{const input=el(id);if(input)input.disabled=!selected;});const exportButton=el('annotationExportSelected');if(exportButton)exportButton.disabled=!exportable;const selectAll=el('annotationSelectAll'),selectNone=el('annotationSelectNone');if(selectAll)selectAll.disabled=!has;if(selectNone)selectNone.disabled=!has;setActive('roiToggle',showRois&&drawable);setActive('labelsToggle',showLabels&&drawable);setActive('crosshairToggle',showCrosshair);const panel=el('roiPanel');setActive('layersToggle',!!(panel&&panel.classList.contains('open')));}\n"
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
    "function upsertViewerLayer(layer){layer=normaliseViewerLayer(layer);const idx=layerFindIndex(layer.id);if(idx>=0&&layer.replace!==false)layers[idx]=layer;else layers.push(layer);buildLayerList();updateButtons();draw();scheduleViewerStateSync('layer_added',{id:layer.id,name:layer.name,type:layer.source_type||layer.type,count:layerCount(layer)});}\n",
    "function setViewerLayerVisible(key,visible=true){const idx=layerFindIndex(key);if(idx<0){notify('Layer not found','warning');return false;}layers[idx].visible=!!visible;buildLayerList();updateButtons();draw();scheduleViewerStateSync('layer_visibility_updated',{id:layers[idx].id,name:layers[idx].name,visible:layers[idx].visible});notify(layers[idx].visible?'Layer shown':'Layer hidden','success');return true;}\n",
    "function removeViewerLayer(key){const idx=layerFindIndex(key);if(idx<0)return false;const removed=layers.splice(idx,1)[0];buildLayerList();updateButtons();draw();scheduleViewerStateSync('layer_removed',{id:removed.id,name:removed.name});return true;}\n",
    "function heatmapLayerColour(layer,value){if(!Number.isFinite(value))return null;const min=Number(layer.min),max=Number(layer.max),den=Number.isFinite(max-min)&&Math.abs(max-min)>1e-12?max-min:1,t=clamp((value-(Number.isFinite(min)?min:0))/den,0,1),a=layerOpacity(layer);if(layer.source_type==='mask'||(Number.isFinite(min)&&Number.isFinite(max)&&min>=0&&max<=1&&layer.colour)){if(value<=0)return null;return hexToRgba(normaliseHexColour(layer.colour||'#22c55e'),Math.max(.05,a*.45));}const r=Math.round(40+215*t),g=Math.round(180*(1-Math.abs(t-.5)*2)+70*t),b=Math.round(255*(1-t)+40*t);return 'rgba('+r+','+g+','+b+','+Math.max(.05,a*.6)+')';}\n",
    "function drawHeatmapLayer(layer){const values=layer.values||[],rows=values.length,cols=rows?(values[0]||[]).length:0,ext=layer.extent||{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};if(!rows||!cols)return;const cw=(Number(ext.xmax)-Number(ext.xmin))/cols,ch=(Number(ext.ymax)-Number(ext.ymin))/rows;ctx.save();for(let r=0;r<rows;r++){const row=values[r]||[];for(let c=0;c<cols;c++){const color=heatmapLayerColour(layer,Number(row[c]));if(!color)continue;const p0=slideToCanvas({x:Number(ext.xmin)+c*cw,y:Number(ext.ymin)+r*ch}),p1=slideToCanvas({x:Number(ext.xmin)+(c+1)*cw,y:Number(ext.ymin)+(r+1)*ch}),x=Math.min(p0.x,p1.x),y=Math.min(p0.y,p1.y),w=Math.abs(p1.x-p0.x),h=Math.abs(p1.y-p0.y);if(x>innerWidth||y>innerHeight||x+w<0||y+h<0)continue;ctx.fillStyle=color;ctx.fillRect(Math.floor(x),Math.floor(y),Math.ceil(w)+1,Math.ceil(h)+1);}}ctx.restore();}\n",
    "function drawImageLayer(layer){if(!layer.data_uri)return;const ext=layer.extent||{xmin:0,ymin:0,xmax:cfg.slide_width,ymax:cfg.slide_height};if(!layer._image){const img=new Image();img.onload=()=>draw();img.src=layer.data_uri;layer._image=img;return;}if(!layer._image.complete)return;const p0=slideToCanvas({x:Number(ext.xmin),y:Number(ext.ymin)}),p1=slideToCanvas({x:Number(ext.xmax),y:Number(ext.ymax)});ctx.save();ctx.globalAlpha=layerOpacity(layer);ctx.drawImage(layer._image,Math.min(p0.x,p1.x),Math.min(p0.y,p1.y),Math.abs(p1.x-p0.x),Math.abs(p1.y-p0.y));ctx.restore();}\n",
    "function drawVectorLayer(layer){const items=layer.items||[];if(!items.length)return;ctx.save();items.forEach(item=>{if(item.visible===false)return;const opacity=layerOpacity(layer),colour=item.colour||layer.colour||'#38bdf8';if(item.type==='point'||(Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y)))){const q=slideToCanvas({x:Number(item.x),y:Number(item.y)}),r=Math.max(2,Number(item.radius||layer.radius||6)*slideUnitScale());if(q.x+r<0||q.y+r<0||q.x-r>innerWidth||q.y-r>innerHeight)return;ctx.globalAlpha=opacity;ctx.beginPath();ctx.arc(q.x,q.y,r,0,Math.PI*2);ctx.fillStyle=item.fill||hexToRgba(colour,.28);ctx.strokeStyle=colour;ctx.lineWidth=1.5;ctx.fill();ctx.stroke();ctx.globalAlpha=1;return;}if(!isDrawable(item))return;const groups=roiDrawGroups(item);groups.forEach(group=>{ctx.beginPath();drawPathRings(group.rings);drawPathRings(group.holes);ctx.globalAlpha=opacity;ctx.fillStyle=item.fill||hexToRgba(colour,.12);ctx.strokeStyle=colour;ctx.lineWidth=Number(layer.line_width||item.line_width||2);ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;});});ctx.restore();}\n",
    "function drawLayers(){(layers||[]).forEach(layer=>{if(!layerVisible(layer))return;const type=String(layer.type||'vector').toLowerCase();if(type==='heatmap'||type==='mask')drawHeatmapLayer(layer);else if(type==='image')drawImageLayer(layer);else drawVectorLayer(layer);});}\n",
    "function buildLayerList(){const list=el('layerList'),summary=el('layerSummary');if(!list||!summary)return;list.innerHTML='';summary.textContent=layers.length?(layers.length+' R-controlled layer'+(layers.length===1?'':'s')):'No R layers yet.';layers.forEach((layer,i)=>{normaliseViewerLayer(layer);const item=document.createElement('div');item.className='layerItem';if(!layerVisible(layer))item.classList.add('hidden');const top=document.createElement('div');top.className='layerTop';const box=document.createElement('input');box.type='checkbox';box.checked=layerVisible(layer);box.title='Toggle layer visibility';box.onchange=e=>{layer.visible=!!e.target.checked;buildLayerList();draw();scheduleViewerStateSync('layer_visibility_updated',{id:layer.id,name:layer.name,visible:layer.visible});};const sw=document.createElement('span');sw.className='swatch';sw.style.background=layer.colour||'#38bdf8';const nm=document.createElement('span');nm.className='roiName';nm.textContent=layerLabel(layer);const meta=document.createElement('span');meta.className='roiClass';meta.textContent=(layer.source_type||layer.type||'layer')+' '+layerCount(layer);top.append(box,sw,nm,meta);const controls=document.createElement('div');controls.className='layerControls';const op=document.createElement('input');op.type='range';op.min='0';op.max='1';op.step='0.05';op.value=String(layerOpacity(layer));op.title='Layer opacity';op.oninput=e=>{layer.opacity=Number(e.target.value);draw();};op.onchange=()=>scheduleViewerStateSync('layer_opacity_updated',{id:layer.id,name:layer.name,opacity:layerOpacity(layer)});const remove=document.createElement('button');remove.type='button';remove.textContent='Remove';remove.title='Remove this R-controlled layer from the viewer';remove.onclick=()=>removeViewerLayer(layer.id);controls.append(document.createTextNode('opacity'),op,remove);item.append(top,controls);list.appendChild(item);});if(layers.length)setRoiPanelOpen(true);}\n"
  )
}

wsi_viewer_cell_controls_js <- function() {
  paste0(
    "function cellphenotyperConfig(){return cfg.cellphenotyper||{};}\n",
    "function cellphenotyperLayerId(){const cp=cellphenotyperConfig();return String(cp.stardist_layer_id||'cellphenotyper_stardist_cells');}\n",
    "function findCellphenotyperLayer(){const id=cellphenotyperLayerId();return (layers||[]).find(layer=>String(layer.id||'')===id||String(layer.source_type||'')==='cellphenotyper_stardist')||null;}\n",
    "function cellLayerBounds(layer){const items=(layer&&Array.isArray(layer.items))?layer.items:[],xs=[],ys=[];items.forEach(item=>{const x=Number(item.x),y=Number(item.y);if(Number.isFinite(x)&&Number.isFinite(y)){xs.push(x);ys.push(y);}});if(!xs.length)return null;return {xmin:Math.min(...xs),ymin:Math.min(...ys),xmax:Math.max(...xs),ymax:Math.max(...ys)};}\n",
    "function updateCellControls(){const layer=findCellphenotyperLayer(),cp=cellphenotyperConfig(),count=layer?layerCount(layer):Number(cp.cell_count||0),has=!!layer&&count>0,toggle=el('cellToggle'),zoom=el('cellZoom'),panel=el('cellPanelToggle'),opacity=el('cellOpacity'),radius=el('cellRadius'),radiusValue=el('cellRadiusValue'),summary=el('cellSummary');if(toggle){toggle.disabled=!has;toggle.classList.toggle('active',has&&layerVisible(layer));}if(zoom)zoom.disabled=!has;if(panel)panel.disabled=!has;if(opacity){opacity.disabled=!has;if(layer)opacity.value=String(layerOpacity(layer));}if(radius){radius.disabled=!has;const r=layer&&Array.isArray(layer.items)&&layer.items.length?Number(layer.items[0].radius||layer.radius||6):Number(layer&&layer.radius||6);if(Number.isFinite(r))radius.value=String(Math.round(r));if(radiusValue)radiusValue.textContent=(Number.isFinite(r)?Math.round(r):6)+' px';}if(summary){if(!cp.enabled&&!has)summary.textContent='No CellPhenotyper project is attached to this viewer.';else if(!has)summary.textContent='No StarDist cell table was found in this project.';else summary.textContent=(layerVisible(layer)?'Showing ':'Hidden ')+count.toLocaleString()+' StarDist cell'+(count===1?'':'s')+'.';}}\n",
    "function setCellLayerVisible(visible){const layer=findCellphenotyperLayer();if(!layer){notify('No StarDist cell layer is loaded','warning');updateCellControls();return false;}layer.visible=!!visible;buildLayerList();updateCellControls();draw();scheduleViewerStateSync('cell_layer_visibility_updated',{id:layer.id,name:layer.name,visible:layer.visible,count:layerCount(layer)});notify(layer.visible?'StarDist cells shown':'StarDist cells hidden',layer.visible?'success':'info',1600);return true;}\n",
    "function toggleCellLayer(){const layer=findCellphenotyperLayer();setCellLayerVisible(!(layer&&layerVisible(layer)));}\n",
    "function setCellLayerOpacity(value){const layer=findCellphenotyperLayer();if(!layer)return;layer.opacity=clamp(Number(value),0,1);buildLayerList();updateCellControls();draw();scheduleViewerStateSync('cell_layer_opacity_updated',{id:layer.id,name:layer.name,opacity:layerOpacity(layer)});}\n",
    "function setCellLayerRadius(value){const layer=findCellphenotyperLayer();if(!layer)return;const r=clamp(Math.round(Number(value)||6),1,40);layer.radius=r;(layer.items||[]).forEach(item=>{if(item.type==='point'||(Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y))))item.radius=r;});updateCellControls();draw();scheduleViewerStateSync('cell_layer_radius_updated',{id:layer.id,name:layer.name,radius:r});}\n",
    "function zoomToCellLayer(){const layer=findCellphenotyperLayer(),b=cellLayerBounds(layer);if(!b){notify('No StarDist cell positions are available','warning');return;}if(typeof zoomToSlideBounds==='function')zoomToSlideBounds(b,1.2);else{const w=Math.max(1,b.xmax-b.xmin),h=Math.max(1,b.ymax-b.ymin),cx=(b.xmin+b.xmax)/2,cy=(b.ymin+b.ymax)/2,corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymax}].map(slideToImage),iw=Math.abs(corners[1].x-corners[0].x),ih=Math.abs(corners[1].y-corners[0].y);scale=clamp(Math.min(innerWidth/Math.max(1,iw*1.2),innerHeight/Math.max(1,ih*1.2)),minScale*.8,80);const c=slideToImage({x:cx,y:cy});offsetX=innerWidth/2-c.x*scale;offsetY=innerHeight/2-c.y*scale;draw();}notify('Zoomed to StarDist cells','success',1400);}\n",
    "function bindCellControls(){const toggle=el('cellToggle'),zoom=el('cellZoom'),panel=el('cellPanelToggle'),opacity=el('cellOpacity'),radius=el('cellRadius');if(toggle)toggle.onclick=toggleCellLayer;if(zoom)zoom.onclick=zoomToCellLayer;if(panel)panel.onclick=()=>{if(typeof setRoiPanelOpen==='function')setRoiPanelOpen(true);if(typeof buildLayerList==='function')buildLayerList();};if(opacity)opacity.oninput=e=>setCellLayerOpacity(e.target.value);if(radius)radius.oninput=e=>setCellLayerRadius(e.target.value);updateCellControls();}\n"
  )
}

wsi_viewer_seurat_js <- function() {
  paste0(
    "function seuratConfig(){return cfg.seurat||{enabled:false,plots:[]};}\n",
    "function seuratEnabled(){return !!seuratConfig().enabled;}\n",
    "function seuratLayerId(){return String(seuratConfig().spot_layer_id||'seurat_spots');}\n",
    "function findSeuratLayer(){const id=seuratLayerId();return (layers||[]).find(layer=>String(layer.id||'')===id||String(layer.source_type||'')==='seurat_spots')||null;}\n",
    "function seuratPlotItem(){const plots=seuratConfig().plots||[];return Array.isArray(plots)&&plots.length?plots[0]:null;}\n",
    "function seuratPlotPoints(){const plot=seuratPlotItem(),pts=plot&&plot.points||[];return Array.isArray(pts)?pts:[];}\n",
    "function seuratSummary(msg){const box=el('seuratSummary');if(box&&msg)box.textContent=msg;}\n",
    "function updateSeuratControls(){const layer=findSeuratLayer(),cfgs=seuratConfig(),has=seuratEnabled()&&!!layer&&layerCount(layer)>0,toggle=el('seuratSpotToggle'),zoom=el('seuratSpotZoom'),panel=el('seuratLayerPanel'),opacity=el('seuratSpotOpacity'),radius=el('seuratSpotRadius'),radiusValue=el('seuratSpotRadiusValue'),open=el('seuratOpenPca'),clear=el('seuratClearSelection'),summary=el('seuratSummary');if(toggle){toggle.disabled=!has;toggle.classList.toggle('active',has&&layerVisible(layer));}if(zoom)zoom.disabled=!has;if(panel)panel.disabled=!has;if(opacity){opacity.disabled=!has;if(layer)opacity.value=String(layerOpacity(layer));}if(radius){radius.disabled=!has;const r=layer&&Array.isArray(layer.items)&&layer.items.length?Number(layer.items[0].radius||layer.radius||8):Number(layer&&layer.radius||cfgs.spot_radius||8);if(Number.isFinite(r))radius.value=String(Math.round(r));if(radiusValue)radiusValue.textContent=(Number.isFinite(r)?Math.round(r):8)+' px';}const hasPlot=seuratEnabled()&&seuratPlotPoints().length>0;if(open)open.disabled=!hasPlot;if(clear)clear.disabled=!hasPlot&&!seuratSelectedLabels.size;if(summary){if(!seuratEnabled())summary.textContent='No Seurat object is attached to this viewer.';else if(!has)summary.textContent='No Seurat spatial spots were found.';else summary.textContent=(layerVisible(layer)?'Showing ':'Hidden ')+layerCount(layer).toLocaleString()+' Seurat spot'+(layerCount(layer)===1?'':'s')+' | '+String((cfgs.reduction||'PCA')).toUpperCase();}}\n",
    "function toggleSeuratSpots(){const layer=findSeuratLayer();if(!layer)return;setViewerLayerVisible(layer.id,!layerVisible(layer));updateSeuratControls();}\n",
    "function setSeuratSpotOpacity(value){const layer=findSeuratLayer();if(!layer)return;layer.opacity=clamp(Number(value),0,1);draw();scheduleViewerStateSync('layer_opacity_updated',{id:layer.id,name:layer.name,opacity:layerOpacity(layer)});}\n",
    "function setSeuratSpotRadius(value){const layer=findSeuratLayer();if(!layer)return;const r=clamp(Math.round(Number(value)||8),1,120);layer.radius=r;(layer.items||[]).forEach(item=>{if(item.type==='point'||(Number.isFinite(Number(item.x))&&Number.isFinite(Number(item.y))))item.radius=r;});updateSeuratControls();draw();scheduleViewerStateSync('layer_updated',{id:layer.id,name:layer.name,radius:r});}\n",
    "function zoomToSeuratSpots(){const layer=findSeuratLayer();if(!layer||!Array.isArray(layer.items)||!layer.items.length)return;const b=cellLayerBounds(layer);if(!b)return;const corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(slideToViewImagePoint),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),pad=1.25;scale=clamp(Math.min(innerWidth/Math.max(1,(Math.max(...xs)-Math.min(...xs))*pad),innerHeight/Math.max(1,(Math.max(...ys)-Math.min(...ys))*pad)),minScale*0.8,40);offsetX=innerWidth/2-((Math.min(...xs)+Math.max(...xs))/2)*scale;offsetY=innerHeight/2-((Math.min(...ys)+Math.max(...ys))/2)*scale;draw();}\n",
    "let seuratPlotTransform=null,seuratSelectionDrag=null,seuratSelectionPolygon=[],seuratSelectedLabels=new Set(),seuratSelectionMatchedCount=0,seuratPlotDrag=null,seuratPlotResizeObserver=null;\n",
    "function seuratPointLabel(point){if(!point)return '';return String(point.label??point.spot_id??point.id??'');}\n",
    "function seuratPointColour(point){if(!point)return '#2B6CB0';return normaliseHexColour(point.colour||point.color||'','#2B6CB0');}\n",
    "function seuratPointBounds(points){const pts=(points||[]).map(p=>({x:Number(p.x),y:Number(p.y)})).filter(p=>Number.isFinite(p.x)&&Number.isFinite(p.y));return pts.length?boundsFromPoints(pts):null;}\n",
    "function resizeSeuratPlotCanvas(canvas,bounds){const viewport=el('seuratPlotViewport'),maxW=Math.max(320,(viewport&&viewport.clientWidth?viewport.clientWidth-8:720)),maxH=Math.max(180,(viewport&&viewport.clientHeight?viewport.clientHeight-8:Math.min(620,innerHeight-280))),bw=Math.max(1,bounds.xmax-bounds.xmin),bh=Math.max(1,bounds.ymax-bounds.ymin),scale0=Math.min(maxW/bw,maxH/bh);canvas.width=Math.max(320,Math.round(bw*scale0));canvas.height=Math.max(180,Math.round(bh*scale0));seuratPlotTransform={scale:scale0,xmin:bounds.xmin,ymin:bounds.ymin,pad:6,sx:(canvas.width-12)/Math.max(1,canvas.width),sy:(canvas.height-12)/Math.max(1,canvas.height)};return seuratPlotTransform;}\n",
    "function seuratPointCanvasPosition(point,tx=seuratPlotTransform){if(!point||!tx)return null;const x0=(Number(point.x)-tx.xmin)*tx.scale,y0=(Number(point.y)-tx.ymin)*tx.scale;if(!Number.isFinite(x0)||!Number.isFinite(y0))return null;return {x:tx.pad+x0*tx.sx,y:tx.pad+y0*tx.sy};}\n",
    "function drawSeuratSelection(ctx2){const points=(seuratSelectionDrag&&seuratSelectionDrag.points&&seuratSelectionDrag.points.length?seuratSelectionDrag.points:seuratSelectionPolygon)||[];if(!points.length)return;ctx2.save();ctx2.strokeStyle='#facc15';ctx2.fillStyle='rgba(250,204,21,.14)';ctx2.lineWidth=1.5;ctx2.setLineDash([5,4]);ctx2.beginPath();points.forEach((p,i)=>{if(i===0)ctx2.moveTo(p.x,p.y);else ctx2.lineTo(p.x,p.y);});if(points.length>2){ctx2.closePath();ctx2.fill();}ctx2.stroke();ctx2.restore();}\n",
    "function drawSeuratSelectedPointOutlines(points,ctx2){if(!seuratSelectedLabels.size)return;ctx2.save();points.forEach(point=>{const key=seuratPointLabel(point).toLowerCase();if(!key||!seuratSelectedLabels.has(key))return;const q=seuratPointCanvasPosition(point);if(!q)return;ctx2.lineWidth=2.2;ctx2.strokeStyle='rgba(17,24,39,.95)';ctx2.beginPath();ctx2.arc(q.x,q.y,4.5,0,Math.PI*2);ctx2.stroke();ctx2.lineWidth=1.2;ctx2.strokeStyle='#facc15';ctx2.beginPath();ctx2.arc(q.x,q.y,6.5,0,Math.PI*2);ctx2.stroke();});ctx2.restore();}\n",
    "function renderSeuratLegend(points){const legend=el('seuratPlotLegend');if(!legend)return;legend.innerHTML='';const row=document.createElement('span');row.className='kodamaLegendItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background='#2B6CB0';const tx=document.createElement('span');tx.textContent='coloured by '+String((seuratConfig().component_names||['PC_1'])[0]||'component 1');row.append(sw,tx);legend.appendChild(row);}\n",
    "function drawSeuratPlot(){const canvas=el('seuratPlotCanvas'),points=seuratPlotPoints();if(!canvas)return;const ctx2=canvas.getContext('2d'),bounds=seuratPointBounds(points);if(!points.length||!bounds){canvas.width=520;canvas.height=280;ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);ctx2.fillStyle='#111827';ctx2.font='14px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx2.fillText('No Seurat PCA points are available.',24,36);return;}const tx=resizeSeuratPlotCanvas(canvas,bounds),pad=tx.pad;ctx2.fillStyle='#f8fafc';ctx2.fillRect(0,0,canvas.width,canvas.height);ctx2.save();ctx2.translate(pad,pad);ctx2.scale(tx.sx,tx.sy);const radius=Math.max(1.2,Math.min(3.2,140/Math.sqrt(points.length)));points.forEach(point=>{const x=(Number(point.x)-tx.xmin)*tx.scale,y=(Number(point.y)-tx.ymin)*tx.scale;if(!Number.isFinite(x)||!Number.isFinite(y))return;ctx2.fillStyle=seuratPointColour(point);ctx2.globalAlpha=.82;ctx2.beginPath();ctx2.arc(x,y,radius,0,Math.PI*2);ctx2.fill();});ctx2.restore();ctx2.globalAlpha=1;ctx2.fillStyle='#111827';ctx2.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';const names=seuratConfig().component_names||['PC_1','PC_2'];ctx2.fillText(String(names[0]||'PC_1'),10,canvas.height-10);ctx2.save();ctx2.translate(12,20);ctx2.rotate(-Math.PI/2);ctx2.fillText(String(names[1]||'PC_2'),0,0);ctx2.restore();drawSeuratSelectedPointOutlines(points,ctx2);drawSeuratSelection(ctx2);renderSeuratLegend(points);}\n",
    "function seuratSelectionPayload(){return {labels:Array.from(seuratSelectedLabels),count:seuratSelectedLabels.size,matched_count:seuratSelectionMatchedCount,reduction:seuratConfig().reduction||null};}\n",
    "function updateSeuratSelectionStatus(message=null){const box=el('seuratPlotSelectionStatus'),clear=el('seuratPlotClearSelection'),clear2=el('seuratClearSelection');if(clear)clear.disabled=!seuratSelectedLabels.size;if(clear2)clear2.disabled=!seuratSelectedLabels.size;if(!box)return;if(message){box.textContent=message;return;}if(seuratSelectedLabels.size)box.textContent='Selected '+seuratSelectedLabels.size.toLocaleString()+' Seurat spot'+(seuratSelectedLabels.size===1?'':'s')+'; highlighted '+seuratSelectionMatchedCount.toLocaleString()+' on the slide.';else box.textContent='Draw a lasso around PCA points to highlight matching spots on the slide. Shift/Ctrl/Command adds; Alt subtracts.';}\n",
    "function removeSeuratSelectionLayer(){if(typeof layerFindIndex!=='function')return;const idx=layerFindIndex('seurat_selected_spots');if(idx>=0){layers.splice(idx,1);buildLayerList();}}\n",
    "function updateSeuratSpotHighlights(){const labels=Array.from(seuratSelectedLabels),points=seuratPlotPoints(),wanted=new Set(labels),matched=points.filter(p=>wanted.has(seuratPointLabel(p).toLowerCase())&&Number.isFinite(Number(p.slide_x))&&Number.isFinite(Number(p.slide_y)));seuratSelectionMatchedCount=matched.length;if(!labels.length||!matched.length){removeSeuratSelectionLayer();buildLayerList();draw();return matched.length;}const items=matched.map(p=>({id:'seurat_selected_'+seuratPointLabel(p),name:seuratPointLabel(p),label:seuratPointLabel(p),type:'point',x:Number(p.slide_x),y:Number(p.slide_y),radius:Math.max(12,Number(seuratConfig().spot_radius||8)*1.7),colour:'#facc15',fill:'rgba(250,204,21,.38)',source:'seurat_pca_selection'}));upsertViewerLayer({id:'seurat_selected_spots',name:'Seurat selected spots',type:'vector',source_type:'seurat_selection',visible:true,opacity:.95,colour:'#facc15',replace:true,count:items.length,items:items});return matched.length;}\n",
    "function clearSeuratSelection(sync=true){seuratSelectedLabels.clear();seuratSelectionMatchedCount=0;seuratSelectionDrag=null;seuratSelectionPolygon=[];removeSeuratSelectionLayer();updateSeuratSelectionStatus();drawSeuratPlot();draw();if(sync)scheduleViewerStateSync('seurat_spots_selected',{count:0,matched_count:0,labels:[]});}\n",
    "function applySeuratSelection(labels,mode='replace'){const unique=Array.from(new Set((labels||[]).map(v=>String(v||'').trim().toLowerCase()).filter(Boolean)));if(mode==='replace')seuratSelectedLabels.clear();if(mode==='subtract')unique.forEach(label=>seuratSelectedLabels.delete(label));else unique.forEach(label=>seuratSelectedLabels.add(label));const matched=updateSeuratSpotHighlights();updateSeuratSelectionStatus();drawSeuratPlot();scheduleViewerStateSync('seurat_spots_selected',{count:seuratSelectedLabels.size,matched_count:matched,labels:Array.from(seuratSelectedLabels),mode:mode,reduction:seuratConfig().reduction||null});if(seuratSelectedLabels.size&&matched)notify('Seurat selection highlighted '+matched.toLocaleString()+' spot'+(matched===1?'':'s'),'success',2200);}\n",
    "function seuratCanvasPoint(evt){const canvas=el('seuratPlotCanvas'),rect=canvas.getBoundingClientRect();return {x:(evt.clientX-rect.left)*canvas.width/Math.max(1,rect.width),y:(evt.clientY-rect.top)*canvas.height/Math.max(1,rect.height)};}\n",
    "function seuratPointInPolygon(p,poly){let inside=false;for(let i=0,j=poly.length-1;i<poly.length;j=i++){const xi=poly[i].x,yi=poly[i].y,xj=poly[j].x,yj=poly[j].y,hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function seuratNearestPointLabels(a){const points=seuratPlotPoints();let best=null,bestDist=Infinity;points.forEach(point=>{const q=seuratPointCanvasPosition(point);if(!q)return;const d=Math.hypot(q.x-a.x,q.y-a.y);if(d<bestDist){bestDist=d;best=point;}});return best&&bestDist<=12?[seuratPointLabel(best)]:[];}\n",
    "function seuratPointsInPolygon(poly){const points=seuratPlotPoints();if(!points.length||!seuratPlotTransform||!poly.length)return[];if(poly.length<3)return seuratNearestPointLabels(poly[0]);return points.filter(point=>{const q=seuratPointCanvasPosition(point);return q&&seuratPointInPolygon(q,poly);}).map(seuratPointLabel);}\n",
    "function bindSeuratPlotCanvasSelection(){const canvas=el('seuratPlotCanvas');if(!canvas||canvas.dataset.seuratSelectionBound==='1')return;canvas.dataset.seuratSelectionBound='1';canvas.addEventListener('mousedown',evt=>{if(evt.button!==0)return;const points=seuratPlotPoints();if(!points.length){notify('This Seurat plot has no selectable points','warning');return;}evt.preventDefault();const p=seuratCanvasPoint(evt);seuratSelectionDrag={points:[p],additive:!!(evt.shiftKey||evt.ctrlKey||evt.metaKey),subtract:!!evt.altKey};seuratSelectionPolygon=[];drawSeuratPlot();});window.addEventListener('mousemove',evt=>{if(!seuratSelectionDrag)return;const p=seuratCanvasPoint(evt),pts=seuratSelectionDrag.points,last=pts[pts.length-1];if(!last||Math.hypot(p.x-last.x,p.y-last.y)>=2){pts.push(p);drawSeuratPlot();}});window.addEventListener('mouseup',()=>{if(!seuratSelectionDrag)return;const drag=seuratSelectionDrag,poly=(drag.points||[]).slice();seuratSelectionDrag=null;seuratSelectionPolygon=poly.length>2?poly:[];const labels=seuratPointsInPolygon(poly),mode=drag.subtract?'subtract':(drag.additive?'add':'replace');if(!labels.length){if(mode==='replace')clearSeuratSelection(true);else{drawSeuratPlot();updateSeuratSelectionStatus('No Seurat spots were touched by the lasso.');}return;}applySeuratSelection(labels,mode);});}\n",
    "function renderSeuratPlotWindow(){const plot=seuratPlotItem(),title=el('seuratPlotTitle'),sub=el('seuratPlotSubtitle');if(title)title.textContent=plot?(plot.label||'Seurat PCA plot'):'Seurat PCA plot';if(sub)sub.textContent=plot?(Number(plot.point_count||seuratPlotPoints().length).toLocaleString()+' spots | '+String(plot.x_label||'PC_1')+' vs '+String(plot.y_label||'PC_2')):'No plot selected';updateSeuratSelectionStatus();drawSeuratPlot();}\n",
    "function openSeuratPlot(){if(!seuratPlotPoints().length){notify('No Seurat PCA points found','warning');return;}const panel=el('seuratPlotWindow');if(panel){panel.classList.add('open');panel.setAttribute('aria-hidden','false');}setTimeout(renderSeuratPlotWindow,0);}\n",
    "function closeSeuratPlot(){const panel=el('seuratPlotWindow');if(panel){panel.classList.remove('open');panel.setAttribute('aria-hidden','true');}}\n",
    "function moveSeuratPlotPanel(left,top){const panel=el('seuratPlotWindow');if(!panel)return;const rect=panel.getBoundingClientRect(),margin=6,maxLeft=Math.max(margin,innerWidth-rect.width-margin),maxTop=Math.max(margin,innerHeight-rect.height-margin);panel.style.right='auto';panel.style.bottom='auto';panel.style.left=clamp(left,margin,maxLeft)+'px';panel.style.top=clamp(top,margin,maxTop)+'px';}\n",
    "function bindSeuratPlotMove(){const panel=el('seuratPlotWindow'),head=panel&&panel.querySelector('.seuratPlotHead');if(!panel||!head||head.dataset.seuratMoveBound==='1')return;head.dataset.seuratMoveBound='1';head.title='Drag to move; resize from the bottom-right corner.';head.addEventListener('mousedown',evt=>{if(evt.button!==0||evt.target.closest('button,input,select,textarea'))return;const rect=panel.getBoundingClientRect();panel.style.width=rect.width+'px';panel.style.height=rect.height+'px';panel.style.left=rect.left+'px';panel.style.top=rect.top+'px';panel.style.right='auto';panel.style.bottom='auto';seuratPlotDrag={dx:evt.clientX-rect.left,dy:evt.clientY-rect.top};panel.classList.add('moving');evt.preventDefault();});window.addEventListener('mousemove',evt=>{if(!seuratPlotDrag)return;moveSeuratPlotPanel(evt.clientX-seuratPlotDrag.dx,evt.clientY-seuratPlotDrag.dy);});window.addEventListener('mouseup',()=>{if(!seuratPlotDrag)return;seuratPlotDrag=null;panel.classList.remove('moving');});}\n",
    "function bindSeuratPlotResize(){const panel=el('seuratPlotWindow');if(!panel||seuratPlotResizeObserver||typeof ResizeObserver==='undefined')return;seuratPlotResizeObserver=new ResizeObserver(()=>{if(panel.classList.contains('open'))requestAnimationFrame(drawSeuratPlot);});seuratPlotResizeObserver.observe(panel);}\n",
    "function bindSeuratControls(){const toggle=el('seuratSpotToggle'),zoom=el('seuratSpotZoom'),panel=el('seuratLayerPanel'),opacity=el('seuratSpotOpacity'),radius=el('seuratSpotRadius'),open=el('seuratOpenPca'),close=el('seuratPlotClose'),reset=el('seuratPlotReset'),clear=el('seuratClearSelection'),clear2=el('seuratPlotClearSelection');if(toggle)toggle.onclick=toggleSeuratSpots;if(zoom)zoom.onclick=zoomToSeuratSpots;if(panel)panel.onclick=()=>{if(typeof setRoiPanelOpen==='function')setRoiPanelOpen(true);if(typeof buildLayerList==='function')buildLayerList();};if(opacity)opacity.oninput=e=>setSeuratSpotOpacity(e.target.value);if(radius)radius.oninput=e=>setSeuratSpotRadius(e.target.value);if(open)open.onclick=openSeuratPlot;if(close)close.onclick=closeSeuratPlot;if(reset)reset.onclick=renderSeuratPlotWindow;if(clear)clear.onclick=()=>clearSeuratSelection(true);if(clear2)clear2.onclick=()=>clearSeuratSelection(true);bindSeuratPlotCanvasSelection();bindSeuratPlotMove();bindSeuratPlotResize();updateSeuratControls();updateSeuratSelectionStatus();window.addEventListener('resize',()=>{if(el('seuratPlotWindow')&&el('seuratPlotWindow').classList.contains('open'))drawSeuratPlot();});}\n"
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
    "function drawMeasureLine(m,preview=false){const a=slideToCanvas(m.start),b=slideToCanvas(m.end),mx=(a.x+b.x)/2,my=(a.y+b.y)/2,text=formatMeasure(m);ctx.save();ctx.strokeStyle=preview?'#facc15':'#38bdf8';ctx.fillStyle=preview?'#facc15':'#38bdf8';ctx.lineWidth=2;if(preview)ctx.setLineDash([6,4]);ctx.beginPath();ctx.moveTo(a.x,a.y);ctx.lineTo(b.x,b.y);ctx.stroke();ctx.setLineDash([]);[a,b].forEach(p=>{ctx.beginPath();ctx.arc(p.x,p.y,4,0,Math.PI*2);ctx.fill();ctx.strokeStyle='#111';ctx.stroke();ctx.strokeStyle=preview?'#facc15':'#38bdf8';});const w=ctx.measureText(text).width+8,x=clamp(mx-w/2,4,innerWidth-w-4),y=clamp(my-22,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.76)';ctx.fillRect(x,y,w,18);ctx.fillStyle=preview?'#facc15':'#e0f2fe';ctx.fillText(text,x+4,y+3);ctx.restore();}\n",
    "function drawMeasurements(){measures.forEach(m=>drawMeasureLine(m,false));if(mode==='measure'&&measureStart&&lastPointer&&pointInsideSlide(lastPointer))drawMeasureLine(measurementRecord(measureStart,lastPointer),true);}\n",
    "function updateMeasureList(){const summary=el('measureSummary'),list=el('measureList'),clear=el('clearMeasures');if(summary)summary.textContent=measures.length?(measures.length+' distance measurement'+(measures.length===1?'':'s')):'No measurements yet.';if(list){list.innerHTML='';measures.forEach((m,i)=>{const b=document.createElement('button');b.type='button';b.className='measureItem';b.innerHTML='Distance '+(i+1)+'<br><code>'+formatMeasure(m)+'</code>';b.onclick=()=>{measureStart=null;notify('Measurement '+(i+1)+': '+formatMeasure(m),'info',4200);draw();};list.appendChild(b);});}if(clear)clear.disabled=measures.length===0;}\n",
    "function addMeasurePoint(p){if(!pointInsideSlide(p))return;if(!measureStart){measureStart={x:p.x,y:p.y};notify('Measurement start set','info');draw();return;}const rec=measurementRecord(measureStart,p);measures.push(rec);measureStart=null;updateMeasureList();updateButtons();recordAnnotationHistory('measurement_added',{id:rec.id,distance_px:Number.isFinite(rec.distance_px)?rec.distance_px.toFixed(1):null});scheduleViewerStateSync('measurement_added',{id:rec.id});notify('Distance measured','success');draw();}\n",
    "function clearMeasurements(){measures=[];measureStart=null;updateMeasureList();updateButtons();scheduleViewerStateSync('measurements_cleared',{});draw();}\n",
    "function measureStatus(){if(mode==='measure'){if(measureStart&&lastPointer&&pointInsideSlide(lastPointer))return ' | measuring '+formatMeasure(measurementRecord(measureStart,lastPointer));return ' | click two points to measure';}return measures.length?(' | measures '+measures.length):'';}\n",
    "function bindMeasureControls(){const tool=el('toolMeasure'),clear=el('clearMeasures');if(tool)tool.onclick=()=>setMode('measure');if(clear)clear.onclick=clearMeasurements;updateMeasureList();}\n"
  )
}

wsi_viewer_scale_bar_js <- function() {
  paste0(
    "function scaleBarMpp(){const m=cfg.mpp||{};const x=Number(m.x),y=Number(m.y);if(Number.isFinite(x)&&x>0)return x;if(Number.isFinite(y)&&y>0)return y;return NaN;}\n",
    "function scaleBarSlideUnitScale(){try{const w=Number(cfg.slide_width||0),step=Math.max(1,Math.min(1000,w>0?w/8:1000)),a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:step,y:0}),d=Math.hypot(b.x-a.x,b.y-a.y)/step;if(Number.isFinite(d)&&d>0)return d;}catch(e){}return Number.isFinite(scale)&&scale>0?scale:NaN;}\n",
    "function niceScaleLength(value){if(!Number.isFinite(value)||value<=0)return NaN;const exponent=Math.floor(Math.log10(value)),base=Math.pow(10,exponent),fraction=value/base;let nice=1;if(fraction<1.5)nice=1;else if(fraction<3.5)nice=2;else if(fraction<7.5)nice=5;else nice=10;return nice*base;}\n",
    "function formatScaleMicrons(um){if(!Number.isFinite(um)||um<=0)return '';let digits=0;if(um<1)digits=2;else if(um<10)digits=1;return String(Number(um.toFixed(digits)))+' \\u00b5m';}\n",
    "function baseObjectivePower(){const obj=Number(cfg.objective_power);if(Number.isFinite(obj)&&obj>0)return obj;const mpp=scaleBarMpp();return Number.isFinite(mpp)&&mpp>0?10/mpp:NaN;}\n",
    "function currentMagnification(){const base=baseObjectivePower(),slideScale=scaleBarSlideUnitScale();return Number.isFinite(base)&&base>0&&Number.isFinite(slideScale)&&slideScale>0?base*slideScale:NaN;}\n",
    "function formatMagnification(value){if(!Number.isFinite(value)||value<=0)return 'NA';const digits=value<10?1:0;return Number(value.toFixed(digits))+'x';}\n",
    "function magnificationStatus(){const mag=currentMagnification();return Number.isFinite(mag)?(' | Mag '+formatMagnification(mag)):'';}\n",
    "function setMagnificationPower(power){const target=Number(power),base=baseObjectivePower(),slideScale=scaleBarSlideUnitScale();if(!Number.isFinite(target)||target<=0||!Number.isFinite(base)||base<=0||!Number.isFinite(slideScale)||slideScale<=0){notify('Magnification unavailable: slide MPP or objective power is missing','warning',3600);return;}zoomAt(target/base/slideScale,innerWidth/2,innerHeight/2);notify('Magnification '+formatMagnification(target),'success',1400);}\n",
    "function updateMagnificationControls(){const summary=el('magnificationSummary'),mag=currentMagnification(),base=baseObjectivePower();document.querySelectorAll('.magnificationPreset').forEach(button=>{const target=Number(button.dataset.magnification);const enabled=Number.isFinite(base)&&base>0;button.disabled=!enabled;button.classList.toggle('active',Number.isFinite(mag)&&Math.abs(mag-target)<=Math.max(0.75,target*0.08));});if(summary){if(Number.isFinite(mag)){summary.textContent='Current '+formatMagnification(mag)+' | full resolution '+formatMagnification(base);}else{summary.textContent='Magnification unavailable: no MPP/objective metadata.';}}}\n",
    "function bindMagnificationControls(){document.querySelectorAll('.magnificationPreset').forEach(button=>{button.onclick=()=>setMagnificationPower(button.dataset.magnification);});updateMagnificationControls();}\n",
    "function updateScaleBar(){const bar=el('scaleBar'),line=el('scaleBarLine'),label=el('scaleBarLabel');if(!bar||!line||!label){updateMagnificationControls();return;}const mpp=scaleBarMpp(),slideScale=scaleBarSlideUnitScale();if(!Number.isFinite(mpp)||mpp<=0||!Number.isFinite(slideScale)||slideScale<=0){bar.classList.add('unavailable');updateMagnificationControls();return;}const targetPx=clamp(innerWidth*0.16,90,180),targetUm=targetPx*mpp/slideScale,niceUm=niceScaleLength(targetUm),barPx=niceUm/mpp*slideScale;if(!Number.isFinite(barPx)||barPx<24){bar.classList.add('unavailable');updateMagnificationControls();return;}line.style.width=Math.round(clamp(barPx,32,Math.max(48,innerWidth*.42)))+'px';label.textContent=formatScaleMicrons(niceUm);bar.classList.remove('unavailable');updateMagnificationControls();}\n"
  )
}

wsi_viewer_multiview_js <- function() {
  paste0(
    "let multiViewLayout=1,multiViewPanes=[],multiViewSync=false,multiViewApplying=false,multiViewActiveIndex=0;\n",
    "function multiViewHost(){return el('multiViewGrid');}\n",
    "function multiViewSupported(){return typeof OpenSeadragon!=='undefined'&&typeof tileSourceFromConfig==='function';}\n",
    "function multiViewStatus(){return multiViewLayout>1?(' | views '+multiViewLayout+(multiViewSync?' linked':' independent')):'';}\n",
    "function updateMultiViewControls(){document.querySelectorAll('.multiViewLayout').forEach(button=>{const n=Number(button.dataset.layout||1);button.classList.toggle('active',n===multiViewLayout);});const sync=el('multiViewSync');if(sync)sync.checked=!!multiViewSync;const summary=el('multiViewSummary');if(summary){if(multiViewLayout<=1)summary.textContent='Single view. Use 2, 4, or 6 views to compare tissue regions side by side.';else summary.textContent=multiViewLayout+' tissue views, '+(multiViewSync?'linked zoom/pan':'independent zoom/pan')+'. Click a pane to make it active for navigation buttons.';}}\n",
    "function setMultiViewActive(index){multiViewActiveIndex=clamp(Number(index)||0,0,Math.max(0,multiViewPanes.length-1));multiViewPanes.forEach((pane,i)=>{if(pane&&pane.element)pane.element.classList.toggle('active',i===multiViewActiveIndex);});}\n",
    "function destroyMultiViewPanes(){multiViewPanes.forEach(pane=>{try{if(pane.viewer&&typeof pane.viewer.destroy==='function')pane.viewer.destroy();}catch(e){}});multiViewPanes=[];const host=multiViewHost();if(host)host.innerHTML='';multiViewActiveIndex=0;}\n",
    "function multiViewColumns(count){return count===6?3:(count===2?2:2);}\n",
    "function multiViewInitialBounds(index,count){const cols=multiViewColumns(count),rows=Math.ceil(count/cols),col=index%cols,row=Math.floor(index/cols),w=Number(cfg.slide_width||1)/cols,h=Number(cfg.slide_height||1)/rows;return {xmin:col*w,ymin:row*h,xmax:Math.min(Number(cfg.slide_width||1),(col+1)*w),ymax:Math.min(Number(cfg.slide_height||1),(row+1)*h)};}\n",
    "function copyViewportBetween(source,target,immediate=true){if(!source||!target||!source.viewport||!target.viewport)return false;try{const center=source.viewport.getCenter(true),zoom=source.viewport.getZoom(true);target.viewport.zoomTo(zoom,null,immediate);target.viewport.panTo(center,immediate);target.viewport.applyConstraints(immediate);return true;}catch(e){return false;}}\n",
    "function zoomPaneToSlideBounds(viewer,b){if(!viewer||!viewer.world||!b)return false;const item=viewer.world.getItemAt(0);if(!item||typeof item.imageToViewportCoordinates!=='function')return false;const p0=item.imageToViewportCoordinates(Number(b.xmin),Number(b.ymin)),p1=item.imageToViewportCoordinates(Number(b.xmax),Number(b.ymax)),rect=new OpenSeadragon.Rect(p0.x,p0.y,p1.x-p0.x,p1.y-p0.y);viewer.viewport.fitBoundsWithConstraints(rect,true);return true;}\n",
    "function syncMultiViewFrom(source){if(!multiViewSync||multiViewApplying||multiViewLayout<=1||!source)return;multiViewApplying=true;multiViewPanes.forEach(pane=>{if(pane.viewer&&pane.viewer!==source)copyViewportBetween(source,pane.viewer,true);});if(typeof osdViewer!=='undefined'&&osdViewer&&source!==osdViewer){copyViewportBetween(source,osdViewer,true);if(typeof syncViewState==='function')syncViewState();if(typeof requestDraw==='function')requestDraw();}multiViewApplying=false;}\n",
    "function multiViewOsdOptions(element){const roundMode=(OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES&&OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES.ALWAYS)||undefined;return {element:element,showNavigationControl:false,showNavigator:false,blendTime:0,alwaysBlend:false,immediateRender:true,placeholderFillStyle:'#fff',subPixelRoundingForTransparency:roundMode,minPixelRatio:1,maxImageCacheCount:256,animationTime:.12,springStiffness:9,visibilityRatio:.8,constrainDuringPan:true,minZoomImageRatio:.7,maxZoomPixelRatio:16,gestureSettingsMouse:{clickToZoom:false,dblClickToZoom:false,scrollToZoom:true,dragToPan:true},gestureSettingsTouch:{pinchToZoom:true,dragToPan:true},tileSources:tileSourceFromConfig()};}\n",
    "function makeMultiViewPane(index,count){const host=multiViewHost(),pane=document.createElement('div'),label=document.createElement('div'),viewerDiv=document.createElement('div');pane.className='multiViewPane';pane.dataset.index=String(index);label.className='multiViewPaneTitle';label.textContent='View '+(index+1);viewerDiv.className='multiViewPaneViewer';pane.append(label,viewerDiv);pane.addEventListener('pointerdown',()=>setMultiViewActive(index));pane.addEventListener('mouseenter',()=>setMultiViewActive(index));host.appendChild(pane);let viewer=null;try{viewer=OpenSeadragon(multiViewOsdOptions(viewerDiv));}catch(e){notify('Could not create multi-view pane','warning',3600);return {element:pane,viewer:null};}viewer.addHandler('open',()=>{if(multiViewSync&&typeof osdViewer!=='undefined'&&osdViewer&&osdReady)copyViewportBetween(osdViewer,viewer,true);else zoomPaneToSlideBounds(viewer,multiViewInitialBounds(index,count));});['animation','animation-finish'].forEach(name=>viewer.addHandler(name,()=>syncMultiViewFrom(viewer)));return {element:pane,viewer:viewer};}\n",
    "function buildMultiViewPanes(count){const host=multiViewHost();if(!host)return;destroyMultiViewPanes();host.className='multiViewGrid layout'+count;host.style.setProperty('--multi-view-count',String(count));for(let i=0;i<count;i++)multiViewPanes.push(makeMultiViewPane(i,count));setMultiViewActive(0);}\n",
    "function setMultiViewLayout(count,silent=false){count=[1,2,4,6].includes(Number(count))?Number(count):1;if(count<=1){destroyMultiViewPanes();multiViewLayout=1;document.body.classList.remove('multiViewActive');updateMultiViewControls();if(typeof requestDraw==='function')requestDraw();if(!silent)scheduleViewerStateSync('multi_view_layout_updated',{layout:1,sync:multiViewSync});return true;}if(!multiViewSupported()){notify('Multi-view tissue display needs the tiled OpenSeadragon viewer. Open the slide in tiled mode for side-by-side tissue panes.','warning',5200);multiViewLayout=1;updateMultiViewControls();return false;}multiViewLayout=count;document.body.classList.add('multiViewActive');buildMultiViewPanes(count);updateMultiViewControls();if(!silent){notify('Multi-view tissue display: '+count+' panes','success',1800);scheduleViewerStateSync('multi_view_layout_updated',{layout:multiViewLayout,sync:multiViewSync});}return true;}\n",
    "function refreshMultiViewSources(){if(multiViewLayout>1)setTimeout(()=>setMultiViewLayout(multiViewLayout,true),0);}\n",
    "function activeMultiViewViewer(){if(multiViewLayout<=1||!multiViewPanes.length)return null;const pane=multiViewPanes[clamp(multiViewActiveIndex,0,multiViewPanes.length-1)];return pane&&pane.viewer?pane.viewer:null;}\n",
    "function multiViewTargets(){if(multiViewLayout<=1)return[];if(multiViewSync)return multiViewPanes.map(p=>p.viewer).filter(Boolean);const v=activeMultiViewViewer();return v?[v]:[];}\n",
    "function multiViewZoomAt(factor){const targets=multiViewTargets();if(!targets.length)return false;targets.forEach(viewer=>{const elmt=viewer.element||viewer.container,rect=elmt&&elmt.getBoundingClientRect?elmt.getBoundingClientRect():{width:innerWidth,height:innerHeight};const point=viewer.viewport.pointFromPixel(new OpenSeadragon.Point(rect.width/2,rect.height/2),true);viewer.viewport.zoomBy(Number(factor)||1,point,false);viewer.viewport.applyConstraints(false);});if(multiViewSync)syncMultiViewFrom(targets[0]);return true;}\n",
    "function multiViewFitView(){const targets=multiViewTargets();if(!targets.length)return false;targets.forEach(viewer=>viewer.viewport.goHome(false));if(multiViewSync)syncMultiViewFrom(targets[0]);return true;}\n",
    "function multiViewOneToOne(){const targets=multiViewTargets();if(!targets.length)return false;targets.forEach(viewer=>{try{const item=viewer.world&&viewer.world.getItemAt(0);const zoom=item&&typeof item.imageToViewportZoom==='function'?item.imageToViewportZoom(1):NaN;if(Number.isFinite(zoom)&&zoom>0)viewer.viewport.zoomTo(zoom,null,false);}catch(e){}});if(multiViewSync)syncMultiViewFrom(targets[0]);return true;}\n",
    "function bindMultiViewControls(){document.querySelectorAll('.multiViewLayout').forEach(button=>{button.onclick=()=>setMultiViewLayout(Number(button.dataset.layout||1));});const sync=el('multiViewSync');if(sync)sync.onchange=e=>{multiViewSync=!!e.target.checked;updateMultiViewControls();if(multiViewSync){const v=activeMultiViewViewer();if(v)syncMultiViewFrom(v);}scheduleViewerStateSync('multi_view_sync_updated',{layout:multiViewLayout,sync:multiViewSync});};updateMultiViewControls();}\n"
  )
}

wsi_viewer_trajectory_js <- function() {
  paste0(
    "function trajectoryResolution(){const input=el('trajectoryResolution'),label=el('trajectoryResolutionValue');const value=Math.max(5,Math.min(200,Math.round(Number(input&&input.value?input.value:20))));if(label)label.textContent=String(value);return value;}\n",
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
    "function selectedTrajectoryRecord(){if(selectedTrajectory>=0&&trajectories[selectedTrajectory])return trajectories[selectedTrajectory];return trajectories.length?trajectories[trajectories.length-1]:null;}\n",
    "function trajectoryAreaSource(){if(trajectoryDraft.length>=2)return {draft:true,id:null,name:'Draft trajectory',points:currentTrajectoryPreview()};const rec=selectedTrajectoryRecord();return rec?{draft:false,id:rec.id,name:rec.name,points:(rec.points||[]).map(copySlidePoint)}:null;}\n",
    "function trajectoryAreaPreviewRing(points,width){const pts=(points||[]).filter(pointInsideSlide).map(copySlidePoint);if(pts.length<2||typeof brushRingFromPoints!=='function')return [];return brushRingFromPoints(pts,Math.max(1,Number(width||trajectoryAreaWidth())/2));}\n",
    "function trajectoryAreaGeometry(points,width,className=null){const pts=(points||[]).filter(pointInsideSlide).map(copySlidePoint),radius=Math.max(1,Number(width||trajectoryAreaWidth())/2);if(pts.length<2)return null;if(typeof brushMaskGeometry==='function'){const protect=(typeof brushProtectionForClass==='function')?brushProtectionForClass(className||currentRoiClass(),-1):[];return brushMaskGeometry(pts,radius,null,'new',protect);}const ring=trajectoryAreaPreviewRing(pts,Number(width||trajectoryAreaWidth()));return ring.length?{type:'Polygon',groups:[[ring]],rings:[ring],ring:ring,bbox:boundsFromRings([ring]),mask_contour:false}:null;}\n",
    "function drawTrajectoryAreaPreview(){if(!trajectoryAreaPreviewEnabled())return;const src=trajectoryAreaSource();if(!src||!src.points||src.points.length<2)return;const ring=trajectoryAreaPreviewRing(src.points,trajectoryAreaWidth());if(!ring.length)return;const colour=classColour(currentRoiClass(),'#facc15');ctx.save();ctx.beginPath();drawPathRings([ring]);ctx.fillStyle=hexToRgba(colour,.16);ctx.strokeStyle=colour;ctx.lineWidth=2;ctx.setLineDash([8,5]);ctx.fill('evenodd');ctx.stroke();ctx.setLineDash([]);ctx.restore();}\n",
    "function createTrajectoryAreaRoi(){let rec=null;if(trajectoryDraft.length>=2)rec=finishTrajectory(false);else rec=selectedTrajectoryRecord();if(!rec||!rec.points||rec.points.length<2){notify('Draw or select a trajectory first','warning');return;}const width=trajectoryAreaWidth(),cls=currentRoiClass(),geom=trajectoryAreaGeometry(rec.points,width,cls);if(!geom||!geom.groups||!geom.groups.length){notify('Could not create trajectory area','warning');return;}const roi=addRoiFromBrushGroups(geom.groups,'trajectory','Trajectory area',cls);if(!roi)return;roi.source='trajectory';roi.trajectory_area=true;roi.trajectory_id=rec.id||null;roi.trajectory_width_px=width;roi.trajectory_point_count=(rec.points||[]).length;roi.properties=Object.assign({},roi.properties||{},{wsiToolsTrajectory:{id:rec.id||null,name:rec.name||null,width_px:width,point_count:(rec.points||[]).length}});refreshRoiGeometry(roi);rec.area_width_px=width;rec.area_roi_id=roi.id||null;renderTrajectoryList();buildRoiList();updateButtons();recordAnnotationHistory('trajectory_area_created',{trajectory_id:rec.id||null,roi_id:roi.id||null,width_px:width,class:roi.class||cls},false);scheduleViewerStateSync('trajectory_area_created',{trajectory_id:rec.id||null,roi_id:roi.id||null,width_px:width,class:roi.class||cls});notify('Trajectory area created: '+width+' px wide','success');draw();}\n",
    "function drawTrajectoryPath(points,trajectory=null,preview=false){points=(points||[]).filter(p=>p&&Number.isFinite(Number(p.x))&&Number.isFinite(Number(p.y)));if(points.length<2)return;ctx.save();ctx.lineCap='round';ctx.lineJoin='round';ctx.beginPath();points.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.strokeStyle='rgba(255,255,255,.92)';ctx.lineWidth=preview?5:6;if(preview)ctx.setLineDash([8,5]);ctx.stroke();ctx.setLineDash([]);ctx.beginPath();points.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.strokeStyle=preview?'#facc15':((trajectory&&trajectory.colour)||'#ef4444');ctx.lineWidth=preview?3:3.5;ctx.stroke();const controls=(trajectory&&trajectory.control_points)||trajectoryDraft;if(controls&&controls.length){controls.forEach((p,i)=>{const q=slideToCanvas(p);ctx.beginPath();if(i===0){ctx.rect(q.x-5,q.y-5,10,10);}else{ctx.arc(q.x,q.y,4,0,Math.PI*2);}ctx.fillStyle='#eeeeee';ctx.strokeStyle=preview?'#facc15':((trajectory&&trajectory.colour)||'#ef4444');ctx.lineWidth=2;ctx.fill();ctx.stroke();});}if(!preview&&trajectory&&trajectory.name){const q=slideToCanvas(points[Math.floor(points.length/2)]);ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';const text=trajectory.name,w=ctx.measureText(text).width+8,x=clamp(q.x-w/2,4,innerWidth-w-4),y=clamp(q.y-24,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.72)';ctx.fillRect(x,y,w,18);ctx.fillStyle=(trajectory&&trajectory.colour)||'#ef4444';ctx.fillText(text,x+4,y+3);}ctx.restore();}\n",
    "function drawTrajectories(){drawTrajectoryAreaPreview();trajectories.forEach((t,i)=>drawTrajectoryPath(t.points,Object.assign({},t,{colour:i===selectedTrajectory?'#facc15':(t.colour||'#ef4444')}),false));if(mode==='trajectory'&&trajectoryDraft.length){const preview=currentTrajectoryPreview();drawTrajectoryPath(preview,{control_points:trajectoryDraft,colour:'#facc15'},true);}}\n",
    "function trajectoryPayload(){return trajectories.map(t=>({id:t.id,name:t.name,n:t.n,length_px:t.length_px,area_width_px:Number.isFinite(Number(t.area_width_px))?Number(t.area_width_px):null,area_roi_id:t.area_roi_id||null,control_points:t.control_points,points:t.points,created:t.created||null}));}\n",
    "function renderTrajectoryList(){if(typeof enforceSingleObjectSelection==='function')enforceSingleObjectSelection('trajectory');const summary=el('trajectorySummary'),list=el('trajectoryList');if(summary)summary.textContent=trajectories.length?(trajectories.length+' trajector'+(trajectories.length===1?'y':'ies')+' saved'):'No trajectories yet.';if(list){list.innerHTML='';trajectories.forEach((t,i)=>{const b=document.createElement('button');b.type='button';b.className='trajectoryItem';b.classList.toggle('active',i===selectedTrajectory);const len=Number.isFinite(Number(t.length_px))?fmt(t.length_px,1)+' px':'NA',area=Number.isFinite(Number(t.area_width_px))?(' | area '+fmt(Number(t.area_width_px),0)+' px'):'';b.append(document.createTextNode(t.name||('Trajectory '+(i+1))));b.append(document.createElement('br'));const code=document.createElement('code');code.textContent=(t.points?t.points.length:0)+' points | '+len+area;b.append(code);b.onclick=()=>{selectTrajectory(i,true);zoomToTrajectory(t);notify((t.name||('Trajectory '+(i+1)))+': '+len,'info',3600);draw();};list.appendChild(b);});}updateTrajectoryButtons();}\n",
    "function updateTrajectoryList(){renderTrajectoryList();}\n",
    "function trajectoryHitTolerance(){try{return Math.max(8,12/Math.max(slideUnitScale(),1e-9));}catch(e){return 12;}}\n",
    "function trajectoryAt(p){if(!p||typeof trajectories==='undefined')return -1;const tol=trajectoryHitTolerance();for(let i=trajectories.length-1;i>=0;i--){const pts=((trajectories[i]&&trajectories[i].points)||[]).filter(q=>q&&Number.isFinite(Number(q.x))&&Number.isFinite(Number(q.y)));for(let j=1;j<pts.length;j++){if(pointLineDistance(p,pts[j-1],pts[j])<=tol)return i;}}return -1;}\n",
    "function updateTrajectoryButtons(){const finish=el('finishTrajectory'),undo=el('undoTrajectoryPoint'),clear=el('clearTrajectories'),area=el('trajectoryAreaRoi');if(finish)finish.disabled=trajectoryDraft.length<2;if(undo)undo.disabled=trajectoryDraft.length<1;if(clear)clear.disabled=!trajectoryDraft.length&&!trajectories.length;if(area)area.disabled=trajectoryDraft.length<2&&!trajectories.length;const tool=el('toolTrajectory');if(tool)tool.classList.toggle('active',mode==='trajectory');trajectoryAreaWidth();}\n",
    "function addTrajectoryPoint(p){if(!pointInsideSlide(p))return;trajectoryDraft.push(copySlidePoint(p));renderTrajectoryList();draw();}\n",
    "function undoTrajectoryPoint(){if(!trajectoryDraft.length)return;trajectoryDraft.pop();renderTrajectoryList();draw();}\n",
    "function finishTrajectory(toast=true){if(trajectoryDraft.length<2){notify('Add at least 2 trajectory points','warning');return null;}pushAnnotationUndo('trajectory_added');const n=trajectoryResolution(),points=smoothTrajectoryPoints(trajectoryDraft,n);trajectorySeq++;const rec={id:'trajectory_'+trajectorySeq,name:'Trajectory '+trajectorySeq,n:n,control_points:trajectoryDraft.map(copySlidePoint),points:points.map(copySlidePoint),length_px:trajectoryLength(points),area_width_px:null,area_roi_id:null,colour:'#ef4444',created:new Date().toISOString()};trajectories.push(rec);trajectoryDraft=[];selectTrajectory(trajectories.length-1,true);recordAnnotationHistory('trajectory_added',{id:rec.id,name:rec.name,control_count:rec.control_points.length,point_count:rec.points.length},false);scheduleViewerStateSync('trajectory_added',{id:rec.id,name:rec.name,control_count:rec.control_points.length,point_count:rec.points.length,length_px:rec.length_px});if(toast!==false){notify(rec.name+' saved; trajectory drawing off','success');setMode('pan');}draw();return rec;}\n",
    "function clearTrajectories(){if(!trajectoryDraft.length&&!trajectories.length)return;pushAnnotationUndo('trajectories_cleared');trajectoryDraft=[];trajectories=[];selectedTrajectory=-1;renderTrajectoryList();recordAnnotationHistory('trajectories_cleared',{},false);scheduleViewerStateSync('trajectories_cleared',{});notify('Trajectories cleared','success');draw();}\n",
    "function trajectoryStatus(){if(mode==='trajectory'){if(trajectoryDraft.length)return ' | trajectory '+trajectoryDraft.length+' control point'+(trajectoryDraft.length===1?'':'s');return ' | click trajectory points';}return trajectories.length?(' | trajectories '+trajectories.length):'';}\n",
    "function bindTrajectoryControls(){const tool=el('toolTrajectory'),finish=el('finishTrajectory'),undo=el('undoTrajectoryPoint'),clear=el('clearTrajectories'),resolution=el('trajectoryResolution'),width=el('trajectoryAreaWidth'),area=el('trajectoryAreaRoi'),preview=el('trajectoryAreaPreview');if(tool)tool.onclick=()=>setMode('trajectory');if(finish)finish.onclick=()=>finishTrajectory(true);if(undo)undo.onclick=undoTrajectoryPoint;if(clear)clear.onclick=clearTrajectories;if(area)area.onclick=createTrajectoryAreaRoi;if(resolution)resolution.oninput=()=>{trajectoryResolution();draw();};if(width)width.oninput=()=>{trajectoryAreaWidth();draw();};if(preview)preview.onchange=draw;trajectoryResolution();trajectoryAreaWidth();renderTrajectoryList();}\n"
  )
}

wsi_viewer_segmentation_js <- function() {
  paste0(
    "function segmentationStatus(msg,type='info',toast=false){const box=el('segmentationSummary');if(box)box.textContent=msg||'';if(msg&&toast)notify(msg,type);}\n",
    "function selectedRoiFeatureText(){if(selectedRoi<0||!rois[selectedRoi])return null;const feature=roiFeature(rois[selectedRoi],selectedRoi);if(!feature)return null;return JSON.stringify({type:'FeatureCollection',features:[feature]},null,2);}\n",
    "function exportSelectedRoiForSegmentation(){const text=selectedRoiFeatureText();if(!text){segmentationStatus('Select an ROI before exporting a StarDist region.','warning',true);return;}const roi=rois[selectedRoi],name=(roi.id||roi.name||'selected_roi').replace(/[^A-Za-z0-9_.-]+/g,'_');downloadText(text,name+'_stardist_roi.geojson');segmentationStatus('Exported selected ROI GeoJSON. Use stardist_segment_roi() or your StarDist pipeline, then load result GeoJSON.');notify('GeoJSON exported','success');}\n",
    "function segmentationOffset(){const local=!!(el('segLocalCoords')&&el('segLocalCoords').checked),base=(local&&selectedRoi>=0&&rois[selectedRoi])?roiBounds(rois[selectedRoi]):null;return base?{x:base.xmin,y:base.ymin}:{x:0,y:0};}\n",
    "function segmentationCellRadius(){const input=el('segCellRadius'),label=el('segCellRadiusValue');const value=Math.max(1,Number(input&&input.value?input.value:8));if(label)label.textContent=Math.round(value)+' px';return value;}\n",
    "function coordPoint(coord,offset){if(!coord||coord.length<2)return null;const x=Number(coord[0]),y=Number(coord[1]);if(!Number.isFinite(x)||!Number.isFinite(y))return null;return {x:x+offset.x,y:y+offset.y};}\n",
    "function ringFromCoords(coords,offset){const ring=(coords||[]).map(c=>coordPoint(c,offset)).filter(Boolean);return closedRing(ring);}\n",
    "function ringsFromGeojsonGeometry(geometry,offset){if(!geometry)return [];const type=String(geometry.type||'').toLowerCase(),coords=geometry.coordinates||[];if(type==='polygon')return coords.map(r=>ringFromCoords(r,offset)).filter(r=>r.length>=4);if(type==='multipolygon'){let rings=[];coords.forEach(poly=>{rings=rings.concat((poly||[]).map(r=>ringFromCoords(r,offset)).filter(r=>r.length>=4));});return rings;}return [];}\n",
    "function featureClassName(properties){const cls=properties&&properties.classification;if(cls&&typeof cls==='object'&&cls.name)return cls.name;if(properties&&properties.class)return properties.class;if(typeof cls==='string')return cls;return 'cell';}\n",
    "function addSegmentationGeojson(obj,options={}){const features=geojsonFeatures(obj);if(!features.length){segmentationStatus('No GeoJSON features found in segmentation file.','warning',true);return;}const previousSelected=selectedRoi,useLocal=Object.prototype.hasOwnProperty.call(options,'local')?!!options.local:!!(el('segLocalCoords')&&el('segLocalCoords').checked),offset=useLocal?segmentationOffset():{x:0,y:0};let added=0;features.forEach((feature,i)=>{const rings=ringsFromGeojsonGeometry(feature.geometry||{},offset);if(!rings.length)return;const props=feature.properties||{},name=props.name||props.label||feature.id||('StarDist cell '+(i+1)),cls=featureClassName(props),colour=classColour(String(cls),'#38bdf8');const roi={id:String(feature.id||('stardist_cell_'+Date.now()+'_'+i)),name:String(name),label:String(name),class:String(cls),geometry_type:'Polygon',source:'stardist',drawable:true,point_count:pointCount({rings:rings}),area:polygonArea(rings),bbox:null,colour:colour,original_colour:colour,fill:hexToRgba(colour,0.12),rings:rings,measurements:props.measurements||null,centroid:props.centroid||props.center||null,edited:true};refreshRoiGeometry(roi);rois.push(roi);added++;});if(!added){segmentationStatus('Segmentation GeoJSON did not contain polygon or multipolygon cells.','warning',true);return;}if(options.keepSelection&&previousSelected>=0&&rois[previousSelected])selectedRoi=previousSelected;else selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();const detail=Object.assign({added:added,type:'geojson'},options.detail||{});recordAnnotationHistory(detail.job_id?'stardist_ran':'segmentation_imported',detail);scheduleViewerStateSync('segmentation_added',detail);segmentationStatus('Loaded '+countText(added)+' StarDist cell polygon'+(added===1?'':'s')+'.');notify('StarDist finished: '+countText(added)+' cell'+(added===1?'':'s'),'success',3600);}\n",
    "function parseDelimitedLine(line,delimiter){const out=[];let cur='',quoted=false;for(let i=0;i<line.length;i++){const ch=line[i];if(ch==='\"'){if(quoted&&line[i+1]==='\"'){cur+='\"';i++;}else quoted=!quoted;}else if(ch===delimiter&&!quoted){out.push(cur);cur='';}else cur+=ch;}out.push(cur);return out.map(x=>x.trim());}\n",
    "function parseDelimitedTable(text){const lines=String(text||'').split(/\\r?\\n/).filter(line=>line.trim().length);if(!lines.length)return {headers:[],rows:[]};const delimiter=lines[0].indexOf('\\t')>=0?'\\t':',';const headers=parseDelimitedLine(lines[0],delimiter);const rows=lines.slice(1).map(line=>parseDelimitedLine(line,delimiter));return {headers:headers,rows:rows};}\n",
    "function headerIndex(headers,names){const lower=headers.map(h=>String(h).toLowerCase().trim());for(const name of names){const idx=lower.indexOf(name);if(idx>=0)return idx;}return -1;}\n",
    "function cellRing(center,radius,steps=18){const pts=[];for(let i=0;i<steps;i++){const a=i/steps*Math.PI*2;pts.push({x:center.x+Math.cos(a)*radius,y:center.y+Math.sin(a)*radius});}return closedRing(pts);}\n",
    "function addSegmentationCentroidTable(text,fileName){const table=parseDelimitedTable(text),headers=table.headers,rows=table.rows,xi=headerIndex(headers,['x','centroid_x','center_x','centre_x']),yi=headerIndex(headers,['y','centroid_y','center_y','centre_y']);if(xi<0||yi<0){segmentationStatus('CSV/TSV must contain x/y or centroid_x/centroid_y columns.','warning',true);return;}const idIdx=headerIndex(headers,['cell_id','id','object_id','label']),offset=segmentationOffset(),radius=segmentationCellRadius(),colour=classColour('cell','#38bdf8');let added=0;rows.forEach((row,i)=>{const x=Number(row[xi]),y=Number(row[yi]);if(!Number.isFinite(x)||!Number.isFinite(y))return;const p={x:x+offset.x,y:y+offset.y},ring=cellRing(p,radius),id=String((idIdx>=0&&row[idIdx])?row[idIdx]:('stardist_cell_'+(i+1))),roi={id:id,name:id,label:id,class:'cell',geometry_type:'Polygon',source:'stardist',drawable:true,point_count:ring.length-1,area:polygonArea([ring]),bbox:boundsFromRing(ring),colour:colour,original_colour:colour,fill:hexToRgba(colour,0.12),rings:[ring],edited:true,centroid:{x:p.x,y:p.y},source_file:fileName||''};refreshRoiGeometry(roi);rois.push(roi);added++;});if(!added){segmentationStatus('No numeric StarDist centroids were found in the table.','warning',true);return;}selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();const detail={added:added,type:'centroids',file:fileName||null};recordAnnotationHistory('segmentation_imported',detail);scheduleViewerStateSync('segmentation_added',detail);segmentationStatus('Loaded '+countText(added)+' StarDist centroid cell marker'+(added===1?'':'s')+'.');notify('StarDist finished: '+countText(added)+' cell'+(added===1?'':'s'),'success',3600);}\n",
    "function segmentationResultDetail(result){if(!result||result.type==='FeatureCollection'||result.type==='Feature')return {};const keys=['message','crop','output','slide_output','roi_id','bbox','status','segmentation_type'];const detail={};keys.forEach(k=>{if(Object.prototype.hasOwnProperty.call(result,k))detail[k]=result[k];});return detail;}\n",
    "function copyViewerText(text){if(navigator.clipboard&&navigator.clipboard.writeText)return navigator.clipboard.writeText(text);return new Promise((resolve,reject)=>{try{const area=document.createElement('textarea');area.value=text;area.setAttribute('readonly','');area.style.position='fixed';area.style.left='-9999px';document.body.appendChild(area);area.select();const ok=document.execCommand('copy');document.body.removeChild(area);ok?resolve():reject(new Error('copy failed'));}catch(e){reject(e);}});}\n",
    "function stardistSetupCommand(){return 'wsi_install_stardist(method = \"conda\")\\nviewer <- wsi_viewer_live(slide, stardist = TRUE, wait = FALSE)';}\n",
    "function stardistNotConfiguredMessage(){return 'No StarDist command was found. Install/configure it, or load a segmentation GeoJSON/CSV instead.';}\n",
    "function showStardistNotConfigured(){const command=stardistSetupCommand(),message=stardistNotConfiguredMessage();segmentationStatus(message+' Copyable R command: '+command.replace(/\\n/g,' '));if(typeof notifyAction==='function')notifyAction(message,'Copy R command',()=>copyViewerText(command).then(()=>notify('R command copied','success')).catch(()=>notify('Could not copy R command','error')),'warning',7600);else notify(message,'warning',5200);}\n",
    "async function startSegmentationForSelectedRoi(){const url=cfg.segmentation_run_url||'';if(!url){showStardistNotConfigured();return;}const text=selectedRoiFeatureText();if(!text){segmentationStatus('Select an ROI before running segmentation.','warning',true);return;}const roi=rois[selectedRoi]||{},roiId=roi.id||roi.name||null,button=el('startSegmentation'),jobId='stardist_'+Date.now();if(typeof upsertViewerJob==='function')upsertViewerJob({id:jobId,name:'StarDist selected ROI',status:'queued',progress:0,progress_available:true,message:'Queued selected ROI segmentation.',log:[]});if(button)button.disabled=true;segmentationStatus('Sending selected ROI to R...');notify('Segmentation started','info');try{if(liveSyncAvailable())await syncViewerState('segmentation_requested',{roi_id:roiId,engine:'stardist',job_id:jobId});if(typeof updateViewerJob==='function')updateViewerJob(jobId,{status:'running',message:'Running segmentation on selected ROI.',log:['ROI sent to R.']});segmentationStatus('Running segmentation on selected ROI...');const response=await fetch(url,{method:'POST',headers:{'Content-Type':'text/plain;charset=UTF-8'},body:text});const body=await response.text();if(!response.ok){let detail=body;try{const err=JSON.parse(body);detail=err.error||err.message||body;}catch(e){}throw new Error(detail||('HTTP '+response.status));}const result=JSON.parse(body);const geojson=result&&result.geojson?result.geojson:result,detail=segmentationResultDetail(result),log=Array.isArray(result&&result.log)?result.log:[];if(result&&result.message)segmentationStatus(result.message);if(geojson&&(geojson.type||geojson.features)){const n=geojsonFeatures(geojson).length;addSegmentationGeojson(geojson,{local:false,detail:Object.assign({job_id:jobId},detail),keepSelection:true});if(typeof updateViewerJob==='function')updateViewerJob(jobId,{status:'completed',progress:100,progress_available:true,message:'StarDist completed: '+countText(n)+' cell'+(n===1?'':'s')+'.',log:log.concat(result&&result.message?[result.message]:[])});}else{scheduleViewerStateSync('segmentation_completed',Object.assign({job_id:jobId},detail));segmentationStatus((result&&result.message)||'Segmentation completed, but no GeoJSON overlay was returned.');if(typeof updateViewerJob==='function')updateViewerJob(jobId,{status:'completed',progress:100,progress_available:true,message:'Segmentation completed, but no GeoJSON overlay was returned.',log:log});notify('Segmentation finished','warning');}}catch(e){segmentationStatus('Segmentation run failed: '+e.message);if(typeof updateViewerJob==='function')updateViewerJob(jobId,{status:'failed',message:e.message,log:[e.message]});notify('Segmentation failed','error',4200);}finally{if(button)button.disabled=false;}}\n",
    "function clearSegmentationOverlays(){const before=rois.length;for(let i=rois.length-1;i>=0;i--){if(rois[i].source==='stardist')rois.splice(i,1);}if(selectedRoi>=rois.length)selectedRoi=rois.length-1;buildRoiList();updateButtons();draw();scheduleViewerStateSync('segmentation_cleared',{});segmentationStatus('Removed '+countText(before-rois.length)+' StarDist overlay'+(before-rois.length===1?'':'s')+'.');notify('Segmentation cleared','success');}\n",
    "function bindSegmentationControls(){const exportButton=el('exportSelectedRoi'),startButton=el('startSegmentation'),loadButton=el('loadSegmentation'),loadCsvButton=el('loadSegmentationCsv'),clearButton=el('clearSegmentation'),file=el('segmentationFile'),tableFile=el('segmentationTableFile'),radius=el('segCellRadius');if(exportButton)exportButton.onclick=exportSelectedRoiForSegmentation;if(startButton)startButton.onclick=startSegmentationForSelectedRoi;if(loadButton&&file)loadButton.onclick=()=>{file.value='';file.click();};if(loadCsvButton&&tableFile)loadCsvButton.onclick=()=>{tableFile.value='';tableFile.click();};if(clearButton)clearButton.onclick=clearSegmentationOverlays;if(radius){radius.oninput=()=>segmentationCellRadius();segmentationCellRadius();}if(file){file.onchange=()=>{const picked=file.files&&file.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addSegmentationGeojson(JSON.parse(reader.result));}catch(e){segmentationStatus('Could not read StarDist GeoJSON: '+e.message);}};reader.readAsText(picked);};}if(tableFile){tableFile.onchange=()=>{const picked=tableFile.files&&tableFile.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addSegmentationCentroidTable(reader.result,picked.name);}catch(e){segmentationStatus('Could not read StarDist centroid table: '+e.message);}};reader.readAsText(picked);};}segmentationStatus('');}\n"
  )
}

wsi_viewer_project_js <- function() {
  paste0(
    "const projectItems=(cfg.project&&cfg.project.items)||[];\n",
    "let activeProjectIndex=Number((cfg.project&&cfg.project.active_index)||0);\n",
    "let activeProjectSectionIndex=-1;\n",
    "let projectDragIndex=-1;\n",
    "const projectAnnotationStore=new Map();\n",
    "function cloneProjectValue(value){try{return JSON.parse(JSON.stringify(value));}catch(e){return value;}}\n",
    "function projectSectionIsPyramidLevel(section){if(!section)return false;const status=String(section.status||'').toLowerCase(),id=String(section.id||'').toLowerCase(),label=String(section.label||'').toLowerCase();return status==='pyramid level'||section.project_section_type==='pyramid_level'||/^level_[0-9]+$/.test(id)||/^level\\s+[0-9]+:/.test(label);}\n",
    "function projectSections(item){return (Array.isArray(item&&item.sections)?item.sections:[]).filter(section=>!projectSectionIsPyramidLevel(section));}\n",
    "function defaultProjectSectionIndex(item){const sections=projectSections(item);if(!sections.length)return -1;const source=String((item&&item.image_data_uri)||'');let idx=sections.findIndex(s=>s&&s.image_data_uri&&String(s.image_data_uri)===source);if(idx<0)idx=sections.findIndex(s=>s&&s.image_data_uri);return idx;}\n",
    "activeProjectSectionIndex=defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);\n",
    "function activeProjectSection(){const item=projectItems[activeProjectIndex]||null,sections=projectSections(item);return activeProjectSectionIndex>=0?sections[activeProjectSectionIndex]:null;}\n",
    "function projectSafeName(value){let x=String(value||'').replace(/\\.[^.]+$/,'').replace(/[^A-Za-z0-9_-]+/g,'_').replace(/^_+|_+$/g,'');return x||'annotations';}\n",
    "function projectAnnotationKey(index=activeProjectIndex,sectionIndex=activeProjectSectionIndex){const item=projectItems[index]||{},sections=projectSections(item),section=sectionIndex>=0?sections[sectionIndex]:null,itemId=projectSafeName(item.id||item.path||item.label||('image_'+(index+1))),sectionId=section?projectSafeName(section.id||section.label||('section_'+(sectionIndex+1))):'image';return itemId+'::'+sectionId;}\n",
    "function projectAnnotationSnapshotForStore(){return {rois:cloneProjectValue(rois),selectedRoi:selectedRoi,newRoiCount:newRoiCount,dirty:annotationsDirty,dirty_reason:annotationDirtyReason||'',undo:cloneProjectValue(annotationUndo||[]),redo:cloneProjectValue(annotationRedo||[]),trajectories:cloneProjectValue(typeof trajectoryPayload==='function'?trajectoryPayload():trajectories),selectedTrajectory:selectedTrajectory,trajectorySeq:trajectorySeq};}\n",
    "function saveActiveProjectAnnotations(){if(!projectItems.length)return;projectAnnotationStore.set(projectAnnotationKey(),projectAnnotationSnapshotForStore());}\n",
    "let projectAnnotationStorePreloaded=false;\n",
    "function preloadProjectAnnotations(){if(projectAnnotationStorePreloaded)return;projectAnnotationStorePreloaded=true;const sets=(cfg.project&&Array.isArray(cfg.project.annotation_sets))?cfg.project.annotation_sets:[];sets.forEach(set=>{if(!set)return;const key=String(set.key||'');if(!key)return;projectAnnotationStore.set(key,{rois:cloneProjectValue(set.rois||[]),selectedRoi:Number.isFinite(Number(set.selectedRoi))?Number(set.selectedRoi):-1,newRoiCount:Number.isFinite(Number(set.newRoiCount))?Number(set.newRoiCount):(set.rois||[]).length,dirty:!!set.dirty,dirty_reason:String(set.dirty_reason||''),undo:cloneProjectValue(set.undo||[]),redo:cloneProjectValue(set.redo||[]),trajectories:cloneProjectValue(set.trajectories||[]),selectedTrajectory:Number.isFinite(Number(set.selectedTrajectory))?Number(set.selectedTrajectory):-1,trajectorySeq:Number.isFinite(Number(set.trajectorySeq))?Number(set.trajectorySeq):(set.trajectories||[]).length});});}\n",
    "function loadProjectAnnotations(redraw=true){if(!projectItems.length)return;const key=projectAnnotationKey();let state=projectAnnotationStore.get(key);if(!state){state={rois:[],selectedRoi:-1,newRoiCount:0,dirty:false,dirty_reason:'',undo:[],redo:[],trajectories:[],selectedTrajectory:-1,trajectorySeq:0};projectAnnotationStore.set(key,cloneProjectValue(state));}rois.splice(0,rois.length);(cloneProjectValue(state.rois)||[]).forEach(roi=>rois.push(roi));selectedRoi=Math.min(Math.max(Number(state.selectedRoi||-1),-1),rois.length-1);if(!Number.isFinite(selectedRoi))selectedRoi=-1;newRoiCount=Number.isFinite(Number(state.newRoiCount))?Number(state.newRoiCount):0;annotationUndo.splice(0,annotationUndo.length);(cloneProjectValue(state.undo)||[]).forEach(x=>annotationUndo.push(x));annotationRedo.splice(0,annotationRedo.length);(cloneProjectValue(state.redo)||[]).forEach(x=>annotationRedo.push(x));trajectories.splice(0,trajectories.length);(cloneProjectValue(state.trajectories)||[]).forEach(x=>trajectories.push(x));selectedTrajectory=Math.min(Math.max(Number(state.selectedTrajectory||-1),-1),trajectories.length-1);if(!Number.isFinite(selectedTrajectory))selectedTrajectory=-1;trajectorySeq=Number.isFinite(Number(state.trajectorySeq))?Number(state.trajectorySeq):trajectories.length;draft=[];measureStart=null;trajectoryDraft=[];brushing=false;brushPoints=[];activeVertex=null;draggingVertex=null;setAnnotationsDirty(!!state.dirty,state.dirty_reason||'',false);if(typeof buildRoiList==='function')buildRoiList();if(typeof updateTrajectoryList==='function')updateTrajectoryList();if(typeof updateButtons==='function')updateButtons();if(redraw&&typeof draw==='function')draw();}\n",
    "function projectAnnotationCounts(){const counts=[];projectAnnotationStore.forEach((state,key)=>counts.push({key:key,count:(state.rois||[]).length,trajectory_count:(state.trajectories||[]).length,dirty:!!state.dirty}));return counts;}\n",
    "function projectAnnotationFilename(){if(!projectItems.length)return cfg.annotation_filename||'wsiTools_annotations.geojson';const item=projectItems[activeProjectIndex]||{},section=activeProjectSection(),base=projectSafeName(item.label||item.path||item.id||'wsiTools'),suffix=section?projectSafeName(section.label||section.id||('section_'+(activeProjectSectionIndex+1))):'image';return base+'_'+suffix+'_annotations.geojson';}\n",
    "function projectStatePayload(){saveActiveProjectAnnotations();const item=projectItems[activeProjectIndex]||null,section=activeProjectSection();return {active_index:activeProjectIndex,active_section_index:activeProjectSectionIndex,active_key:projectAnnotationKey(),active:item?{id:item.id||null,label:item.label||null,path:item.path||null,backend:item.backend||null,type:item.type||null,status:item.status||null}:null,section:section?{id:section.id||null,label:section.label||null,scene:section.scene||null,status:section.status||null}:null,count:projectItems.length,annotation_sets:projectAnnotationCounts()};}\n",
    "function projectAnnotationSetsFull(){saveActiveProjectAnnotations();const sets=[];projectAnnotationStore.forEach((state,key)=>sets.push(Object.assign({key:key},cloneProjectValue(state))));return sets;}\n",
    "function projectItemsForSnapshot(includeImageData=true){return cloneProjectValue(projectItems).map(item=>{if(includeImageData)return item;const strip=obj=>{if(!obj)return obj;delete obj.image_data_uri;delete obj.navigator_image_data_uri;return obj;};strip(item);if(Array.isArray(item.sections))item.sections=item.sections.map(strip);return item;});}\n",
    "function projectBrowserSnapshot(includeImageData=true){saveActiveProjectAnnotations();return {schema:'wsiTools-viewer-project',schema_version:1,saved_at:new Date().toISOString(),title:cfg.title||'wsiTools project',viewer_mode:cfg.viewer_mode||'viewer',slide:{width:Number(cfg.slide_width||0),height:Number(cfg.slide_height||0),mpp:cfg.mpp||null,objective_power:cfg.objective_power||null},project:{items:projectItemsForSnapshot(includeImageData),active_index:activeProjectIndex,active_section_index:activeProjectSectionIndex,annotation_sets:projectAnnotationSetsFull()},rois:cloneProjectValue(rois),trajectories:(typeof trajectoryPayload==='function'?trajectoryPayload():cloneProjectValue(trajectories)),measurements:cloneProjectValue(measures||[]),layers:cloneProjectValue(layers||[]),channel_sources:cloneProjectValue(typeof channelSources!=='undefined'?channelSources:[]),channel_settings:(typeof currentChannelSettingsPayload==='function'?currentChannelSettingsPayload():[]),stain:(typeof currentStainPayload==='function'?currentStainPayload():null),view:{mode:mode,scale:scale,offset_x:offsetX,offset_y:offsetY,roi_opacity:roiOpacity,show_rois:showRois,show_labels:showLabels,image_transform:(typeof imageTransformPayload==='function'?imageTransformPayload():null),base_layer:(typeof baseImagePayload==='function'?baseImagePayload():null)},annotations:{dirty:annotationsDirty,dirty_reason:annotationDirtyReason||''},history:(typeof annotationHistoryPayload==='function'?annotationHistoryPayload():[])};}\n",
    "function projectSnapshotName(){return projectSafeName(cfg.title||'wsiTools_project')+'.wsiproject.json';}\n",
    "async function saveProjectFile(){const name=projectSnapshotName();try{let handle=null;if(window.showSaveFilePicker){handle=await window.showSaveFilePicker({suggestedName:name,types:[{description:'wsiTools viewer project',accept:{'application/json':['.json','.wsiproject.json']}}]});}markAnnotationsSaved('project_file_saved');saveActiveProjectAnnotations();const snapshot=projectBrowserSnapshot(true),text=JSON.stringify(snapshot,null,2);if(handle){const w=await handle.createWritable();await w.write(text);await w.close();}else{const blob=new Blob([text],{type:'application/json'}),a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}recordAnnotationHistory('project_saved',{name:name,image_count:projectItems.length,annotation_sets:snapshot.project.annotation_sets.length},false);scheduleViewerStateSync('project_saved',{mode:'browser_file',project_snapshot:projectBrowserSnapshot(false)});projectMenuStatus('Saved '+name+'.');notify('Project saved','success',2200);}catch(e){if(e&&e.name==='AbortError')return;projectMenuStatus('Could not save project: '+e.message);notify('Project save failed','error',4200);}}\n",
    "function restoreBrowserProject(snapshot){if(!snapshot||snapshot.schema!=='wsiTools-viewer-project')throw new Error('This is not a wsiTools viewer project JSON.');saveActiveProjectAnnotations();const project=snapshot.project||{},items=Array.isArray(project.items)?project.items:[];if(!items.length)throw new Error('Project contains no images.');projectItems.splice(0,projectItems.length);items.forEach(item=>projectItems.push(item));activeProjectIndex=clamp(Number(project.active_index||0),0,Math.max(0,projectItems.length-1));activeProjectSectionIndex=Number.isFinite(Number(project.active_section_index))?Number(project.active_section_index):defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);projectAnnotationStore.clear();const sets=Array.isArray(project.annotation_sets)?project.annotation_sets:[];if(sets.length){sets.forEach(set=>{if(set&&set.key)projectAnnotationStore.set(String(set.key),cloneProjectValue(set));});}else{projectAnnotationStore.set(projectAnnotationKey(),{rois:cloneProjectValue(snapshot.rois||[]),selectedRoi:-1,newRoiCount:(snapshot.rois||[]).length,dirty:false,dirty_reason:'',undo:[],redo:[],trajectories:cloneProjectValue(snapshot.trajectories||[]),selectedTrajectory:-1,trajectorySeq:(snapshot.trajectories||[]).length});}projectAnnotationStorePreloaded=true;loadProjectAnnotations(false);if(Array.isArray(snapshot.measurements)){measures.splice(0,measures.length);snapshot.measurements.forEach(m=>measures.push(m));if(typeof updateMeasureList==='function')updateMeasureList();}if(Array.isArray(snapshot.layers)){layers.splice(0,layers.length);snapshot.layers.forEach(layer=>layers.push(layer));if(typeof buildLayerList==='function')buildLayerList();}if(Array.isArray(snapshot.channel_sources)&&typeof upsertChannelSource==='function')snapshot.channel_sources.forEach(upsertChannelSource);if(Array.isArray(snapshot.channel_settings)&&typeof setChannelSettings==='function')snapshot.channel_settings.forEach(s=>setChannelSettings(s.id,s));if(snapshot.stain&&typeof applyStainPreferences==='function')applyStainPreferences({stain:snapshot.stain});openProjectPanel();renderProjectPanel();const item=projectItems[activeProjectIndex]||null;applyProjectPreview(item,activeProjectSection());recordAnnotationHistory('project_opened',{image_count:projectItems.length,source:'file'},false);scheduleViewerStateSync('project_opened',{mode:'browser_file',project_snapshot:projectBrowserSnapshot(false)});projectMenuStatus('Opened saved project with '+projectItems.length+' image'+(projectItems.length===1?'':'s')+'.');notify('Project opened','success',2200);}\n",
    "function openProjectFile(file){if(!file)return;projectMenuStatus('Opening '+file.name+'...');const reader=new FileReader();reader.onerror=()=>{projectMenuStatus('Could not read project file.');notify('Could not read project file','error');};reader.onload=()=>{try{restoreBrowserProject(JSON.parse(String(reader.result||'')));}catch(e){projectMenuStatus('Could not open project: '+e.message);notify('Project open failed','error',5200);}};reader.readAsText(file);}\n",
    "function projectItemCanPreview(item){return !!(item&&item.image_data_uri);}\n",
    "function projectItemCanTile(item){return !!(item&&(item.tile_url_base||item.tile_url_template)&&item.tile_format&&Number.isFinite(Number(item.max_level))&&Number.isFinite(Number(item.tile_size||cfg.tile_size||0)));}\n",
    "const initialProjectSource={width:Number(cfg.slide_width||0),height:Number(cfg.slide_height||0),image_data_uri:String(cfg.image_data_uri||''),navigator_image_data_uri:String(cfg.navigator_image_data_uri||''),tile_size:Number(cfg.tile_size||0),tile_format:String(cfg.tile_format||''),tile_url_base:String(cfg.tile_url_base||''),tile_url_template:String(cfg.tile_url_template||''),tile_url_style:String(cfg.tile_url_style||'deepzoom'),tile_overlap:Number(cfg.tile_overlap||0),min_level:Number(cfg.min_level||0),max_level:Number(cfg.max_level||0)};\n",
    "function projectInitialSource(item){if(!item||!item.active)return null;const hasTiles=!!((initialProjectSource.tile_url_base||initialProjectSource.tile_url_template)&&initialProjectSource.tile_format&&Number.isFinite(Number(initialProjectSource.max_level))&&Number(initialProjectSource.max_level)>0&&Number.isFinite(Number(initialProjectSource.tile_size)));const hasImage=!!initialProjectSource.image_data_uri;if(!hasTiles&&!hasImage)return null;return Object.assign({id:'active_project_image',label:item.label||cfg.title||'Active image',status:'active'},initialProjectSource,{width:Number(item.width||initialProjectSource.width||cfg.slide_width),height:Number(item.height||initialProjectSource.height||cfg.slide_height)});}\n",
    "function projectDisplaySource(item,section=null){if(projectItemCanTile(section))return section;if(projectItemCanTile(item))return item;const active=projectInitialSource(item);if(active)return active;return section||item;}\n",
    "function projectItemMessage(item){return (item&&String(item.message||'').trim())||'';}\n",
    "function projectItemStatus(item){return (item&&String(item.status||'').trim())||'ready';}\n",
    "function projectSwitchable(item){return projectItemCanTile(item)||projectItemCanPreview(item)||!!(item&&item.active);}\n",
    "function clearProjectDragClasses(){document.querySelectorAll('.projectItem.dragging,.projectItem.dragOver').forEach(item=>item.classList.remove('dragging','dragOver'));}\n",
    "function moveProjectItem(from,to){from=Number(from);to=Number(to);if(!Number.isInteger(from)||!Number.isInteger(to)||from<0||from>=projectItems.length)return false;to=clamp(to,0,Math.max(0,projectItems.length-1));if(from===to)return false;saveActiveProjectAnnotations();const activeItem=projectItems[activeProjectIndex]||null,activeSection=activeProjectSectionIndex,moved=projectItems.splice(from,1)[0];projectItems.splice(to,0,moved);activeProjectIndex=activeItem?projectItems.indexOf(activeItem):to;if(activeProjectIndex<0)activeProjectIndex=to;activeProjectSectionIndex=activeSection;renderProjectPanel();recordAnnotationHistory('project_image_reordered',{from:from+1,to:to+1,label:moved&&moved.label||null},false);scheduleViewerStateSync('project_image_reordered',projectStatePayload());projectMenuStatus('Project image order updated.');notify('Project image order updated','success',1400);return true;}\n",
    "function projectAnnotationKeysForItem(index){const keys=[];if(index<0||index>=projectItems.length)return keys;keys.push(projectAnnotationKey(index,-1));projectSections(projectItems[index]).forEach((section,i)=>keys.push(projectAnnotationKey(index,i)));return Array.from(new Set(keys));}\n",
    "function removeProjectItem(index){index=Number(index);if(!Number.isInteger(index)||index<0||index>=projectItems.length)return false;if(projectItems.length<=1){projectMenuStatus('At least one project image must stay open.');notify('At least one project image must stay open.','warning',2600);return false;}saveActiveProjectAnnotations();const removed=projectItems[index]||{},removedLabel=removed.label||removed.path||('Image '+(index+1)),removedKeys=projectAnnotationKeysForItem(index),wasActive=index===activeProjectIndex;projectItems.splice(index,1);removedKeys.forEach(key=>projectAnnotationStore.delete(key));if(wasActive){activeProjectIndex=Math.min(index,projectItems.length-1);if(!projectSwitchable(projectItems[activeProjectIndex])){const alt=projectItems.findIndex(projectSwitchable);if(alt>=0)activeProjectIndex=alt;}activeProjectSectionIndex=defaultProjectSectionIndex(projectItems[activeProjectIndex]||null);renderProjectPanel();const next=projectItems[activeProjectIndex]||null;if(next&&projectSwitchable(next))applyProjectPreview(next,activeProjectSection());else{loadProjectAnnotations(false);if(typeof draw==='function')draw();}}else{if(index<activeProjectIndex)activeProjectIndex-=1;renderProjectPanel();}recordAnnotationHistory('project_image_closed',{index:index+1,label:removedLabel},false);scheduleViewerStateSync('project_image_closed',Object.assign({closed:{index:index+1,label:removedLabel}},projectStatePayload()));projectMenuStatus('Closed '+removedLabel+'.');notify('Closed '+removedLabel,'success',1600);return true;}\n",
    "function bindProjectItemDrag(button,index){button.draggable=projectItems.length>1;button.dataset.projectIndex=String(index);button.ondragstart=e=>{projectDragIndex=index;button.classList.add('dragging');try{e.dataTransfer.effectAllowed='move';e.dataTransfer.setData('text/plain',String(index));}catch(err){}};button.ondragover=e=>{if(projectDragIndex<0)return;e.preventDefault();button.classList.add('dragOver');try{e.dataTransfer.dropEffect='move';}catch(err){}};button.ondragleave=()=>button.classList.remove('dragOver');button.ondrop=e=>{e.preventDefault();const raw=e.dataTransfer?e.dataTransfer.getData('text/plain'):'',from=Number.isFinite(Number(raw))?Number(raw):projectDragIndex;let to=index;const rect=button.getBoundingClientRect();if(e.clientY>rect.top+rect.height/2)to=index+1;if(from<to)to--;clearProjectDragClasses();projectDragIndex=-1;moveProjectItem(from,to);};button.ondragend=()=>{projectDragIndex=-1;clearProjectDragClasses();};}\n",
    "function projectMenuStatus(message=''){const box=el('projectMenuSummary');if(box&&message)box.textContent=message;}\n",
    "function projectPanelIsClosed(){const panel=el('projectPanel');return !!(panel&&(panel.classList.contains('closed')||panel.style.display==='none'));}\n",
    "function updateProjectPanelToggle(){const button=el('projectPanelToggle'),open=el('projectOpenPanel'),closed=projectPanelIsClosed();if(button)button.classList.toggle('active',!closed);if(open)open.classList.toggle('active',!closed);}\n",
    "function ensureProjectWorkspaceVisible(){const workspace=el('workspacePanel'),panel=el('projectPanel');if(workspace){workspace.style.visibility='visible';workspace.style.opacity='1';workspace.style.pointerEvents='auto';workspace.removeAttribute('aria-hidden');const rect=workspace.getBoundingClientRect();if(rect.width<8||rect.height<8||rect.right<24||rect.bottom<72||rect.left>innerWidth-24||rect.top>innerHeight-24){workspace.style.left='12px';workspace.style.top=(innerWidth<=900?'118px':'72px');workspace.style.right='auto';}}if(panel){panel.style.display='';panel.classList.remove('closed','minimized');const header=el('projectPanelHeader'),state=el('projectPanelMinimizeState');if(header)header.setAttribute('aria-expanded','true');if(state)state.textContent='double-click to minimize';}}\n",
    "function openProjectPanel(){ensureProjectWorkspaceVisible();renderProjectPanel();updateProjectPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();projectMenuStatus('Project panel open.');notify('Project panel opened','success',1400);}\n",
    "function addProjectImageDataUri(dataUri,fileName,width,height){if(!dataUri)return;saveActiveProjectAnnotations();const index=projectItems.length+1,item={id:'browser_image_'+Date.now()+'_'+index,label:String(fileName||('Browser image '+index)),path:String(fileName||'browser image'),backend:'browser',type:'image',status:'browser image',width:Number(width)||Number(cfg.slide_width)||1,height:Number(height)||Number(cfg.slide_height)||1,image_data_uri:String(dataUri),navigator_image_data_uri:String(dataUri),sections:[]};projectItems.push(item);activeProjectIndex=projectItems.length-1;activeProjectSectionIndex=-1;projectAnnotationStore.set(projectAnnotationKey(),{rois:[],selectedRoi:-1,newRoiCount:0,dirty:false,dirty_reason:'',undo:[],redo:[],trajectories:[],selectedTrajectory:-1,trajectorySeq:0});openProjectPanel();renderProjectPanel();applyProjectPreview(item,null);projectMenuStatus('Added '+item.label+' as a project image.');recordAnnotationHistory('project_image_added',{name:item.label,source:'browser'},false);scheduleViewerStateSync('project_image_added',projectStatePayload());}\n",
    "function projectFileExtension(fileName){const name=String(fileName||'').toLowerCase();if(name.endsWith('.ome.tif'))return 'ome.tif';if(name.endsWith('.ome.tiff'))return 'ome.tiff';if(name.endsWith('.ome.zarr'))return 'ome.zarr';const match=name.match(/\\.([^.]+)$/);return match?match[1]:'';}\n",
    "function projectBrowserReadableExtension(ext){return ['png','jpg','jpeg','webp','gif','bmp','avif'].includes(String(ext||'').toLowerCase());}\n",
    "function addProjectFileReference(fileName,fileType='',fileSize=null){const ext=projectFileExtension(fileName),label=String(fileName||('Project file '+(projectItems.length+1))),index=projectItems.length+1,msg='File reference added. Raw WSI/microscopy formats such as CZI, SVS, NDPI and OME-TIFF need R backends or a live/tiled project source for visualization.';saveActiveProjectAnnotations();const item={id:'browser_file_reference_'+Date.now()+'_'+index,label:label,path:label,backend:'browser-reference',type:ext||String(fileType||'file'),status:'needs backend',message:msg,width:Number(cfg.slide_width)||1,height:Number(cfg.slide_height)||1,sections:[],file_name:label,file_type:String(fileType||''),file_size:Number(fileSize)||null};projectItems.push(item);openProjectPanel();renderProjectPanel();projectMenuStatus('Added '+label+' as a project file reference. Open from R for full-resolution viewing.');recordAnnotationHistory('project_image_added',{name:label,source:'browser-reference',type:item.type},false);scheduleViewerStateSync('project_image_added',projectStatePayload());notify('Added file reference: '+label,'info',3600);}\n",
    "function loadProjectImageFile(file){if(!file)return;const ext=projectFileExtension(file.name);if(!projectBrowserReadableExtension(ext)){addProjectFileReference(file.name,file.type,file.size);return;}projectMenuStatus('Reading '+file.name+'...');const reader=new FileReader();reader.onerror=()=>{projectMenuStatus('Could not read image file.');notify('Could not read image file','error');};reader.onload=()=>{const dataUri=String(reader.result||''),img=new Image();img.onload=()=>addProjectImageDataUri(dataUri,file.name,img.naturalWidth,img.naturalHeight);img.onerror=()=>addProjectFileReference(file.name,file.type,file.size);img.src=dataUri;};reader.readAsDataURL(file);}\n",
    "function loadProjectImageFiles(fileList){const files=Array.from(fileList||[]);if(!files.length)return;projectMenuStatus('Adding '+files.length+' image'+(files.length===1?'':'s')+'...');files.forEach(loadProjectImageFile);}\n",
    "function setProjectPanelMinimized(minimized){const panel=el('projectPanel'),header=el('projectPanelHeader'),state=el('projectPanelMinimizeState');if(!panel)return;panel.classList.toggle('minimized',!!minimized);if(header)header.setAttribute('aria-expanded',minimized?'false':'true');if(state)state.textContent=minimized?'double-click to expand':'double-click to minimize';if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function toggleProjectPanelMinimized(){const panel=el('projectPanel');if(!panel)return;setProjectPanelMinimized(!panel.classList.contains('minimized'));}\n",
    "function setProjectPanelClosed(closed){const panel=el('projectPanel'),header=el('projectPanelHeader');if(!panel)return;closed=!!closed;panel.classList.toggle('closed',closed);panel.style.display=closed?'none':'';if(closed)panel.classList.remove('minimized');if(header)header.setAttribute('aria-expanded',closed?'false':'true');updateProjectPanelToggle();if(typeof savePanelPreferences==='function')savePanelPreferences();}\n",
    "function closeProjectPanel(e){if(e){e.preventDefault();e.stopPropagation();}setProjectPanelClosed(true);notify('Project panel closed','info',1600);return false;}\n",
    "function toggleProjectPanelClosed(){const panel=el('projectPanel');if(!panel)return;setProjectPanelClosed(!panel.classList.contains('closed'));}\n",
    "function bindProjectPanelControls(){const header=el('projectPanelHeader'),close=el('projectPanelClose'),toggle=el('projectPanelToggle'),open=el('projectOpenPanel'),openImage=el('projectOpenImage'),file=el('projectImageFile'),saveFile=el('projectSaveFile'),openFile=el('projectOpenFile'),projectFile=el('projectFile');if(header&&header.dataset.bound!=='1'){header.dataset.bound='1';header.ondblclick=e=>{e.preventDefault();toggleProjectPanelMinimized();};header.onkeydown=e=>{if(e.key==='Enter'||e.key===' '){e.preventDefault();toggleProjectPanelMinimized();}};}if(close&&close.dataset.bound!=='1'){close.dataset.bound='1';['pointerdown','mousedown','dblclick'].forEach(name=>close.addEventListener(name,e=>{e.stopPropagation();}));close.onclick=e=>closeProjectPanel(e);}if(toggle&&toggle.dataset.bound!=='1'){toggle.dataset.bound='1';toggle.onclick=()=>{toggleProjectPanelClosed();};}if(open&&open.dataset.bound!=='1'){open.dataset.bound='1';open.onclick=e=>{e.preventDefault();e.stopPropagation();openProjectPanel();closeContainingToolMenu(e.currentTarget);};}if(openImage&&file&&openImage.dataset.bound!=='1'){openImage.dataset.bound='1';openImage.onclick=e=>{e.preventDefault();e.stopPropagation();file.value='';file.click();closeContainingToolMenu(e.currentTarget);};}if(saveFile&&saveFile.dataset.bound!=='1'){saveFile.dataset.bound='1';saveFile.onclick=e=>{e.preventDefault();e.stopPropagation();saveProjectFile();closeContainingToolMenu(e.currentTarget);};}if(openFile&&projectFile&&openFile.dataset.bound!=='1'){openFile.dataset.bound='1';openFile.onclick=e=>{e.preventDefault();e.stopPropagation();projectFile.value='';projectFile.click();closeContainingToolMenu(e.currentTarget);};}if(file&&file.dataset.bound!=='1'){file.dataset.bound='1';file.onchange=()=>loadProjectImageFiles(file.files);}if(projectFile&&projectFile.dataset.bound!=='1'){projectFile.dataset.bound='1';projectFile.onchange=()=>{const picked=projectFile.files&&projectFile.files[0];if(picked)openProjectFile(picked);};}updateProjectPanelToggle();}\n",
    "function renderProjectPanel(){const panel=el('projectPanel'),summary=el('projectSummary'),list=el('projectImageList'),sections=el('projectSectionList');if(!panel||!summary||!list||!sections)return;if(!projectItems.length){summary.textContent='No project images configured.';list.innerHTML='';sections.innerHTML='';return;}summary.textContent=projectItems.length+' image'+(projectItems.length===1?'':'s')+' available. Annotations are stored separately for each image/section. Drag images to reorder them; annotations and trajectories stay with each image/section. Current section has '+rois.length+' ROI'+(rois.length===1?'':'s')+'.';list.innerHTML='';projectItems.forEach((item,i)=>{const row=document.createElement('div');row.className='projectItemRow';const b=document.createElement('button');b.type='button';b.className='projectItem';b.classList.toggle('active',i===activeProjectIndex);b.classList.toggle('unavailable',!projectSwitchable(item));b.setAttribute('aria-disabled',projectSwitchable(item)?'false':'true');b.title='Drag to reorder project images';bindProjectItemDrag(b,i);const name=document.createElement('span');name.className='projectName';name.textContent=item.label||item.path||('Image '+(i+1));const status=document.createElement('span');status.className='projectStatus';status.textContent=projectItemStatus(item);const path=document.createElement('span');path.className='projectPath';path.textContent=item.path||item.backend||'';b.append(name,status,path);const msg=projectItemMessage(item);if(msg){const m=document.createElement('span');m.className='projectMessage';m.textContent=msg;b.appendChild(m);}b.onclick=()=>switchProjectItem(i);const close=document.createElement('button');close.type='button';close.className='projectItemClose';close.textContent='X';close.title='Close this project image';close.setAttribute('aria-label','Close '+(item.label||item.path||('image '+(i+1))));close.disabled=projectItems.length<=1;close.onclick=e=>{e.preventDefault();e.stopPropagation();removeProjectItem(i);};row.append(b,close);list.appendChild(row);});renderProjectSections();}\n",
    "function renderProjectSections(){const sections=el('projectSectionList');if(!sections)return;sections.innerHTML='';const item=projectItems[activeProjectIndex]||null;const values=projectSections(item);if(!values.length)return;const title=document.createElement('div');title.className='sideMeta';title.textContent='Sections';sections.appendChild(title);values.forEach((section,i)=>{const b=document.createElement('button');b.type='button';b.className='projectSectionItem';b.classList.toggle('active',i===activeProjectSectionIndex);b.disabled=!section.image_data_uri&&!projectItemCanTile(section);const name=document.createElement('span');name.className='projectName';name.textContent=section.label||section.id||('Section '+(i+1));const status=document.createElement('span');status.className='projectStatus';const saved=projectAnnotationStore.get(projectAnnotationKey(activeProjectIndex,i));const n=saved&&saved.rois?saved.rois.length:(i===activeProjectSectionIndex?rois.length:0);status.textContent=(section.status||'')+(n?(' | '+n+' ROI'+(n===1?'':'s')):'');b.append(name,status);const msg=section.message||'';if(msg){const m=document.createElement('span');m.className='projectMessage';m.textContent=msg;b.appendChild(m);}b.onclick=()=>switchProjectSection(i);sections.appendChild(b);});}\n",
    "function projectTileSourceFromItem(item,section=null){const source=projectDisplaySource(item,section);if(projectItemCanTile(source)){const src=source,base=String(src.tile_url_base||''),template=String(src.tile_url_template||''),fmt=String(src.tile_format),style=String(src.tile_url_style||'deepzoom'),tileSize=Number(src.tile_size||cfg.tile_size),maxLevel=Number(src.max_level),out={width:Number(src.width||(item&&item.width)||cfg.slide_width),height:Number(src.height||(item&&item.height)||cfg.slide_height),tileSize:tileSize,tileOverlap:Number(src.tile_overlap||0),minLevel:Number(src.min_level||0),maxLevel:maxLevel,getTileUrl:(level,x,y)=>tileUrlFromParts(base,template,style,fmt,level,x,y)};return withTileCors(out,base,template);}const imageSource=(source&&source.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri);if(imageSource)return {type:'image',url:imageSource};return null;}\n",
    "function projectContentBounds(item,section=null){const source=projectDisplaySource(item,section),tile=projectItemCanTile(source);const b=(source&&source.content_bbox)||(!tile&&section&&section.content_bbox)||(item&&item.content_bbox)||null;if(!b)return null;const vals=['xmin','ymin','xmax','ymax'].map(k=>Number(b[k]));if(vals.some(v=>!Number.isFinite(v)))return null;if(vals[2]<=vals[0]||vals[3]<=vals[1])return null;return {xmin:vals[0],ymin:vals[1],xmax:vals[2],ymax:vals[3]};}\n",
    "function zoomToProjectContent(item,section=null){const b=projectContentBounds(item,section);if(!b||typeof zoomToSlideBounds!=='function')return false;zoomToSlideBounds(b,1.18);return true;}\n",
    "function finishProjectSwitch(item,section=null,redraw=true){loadProjectAnnotations(false);renderProjectPanel();if(typeof buildRoiList==='function')buildRoiList();if(typeof updateButtons==='function')updateButtons();if(typeof syncChannelSourcesForActiveImage==='function')syncChannelSourcesForActiveImage();if(redraw){if(!(typeof zoomToProjectContent==='function'&&zoomToProjectContent(item,section))&&typeof fitView==='function')fitView();}if(typeof draw==='function')draw();const label=(item.label||item.path||'image')+(section?(' / '+(section.label||section.id||'section')):'');notify('Project section selected: '+label+' | '+rois.length+' ROI'+(rois.length===1?'':'s'),'success',2200);scheduleViewerStateSync('project_section_selected',projectStatePayload());}\n",
    "function applyProjectOsd(item,section=null){if(typeof osdViewer==='undefined'||!osdViewer)return false;const display=projectDisplaySource(item,section),tileSource=projectTileSourceFromItem(item,section);if(!tileSource){notify(projectItemMessage(section||item)||'No tiled source or preview is available for this project item.','warning',5200);return true;}const dims=display||section||item||{};cfg.slide_width=Number(dims.width||(item&&item.width)||cfg.slide_width);cfg.slide_height=Number(dims.height||(item&&item.height)||cfg.slide_height);cfg.title=(item&&item.label)||cfg.title;if(projectItemCanTile(display)){const src=display;cfg.tile_url_base=String(src.tile_url_base||'');cfg.tile_url_template=String(src.tile_url_template||'');cfg.tile_url_style=String(src.tile_url_style||'deepzoom');cfg.tile_format=String(src.tile_format||'');cfg.tile_size=Number(src.tile_size||cfg.tile_size);cfg.min_level=Number(src.min_level||0);cfg.max_level=Number(src.max_level);cfg.tile_overlap=Number(src.tile_overlap||0);}else{cfg.tile_url_base='';cfg.tile_url_template='';cfg.tile_url_style='deepzoom';cfg.tile_format='';cfg.min_level=0;cfg.max_level=0;}if(prefetchCache&&typeof prefetchCache.clear==='function')prefetchCache.clear();if(typeof clearChannelItems==='function')clearChannelItems();if(typeof navigatorImage!=='undefined')navigatorImage.src=(display&&display.navigator_image_data_uri)||(section&&section.navigator_image_data_uri)||(item&&item.navigator_image_data_uri)||(display&&display.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri)||'';if(typeof setImageTransform==='function')setImageTransform(0,false,false,false);osdReady=false;if(typeof osdViewer.addOnceHandler==='function')osdViewer.addOnceHandler('open',()=>finishProjectSwitch(item,section,true));else setTimeout(()=>finishProjectSwitch(item,section,true),120);osdViewer.open(tileSource);return true;}\n",
    "function applyProjectPreview(item,section=null){if(applyProjectOsd(item,section))return true;const display=projectDisplaySource(item,section),source=(display&&display.image_data_uri)||(section&&section.image_data_uri)||(item&&item.image_data_uri);if(!source){notify(projectItemMessage(section||item)||'No preview is available for this project item','warning',5200);return false;}if(typeof image==='undefined'){notify('Project image switching needs a tiled source or a thumbnail/project viewer preview.','warning',5200);return false;}if(typeof clearChannelItems==='function')clearChannelItems();const dims=display||section||item||{};cfg.slide_width=Number(dims.width||(item&&item.width)||cfg.slide_width);cfg.slide_height=Number(dims.height||(item&&item.height)||cfg.slide_height);cfg.title=(item&&item.label)||cfg.title;if(typeof setImageTransform==='function')setImageTransform(0,false,false,false);image.onload=()=>{fitView();finishProjectSwitch(item,section,false);};image.src=source;if(typeof navigatorImage!=='undefined')navigatorImage.src=(display&&display.navigator_image_data_uri)||(section&&section.navigator_image_data_uri)||(item&&item.navigator_image_data_uri)||source;return true;}\n",
    "function switchProjectItem(index){if(index<0||index>=projectItems.length)return;const item=projectItems[index];if(!projectSwitchable(item)){notify(projectItemMessage(item)||'This project item has no preview or tiled source yet. Install/configure the required backend or convert it to a supported tiled image.','warning',6200);return;}saveActiveProjectAnnotations();activeProjectIndex=index;activeProjectSectionIndex=defaultProjectSectionIndex(item);renderProjectPanel();applyProjectPreview(item,activeProjectSection());}\n",
    "function switchProjectSection(index){const item=projectItems[activeProjectIndex]||null;const section=item&&projectSections(item)[index];if(!item||!section)return;if(!section.image_data_uri&&!projectItemCanTile(section)){notify(section.message||'This section is listed from metadata but has no preview or tiled source yet.','warning',5200);return;}saveActiveProjectAnnotations();activeProjectSectionIndex=index;renderProjectPanel();applyProjectPreview(item,section);}\n",
    "function bindProjectPanel(){bindProjectPanelControls();preloadProjectAnnotations();if(projectItems.length&&projectAnnotationStore.has(projectAnnotationKey()))loadProjectAnnotations(false);else if(projectItems.length&&!projectAnnotationStore.has(projectAnnotationKey()))saveActiveProjectAnnotations();renderProjectPanel();}\n"
  )
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
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,lastX=0,lastY=0,lastPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0,nextRoiClass='tumour',activeRoiClass='tumour',activeRoiName='',nextRoiNameDirty=false,measureStart=null,measures=[],trajectoryDraft=[],trajectories=[],trajectorySeq=0,selectedTrajectory=-1,brushing=false,brushPoints=[],brushRadius=32,brushScreenRadius=32,brushOperation='new',brushTargetRoi=-1,brushClass='',brushAdditiveSelection=false,brushTouchedSelection=new Set(),brushAltDown=false,draggingVertex=null,activeVertex=null,annotationUndo=[],annotationRedo=[];\n",
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
    wsi_viewer_project_js(),
    wsi_viewer_navigator_js(),
    wsi_viewer_scale_bar_js(),
    wsi_viewer_multiview_js(),
    "function setMode(m){mode=m;if(m==='brush'&&typeof setRoiPanelOpen==='function')setRoiPanelOpen(true);if(m!=='edit'){draggingVertex=null;activeVertex=null;}canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');canvas.classList.toggle('brushing',m==='brush');canvas.classList.toggle('editing',m==='edit');canvas.classList.toggle('measuring',m==='measure');canvas.classList.toggle('trajectory',m==='trajectory');const setToolActive=(id,on)=>{const button=el(id);if(button)button.classList.toggle('active',!!on);};setToolActive('toolPan',m==='pan');setToolActive('toolSelect',m==='select');setToolActive('toolDraw',m==='draw');setToolActive('toolBrush',m==='brush');setToolActive('toolEdit',m==='edit');setToolActive('toolMeasure',m==='measure');setToolActive('toolTrajectory',m==='trajectory');updateCursorFeedback();updateButtons();saveToolPreference();if(canvas.width&&((typeof image==='undefined')||image.complete))draw();}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);fitView();}\n",
    "function fitView(){if(!image.naturalWidth)return;const dims=viewImageSize();minScale=Math.min(innerWidth/dims.width,innerHeight/dims.height);scale=minScale;offsetX=(innerWidth-dims.width*scale)/2;offsetY=(innerHeight-dims.height*scale)/2;draw();}\n",
    "function oneToOne(){if(!image.naturalWidth)return;const dims=viewImageSize();scale=1;offsetX=(innerWidth-dims.width)/2;offsetY=(innerHeight-dims.height)/2;draw();}\n",
    "function slideToImage(p){return {x:p.x/cfg.slide_width*image.naturalWidth,y:p.y/cfg.slide_height*image.naturalHeight};}\n",
    "function imageToSlide(p){return {x:p.x/image.naturalWidth*cfg.slide_width,y:p.y/image.naturalHeight*cfg.slide_height};}\n",
    "function slideToCanvas(p){const q=slideToViewImagePoint(p);return {x:offsetX+q.x*scale,y:offsetY+q.y*scale};}\n",
    "function pointerToSlide(evt){const rect=canvas.getBoundingClientRect();const px=evt.clientX-rect.left;const py=evt.clientY-rect.top;return imageToSlide(viewToImagePoint({x:(px-offsetX)/scale,y:(py-offsetY)/scale}));}\n",
    "function pointInsideSlide(p){return p.x>=0&&p.y>=0&&p.x<=cfg.slide_width&&p.y<=cfg.slide_height;}\n",
    "function roiBounds(roi){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointInRing(p,ring){let inside=false;for(let i=0,j=ring.length-1;i<ring.length;j=i++){const xi=ring[i].x,yi=ring[i].y,xj=ring[j].x,yj=ring[j].y;const hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!rois.length)return;selectedRoi=(i+rois.length)%rois.length;const b=roiBounds(rois[selectedRoi]),corners=[{x:b.xmin,y:b.ymin},{x:b.xmax,y:b.ymin},{x:b.xmax,y:b.ymax},{x:b.xmin,y:b.ymax}].map(slideToViewImagePoint),xs=corners.map(p=>p.x),ys=corners.map(p=>p.y),xmin=Math.min(...xs),xmax=Math.max(...xs),ymin=Math.min(...ys),ymax=Math.max(...ys);const pad=1.35;scale=clamp(Math.min(innerWidth/Math.max(1,(xmax-xmin)*pad),innerHeight/Math.max(1,(ymax-ymin)*pad)),minScale*0.8,40);offsetX=innerWidth/2-((xmin+xmax)/2)*scale;offsetY=innerHeight/2-((ymin+ymax)/2)*scale;updateRoiList();draw();}\n",
    "function zoomAt(factor,cx,cy){const beforeX=(cx-offsetX)/scale,beforeY=(cy-offsetY)/scale;scale=clamp(scale*factor,minScale*0.6,80);offsetX=cx-beforeX*scale;offsetY=cy-beforeY*scale;draw();}\n",
    "function draw(){if(typeof syncBrushRadiusToZoom==='function')syncBrushRadiusToZoom();ctx.clearRect(0,0,innerWidth,innerHeight);ctx.imageSmoothingEnabled=true;drawTransformedImage(image);applyStainToCanvas();drawLayers();drawTileGrid();drawArtifactOverlays();drawRois();drawDraft();drawBrushPreview();drawEditHandles();drawMeasurements();drawTrajectories();drawCrosshair();drawMiniNavigator();updateScaleBar();updateStatus(lastPointer);}\n",
    "function drawRois(){if(!showRois||!rois.length||!image.naturalWidth)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{let label=null;ctx.beginPath();roi.rings.forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(!label)label=q;if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;if(showLabels&&label){const text=roi.name||roi.id;const w=ctx.measureText(text).width+8;ctx.fillStyle='rgba(0,0,0,.68)';ctx.fillRect(label.x,label.y,w,18);ctx.fillStyle=roi.colour;ctx.fillText(text,label.x+4,label.y+3);}});ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!lastPointer||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function updateStatus(p){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x';msg+=magnificationStatus();msg+=(typeof multiViewStatus==='function'?multiViewStatus():'');msg+=stainStatus();msg+=measureStatus();msg+=trajectoryStatus();msg+=imageTransformStatus();if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(p&&pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg+' | thumbnail preview, full slide not loaded into R';}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){notify('Add at least 3 points','warning');return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;markAnnotationsDirty('roi_added');recordAnnotationHistory('roi_added',{id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount});buildRoiList();updateButtons();setMode('select');notify('ROI saved','success');draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});const cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:cls},class:cls,source:roi.source||null},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(!rois.length&&draft.length<3){notify('Draw an ROI first','warning');return;}const text=geojsonText();const name=(typeof projectAnnotationFilename==='function'?projectAnnotationFilename():null)||cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();markAnnotationsSaved('geojson_saved');notify('ROI saved','success');return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);markAnnotationsSaved('geojson_exported');notify('GeoJSON exported','success');}\n",
    "function updateButtons(){const has=rois.length>0,setDisabled=(id,value)=>{const button=el(id);if(button)button.disabled=!!value;},setActive=(id,value)=>{const button=el(id);if(button)button.classList.toggle('active',!!value);};['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>setDisabled(id,!has));setDisabled('finishRoi',draft.length<3);setDisabled('undoPoint',draft.length<1);setDisabled('saveGeojson',!has&&draft.length<3);if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();setActive('roiToggle',showRois&&has);setActive('labelsToggle',showLabels&&has);setActive('crosshairToggle',showCrosshair);}\n",
    wsi_viewer_geometry_js(),
    wsi_viewer_layers_js(),
    wsi_viewer_cell_controls_js(),
    wsi_viewer_seurat_js(),
    wsi_viewer_kodama_js(),
    wsi_viewer_measure_js(),
    wsi_viewer_trajectory_js(),
    wsi_viewer_segmentation_js(),
    "canvas.addEventListener('mousedown',e=>{lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='trajectory'){if(e.detail===1)addTrajectoryPoint(lastPointer);return;}if(mode==='brush'){startBrush(lastPointer,e);return;}if(mode==='edit'){activeVertex=findVertexAt(e.clientX,e.clientY);if(activeVertex){pushAnnotationUndo('roi_vertex_moved');selectAnnotation(activeVertex.roi,false);draggingVertex=activeVertex;updateRoiList();draw();return;}selectAnnotation(roiAt(lastPointer),true);draw();return;}if(mode==='measure'){addMeasurePoint(lastPointer);return;}if(mode==='select'){if(!selectObjectAtPoint(lastPointer)){clearSelectedAnnotation(true);clearSelectedTrajectory(true);draw();}return;}dragging=true;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',e=>{if(brushing)finishBrush();if(draggingVertex){draggingVertex=null;buildRoiList();}dragging=false;canvas.classList.remove('dragging');updateCursorFeedback(e);});\n",
    "window.addEventListener('mousemove',e=>{lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(brushing){addBrushPoint(lastPointer,e);return;}if(draggingVertex){moveActiveVertex(lastPointer);return;}if(dragging){offsetX+=e.clientX-lastX;offsetY+=e.clientY-lastY;lastX=e.clientX;lastY=e.clientY;draw();}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.2:1/1.2,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}if(mode==='trajectory'){e.preventDefault();finishTrajectory();return;}if(mode==='edit'){e.preventDefault();insertVertexAt(pointerToSlide(e),e.clientX,e.clientY);return;}const p=pointerToSlide(e);if(selectObjectAtPoint(p)){e.preventDefault();return;}e.preventDefault();clearSelectionAndPan();});\n",
    "const bindButton=(id,handler)=>{const button=el(id);if(button)button.onclick=handler;};bindButton('toolPan',()=>setMode('pan'));bindButton('toolSelect',()=>setMode('select'));bindButton('toolDraw',()=>setMode('draw'));bindButton('toolBrush',e=>{setMode('brush');closeContainingToolMenu(e.currentTarget);});bindButton('newRoi',()=>startNewAnnotation(mode==='draw'?'draw':'brush'));bindButton('toolEdit',()=>setMode('edit'));bindButton('finishRoi',finishDraft);bindButton('undoPoint',()=>{if(mode==='brush'&&brushPoints.length){brushPoints.pop();draw();}else undoDraftPoint();});bindButton('saveGeojson',saveGeojson);bindButton('zoomIn',()=>zoomAt(1.25,innerWidth/2,innerHeight/2));bindButton('zoomOut',()=>zoomAt(1/1.25,innerWidth/2,innerHeight/2));bindButton('fit',fitView);bindButton('oneToOne',oneToOne);\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);el('layersToggle').onclick=()=>{toggleRoiPanel();updateButtons();};el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);saveRoiOpacityPreference();draw();};el('crosshairToggle').onclick=()=>{showCrosshair=!showCrosshair;updateButtons();draw();};\n",
    "window.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase(),typing=e.target&&['INPUT','TEXTAREA','SELECT'].includes(e.target.tagName);if(brushSubtractKeyEvent(e)){brushAltDown=true;updateCursorFeedback(e);draw();}if((e.ctrlKey||e.metaKey)&&!typing&&((e.shiftKey&&key==='z')||key==='y')){e.preventDefault();restoreAnnotationRedo();return;}if((e.ctrlKey||e.metaKey)&&!e.shiftKey&&key==='z'&&!typing){e.preventDefault();if(mode==='trajectory'&&trajectoryDraft.length){undoTrajectoryPoint();return;}restoreAnnotationUndo();return;}if(!typing&&!e.ctrlKey&&!e.metaKey&&!e.altKey&&!e.shiftKey&&key==='n'){e.preventDefault();startNewAnnotation(mode==='draw'?'draw':'brush');return;}if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='b')setMode('brush');if(e.key==='e')setMode('edit');if(e.key==='m')setMode('measure');if(e.key==='t')setMode('trajectory');if(e.key==='Enter'&&mode==='draw')finishDraft();if(e.key==='Enter'&&mode==='trajectory'){e.preventDefault();finishTrajectory();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='draw'){e.preventDefault();undoDraftPoint();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='trajectory'){e.preventDefault();undoTrajectoryPoint();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='edit'&&activeVertex){e.preventDefault();deleteSelectedVertex();}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='c')el('crosshairToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape'){measureStart=null;trajectoryDraft=[];brushing=false;brushPoints=[];brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();draggingVertex=null;activeVertex=null;setMode('pan');draw();}});\n",
    "window.addEventListener('keyup',e=>{if(e.key==='Alt'||e.key==='Meta'||mode==='brush'||mode==='edit'){brushAltDown=brushSubtractModifier(e);updateCursorFeedback(e);draw();}});\n",
    "bindExclusiveMenus();bindShortcutHelp();bindCommandPalette();bindMiniNavigator();bindProjectPanel();bindRoiPanelControls();bindSelectionCardControls();bindJobControls();bindStainControls();bindBaseImageControls();bindRoiClassControls();bindAnnotationListControls();bindAnnotationHistoryControls();bindAnnotationSectionMaximizeControls();bindMeasureControls();bindTrajectoryControls();bindImageTransformControls();bindGeojsonImportControls();bindSegmentationControls();bindArtifactControls();bindCellControls();bindSeuratControls();bindKodamaControls();bindMagnificationControls();bindMultiViewControls();bindPreferenceControls();buildRoiList();buildLayerList();buildChannelList();const initialMode=applyViewerPreferences();updateButtons();updateAnnotationDirtyIndicator();syncMessage('');setMode(initialMode||'pan');startViewerStateSocket();scheduleViewerStateSync('viewer_loaded',{});startViewerCommandPolling();startViewerAutosave();\n",
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
    wsi_viewer_chrome(config, "Loading Deep Zoom tiles...", tiled = TRUE),
    "<script>\n",
    "const cfg = ", config_json, ";\n",
    "const viewerEl = document.getElementById('viewer');\n",
    "const canvas = document.getElementById('overlay');\n",
    "const ctx = canvas.getContext('2d');\n",
    "const status = document.getElementById('status');\n",
    "const navigatorImage = new Image();\n",
    "const el=id=>document.getElementById(id), rois=cfg.rois||[], layers=cfg.layers||[];\n",
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,lastX=0,lastY=0,lastPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0,nextRoiClass='tumour',activeRoiClass='tumour',activeRoiName='',nextRoiNameDirty=false,measureStart=null,measures=[],trajectoryDraft=[],trajectories=[],trajectorySeq=0,selectedTrajectory=-1,brushing=false,brushPoints=[],brushRadius=32,brushScreenRadius=32,brushOperation='new',brushTargetRoi=-1,brushClass='',brushAdditiveSelection=false,brushTouchedSelection=new Set(),brushAltDown=false,draggingVertex=null,activeVertex=null,annotationUndo=[],annotationRedo=[];\n",
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
    wsi_viewer_project_js(),
    wsi_viewer_navigator_js(),
    wsi_viewer_scale_bar_js(),
    wsi_viewer_multiview_js(),
    "function setMode(m){mode=m;if(m==='brush'&&typeof setRoiPanelOpen==='function')setRoiPanelOpen(true);if(m!=='edit'){draggingVertex=null;activeVertex=null;}canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');canvas.classList.toggle('brushing',m==='brush');canvas.classList.toggle('editing',m==='edit');canvas.classList.toggle('measuring',m==='measure');canvas.classList.toggle('trajectory',m==='trajectory');const setToolActive=(id,on)=>{const button=el(id);if(button)button.classList.toggle('active',!!on);};setToolActive('toolPan',m==='pan');setToolActive('toolSelect',m==='select');setToolActive('toolDraw',m==='draw');setToolActive('toolBrush',m==='brush');setToolActive('toolEdit',m==='edit');setToolActive('toolMeasure',m==='measure');setToolActive('toolTrajectory',m==='trajectory');updateCursorFeedback();updateButtons();saveToolPreference();if(canvas.width)draw();}\n",
    "function activeTileMode(){return !!((cfg.tile_url_base||cfg.tile_url_template)&&cfg.tile_format&&Number.isFinite(Number(cfg.max_level))&&Number(cfg.max_level)>0);}\n",
    "function tileUrlFromParts(base,template,style,fmt,level,col,row){if(template)return String(template).replace(/\\{level\\}/g,level).replace(/\\{x\\}/g,col).replace(/\\{y\\}/g,row).replace(/\\{format\\}/g,fmt);return String(base).replace(/\\/$/,'')+'/'+level+'/'+col+(style==='slash'?('/'+row):('_'+row))+'.'+fmt;}\n",
    "function tileUrl(level,col,row){return tileUrlFromParts(cfg.tile_url_base||'',cfg.tile_url_template||'',cfg.tile_url_style||'deepzoom',cfg.tile_format,level,col,row);}\n",
    "function tileNeedsCors(url){return /^https?:\\/\\//i.test(String(url||''));}\n",
    "function withTileCors(source,base,template){if(source&&(tileNeedsCors(base)||tileNeedsCors(template)))source.crossOriginPolicy='Anonymous';return source;}\n",
    "function tileSourceFromConfig(){if(activeTileMode()){const out={width:cfg.slide_width,height:cfg.slide_height,tileSize:cfg.tile_size,tileOverlap:Number(cfg.tile_overlap||0),minLevel:0,maxLevel:cfg.max_level,getTileUrl:(level,x,y)=>tileUrl(level,x,y)};return withTileCors(out,cfg.tile_url_base,cfg.tile_url_template);}const source=cfg.image_data_uri||cfg.navigator_image_data_uri;if(source)return {type:'image',url:source};return {width:cfg.slide_width,height:cfg.slide_height,tileSize:cfg.tile_size,tileOverlap:0,minLevel:0,maxLevel:0,getTileUrl:()=>''};}\n",
    "function requestDraw(){if(renderQueued)return;renderQueued=true;requestAnimationFrame(()=>{renderQueued=false;draw();});}\n",
    "function osdItem(){return osdViewer&&osdViewer.world&&osdViewer.world.getItemAt(0);}\n",
    "let stainCanvasWarningShown=false;\n",
    "function osdBaseCanvasCandidates(){if(!viewerEl||!viewerEl.querySelectorAll)return[];return Array.from(viewerEl.querySelectorAll('canvas')).filter(c=>c&&c.width>0&&c.height>0&&getComputedStyle(c).display!=='none').sort((a,b)=>(b.width*b.height)-(a.width*a.height));}\n",
    "function osdBaseCanvas(){const candidates=osdBaseCanvasCandidates();return candidates.length?candidates[0]:null;}\n",
    "function syncViewState(){if(!osdReady||!osdItem())return;const a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:1,y:0});scale=Math.max(1e-9,Math.hypot(b.x-a.x,b.y-a.y));minScale=Math.min(innerWidth/cfg.slide_width,innerHeight/cfg.slide_height);offsetX=a.x;offsetY=a.y;}\n",
    "function markBaseImageDirty(){baseImageDirty=true;}\n",
    "function invalidateBaseImage(){markBaseImageDirty();if(osdViewer&&typeof osdViewer.forceRedraw==='function')osdViewer.forceRedraw();requestDraw();}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);syncViewState();draw();}\n",
    "function fitView(){if(typeof multiViewFitView==='function'&&multiViewFitView()){draw();return;}if(osdViewer){osdViewer.viewport.goHome(false);syncViewState();prefetchNeighborTiles();draw();}}\n",
    "function zoomAt(factor,cx,cy){if(typeof multiViewZoomAt==='function'&&multiViewZoomAt(factor)){draw();return;}if(!osdViewer)return;const point=osdViewer.viewport.pointFromPixel(new OpenSeadragon.Point(cx,cy),true);osdViewer.viewport.zoomBy(factor,point,false);osdViewer.viewport.applyConstraints(false);syncViewState();prefetchNeighborTiles();draw();}\n",
    "function oneToOne(){if(typeof multiViewOneToOne==='function'&&multiViewOneToOne()){draw();return;}syncViewState();zoomAt(1/Math.max(scale,1e-9),innerWidth/2,innerHeight/2);}\n",
    "function currentLevel(){syncViewState();if(!activeTileMode())return 0;return clamp(Math.ceil(cfg.max_level+Math.log2(Math.max(scale,1e-9))),0,cfg.max_level);}\n",
    "function visibleTileRange(level,margin=1){if(!activeTileMode()||!osdReady||!osdItem())return null;const item=osdItem(),bounds=osdViewer.viewport.getBounds(true),p0=item.viewportToImageCoordinates(new OpenSeadragon.Point(bounds.x,bounds.y)),p1=item.viewportToImageCoordinates(new OpenSeadragon.Point(bounds.x+bounds.width,bounds.y+bounds.height)),down=Math.pow(2,cfg.max_level-level),levelW=Math.ceil(cfg.slide_width/down),levelH=Math.ceil(cfg.slide_height/down),tileSlide=cfg.tile_size*down;const left=clamp(Math.min(p0.x,p1.x),0,cfg.slide_width),right=clamp(Math.max(p0.x,p1.x),0,cfg.slide_width),top=clamp(Math.min(p0.y,p1.y),0,cfg.slide_height),bottom=clamp(Math.max(p0.y,p1.y),0,cfg.slide_height);return {c0:clamp(Math.floor(left/tileSlide)-margin,0,Math.ceil(levelW/cfg.tile_size)-1),c1:clamp(Math.floor(right/tileSlide)+margin,0,Math.ceil(levelW/cfg.tile_size)-1),r0:clamp(Math.floor(top/tileSlide)-margin,0,Math.ceil(levelH/cfg.tile_size)-1),r1:clamp(Math.floor(bottom/tileSlide)+margin,0,Math.ceil(levelH/cfg.tile_size)-1)};}\n",
    "function prefetchTile(level,col,row){if(!activeTileMode())return;const key=tileUrl(level,col,row);if(prefetchCache.has(key))return;const img=new Image();if(tileNeedsCors(key))img.crossOrigin='anonymous';img.decoding='async';img.src=key;prefetchCache.set(key,img);if(prefetchCache.size>384){const first=prefetchCache.keys().next().value;prefetchCache.delete(first);}}\n",
    "function prefetchNeighborTiles(){const margin=Number(cfg.tile_prefetch_margin??-1);if(margin<0)return;const level=currentLevel(),range=visibleTileRange(level,margin);if(!range)return;if(window.requestIdleCallback){requestIdleCallback(()=>prefetchTileRange(level,range),{timeout:400});}else{setTimeout(()=>prefetchTileRange(level,range),50);}}\n",
    "function prefetchTileRange(level,range){for(let row=range.r0;row<=range.r1;row++){for(let col=range.c0;col<=range.c1;col++)prefetchTile(level,col,row);}}\n",
    "function stainOverlayCacheKey(){const prefs=(typeof currentStainPayload==='function'?currentStainPayload():null)||{};return [canvas.width,canvas.height,JSON.stringify(prefs)].join('|');}\n",
    "function ensureStainOverlayCanvas(){if(!stainOverlayCanvas)stainOverlayCanvas=document.createElement('canvas');if(stainOverlayCanvas.width!==canvas.width||stainOverlayCanvas.height!==canvas.height){stainOverlayCanvas.width=canvas.width;stainOverlayCanvas.height=canvas.height;stainOverlayKey='';}return stainOverlayCanvas;}\n",
    "function applyOpenSeadragonStain(){if(typeof hasTiledStainChannels==='function'&&hasTiledStainChannels()){stainOverlayCanvas=null;stainOverlayKey='';baseImageDirty=false;return false;}if(!stainEnabled||!stainOn){baseImageDirty=false;return false;}const key=stainOverlayCacheKey();if(!baseImageDirty&&stainOverlayCanvas&&stainOverlayKey===key){ctx.drawImage(stainOverlayCanvas,0,0,innerWidth,innerHeight);return true;}const bases=osdBaseCanvasCandidates();if(!bases.length)return false;const overlay=ensureStainOverlayCanvas(),overlayCtx=overlay.getContext('2d',{willReadFrequently:true});overlayCtx.clearRect(0,0,overlay.width,overlay.height);for(const base of bases){if(applyStainToCanvas(overlayCtx,overlay,base)){stainOverlayKey=key;baseImageDirty=false;ctx.drawImage(overlay,0,0,innerWidth,innerHeight);return true;}}if(!stainCanvasWarningShown){stainCanvasWarningShown=true;notify('Stain channel selection needs readable tiles. For full-resolution H/E/residual display, open with wsi_viewer_live(..., dynamic_tiles = TRUE) or serve the viewer through localhost.','warning',7000);}baseImageDirty=false;return false;}\n",
    "function draw(){ctx.clearRect(0,0,innerWidth,innerHeight);syncViewState();if(typeof syncBrushRadiusToZoom==='function')syncBrushRadiusToZoom();applyOpenSeadragonStain();drawLayers();drawTileGrid();drawArtifactOverlays();drawRois();drawDraft();drawBrushPreview();drawEditHandles();drawMeasurements();drawTrajectories();drawCrosshair();drawMiniNavigator();updateScaleBar();updateStatus(lastPointer,currentLevel());}\n",
    "function slideToCanvas(p){if(!osdReady||!osdItem())return {x:offsetX+p.x*scale,y:offsetY+p.y*scale};const vp=osdItem().imageToViewportCoordinates(Number(p.x),Number(p.y));const px=osdViewer.viewport.pixelFromPoint(vp,true);return {x:px.x,y:px.y};}\n",
    "function pointInsideSlide(p){return p&&p.x>=0&&p.y>=0&&p.x<=cfg.slide_width&&p.y<=cfg.slide_height;}\n",
    "function zoomToSlideBounds(b,pad=1.35){if(!osdReady||!osdItem()||!b)return;const w=Math.max(1,b.xmax-b.xmin),h=Math.max(1,b.ymax-b.ymin),cx=(b.xmin+b.xmax)/2,cy=(b.ymin+b.ymax)/2,x0=cx-w*pad/2,y0=cy-h*pad/2,x1=cx+w*pad/2,y1=cy+h*pad/2,p0=osdItem().imageToViewportCoordinates(x0,y0),p1=osdItem().imageToViewportCoordinates(x1,y1),vpRect=new OpenSeadragon.Rect(p0.x,p0.y,p1.x-p0.x,p1.y-p0.y);osdViewer.viewport.fitBoundsWithConstraints(vpRect,false);syncViewState();prefetchNeighborTiles();}\n",
    "function roiBounds(roi){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointInRing(p,ring){let inside=false;for(let i=0,j=ring.length-1;i<ring.length;j=i++){const xi=ring[i].x,yi=ring[i].y,xj=ring[j].x,yj=ring[j].y;const hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!rois.length)return;selectedRoi=(i+rois.length)%rois.length;const b=roiBounds(rois[selectedRoi]);zoomToSlideBounds(b,1.35);updateRoiList();draw();}\n",
    "function drawRois(){if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{let label=null;ctx.beginPath();roi.rings.forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(!label)label=q;if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;if(showLabels&&label){const text=roi.name||roi.id;const w=ctx.measureText(text).width+8;ctx.fillStyle='rgba(0,0,0,.68)';ctx.fillRect(label.x,label.y,w,18);ctx.fillStyle=roi.colour;ctx.fillText(text,label.x+4,label.y+3);}});ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function panByPixels(dx,dy){if(!osdViewer)return;const delta=osdViewer.viewport.deltaPointsFromPixels(new OpenSeadragon.Point(-dx,-dy),true);osdViewer.viewport.panBy(delta,false);osdViewer.viewport.applyConstraints(false);syncViewState();prefetchNeighborTiles();draw();}\n",
    "function pointerToSlide(evt){const rect=canvas.getBoundingClientRect();const px=evt.clientX-rect.left,py=evt.clientY-rect.top;if(osdReady&&osdItem()){const vp=osdViewer.viewport.pointFromPixel(new OpenSeadragon.Point(px,py),true),img=osdItem().viewportToImageCoordinates(vp);return {x:img.x,y:img.y};}return {x:(px-offsetX)/scale,y:(py-offsetY)/scale};}\n",
    "function updateStatus(p,level){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x';msg+=magnificationStatus();msg+=(typeof multiViewStatus==='function'?multiViewStatus():'');msg+=activeTileMode()?(' | Deep Zoom level '+level+'/'+cfg.max_level):' | preview image';msg+=stainStatus();msg+=measureStatus();msg+=trajectoryStatus();msg+=imageTransformStatus();if(loadingTiles)msg+=' | loading '+loadingTiles+' tile'+(loadingTiles===1?'':'s');if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg+(activeTileMode()?' | full-resolution tiled viewer, full slide not loaded into R':' | preview item; convert to tiled source for full-resolution zoom');}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){notify('Add at least 3 points','warning');return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;markAnnotationsDirty('roi_added');recordAnnotationHistory('roi_added',{id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount});buildRoiList();updateButtons();setMode('select');notify('ROI saved','success');draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});const cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:cls},class:cls,source:roi.source||null},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(!rois.length&&draft.length<3){notify('Draw an ROI first','warning');return;}const text=geojsonText();const name=(typeof projectAnnotationFilename==='function'?projectAnnotationFilename():null)||cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();markAnnotationsSaved('geojson_saved');notify('ROI saved','success');return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);markAnnotationsSaved('geojson_exported');notify('GeoJSON exported','success');}\n",
    "function updateButtons(){const has=rois.length>0,setDisabled=(id,value)=>{const button=el(id);if(button)button.disabled=!!value;},setActive=(id,value)=>{const button=el(id);if(button)button.classList.toggle('active',!!value);};['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>setDisabled(id,!has));setDisabled('finishRoi',draft.length<3);setDisabled('undoPoint',draft.length<1);setDisabled('saveGeojson',!has&&draft.length<3);if(typeof updateTrajectoryButtons==='function')updateTrajectoryButtons();setActive('roiToggle',showRois&&has);setActive('labelsToggle',showLabels&&has);setActive('crosshairToggle',showCrosshair);}\n",
    wsi_viewer_geometry_js(),
    wsi_viewer_layers_js(),
    wsi_viewer_cell_controls_js(),
    wsi_viewer_seurat_js(),
    wsi_viewer_kodama_js(),
    wsi_viewer_measure_js(),
    wsi_viewer_trajectory_js(),
    wsi_viewer_segmentation_js(),
    "canvas.addEventListener('mousedown',e=>{lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='trajectory'){if(e.detail===1)addTrajectoryPoint(lastPointer);return;}if(mode==='brush'){startBrush(lastPointer,e);return;}if(mode==='edit'){activeVertex=findVertexAt(e.clientX,e.clientY);if(activeVertex){pushAnnotationUndo('roi_vertex_moved');selectAnnotation(activeVertex.roi,false);draggingVertex=activeVertex;updateRoiList();draw();return;}selectAnnotation(roiAt(lastPointer),true);draw();return;}if(mode==='measure'){addMeasurePoint(lastPointer);return;}if(mode==='select'){if(!selectObjectAtPoint(lastPointer)){clearSelectedAnnotation(true);clearSelectedTrajectory(true);draw();}return;}dragging=true;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',e=>{if(brushing)finishBrush();if(draggingVertex){draggingVertex=null;buildRoiList();}dragging=false;canvas.classList.remove('dragging');updateCursorFeedback(e);});\n",
    "window.addEventListener('mousemove',e=>{lastPointer=pointerToSlide(e);updateCursorFeedback(e);if(brushing){addBrushPoint(lastPointer,e);return;}if(draggingVertex){moveActiveVertex(lastPointer);return;}if(dragging){panByPixels(e.clientX-lastX,e.clientY-lastY);lastX=e.clientX;lastY=e.clientY;}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.25:1/1.25,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}if(mode==='trajectory'){e.preventDefault();finishTrajectory();return;}if(mode==='edit'){e.preventDefault();insertVertexAt(pointerToSlide(e),e.clientX,e.clientY);return;}const p=pointerToSlide(e);if(selectObjectAtPoint(p)){e.preventDefault();return;}e.preventDefault();clearSelectionAndPan();});\n",
    "const bindButton=(id,handler)=>{const button=el(id);if(button)button.onclick=handler;};bindButton('toolPan',()=>setMode('pan'));bindButton('toolSelect',()=>setMode('select'));bindButton('toolDraw',()=>setMode('draw'));bindButton('toolBrush',e=>{setMode('brush');closeContainingToolMenu(e.currentTarget);});bindButton('newRoi',()=>startNewAnnotation(mode==='draw'?'draw':'brush'));bindButton('toolEdit',()=>setMode('edit'));bindButton('finishRoi',finishDraft);bindButton('undoPoint',()=>{if(mode==='brush'&&brushPoints.length){brushPoints.pop();draw();}else undoDraftPoint();});bindButton('saveGeojson',saveGeojson);bindButton('zoomIn',()=>zoomAt(1.5,innerWidth/2,innerHeight/2));bindButton('zoomOut',()=>zoomAt(1/1.5,innerWidth/2,innerHeight/2));bindButton('fit',fitView);bindButton('oneToOne',oneToOne);\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);el('layersToggle').onclick=()=>{toggleRoiPanel();updateButtons();};el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);saveRoiOpacityPreference();draw();};el('crosshairToggle').onclick=()=>{showCrosshair=!showCrosshair;updateButtons();draw();};\n",
    "window.addEventListener('keydown',e=>{const key=String(e.key||'').toLowerCase(),typing=e.target&&['INPUT','TEXTAREA','SELECT'].includes(e.target.tagName);if(brushSubtractKeyEvent(e)){brushAltDown=true;updateCursorFeedback(e);draw();}if((e.ctrlKey||e.metaKey)&&!typing&&((e.shiftKey&&key==='z')||key==='y')){e.preventDefault();restoreAnnotationRedo();return;}if((e.ctrlKey||e.metaKey)&&!e.shiftKey&&key==='z'&&!typing){e.preventDefault();if(mode==='trajectory'&&trajectoryDraft.length){undoTrajectoryPoint();return;}restoreAnnotationUndo();return;}if(!typing&&!e.ctrlKey&&!e.metaKey&&!e.altKey&&!e.shiftKey&&key==='n'){e.preventDefault();startNewAnnotation(mode==='draw'?'draw':'brush');return;}if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='b')setMode('brush');if(e.key==='e')setMode('edit');if(e.key==='m')setMode('measure');if(e.key==='t')setMode('trajectory');if(e.key==='Enter'&&mode==='draw')finishDraft();if(e.key==='Enter'&&mode==='trajectory'){e.preventDefault();finishTrajectory();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='draw'){e.preventDefault();undoDraftPoint();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='trajectory'){e.preventDefault();undoTrajectoryPoint();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='edit'&&activeVertex){e.preventDefault();deleteSelectedVertex();}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='c')el('crosshairToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape'){measureStart=null;trajectoryDraft=[];brushing=false;brushPoints=[];brushOperation='new';brushTargetRoi=-1;brushClass='';brushAdditiveSelection=false;brushTouchedSelection=new Set();draggingVertex=null;activeVertex=null;setMode('pan');draw();}});\n",
    "window.addEventListener('keyup',e=>{if(e.key==='Alt'||e.key==='Meta'||mode==='brush'||mode==='edit'){brushAltDown=brushSubtractModifier(e);updateCursorFeedback(e);draw();}});\n",
    "function initOpenSeadragon(){if(!window.OpenSeadragon){notify('OpenSeadragon failed to load','error',4200);return;}const roundMode=(OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES&&OpenSeadragon.SUBPIXEL_ROUNDING_OCCURRENCES.ALWAYS)||undefined;osdViewer=OpenSeadragon({element:viewerEl,showNavigationControl:false,showNavigator:false,blendTime:0,alwaysBlend:false,immediateRender:true,placeholderFillStyle:'#fff',subPixelRoundingForTransparency:roundMode,minPixelRatio:1,maxImageCacheCount:512,animationTime:0.12,springStiffness:9,visibilityRatio:0.8,constrainDuringPan:true,minZoomImageRatio:0.7,maxZoomPixelRatio:16,gestureSettingsMouse:{clickToZoom:false,dblClickToZoom:false,scrollToZoom:false,dragToPan:false},gestureSettingsTouch:{pinchToZoom:true,dragToPan:false},tileSources:tileSourceFromConfig()});osdViewer.addHandler('open',()=>{osdReady=true;tileFailureNotified=false;applyOpenSeadragonImageTransform();applyBaseImageDisplay();installInitialChannelSources();resize();if(typeof projectItems!=='undefined'&&projectItems.length&&typeof activeProjectSection==='function'&&typeof zoomToProjectContent==='function')zoomToProjectContent(projectItems[activeProjectIndex]||null,activeProjectSection());if(typeof refreshMultiViewSources==='function')refreshMultiViewSources();prefetchNeighborTiles();notify(activeTileMode()?'Tiled viewer ready':'Preview image ready',activeTileMode()?'success':'info');draw();});['animation','animation-finish','tile-drawn','tile-loaded','tile-load-failed','resize'].forEach(name=>osdViewer.addHandler(name,()=>{markBaseImageDirty();syncViewState();if(name==='animation-finish'){if(typeof syncMultiViewFrom==='function')syncMultiViewFrom(osdViewer);scheduleViewerStateSync('viewport_changed',{scale:scale,offset_x:offsetX,offset_y:offsetY});prefetchNeighborTiles();}else if(name==='tile-loaded')prefetchNeighborTiles();else if(name==='tile-load-failed'&&!tileFailureNotified){tileFailureNotified=true;notify('Tiles did not load. For live CZI viewing, keep the R session that created the viewer running.','warning',7200);}requestDraw();}));}\n",
    "bindExclusiveMenus();bindShortcutHelp();bindCommandPalette();bindMiniNavigator();bindProjectPanel();bindRoiPanelControls();bindSelectionCardControls();bindJobControls();bindStainControls();bindBaseImageControls();bindRoiClassControls();bindAnnotationListControls();bindAnnotationHistoryControls();bindAnnotationSectionMaximizeControls();bindMeasureControls();bindTrajectoryControls();bindImageTransformControls();bindGeojsonImportControls();bindSegmentationControls();bindArtifactControls();bindCellControls();bindSeuratControls();bindKodamaControls();bindMagnificationControls();bindMultiViewControls();bindPreferenceControls();buildRoiList();buildLayerList();buildChannelList();const initialMode=applyViewerPreferences();updateButtons();updateAnnotationDirtyIndicator();syncMessage('');setMode(initialMode||'pan');startViewerStateSocket();scheduleViewerStateSync('viewer_loaded',{});startViewerCommandPolling();startViewerAutosave();\n",
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
#' disk so zooming can reveal full-resolution detail in the browser.
#'
#' The viewer includes lightweight pathology-viewer controls inspired by tools
#' such as QuPath and napari. Controls are grouped into menus for navigation,
#' annotations, GeoJSON overlays, view aids, and optional stains. These cover
#' pan and annotation modes, fit and 1:1 zoom, ROI visibility and label toggles, ROI
#' opacity, ROI previous/next navigation, a left-side annotation manager with
#' GeoJSON geometry listing, visibility toggles, lock/unlock, colour editing,
#' name/class editing, duplicate/delete, zoom-to-ROI, selected-ROI export,
#' browser-side GeoJSON import, crosshair display, polygon drawing,
#' brush-style annotation painting with a brush-size slider,
#' selected-ROI extension, platform-aware brush removal (`Alt` on Windows/Linux
#' and `Command` on macOS), hole filling, selected-ROI vertex editing,
#' name/class reassignment including custom annotation categories, 10-step
#' `Ctrl+Z` undo and `Ctrl+Shift+Z`/`Ctrl+Y` redo for annotations and
#' trajectories, selected-ROI StarDist export,
#' optional selected-ROI segmentation runs, segmentation GeoJSON/CSV import,
#' distance measurement in pixels and micrometres when MPP metadata is
#' available, browser-based GeoJSON export, and a `Ctrl+K` command palette for
#' common actions such as importing GeoJSON, exporting selected ROIs, running
#' StarDist, showing a coordinate-only tile grid, and requesting a project save
#' from a live R session. A `?` shortcut and Help menu show a compact viewer
#' guide plus the keyboard map for pan, draw, brush, edit, measure, undo, redo,
#' save, import, and export. The View menu can split tiled slides into 2, 4, or
#' 6 OpenSeadragon panes with linked or independent zoom/pan for comparing
#' separate tissue regions.
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
#' @param hematoxylin,hrp RGB optical-density vectors used when
#'   `stain = "ihc"` and `channels` is not supplied.
#' @param hematoxylin_colour,hrp_colour Initial display colours for the
#'   hematoxylin and HRP/DAB channels.
#' @param hematoxylin_strength,hrp_strength Initial display gains for the
#'   hematoxylin and HRP/DAB channels.
#' @param segmentation_run_url Optional HTTP endpoint used by the viewer's
#'   `Run segmentation` button. The browser posts the selected ROI GeoJSON to
#'   this URL and expects either GeoJSON or JSON with a `geojson` field in
#'   response. Use
#'   [wsi_stardist_server()] to create a local endpoint.
#' @param viewer_state_url Optional HTTP endpoint used to sync annotations,
#'   measurements, segmentation overlays, and display state back to R. Use
#'   [wsi_viewer_live()] or [wsi_viewer_session()] to create this endpoint.
#' @param viewer_state_ws_url Optional WebSocket endpoint used by live viewers
#'   for lower-latency browser-to-R sync. The HTTP polling bridge remains the
#'   fallback when this is `NULL` or WebSockets are unavailable.
#' @param viewer_transport Browser-to-R live transport advertised to the
#'   viewer: `"auto"`, `"websocket"`, or `"polling"`. Static viewers ignore
#'   this value.
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
#'   CellPhenotyper/StarDist outputs.
#' @param seurat Optional object returned by [wsi_link_seurat_image()] used to
#'   enable the top **Seurat** menu and PCA plot.
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
                       mode = c("thumbnail", "tiles"), tile_dir = NULL,
                       tile_size = 512, tile_format = c("jpg", "png"),
                       quality = 90, rebuild = FALSE,
                       tile_overlap = NULL,
                       tile_url_base = NULL,
                       tile_url_template = NULL,
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
                       hematoxylin = c(0.650, 0.704, 0.286),
                       hrp = c(0.268, 0.570, 0.776),
                       hematoxylin_colour = "#4b3f99",
                       hrp_colour = "#8b5a2b",
                       hematoxylin_strength = 1,
                       hrp_strength = 1,
                       segmentation_run_url = NULL,
                       viewer_state_url = NULL,
                       viewer_state_ws_url = NULL,
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
  mode <- match.arg(mode)
  tile_format <- match.arg(tile_format)
  tile_url_style <- match.arg(tile_url_style)
  viewer_transport <- match.arg(viewer_transport)
  stain <- match.arg(stain)
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
  if (!is.null(tile_overlap)) {
    tile_overlap <- as.integer(wsi_check_scalar_number(tile_overlap, "tile_overlap"))
    if (tile_overlap >= tile_size) {
      wsi_abort("`tile_overlap` must be smaller than `tile_size`.")
    }
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
    visible = TRUE,
    opacity = 1
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
  layers_config <- wsi_viewer_layers_config(layers)
  seurat_config <- wsi_viewer_seurat_config(seurat)
  cellphenotyper_config <- wsi_viewer_cellphenotyper_config(cellphenotyper)

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
      autosave_enabled = isTRUE(autosave_enabled) && (!is.null(viewer_state_url) || !is.null(viewer_state_ws_url)),
      autosave_interval_ms = as.integer(max(1000, round(autosave_interval * 1000))),
      autosave_path = autosave_path,
      stain = stain_config,
      base_layer = base_layer_config,
      project = project_config,
      channel_sources = wsi_channel_sources_payload(channel_sources),
      tile_sources = tile_sources %||% list(),
      rois = rois,
      layers = layers_config,
      seurat = seurat_config,
      cellphenotyper = cellphenotyper_config
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
      autosave_enabled = isTRUE(autosave_enabled) && (!is.null(viewer_state_url) || !is.null(viewer_state_ws_url)),
      autosave_interval_ms = as.integer(max(1000, round(autosave_interval * 1000))),
      autosave_path = autosave_path,
      stain = stain_config,
      base_layer = base_layer_config,
      project = project_config,
      channel_sources = wsi_channel_sources_payload(channel_sources),
      tile_sources = tile_sources %||% list(),
      rois = rois,
      layers = layers_config,
      seurat = seurat_config,
      cellphenotyper = cellphenotyper_config
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
