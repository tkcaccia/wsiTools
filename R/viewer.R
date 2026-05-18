wsi_html_escape <- function(x) {
  x <- as.character(x %||% "")
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

wsi_create_deepzoom_tiles <- function(slide, tile_dir, tile_size = 512,
                                      tile_format = c("jpg", "png"),
                                      quality = 90, rebuild = FALSE) {
  tile_format <- match.arg(tile_format)
  tile_size <- as.integer(wsi_check_scalar_number(tile_size, "tile_size", allow_zero = FALSE))
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
    return(list(dzi = dzi_file, tiles = tile_files))
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
    "0",
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

  list(dzi = dzi_file, tiles = tile_files)
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
  geometry_type <- tolower(geometry_type %||% "")

  if (identical(geometry_type, "polygon")) {
    rings <- lapply(coordinates, wsi_viewer_ring)
    return(rings[vapply(rings, length, integer(1)) >= 3L])
  }

  if (identical(geometry_type, "multipolygon")) {
    rings <- unlist(lapply(coordinates, function(polygon) {
      polygon_rings <- lapply(polygon, wsi_viewer_ring)
      polygon_rings[vapply(polygon_rings, length, integer(1)) >= 3L]
    }), recursive = FALSE)
    return(rings)
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

wsi_viewer_point_count <- function(coordinates) {
  nrow(wsi_collect_points(coordinates))
}

wsi_viewer_hex_to_rgba <- function(hex, alpha = 0.15) {
  rgb <- grDevices::col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.3f)", rgb[1L], rgb[2L], rgb[3L], alpha)
}

wsi_viewer_roi_features <- function(roi = NULL, fill_alpha = 0.15) {
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
  features <- list()

  for (i in seq_len(nrow(roi))) {
    colour <- palette[((i - 1L) %% length(palette)) + 1L]
    rings <- wsi_viewer_roi_rings(roi$geometry_type[[i]], roi$coordinates[[i]])
    drawable <- length(rings) > 0L
    features[[length(features) + 1L]] <- list(
      id = as.character(roi$roi_id[[i]]),
      name = as.character(roi$name[[i]] %||% roi$roi_id[[i]]),
      label = as.character(roi$name[[i]] %||% roi$roi_id[[i]]),
      class = as.character(roi$class[[i]] %||% NA_character_),
      geometry_type = as.character(roi$geometry_type[[i]] %||% NA_character_),
      source = "geojson",
      drawable = drawable,
      point_count = wsi_viewer_point_count(roi$coordinates[[i]]),
      area = if (drawable) wsi_viewer_polygon_area(rings) else NA_real_,
      bbox = list(
        xmin = unname(as.numeric(roi$xmin[[i]])),
        ymin = unname(as.numeric(roi$ymin[[i]])),
        xmax = unname(as.numeric(roi$xmax[[i]])),
        ymax = unname(as.numeric(roi$ymax[[i]]))
      ),
      coordinates = roi$coordinates[[i]],
      colour = colour,
      fill = wsi_viewer_hex_to_rgba(colour, alpha = fill_alpha),
      rings = rings
    )
  }

  features
}

wsi_viewer_styles <- function(background = "#101010") {
  paste0(
    "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:", background, ";color:#f1f1f1;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;}\n",
    "#viewer{display:block;width:100vw;height:100vh;background:", background, ";cursor:grab;}\n",
    "#viewer.dragging{cursor:grabbing;}\n",
    "#viewer.selecting,#viewer.drawing,#viewer.brushing,#viewer.editing,#viewer.measuring{cursor:crosshair;}\n",
    ".bar{position:fixed;left:12px;right:12px;top:12px;display:flex;gap:8px;align-items:center;pointer-events:none;}\n",
    ".panel{background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;backdrop-filter:blur(6px);pointer-events:auto;}\n",
    ".title{font-weight:600;max-width:34vw;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".meta{font-size:12px;color:#d2d2d2;}\n",
    ".spacer{flex:1;}\n",
    ".tools{display:flex;gap:6px;align-items:center;flex-wrap:wrap;justify-content:flex-end;position:relative;}\n",
    ".sep{width:1px;height:22px;background:rgba(255,255,255,.18);display:inline-block;}\n",
    "button{appearance:none;border:1px solid rgba(255,255,255,.24);background:#252525;color:#f2f2f2;border-radius:5px;padding:6px 9px;font-size:13px;line-height:1;}\n",
    "button:hover{background:#333;}\n",
    "button.active{background:#0f766e;border-color:#5eead4;color:#fff;}\n",
    "button:disabled{opacity:.38;cursor:not-allowed;}\n",
    ".toolMenu{position:relative;}\n",
    ".toolMenu summary{list-style:none;appearance:none;border:1px solid rgba(255,255,255,.24);background:#252525;color:#f2f2f2;border-radius:5px;padding:6px 9px;font-size:13px;line-height:1;cursor:pointer;user-select:none;}\n",
    ".toolMenu summary::-webkit-details-marker{display:none;}\n",
    ".toolMenu summary::after{content:'v';font-size:10px;margin-left:7px;color:#bdbdbd;}\n",
    ".toolMenu[open] summary{background:#333;border-color:#5eead4;}\n",
    ".menuBody{position:absolute;right:0;top:calc(100% + 7px);z-index:20;min-width:230px;max-width:min(360px,calc(100vw - 24px));max-height:calc(100vh - 108px);overflow:auto;background:rgba(18,18,18,.96);border:1px solid rgba(255,255,255,.18);border-radius:6px;padding:8px;box-shadow:0 16px 36px rgba(0,0,0,.34);display:flex;flex-direction:column;gap:6px;}\n",
    ".menuGrid{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:6px;}\n",
    ".menuBody button{width:100%;text-align:center;}\n",
    ".menuBody label.control{justify-content:space-between;gap:10px;min-height:28px;}\n",
    "#stainChannelControls{display:flex;flex-direction:column;gap:6px;}\n",
    ".menuTitle{font-size:11px;text-transform:uppercase;letter-spacing:.04em;color:#a8a8a8;margin:2px 2px 0;}\n",
    ".menuHint{font-size:11px;color:#aaa;line-height:1.3;margin:0 2px 2px;}\n",
    "label.control{display:flex;gap:6px;align-items:center;color:#d7d7d7;font-size:12px;}\n",
    "input[type=range]{width:82px;accent-color:#5eead4;}\n",
    "#brushSizeValue{min-width:42px;text-align:right;color:#d7d7d7;font-size:11px;}\n",
    "input[type=color]{width:28px;height:22px;border:1px solid rgba(255,255,255,.28);border-radius:4px;background:transparent;padding:0;}\n",
    "input[type=checkbox]{accent-color:#5eead4;}\n",
    "input[type=text]{background:#202020;color:#f2f2f2;border:1px solid rgba(255,255,255,.24);border-radius:5px;padding:5px 7px;font-size:12px;min-width:0;width:160px;}\n",
    "select{background:#202020;color:#f2f2f2;border:1px solid rgba(255,255,255,.24);border-radius:5px;padding:5px 7px;font-size:12px;}\n",
    "#status{position:fixed;left:12px;bottom:12px;font-size:12px;color:#eee;background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;max-width:calc(100vw - 24px);}\n",
    "#roiPanel{position:fixed;left:12px;top:72px;width:380px;max-height:calc(100vh - 132px);overflow:auto;display:none;}\n",
    "#roiPanel.open{display:block;}\n",
    ".sideTitle{font-weight:600;margin-bottom:3px;}\n",
    ".sideMeta{font-size:11px;color:#b8b8b8;margin-bottom:8px;line-height:1.35;}\n",
    ".roiItem{display:block;width:100%;margin:6px 0;padding:8px;border-radius:5px;border:1px solid rgba(255,255,255,.12);background:rgba(255,255,255,.04);color:#eee;text-align:left;}\n",
    ".roiItem:disabled{opacity:.72;cursor:default;}\n",
    ".roiItem.active{border-color:#5eead4;background:rgba(20,184,166,.2);}\n",
    ".roiTop{display:flex;align-items:center;gap:8px;margin-bottom:5px;}\n",
    ".swatch{width:10px;height:10px;border-radius:50%;display:inline-block;flex:0 0 auto;}\n",
    ".roiName{overflow:hidden;text-overflow:ellipsis;white-space:nowrap;font-weight:600;}\n",
    ".roiClass{color:#aaa;font-size:11px;margin-left:auto;max-width:90px;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".roiDetails{display:grid;grid-template-columns:76px 1fr;gap:2px 8px;font-size:11px;color:#cfcfcf;line-height:1.25;}\n",
    ".roiDetails span:nth-child(odd){color:#8f8f8f;}\n",
    ".roiDetails code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;white-space:normal;word-break:break-word;}\n",
    ".roiBadge{display:inline-block;border:1px solid rgba(255,255,255,.16);border-radius:4px;padding:1px 5px;font-size:10px;color:#d7d7d7;background:rgba(255,255,255,.06);}\n",
    "#measureList{display:flex;flex-direction:column;gap:5px;max-height:180px;overflow:auto;}\n",
    ".measureItem{display:block;width:100%;text-align:left;line-height:1.25;}\n",
    ".measureItem code{font-family:ui-monospace,SFMono-Regular,Menlo,monospace;color:#e7e7e7;font-size:10.5px;}\n",
    "@media(max-width:900px){.title{max-width:42vw}.bar{align-items:flex-start}.tools{max-width:56vw}.menuBody{right:auto;left:0}#roiPanel{left:12px;right:12px;top:118px;width:auto;}}\n"
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
    "function bindExclusiveMenus(){const menus=Array.from(document.querySelectorAll('.toolMenu'));const closeOtherMenus=active=>menus.forEach(menu=>{if(menu!==active)menu.open=false;});const closeAllMenus=()=>menus.forEach(menu=>{menu.open=false;});menus.forEach(menu=>{const summary=menu.querySelector('summary');if(summary){summary.addEventListener('pointerdown',()=>closeOtherMenus(menu));summary.addEventListener('click',()=>setTimeout(()=>{if(menu.open)closeOtherMenus(menu);},0));}menu.addEventListener('toggle',()=>{if(menu.open)closeOtherMenus(menu);});});document.addEventListener('click',e=>{if(!e.target.closest('.toolMenu'))closeAllMenus();});document.addEventListener('keydown',e=>{if(e.key==='Escape')closeAllMenus();});}\n"
  )
}

wsi_viewer_stain_controls <- function(config) {
  if (!isTRUE(config$stain$enabled)) {
    return("")
  }
  channels <- config$stain$channels
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
  channel_controls <- vapply(channels, function(channel) {
    id <- wsi_html_escape(channel$id)
    name <- wsi_html_escape(channel$name)
    strength <- format(channel$strength, trim = TRUE, scientific = FALSE)
    paste0(
      "<label class=\"control stainChannel\" title=\"Show ", name, " channel\"><input id=\"stainVisible_",
      id,
      "\" type=\"checkbox\"",
      if (isTRUE(channel$visible)) " checked" else "",
      ">",
      name,
      "</label>",
      "<label class=\"control\" title=\"", name, " display colour\"><input id=\"stainColor_",
      id,
      "\" type=\"color\" value=\"",
      wsi_html_escape(channel$colour),
      "\"></label>",
      "<label class=\"control\" title=\"", name, " display gain\">gain <input id=\"stainStrength_",
      id,
      "\" type=\"range\" min=\"0\" max=\"3\" step=\"0.05\" value=\"",
      wsi_html_escape(strength),
      "\"></label>"
    )
  }, character(1))
  wsi_viewer_menu(
    label = "Stains",
    title = "Stain deconvolution display options",
    class = "stainMenu",
    contents = paste0(
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
      "<div class=\"menuTitle\">Channels</div>",
      "<span id=\"stainChannelControls\">",
      paste(channel_controls, collapse = ""),
      "</span>",
      "<div id=\"stainMessage\" class=\"menuHint\"></div>"
    )
  )
}

wsi_viewer_chrome <- function(config, loading_message) {
  has_rois <- length(config$rois %||% list()) > 0L
  roi_panel_class <- if (has_rois) "panel open" else "panel"
  paste0(
    "<canvas id=\"viewer\"></canvas>\n",
    "<div class=\"bar\">\n",
    "<div class=\"panel\"><div class=\"title\">", wsi_html_escape(config$title), "</div><div class=\"meta\">",
    wsi_html_escape(config$subtitle), "</div></div>\n",
    "<div class=\"spacer\"></div>\n",
    "<div class=\"panel tools\" role=\"toolbar\" aria-label=\"Viewer tools\">",
    wsi_viewer_menu(
      "Navigate",
      "Pan and zoom controls",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"toolPan\" class=\"active\" title=\"Pan mode\">Pan</button>",
        "<button id=\"fit\" title=\"Fit slide to window\">Fit</button>",
        "<button id=\"zoomIn\" title=\"Zoom in\">+</button>",
        "<button id=\"zoomOut\" title=\"Zoom out\">-</button>",
        "<button id=\"oneToOne\" title=\"Show image pixels at 1:1\">1:1</button>",
        "</div>"
      )
    ),
    wsi_viewer_menu(
      "Annotations",
      "Draw, select, undo, and export annotations",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"toolSelect\" title=\"Select ROI mode\">Select</button>",
        "<button id=\"toolDraw\" title=\"Draw a polygon ROI\">Draw ROI</button>",
        "<button id=\"toolBrush\" title=\"Paint to create or extend the selected annotation; hold Alt to remove from the selected annotation\">Brush</button>",
        "<button id=\"toolEdit\" title=\"Edit selected ROI vertices\">Edit</button>",
        "<button id=\"finishRoi\" title=\"Finish current polygon\">Finish</button>",
        "<button id=\"undoPoint\" title=\"Undo last polygon point\">Undo</button>",
        "</div>",
        "<label class=\"control\" title=\"Brush radius in slide pixels\">Brush size <input id=\"brushSize\" type=\"range\" min=\"5\" max=\"1000\" step=\"5\" value=\"80\"><span id=\"brushSizeValue\">80 px</span></label>",
        "<div class=\"menuHint\">Brush creates a new ROI when none is selected, extends the selected ROI, and removes from it while Alt is held.</div>",
        "<label class=\"control\" title=\"Free-text annotation label for drawn or selected ROIs\">Name <input id=\"roiLabelInput\" type=\"text\" maxlength=\"120\" placeholder=\"annotation label\"></label>",
        "<label class=\"control\" title=\"Pathology class for drawn or selected ROIs\">Class <select id=\"roiClassSelect\">",
        "<option value=\"tumour\">tumour</option>",
        "<option value=\"stroma\">stroma</option>",
        "<option value=\"necrosis\">necrosis</option>",
        "<option value=\"normal\">normal</option>",
        "<option value=\"artefact\">artefact</option>",
        "<option value=\"exclusion\">exclusion</option>",
        "</select></label>",
        "<button id=\"applyRoiClass\" title=\"Apply the name and class to the selected ROI\">Apply to selected</button>",
        "<button id=\"deleteRoi\" title=\"Delete the selected ROI\">Delete selected</button>",
        "<button id=\"saveGeojson\" title=\"Save annotations as GeoJSON\">Save GeoJSON</button>"
      )
    ),
    wsi_viewer_menu(
      "GeoJSON",
      "ROI overlay and GeoJSON geometry list",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"roiToggle\" title=\"Toggle ROI overlays\">ROI</button>",
        "<button id=\"labelsToggle\" title=\"Toggle ROI labels\">Labels</button>",
        "<button id=\"prevRoi\" title=\"Previous ROI\">Prev</button>",
        "<button id=\"nextRoi\" title=\"Next ROI\">Next</button>",
        "</div>",
        "<button id=\"importGeojson\" title=\"Import QuPath or wsiTools GeoJSON annotations\">Import GeoJSON</button>",
        "<button id=\"layersToggle\" title=\"Show GeoJSON geometry list\">Geometry list</button>",
        "<label class=\"control\" title=\"ROI opacity\">Opacity <input id=\"roiOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>",
        "<input id=\"geojsonImportFile\" type=\"file\" accept=\".geojson,.json,application/geo+json,application/json\" style=\"display:none\">",
        "<div id=\"geojsonImportSummary\" class=\"menuHint\"></div>"
      )
    ),
    wsi_viewer_menu(
      "Segmentation",
      "StarDist segmentation import for selected ROIs",
      paste0(
        "<div class=\"menuTitle\">StarDist</div>",
        "<div class=\"menuGrid\">",
        "<button id=\"exportSelectedRoi\" title=\"Export selected ROI as GeoJSON for StarDist crop analysis\">Export ROI</button>",
        "<button id=\"startSegmentation\" title=\"Run configured StarDist service on the selected ROI\">Start StarDist</button>",
        "<button id=\"loadSegmentation\" title=\"Import StarDist GeoJSON polygons as cell overlays\">Load GeoJSON</button>",
        "<button id=\"loadSegmentationCsv\" title=\"Import StarDist CSV/TSV centroid table as cell markers\">Load CSV</button>",
        "<button id=\"clearSegmentation\" title=\"Remove imported StarDist overlays\">Clear cells</button>",
        "</div>",
        "<label class=\"control\" title=\"Treat imported coordinates as crop-local and offset by the selected ROI bounding box\"><input id=\"segLocalCoords\" type=\"checkbox\" checked>crop coords</label>",
        "<label class=\"control\" title=\"Cell marker radius for CSV/TSV centroid imports\">cell radius <input id=\"segCellRadius\" type=\"range\" min=\"2\" max=\"80\" step=\"1\" value=\"8\"><span id=\"segCellRadiusValue\">8 px</span></label>",
        "<input id=\"segmentationFile\" type=\"file\" accept=\".geojson,.json,application/geo+json,application/json\" style=\"display:none\">",
        "<input id=\"segmentationTableFile\" type=\"file\" accept=\".csv,.tsv,.txt,text/csv,text/tab-separated-values,text/plain\" style=\"display:none\">",
        "<div id=\"segmentationSummary\" class=\"menuHint\"></div>"
      )
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
      "View",
      "Display aids and coordinates",
      paste0(
        "<div class=\"menuGrid\">",
        "<button id=\"crosshairToggle\" title=\"Toggle crosshair\">Crosshair</button>",
        "<button id=\"copyCoord\" title=\"Copy current coordinates\">Copy XY</button>",
        "</div>",
        "<div id=\"syncSummary\" class=\"menuHint\"></div>"
      )
    ),
    wsi_viewer_stain_controls(config),
    "</div>\n",
    "</div>\n",
    "<div id=\"roiPanel\" class=\"", roi_panel_class, "\" aria-label=\"GeoJSON geometry list\"><div class=\"sideTitle\">GeoJSON Geometries</div><div id=\"roiSummary\" class=\"sideMeta\"></div><div id=\"roiList\"></div></div>\n",
    "<div id=\"status\">", wsi_html_escape(loading_message), "</div>\n"
  )
}

wsi_viewer_stain_js <- function() {
  paste0(
    "const stainEnabled=!!(cfg.stain&&cfg.stain.enabled);\n",
    "const stainChannels=stainEnabled?(cfg.stain.channels||[]):[];\n",
    "let stainOn=stainEnabled;\n",
    "let stainState=stainChannels.map(ch=>({visible:ch.visible!==false,color:ch.colour||'#666666',strength:Number(ch.strength||1)}));\n",
    "let stainInv=null;\n",
    "let stainError='';\n",
    "function rgbHex(hex){const h=String(hex||'#000000').replace('#','');const s=h.length===3?h.split('').map(c=>c+c).join(''):h;const n=parseInt(s,16);return {r:(n>>16)&255,g:(n>>8)&255,b:n&255};}\n",
    "function norm3(v){const n=Math.hypot(Number(v[0]),Number(v[1]),Number(v[2]));return n>0?[Number(v[0])/n,Number(v[1])/n,Number(v[2])/n]:[0,0,0];}\n",
    "function cross3(a,b){return [a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]];}\n",
    "function inv3(m){const a=m[0][0],b=m[0][1],c=m[0][2],d=m[1][0],e=m[1][1],f=m[1][2],g=m[2][0],h=m[2][1],i=m[2][2];const A=e*i-f*h,B=-(d*i-f*g),C=d*h-e*g,D=-(b*i-c*h),E=a*i-c*g,F=-(a*h-b*g),G=b*f-c*e,H=-(a*f-c*d),I=a*e-b*d;const det=a*A+b*B+c*C;if(Math.abs(det)<1e-8)return null;return [[A/det,D/det,G/det],[B/det,E/det,H/det],[C/det,F/det,I/det]];}\n",
    "function initStain(){if(!stainEnabled)return;const b=(cfg.stain.basis||[]).map(norm3);if(b.length!==3){status.textContent='IHC deconvolution needs a three-vector RGB stain basis.';return;}stainInv=inv3([[b[0][0],b[1][0],b[2][0]],[b[0][1],b[1][1],b[2][1]],[b[0][2],b[1][2],b[2][2]]]);if(!stainInv)status.textContent='IHC stain vectors are not independent enough for deconvolution.';}\n",
    "function syncStainStateFromControls(){if(!stainEnabled)return;stainChannels.forEach((ch,i)=>{if(!stainState[i])stainState[i]={visible:true,color:'#666666',strength:1};const vis=el('stainVisible_'+ch.id),color=el('stainColor_'+ch.id),strength=el('stainStrength_'+ch.id);if(vis)stainState[i].visible=!!vis.checked;if(color)stainState[i].color=color.value;if(strength)stainState[i].strength=Number(strength.value);});}\n",
    "function activeStainNames(){syncStainStateFromControls();return stainChannels.filter((ch,i)=>stainState[i]&&stainState[i].visible).map(ch=>ch.name||ch.id).join(', ');}\n",
    "function setStainMessage(msg){const box=el('stainMessage');if(box)box.textContent=msg||'';}\n",
    "function stainStatus(){if(!stainEnabled)return '';if(stainError)return ' | stains unavailable: '+stainError;if(!stainOn)return ' | original RGB';const active=activeStainNames();return ' | '+(cfg.stain.label||'IHC channels')+(active?' '+active:' no channels');}\n",
    "function applyStainToCanvas(){if(!stainEnabled||!stainOn||!stainInv||!stainChannels.length)return;syncStainStateFromControls();let img;try{img=ctx.getImageData(0,0,canvas.width,canvas.height);stainError='';setStainMessage('');}catch(e){stainError=(location.protocol==='file:')?'open the viewer through localhost/http, not file://':'canvas pixel access blocked';setStainMessage('Stain selection needs canvas pixel access. Open this viewer through http://127.0.0.1/localhost.');return;}const data=img.data,colors=stainState.map(s=>rgbHex(s.color));for(let p=0;p<data.length;p+=4){const r=data[p],g=data[p+1],b=data[p+2];if(data[p+3]===0||(r<28&&g<28&&b<28))continue;const odR=-Math.log((r+1)/256),odG=-Math.log((g+1)/256),odB=-Math.log((b+1)/256);const c=[Math.max(0,stainInv[0][0]*odR+stainInv[0][1]*odG+stainInv[0][2]*odB),Math.max(0,stainInv[1][0]*odR+stainInv[1][1]*odG+stainInv[1][2]*odB),Math.max(0,stainInv[2][0]*odR+stainInv[2][1]*odG+stainInv[2][2]*odB)];let rr=255,gg=255,bb=255;for(let i=0;i<stainChannels.length&&i<c.length;i++){const state=stainState[i];if(!state||!state.visible)continue;const t=clamp(1-Math.exp(-c[i]*state.strength),0,1),col=colors[i];rr=rr*(1-t)+col.r*t;gg=gg*(1-t)+col.g*t;bb=bb*(1-t)+col.b*t;}data[p]=rr;data[p+1]=gg;data[p+2]=bb;}ctx.putImageData(img,0,0);}\n",
    "function setStainVisible(indices){const keep=new Set(indices);stainOn=true;stainChannels.forEach((ch,i)=>{const input=el('stainVisible_'+ch.id);const visible=keep.has(i);if(input)input.checked=visible;if(!stainState[i])stainState[i]={visible:true,color:'#666666',strength:1};stainState[i].visible=visible;});updateStainControls();scheduleViewerStateSync('stain_updated',{});draw();}\n",
    "function showOriginalStain(){stainOn=false;updateStainControls();scheduleViewerStateSync('stain_updated',{});draw();}\n",
    "function updateStainControls(){if(!stainEnabled)return;const toggle=el('stainToggle');if(toggle)toggle.classList.toggle('active',stainOn);const original=el('stainShowOriginal'),all=el('stainShowAll');if(original)original.classList.toggle('active',!stainOn);if(all)all.classList.toggle('active',stainOn&&stainState.every(s=>s&&s.visible));document.querySelectorAll('.stainOnly').forEach(button=>{const idx=Number(button.dataset.stainIndex);button.classList.toggle('active',stainOn&&stainState[idx]&&stainState[idx].visible&&stainState.filter(s=>s&&s.visible).length===1);});stainChannels.forEach(ch=>['Visible_','Color_','Strength_'].forEach(prefix=>{const input=el('stain'+prefix+ch.id);if(input)input.disabled=!stainOn;}));}\n",
    "function bindStainControls(){if(!stainEnabled)return;initStain();syncStainStateFromControls();const toggle=el('stainToggle');if(toggle)toggle.onclick=()=>{stainOn=!stainOn;updateStainControls();scheduleViewerStateSync('stain_updated',{});draw();};const original=el('stainShowOriginal'),all=el('stainShowAll');if(original)original.onclick=showOriginalStain;if(all)all.onclick=()=>setStainVisible(stainChannels.map((_,i)=>i));document.querySelectorAll('.stainOnly').forEach(button=>{button.onclick=()=>setStainVisible([Number(button.dataset.stainIndex)]);});const redraw=()=>{stainOn=true;syncStainStateFromControls();updateStainControls();scheduleViewerStateSync('stain_updated',{});draw();};stainChannels.forEach(ch=>{['Visible_','Color_','Strength_'].forEach(prefix=>{const input=el('stain'+prefix+ch.id);if(input){input.addEventListener('input',redraw);input.addEventListener('change',redraw);}});});updateStainControls();}\n"
  )
}

wsi_viewer_sync_js <- function() {
  paste0(
    "let stateSyncTimer=null,stateSyncEvent='viewer_state',stateSyncDetail={},stateSyncSeq=0;\n",
    "function syncMessage(msg){const box=el('syncSummary');if(box)box.textContent=msg||((cfg.viewer_state_url||'')?'R sync ready':'R sync off');}\n",
    "function roiGeojsonObject(filterFn=null){const features=[];rois.forEach((roi,i)=>{if(filterFn&&!filterFn(roi,i))return;const feature=roiFeature(roi,i);if(feature)features.push(feature);});return {type:'FeatureCollection',features:features};}\n",
    "function selectedRoiFeatureObject(){if(selectedRoi<0||!rois[selectedRoi])return null;return roiFeature(rois[selectedRoi],selectedRoi);}\n",
    "function segmentationGeojsonObject(){return roiGeojsonObject(roi=>{const source=String(roi.source||'').toLowerCase(),cls=String(roi.class||'').toLowerCase();return source.includes('stardist')||source.includes('segmentation')||cls==='cell'||cls==='cells';});}\n",
    "function currentStainPayload(){if(!stainEnabled)return null;syncStainStateFromControls();return {enabled:stainOn,channels:stainChannels.map((ch,i)=>({id:ch.id,name:ch.name,visible:!!(stainState[i]&&stainState[i].visible),color:stainState[i]?stainState[i].color:ch.colour,strength:stainState[i]?stainState[i].strength:ch.strength}))};}\n",
    "function viewerStatePayload(event,detail={}){return {event:event||'viewer_state',time:new Date().toISOString(),sequence:++stateSyncSeq,slide:{title:cfg.title,width:cfg.slide_width,height:cfg.slide_height},selected_index:selectedRoi,selected_roi:selectedRoiFeatureObject(),rois:roiGeojsonObject(),segmentation:segmentationGeojsonObject(),measurements:measures,view:{mode:mode,scale:scale,offset_x:offsetX,offset_y:offsetY,roi_opacity:roiOpacity,show_rois:showRois,show_labels:showLabels},stain:currentStainPayload(),detail:detail};}\n",
    "async function syncViewerState(event='viewer_state',detail={}){const url=cfg.viewer_state_url||'';if(!url)return false;try{const response=await fetch(url,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(viewerStatePayload(event,detail))});if(!response.ok){const text=await response.text();throw new Error(text||('HTTP '+response.status));}syncMessage('R sync: '+event);return true;}catch(e){syncMessage('R sync failed: '+e.message);return false;}}\n",
    "function scheduleViewerStateSync(event='viewer_state',detail={}){if(!(cfg.viewer_state_url||'')){syncMessage('');return;}stateSyncEvent=event;stateSyncDetail=detail||{};clearTimeout(stateSyncTimer);stateSyncTimer=setTimeout(()=>syncViewerState(stateSyncEvent,stateSyncDetail),250);}\n"
  )
}

wsi_viewer_geometry_js <- function() {
  paste0(
    "function fmt(v,d=0){return Number.isFinite(Number(v))?Number(v).toFixed(d):'NA';}\n",
    "function isDrawable(roi){return roi&&roi.drawable!==false&&((roi.rings&&roi.rings.length>0)||(roi.add_rings&&roi.add_rings.length>0));}\n",
    "function positiveRingGroups(roi){const groups=[];if(roi&&roi.rings&&roi.rings.length)groups.push(roi.rings);(roi&&roi.add_rings?roi.add_rings:[]).forEach(r=>{if(r&&r.length>=4)groups.push([r]);});return groups;}\n",
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
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(isDrawable(rois[i])&&roiContainsPoint(rois[i],p))return i;}return -1;}\n",
    "function centerRoi(i){if(!hasDrawable())return;let idx=-1;for(let k=0;k<rois.length;k++){const candidate=(i+rois.length+k)%rois.length;if(isDrawable(rois[candidate])){idx=candidate;break;}}if(idx<0)return;selectedRoi=idx;const b=roiBounds(rois[selectedRoi]);if(!b){status.textContent='This GeoJSON geometry has no drawable bounds.';updateRoiList();draw();return;}let viewW=b.xmax-b.xmin,viewH=b.ymax-b.ymin,centerX=(b.xmin+b.xmax)/2,centerY=(b.ymin+b.ymax)/2;const pad=1.35;if(typeof slideToImage==='function'){const p0=slideToImage({x:b.xmin,y:b.ymin}),p1=slideToImage({x:b.xmax,y:b.ymax});viewW=p1.x-p0.x;viewH=p1.y-p0.y;centerX=(p0.x+p1.x)/2;centerY=(p0.y+p1.y)/2;}const maxScale=(typeof image!=='undefined')?40:4;scale=clamp(Math.min(innerWidth/Math.max(1,viewW*pad),innerHeight/Math.max(1,viewH*pad)),minScale*0.8,maxScale);offsetX=innerWidth/2-centerX*scale;offsetY=innerHeight/2-centerY*scale;updateRoiList();draw();}\n",
    "function roiLabelText(roi,i){return roi.label||roi.name||roi.id||('geometry '+(i+1));}\n",
    "function roiLabelPoint(roi){const b=roiBounds(roi);if(b)return slideToCanvas({x:(b.xmin+b.xmax)/2,y:(b.ymin+b.ymax)/2});if(isDrawable(roi)&&roi.rings[0]&&roi.rings[0][0])return slideToCanvas(roi.rings[0][0]);return null;}\n",
    "function drawPathRings(rings){(rings||[]).forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});}\n",
    "function drawRois(){if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{if(!isDrawable(roi))return;const groups=roiDrawGroups(roi);groups.forEach(group=>{ctx.beginPath();drawPathRings(group.rings);drawPathRings(group.holes);ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;});if(showLabels){const label=roiLabelPoint(roi),text=roiLabelText(roi,i);if(label&&text){const w=ctx.measureText(text).width+8;const x=clamp(label.x-w/2,4,innerWidth-w-4),y=clamp(label.y-9,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.72)';ctx.fillRect(x,y,w,18);ctx.fillStyle=roi.colour||'#5eead4';ctx.fillText(text,x+4,y+3);}}});ctx.restore();}\n",
    "function annotationLabelValue(){const input=el('roiLabelInput');return input?input.value.trim():'';}\n",
    "function currentRoiClass(){const select=el('roiClassSelect');return select&&select.value?select.value:(activeRoiClass||'annotation');}\n",
    "function ensureRoiClassOption(value){const select=el('roiClassSelect');if(!select||!value)return;const known=Array.from(select.options).some(o=>o.value===value);if(!known){const opt=document.createElement('option');opt.value=value;opt.textContent=value;select.appendChild(opt);}select.value=value;}\n",
    "function setRoiPanelOpen(open){const panel=el('roiPanel'),button=el('layersToggle');if(panel)panel.classList.toggle('open',!!open);if(button)button.classList.toggle('active',!!(panel&&panel.classList.contains('open')));}\n",
    "function toggleRoiPanel(){const panel=el('roiPanel');setRoiPanelOpen(!(panel&&panel.classList.contains('open')));}\n",
    "function setRoiControlsFromSelection(){const roi=selectedRoi>=0?rois[selectedRoi]:null;if(!roi)return;const input=el('roiLabelInput');if(input)input.value=roi.name||roi.label||'';if(roi.class){ensureRoiClassOption(roi.class);activeRoiClass=roi.class;}}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));setRoiControlsFromSelection();}\n",
    "function addDetail(parent,label,value,asCode=false){const l=document.createElement('span');l.textContent=label;const v=document.createElement(asCode?'code':'span');v.textContent=value;parent.append(l,v);}\n",
    "function buildRoiList(){const list=el('roiList'),summary=el('roiSummary');list.innerHTML='';summary.textContent=geometrySummary();rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';b.type='button';const top=document.createElement('div');top.className='roiTop';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour||'#cccccc';const nm=document.createElement('span');nm.className='roiName';nm.textContent=roiLabelText(roi,i);const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';top.append(sw,nm,cl);const details=document.createElement('div');details.className='roiDetails';const bb=roiBounds(roi);addDetail(details,'Geometry',geometryType(roi));addDetail(details,'Bounds',formatBounds(bb),true);addDetail(details,'Points',String(pointCount(roi)));const area=Number.isFinite(Number(roi.area))?Number(roi.area):(isDrawable(roi)?polygonArea(roi.rings):NaN);addDetail(details,'Area',Number.isFinite(area)?fmt(area,1):'NA');addDetail(details,'Source',roi.source||'geojson');addDetail(details,'ID',String(roi.id||i+1),true);if(!isDrawable(roi)){const badge=document.createElement('span');badge.className='roiBadge';badge.textContent='listed only';details.append(document.createElement('span'),badge);}b.append(top,details);b.onclick=()=>{if(isDrawable(roi)){centerRoi(i);}else{selectedRoi=i;updateRoiList();draw();status.textContent='Selected '+geometryType(roi)+' geometry in the GeoJSON list; this type is not drawn as a polygon overlay yet.';}};list.appendChild(b);});if(rois.length)setRoiPanelOpen(true);updateRoiList();}\n",
    "function geojsonImportStatus(msg){const box=el('geojsonImportSummary');if(box)box.textContent=msg||'';}\n",
    "function geojsonFeatures(obj){if(!obj)return [];if(obj.type==='FeatureCollection')return obj.features||[];if(obj.type==='Feature')return [obj];if(obj.geometry)return [{type:'Feature',geometry:obj.geometry,properties:obj.properties||{}}];return [];}\n",
    "function collectGeojsonPoints(coords,out=[]){if(Array.isArray(coords)&&coords.length>=2&&typeof coords[0]==='number'&&typeof coords[1]==='number'){const x=Number(coords[0]),y=Number(coords[1]);if(Number.isFinite(x)&&Number.isFinite(y))out.push({x:x,y:y});return out;}if(Array.isArray(coords))coords.forEach(c=>collectGeojsonPoints(c,out));return out;}\n",
    "function boundsFromPoints(points){if(!points||!points.length)return null;const xs=points.map(p=>p.x),ys=points.map(p=>p.y);return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointFromGeojsonCoord(coord){if(!coord||coord.length<2)return null;const x=Number(coord[0]),y=Number(coord[1]);if(!Number.isFinite(x)||!Number.isFinite(y))return null;return {x:x,y:y};}\n",
    "function ringFromGeojsonCoords(coords){const ring=(coords||[]).map(pointFromGeojsonCoord).filter(Boolean);return closedRing(ring);}\n",
    "function polygonRingsFromGeojsonCoords(coords){return (coords||[]).map(ringFromGeojsonCoords).filter(r=>r.length>=4);}\n",
    "function geojsonGeometryParts(geometry){const type=String((geometry&&geometry.type)||'Geometry'),lower=type.toLowerCase(),coords=(geometry&&geometry.coordinates)||[];if(lower==='polygon'){const rings=polygonRingsFromGeojsonCoords(coords);return rings.length?[{geometry_type:'Polygon',rings:rings,coordinates:coords}]:[];}if(lower==='multipolygon'){const out=[];(coords||[]).forEach((poly,idx)=>{const rings=polygonRingsFromGeojsonCoords(poly);if(rings.length)out.push({geometry_type:'Polygon',rings:rings,coordinates:poly,part:idx+1});});return out;}return [{geometry_type:type,rings:[],coordinates:coords}];}\n",
    "function importedFeatureClass(properties){const cls=properties&&properties.classification;if(cls&&typeof cls==='object'&&cls.name)return String(cls.name);if(typeof cls==='string')return cls;if(properties&&properties.class)return String(properties.class);if(properties&&properties.objectType)return String(properties.objectType);return 'annotation';}\n",
    "function importedFeatureName(feature,properties,i){const cls=importedFeatureClass(properties);return String((properties&&(properties.name||properties.label||properties.objectName))||feature.id||cls||('Imported ROI '+(i+1)));}\n",
    "function importedRoiFromFeature(feature,part,i,fileName,partCount){const props=feature.properties||{},rings=part.rings||[],points=collectGeojsonPoints(part.coordinates||((feature.geometry||{}).coordinates)||[]),ringPoints=[];rings.forEach(r=>r.forEach(p=>ringPoints.push(p)));const colour=paletteColour(rois.length+i),baseName=importedFeatureName(feature,props,i),suffix=partCount>1?(' part '+part.part):'',name=baseName+suffix,id=String(feature.id||('imported_roi_'+Date.now()+'_'+i))+(partCount>1?('_'+part.part):''),bbox=rings.length?boundsFromPoints(ringPoints):boundsFromPoints(points);const roi={id:id,name:name,label:name,class:importedFeatureClass(props),geometry_type:part.geometry_type||((feature.geometry||{}).type||'Geometry'),source:fileName?('imported: '+fileName):'imported geojson',drawable:rings.length>0,point_count:points.length,area:rings.length?polygonArea(rings):NaN,bbox:bbox,coordinates:part.coordinates,colour:colour,fill:hexToRgba(colour,0.18),rings:rings,imported:true};if(roi.drawable)refreshRoiGeometry(roi);return roi;}\n",
    "function addImportedGeojson(obj,fileName){const features=geojsonFeatures(obj);if(!features.length){geojsonImportStatus('No GeoJSON features found.');return;}let added=0,listed=0;features.forEach((feature,fi)=>{const parts=geojsonGeometryParts(feature.geometry||{});parts.forEach((part,pi)=>{const roi=importedRoiFromFeature(feature,part,added,fileName,parts.length);if(!roi.drawable&&(!roi.bbox||!Number.isFinite(Number(roi.bbox.xmin))))return;rois.push(roi);if(roi.drawable)added++;else listed++;});});if(!added&&!listed){geojsonImportStatus('No supported GeoJSON geometries found.');return;}selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();scheduleViewerStateSync('geojson_imported',{file:fileName||null,added:added,listed:listed});geojsonImportStatus('Imported '+added+' drawable ROI'+(added===1?'':'s')+(listed?(' and listed '+listed+' other geometr'+(listed===1?'y':'ies')):'' )+'.');}\n",
    "function bindGeojsonImportControls(){const button=el('importGeojson'),file=el('geojsonImportFile');if(button&&file)button.onclick=()=>{file.value='';file.click();};if(file){file.onchange=()=>{const picked=file.files&&file.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addImportedGeojson(JSON.parse(reader.result),picked.name);}catch(e){geojsonImportStatus('Could not import GeoJSON: '+e.message);}};reader.readAsText(picked);};}geojsonImportStatus('');}\n",
    "function paletteColour(i){const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];return palette[i%palette.length];}\n",
    "function refreshRoiGeometry(roi){if(!isDrawable(roi))return;let xs=[],ys=[],area=0;positiveRingGroups(roi).forEach(group=>{group.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));area+=polygonArea(group);});subtractRings(roi).forEach(h=>{area-=ringArea(h);});roi.drawable=true;roi.bbox=xs.length?{xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)}:null;roi.area=Math.max(0,area);roi.point_count=allRoiRings(roi,true).reduce((n,r)=>n+r.length,0);roi.geometry_type=(roi.add_rings&&roi.add_rings.length)?'MultiPolygon':'Polygon';roi.edited=true;}\n",
    "function closedRing(points){const ring=points.map(p=>({x:Math.round(clamp(p.x,0,cfg.slide_width)),y:Math.round(clamp(p.y,0,cfg.slide_height))}));if(ring.length<3)return ring;const f=ring[0],l=ring[ring.length-1];if(!l||f.x!==l.x||f.y!==l.y)ring.push({x:f.x,y:f.y});return ring;}\n",
    "function addRoiFromRing(ring,source,labelPrefix){if(!ring||ring.length<4){status.textContent='Annotation needs at least 3 points.';return null;}const colour=paletteColour(rois.length);newRoiCount++;activeRoiClass=currentRoiClass();activeRoiName=annotationLabelValue();const roiName=activeRoiName||(labelPrefix+' '+newRoiCount);const roi={id:source+'_roi_'+newRoiCount,name:roiName,label:roiName,class:activeRoiClass,geometry_type:'Polygon',source:source,drawable:true,point_count:ring.length-1,area:polygonArea([ring]),bbox:boundsFromRing(ring),colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:source==='drawn',brushed:source==='brush'};rois.push(roi);selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();scheduleViewerStateSync('roi_added',{source:source,id:roi.id});return roi;}\n",
    "function finishDraft(){if(draft.length<3){status.textContent='Add at least 3 points before finishing an ROI.';return;}const ring=closedRing(draft);addRoiFromRing(ring,'drawn','Drawn ROI');draft=[];setMode('select');draw();}\n",
    "function brushCircleRing(p,radius,steps=28){const ring=[];for(let i=0;i<steps;i++){const a=i/steps*Math.PI*2;ring.push({x:p.x+Math.cos(a)*radius,y:p.y+Math.sin(a)*radius});}return closedRing(ring);}\n",
    "function brushRingFromPoints(points,radius){const pts=points.filter(pointInsideSlide);if(!pts.length)return [];if(pts.length===1)return brushCircleRing(pts[0],radius);const left=[],right=[];for(let i=0;i<pts.length;i++){const prev=pts[Math.max(0,i-1)],next=pts[Math.min(pts.length-1,i+1)],dx=next.x-prev.x,dy=next.y-prev.y,len=Math.hypot(dx,dy)||1,nx=-dy/len,ny=dx/len;left.push({x:pts[i].x+nx*radius,y:pts[i].y+ny*radius});right.push({x:pts[i].x-nx*radius,y:pts[i].y-ny*radius});}return closedRing(left.concat(right.reverse()));}\n",
    "function brushRadiusValue(){const input=el('brushSize');return input?Number(input.value||80):brushRadius;}\n",
    "function updateBrushControls(){const input=el('brushSize'),label=el('brushSizeValue');if(input)brushRadius=Number(input.value||80);if(label)label.textContent=Math.round(brushRadius)+' px';}\n",
    "function brushEditableSelection(p){if(selectedRoi<0){const hit=roiAt(p);if(hit>=0)selectedRoi=hit;}return selectedRoi>=0&&isDrawable(rois[selectedRoi]);}\n",
    "function startBrush(p,e={}){if(!pointInsideSlide(p))return;updateBrushControls();const editable=brushEditableSelection(p);brushOperation=editable?(e.altKey?'subtract':'extend'):'new';brushing=true;brushPoints=[];updateRoiList();addBrushPoint(p);status.textContent=brushOperation==='subtract'?'Alt brush: removing from selected ROI':(brushOperation==='extend'?'Brush: extending selected ROI':'Brush: painting a new ROI');}\n",
    "function addBrushPoint(p){if(!brushing||!pointInsideSlide(p))return;const last=brushPoints[brushPoints.length-1];if(!last||Math.hypot(p.x-last.x,p.y-last.y)>=Math.max(2,brushRadius*.25)){brushPoints.push({x:p.x,y:p.y});draw();}}\n",
    "function extendSelectedRoiWithBrush(ring){const roi=rois[selectedRoi];if(!roi||!isDrawable(roi))return false;if(!roi.add_rings)roi.add_rings=[];roi.add_rings.push(ring);roi.brush_edited=true;refreshRoiGeometry(roi);buildRoiList();updateButtons();scheduleViewerStateSync('roi_brush_edited',{id:roi.id||null,operation:'extend'});status.textContent='Extended '+(roi.name||roi.id||'selected ROI')+' with brush.';return true;}\n",
    "function subtractSelectedRoiWithBrush(ring){const roi=rois[selectedRoi];if(!roi||!isDrawable(roi))return false;if(!roi.subtract_rings)roi.subtract_rings=[];roi.subtract_rings.push(ring);roi.brush_edited=true;refreshRoiGeometry(roi);buildRoiList();updateButtons();scheduleViewerStateSync('roi_brush_edited',{id:roi.id||null,operation:'subtract'});status.textContent='Removed brushed area from '+(roi.name||roi.id||'selected ROI')+'.';return true;}\n",
    "function finishBrush(){if(!brushing)return;brushing=false;const ring=brushRingFromPoints(brushPoints,brushRadius);const op=brushOperation;brushPoints=[];brushOperation='new';if(ring.length>=4){if(op==='extend'&&extendSelectedRoiWithBrush(ring)){draw();return;}if(op==='subtract'&&subtractSelectedRoiWithBrush(ring)){draw();return;}addRoiFromRing(ring,'brush','Painted ROI');status.textContent='Painted ROI added. Select it and use Brush to extend, or hold Alt with Brush to remove areas.';}draw();}\n",
    "function slideUnitScale(){const a=slideToCanvas({x:0,y:0}),b=slideToCanvas({x:1,y:0});return Math.max(.0001,Math.hypot(b.x-a.x,b.y-a.y));}\n",
    "function drawBrushPreview(){if(mode!=='brush')return;const px=slideUnitScale(),remove=brushOperation==='subtract';ctx.save();ctx.strokeStyle=remove?'rgba(248,113,113,.9)':'rgba(250,204,21,.85)';ctx.fillStyle=remove?'rgba(248,113,113,.2)':'rgba(250,204,21,.18)';ctx.lineWidth=Math.max(1,brushRadius*2*px);ctx.lineCap='round';ctx.lineJoin='round';if(brushPoints.length){ctx.beginPath();brushPoints.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.stroke();}if(lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.beginPath();ctx.arc(q.x,q.y,Math.max(2,brushRadius*px),0,Math.PI*2);ctx.fill();ctx.strokeStyle=remove?'#ef4444':'#facc15';ctx.lineWidth=1;ctx.stroke();}ctx.restore();}\n",
    "function canvasPoint(clientX,clientY){const rect=canvas.getBoundingClientRect();return {x:clientX-rect.left,y:clientY-rect.top};}\n",
    "function ringClosed(ring){if(!ring||ring.length<2)return false;const f=ring[0],l=ring[ring.length-1];return f.x===l.x&&f.y===l.y;}\n",
    "function findVertexAt(clientX,clientY){const c=canvasPoint(clientX,clientY),order=selectedRoi>=0?[selectedRoi]:rois.map((_,i)=>i);for(const ri of order){const roi=rois[ri];if(!isDrawable(roi))continue;for(let r=0;r<roi.rings.length;r++){const ring=roi.rings[r],limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=slideToCanvas(ring[j]);if(Math.hypot(q.x-c.x,q.y-c.y)<=9)return {roi:ri,ring:r,point:j};}}}return null;}\n",
    "function moveActiveVertex(p){if(!activeVertex||!pointInsideSlide(p))return;const roi=rois[activeVertex.roi],ring=roi&&roi.rings?roi.rings[activeVertex.ring]:null;if(!ring)return;const closed=ringClosed(ring),pt={x:Math.round(p.x),y:Math.round(p.y)};ring[activeVertex.point]=pt;if(activeVertex.point===0&&closed)ring[ring.length-1]={x:pt.x,y:pt.y};selectedRoi=activeVertex.roi;refreshRoiGeometry(roi);scheduleViewerStateSync('roi_edited',{id:roi.id||null});draw();}\n",
    "function drawEditHandles(){if(mode!=='edit'||selectedRoi<0||!isDrawable(rois[selectedRoi]))return;const roi=rois[selectedRoi];ctx.save();roi.rings.forEach((ring,r)=>{const limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const q=slideToCanvas(ring[j]),active=activeVertex&&activeVertex.roi===selectedRoi&&activeVertex.ring===r&&activeVertex.point===j;ctx.beginPath();ctx.arc(q.x,q.y,active?6:4,0,Math.PI*2);ctx.fillStyle=active?'#facc15':'#ffffff';ctx.strokeStyle='#111';ctx.lineWidth=2;ctx.fill();ctx.stroke();}});ctx.restore();}\n",
    "function segmentDistance(c,a,b){const dx=b.x-a.x,dy=b.y-a.y,len2=dx*dx+dy*dy;if(!len2)return Math.hypot(c.x-a.x,c.y-a.y);let t=((c.x-a.x)*dx+(c.y-a.y)*dy)/len2;t=clamp(t,0,1);return Math.hypot(c.x-(a.x+t*dx),c.y-(a.y+t*dy));}\n",
    "function insertVertexAt(p,clientX,clientY){if(selectedRoi<0||!isDrawable(rois[selectedRoi])||!pointInsideSlide(p))return false;const c=canvasPoint(clientX,clientY),roi=rois[selectedRoi];let best=null;roi.rings.forEach((ring,r)=>{const limit=ringClosed(ring)?ring.length-1:ring.length;for(let j=0;j<limit;j++){const a=slideToCanvas(ring[j]),b=slideToCanvas(ring[(j+1)%ring.length]),d=segmentDistance(c,a,b);if(!best||d<best.d)best={d:d,ring:r,after:j};}});if(!best||best.d>14)return false;const ring=roi.rings[best.ring];ring.splice(best.after+1,0,{x:Math.round(p.x),y:Math.round(p.y)});activeVertex={roi:selectedRoi,ring:best.ring,point:best.after+1};refreshRoiGeometry(roi);buildRoiList();draw();return true;}\n",
    "function deleteSelectedVertex(){if(!activeVertex)return false;const roi=rois[activeVertex.roi],ring=roi&&roi.rings?roi.rings[activeVertex.ring]:null;if(!ring)return false;const closed=ringClosed(ring),limit=closed?ring.length-1:ring.length;if(limit<=3){status.textContent='A polygon ROI needs at least 3 vertices.';return false;}ring.splice(activeVertex.point,1);if(activeVertex.point===0&&closed)ring[ring.length-1]={x:ring[0].x,y:ring[0].y};activeVertex=null;refreshRoiGeometry(roi);buildRoiList();scheduleViewerStateSync('roi_edited',{id:roi.id||null});draw();return true;}\n",
    "function deleteSelectedRoi(){if(selectedRoi<0||!rois[selectedRoi]){status.textContent='Select an ROI before deleting.';return;}const removed=rois.splice(selectedRoi,1)[0];selectedRoi=Math.min(selectedRoi,rois.length-1);activeVertex=null;buildRoiList();updateButtons();draw();scheduleViewerStateSync('roi_deleted',{id:removed.id||null});status.textContent='Deleted '+(removed.name||removed.id||'selected ROI')+'.';}\n",
    "function ringCoordinates(r){const ring=(r||[]).map(p=>[Math.round(p.x),Math.round(p.y)]);if(ring.length){const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);}return ring;}\n",
    "function roiCompositeGeometry(roi){const groups=roiDrawGroups(roi).map(g=>g.rings.concat(g.holes).map(ringCoordinates)).filter(g=>g.length&&g[0].length>=4);if(!groups.length)return null;if(groups.length===1)return {type:'Polygon',coordinates:groups[0]};return {type:'MultiPolygon',coordinates:groups};}\n",
    "function roiFeature(roi,i){let geometry=null;if(isDrawable(roi)&&(roi.edited||roi.drawn||roi.brushed||roi.brush_edited||roi.add_rings||roi.subtract_rings||!roi.coordinates)){geometry=roiCompositeGeometry(roi);}else if(roi.coordinates){geometry={type:geometryType(roi),coordinates:roi.coordinates};}if(!geometry)return null;const name=roiLabelText(roi,i),cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:name,label:name,classification:{name:cls},class:cls,source:roi.source||null},geometry:geometry};}\n",
    "function geojsonText(){if(brushing)finishBrush();if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature).filter(Boolean)},null,2);}\n",
    "function applySelectedRoiClass(){if(selectedRoi<0||!rois[selectedRoi]){status.textContent='Select an ROI before applying a name or class.';return;}activeRoiClass=currentRoiClass();activeRoiName=annotationLabelValue();const roi=rois[selectedRoi];roi.class=activeRoiClass;if(activeRoiName){roi.name=activeRoiName;roi.label=activeRoiName;}roi.edited=true;buildRoiList();scheduleViewerStateSync('roi_updated',{id:roi.id||null,class:roi.class||null,name:roi.name||null});draw();}\n",
    "function bindRoiClassControls(){const select=el('roiClassSelect'),input=el('roiLabelInput'),brush=el('brushSize');if(select){activeRoiClass=select.value;select.onchange=e=>{activeRoiClass=e.target.value;if(selectedRoi>=0)applySelectedRoiClass();};}if(input){activeRoiName=input.value;input.oninput=e=>{activeRoiName=e.target.value;};input.onkeydown=e=>{if(e.key==='Enter'&&selectedRoi>=0){e.preventDefault();applySelectedRoiClass();}};}if(brush){brush.oninput=()=>{updateBrushControls();draw();};updateBrushControls();}const apply=el('applyRoiClass');if(apply)apply.onclick=applySelectedRoiClass;const del=el('deleteRoi');if(del)del.onclick=deleteSelectedRoi;}\n",
    "function updateButtons(){const has=rois.length>0,drawable=hasDrawable();['roiToggle','labelsToggle','prevRoi','nextRoi'].forEach(id=>el(id).disabled=!drawable);el('layersToggle').disabled=!has;el('finishRoi').disabled=draft.length<3;el('undoPoint').disabled=draft.length<1&&!brushing;el('saveGeojson').disabled=!has&&draft.length<3&&brushPoints.length<2;const apply=el('applyRoiClass'),del=el('deleteRoi');if(apply)apply.disabled=selectedRoi<0;if(del)del.disabled=selectedRoi<0;el('roiToggle').classList.toggle('active',showRois&&drawable);el('labelsToggle').classList.toggle('active',showLabels&&drawable);el('crosshairToggle').classList.toggle('active',showCrosshair);const panel=el('roiPanel');el('layersToggle').classList.toggle('active',!!(panel&&panel.classList.contains('open')));}\n"
  )
}

wsi_viewer_measure_js <- function() {
  paste0(
    "function measurePixelSize(){const m=cfg.mpp||{};const x=Number(m.x),y=Number(m.y);return Number.isFinite(x)&&Number.isFinite(y)&&x>0&&y>0?{x:x,y:y}:null;}\n",
    "function measurementRecord(p1,p2){const dx=p2.x-p1.x,dy=p2.y-p1.y,px=measurePixelSize(),distancePx=Math.hypot(dx,dy),distanceUm=px?Math.hypot(dx*px.x,dy*px.y):NaN;return {id:'measure_'+(measures.length+1),start:{x:p1.x,y:p1.y},end:{x:p2.x,y:p2.y},distance_px:distancePx,distance_um:distanceUm};}\n",
    "function formatMeasure(m){let text=fmt(m.distance_px,1)+' px';if(Number.isFinite(m.distance_um))text+=' | '+fmt(m.distance_um,1)+' um';return text;}\n",
    "function drawMeasureLine(m,preview=false){const a=slideToCanvas(m.start),b=slideToCanvas(m.end),mx=(a.x+b.x)/2,my=(a.y+b.y)/2,text=formatMeasure(m);ctx.save();ctx.strokeStyle=preview?'#facc15':'#38bdf8';ctx.fillStyle=preview?'#facc15':'#38bdf8';ctx.lineWidth=2;if(preview)ctx.setLineDash([6,4]);ctx.beginPath();ctx.moveTo(a.x,a.y);ctx.lineTo(b.x,b.y);ctx.stroke();ctx.setLineDash([]);[a,b].forEach(p=>{ctx.beginPath();ctx.arc(p.x,p.y,4,0,Math.PI*2);ctx.fill();ctx.strokeStyle='#111';ctx.stroke();ctx.strokeStyle=preview?'#facc15':'#38bdf8';});const w=ctx.measureText(text).width+8,x=clamp(mx-w/2,4,innerWidth-w-4),y=clamp(my-22,4,innerHeight-22);ctx.fillStyle='rgba(0,0,0,.76)';ctx.fillRect(x,y,w,18);ctx.fillStyle=preview?'#facc15':'#e0f2fe';ctx.fillText(text,x+4,y+3);ctx.restore();}\n",
    "function drawMeasurements(){measures.forEach(m=>drawMeasureLine(m,false));if(mode==='measure'&&measureStart&&lastPointer&&pointInsideSlide(lastPointer))drawMeasureLine(measurementRecord(measureStart,lastPointer),true);}\n",
    "function updateMeasureList(){const summary=el('measureSummary'),list=el('measureList'),clear=el('clearMeasures');if(summary)summary.textContent=measures.length?(measures.length+' distance measurement'+(measures.length===1?'':'s')):'No measurements yet.';if(list){list.innerHTML='';measures.forEach((m,i)=>{const b=document.createElement('button');b.type='button';b.className='measureItem';b.innerHTML='Distance '+(i+1)+'<br><code>'+formatMeasure(m)+'</code>';b.onclick=()=>{measureStart=null;status.textContent='Measurement '+(i+1)+': '+formatMeasure(m);draw();};list.appendChild(b);});}if(clear)clear.disabled=measures.length===0;}\n",
    "function addMeasurePoint(p){if(!pointInsideSlide(p))return;if(!measureStart){measureStart={x:p.x,y:p.y};status.textContent='Measurement start x '+Math.round(p.x)+' y '+Math.round(p.y)+' | click endpoint';draw();return;}const rec=measurementRecord(measureStart,p);measures.push(rec);measureStart=null;updateMeasureList();updateButtons();scheduleViewerStateSync('measurement_added',{id:rec.id});status.textContent='Measured '+formatMeasure(rec);draw();}\n",
    "function clearMeasurements(){measures=[];measureStart=null;updateMeasureList();updateButtons();scheduleViewerStateSync('measurements_cleared',{});draw();}\n",
    "function measureStatus(){if(mode==='measure'){if(measureStart&&lastPointer&&pointInsideSlide(lastPointer))return ' | measuring '+formatMeasure(measurementRecord(measureStart,lastPointer));return ' | click two points to measure';}return measures.length?(' | measures '+measures.length):'';}\n",
    "function bindMeasureControls(){const tool=el('toolMeasure'),clear=el('clearMeasures');if(tool)tool.onclick=()=>setMode('measure');if(clear)clear.onclick=clearMeasurements;updateMeasureList();}\n"
  )
}

wsi_viewer_segmentation_js <- function() {
  paste0(
    "function segmentationStatus(msg){const box=el('segmentationSummary');if(box)box.textContent=msg||'';if(msg)status.textContent=msg;}\n",
    "function selectedRoiFeatureText(){if(selectedRoi<0||!rois[selectedRoi])return null;const feature=roiFeature(rois[selectedRoi],selectedRoi);if(!feature)return null;return JSON.stringify({type:'FeatureCollection',features:[feature]},null,2);}\n",
    "function exportSelectedRoiForSegmentation(){const text=selectedRoiFeatureText();if(!text){segmentationStatus('Select an ROI before exporting a StarDist region.');return;}const roi=rois[selectedRoi],name=(roi.id||roi.name||'selected_roi').replace(/[^A-Za-z0-9_.-]+/g,'_');downloadText(text,name+'_stardist_roi.geojson');segmentationStatus('Exported selected ROI GeoJSON. Use stardist_segment_roi() or your StarDist pipeline, then load result GeoJSON.');}\n",
    "function segmentationOffset(){const local=!!(el('segLocalCoords')&&el('segLocalCoords').checked),base=(local&&selectedRoi>=0&&rois[selectedRoi])?roiBounds(rois[selectedRoi]):null;return base?{x:base.xmin,y:base.ymin}:{x:0,y:0};}\n",
    "function segmentationCellRadius(){const input=el('segCellRadius'),label=el('segCellRadiusValue');const value=Math.max(1,Number(input&&input.value?input.value:8));if(label)label.textContent=Math.round(value)+' px';return value;}\n",
    "function coordPoint(coord,offset){if(!coord||coord.length<2)return null;const x=Number(coord[0]),y=Number(coord[1]);if(!Number.isFinite(x)||!Number.isFinite(y))return null;return {x:x+offset.x,y:y+offset.y};}\n",
    "function ringFromCoords(coords,offset){const ring=(coords||[]).map(c=>coordPoint(c,offset)).filter(Boolean);return closedRing(ring);}\n",
    "function ringsFromGeojsonGeometry(geometry,offset){if(!geometry)return [];const type=String(geometry.type||'').toLowerCase(),coords=geometry.coordinates||[];if(type==='polygon')return coords.map(r=>ringFromCoords(r,offset)).filter(r=>r.length>=4);if(type==='multipolygon'){let rings=[];coords.forEach(poly=>{rings=rings.concat((poly||[]).map(r=>ringFromCoords(r,offset)).filter(r=>r.length>=4));});return rings;}return [];}\n",
    "function featureClassName(properties){const cls=properties&&properties.classification;if(cls&&typeof cls==='object'&&cls.name)return cls.name;if(properties&&properties.class)return properties.class;if(typeof cls==='string')return cls;return 'cell';}\n",
    "function addSegmentationGeojson(obj,options={}){const features=geojsonFeatures(obj);if(!features.length){segmentationStatus('No GeoJSON features found in segmentation file.');return;}const useLocal=Object.prototype.hasOwnProperty.call(options,'local')?!!options.local:!!(el('segLocalCoords')&&el('segLocalCoords').checked),offset=useLocal?segmentationOffset():{x:0,y:0};let added=0;features.forEach((feature,i)=>{const rings=ringsFromGeojsonGeometry(feature.geometry||{},offset);if(!rings.length)return;const props=feature.properties||{},name=props.name||props.label||feature.id||('StarDist cell '+(i+1)),cls=featureClassName(props),colour='#38bdf8';const roi={id:String(feature.id||('stardist_cell_'+Date.now()+'_'+i)),name:String(name),label:String(name),class:String(cls),geometry_type:'Polygon',source:'stardist',drawable:true,point_count:pointCount({rings:rings}),area:polygonArea(rings),bbox:null,colour:colour,fill:'rgba(56,189,248,0.12)',rings:rings,edited:true};refreshRoiGeometry(roi);rois.push(roi);added++;});if(!added){segmentationStatus('Segmentation GeoJSON did not contain polygon or multipolygon cells.');return;}selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();scheduleViewerStateSync('segmentation_added',{added:added,type:'geojson'});segmentationStatus('Loaded '+added+' StarDist cell polygon'+(added===1?'':'s')+'.');}\n",
    "function parseDelimitedLine(line,delimiter){const out=[];let cur='',quoted=false;for(let i=0;i<line.length;i++){const ch=line[i];if(ch==='\"'){if(quoted&&line[i+1]==='\"'){cur+='\"';i++;}else quoted=!quoted;}else if(ch===delimiter&&!quoted){out.push(cur);cur='';}else cur+=ch;}out.push(cur);return out.map(x=>x.trim());}\n",
    "function parseDelimitedTable(text){const lines=String(text||'').split(/\\r?\\n/).filter(line=>line.trim().length);if(!lines.length)return {headers:[],rows:[]};const delimiter=lines[0].indexOf('\\t')>=0?'\\t':',';const headers=parseDelimitedLine(lines[0],delimiter);const rows=lines.slice(1).map(line=>parseDelimitedLine(line,delimiter));return {headers:headers,rows:rows};}\n",
    "function headerIndex(headers,names){const lower=headers.map(h=>String(h).toLowerCase().trim());for(const name of names){const idx=lower.indexOf(name);if(idx>=0)return idx;}return -1;}\n",
    "function cellRing(center,radius,steps=18){const pts=[];for(let i=0;i<steps;i++){const a=i/steps*Math.PI*2;pts.push({x:center.x+Math.cos(a)*radius,y:center.y+Math.sin(a)*radius});}return closedRing(pts);}\n",
    "function addSegmentationCentroidTable(text,fileName){const table=parseDelimitedTable(text),headers=table.headers,rows=table.rows,xi=headerIndex(headers,['x','centroid_x','center_x','centre_x']),yi=headerIndex(headers,['y','centroid_y','center_y','centre_y']);if(xi<0||yi<0){segmentationStatus('CSV/TSV must contain x/y or centroid_x/centroid_y columns.');return;}const idIdx=headerIndex(headers,['cell_id','id','object_id','label']),offset=segmentationOffset(),radius=segmentationCellRadius();let added=0;rows.forEach((row,i)=>{const x=Number(row[xi]),y=Number(row[yi]);if(!Number.isFinite(x)||!Number.isFinite(y))return;const p={x:x+offset.x,y:y+offset.y},ring=cellRing(p,radius),id=String((idIdx>=0&&row[idIdx])?row[idIdx]:('stardist_cell_'+(i+1))),colour='#38bdf8',roi={id:id,name:id,label:id,class:'cell',geometry_type:'Polygon',source:'stardist',drawable:true,point_count:ring.length-1,area:polygonArea([ring]),bbox:boundsFromRing(ring),colour:colour,fill:'rgba(56,189,248,0.12)',rings:[ring],edited:true,centroid:{x:p.x,y:p.y},source_file:fileName||''};refreshRoiGeometry(roi);rois.push(roi);added++;});if(!added){segmentationStatus('No numeric StarDist centroids were found in the table.');return;}selectedRoi=rois.length-1;showRois=true;buildRoiList();updateButtons();draw();scheduleViewerStateSync('segmentation_added',{added:added,type:'centroids',file:fileName||null});segmentationStatus('Loaded '+added+' StarDist centroid cell marker'+(added===1?'':'s')+'.');}\n",
    "async function startSegmentationForSelectedRoi(){const url=cfg.segmentation_run_url||'';if(!url){segmentationStatus('Start StarDist is not configured for this viewer. Create the viewer with segmentation_run_url, or export the ROI and run wsitools stardist-roi.');return;}const text=selectedRoiFeatureText();if(!text){segmentationStatus('Select an ROI before starting StarDist.');return;}const button=el('startSegmentation');if(button)button.disabled=true;segmentationStatus('Running StarDist on selected ROI...');try{const response=await fetch(url,{method:'POST',headers:{'Content-Type':'text/plain;charset=UTF-8'},body:text});const body=await response.text();if(!response.ok){let detail=body;try{const err=JSON.parse(body);detail=err.error||err.message||body;}catch(e){}throw new Error(detail||('HTTP '+response.status));}const result=JSON.parse(body);const geojson=result.geojson||result;if(result.message)segmentationStatus(result.message);addSegmentationGeojson(geojson,{local:false});}catch(e){segmentationStatus('StarDist run failed: '+e.message);}finally{if(button)button.disabled=false;}}\n",
    "function clearSegmentationOverlays(){const before=rois.length;for(let i=rois.length-1;i>=0;i--){if(rois[i].source==='stardist')rois.splice(i,1);}if(selectedRoi>=rois.length)selectedRoi=rois.length-1;buildRoiList();updateButtons();draw();scheduleViewerStateSync('segmentation_cleared',{});segmentationStatus('Removed '+(before-rois.length)+' StarDist overlay'+(before-rois.length===1?'':'s')+'.');}\n",
    "function bindSegmentationControls(){const exportButton=el('exportSelectedRoi'),startButton=el('startSegmentation'),loadButton=el('loadSegmentation'),loadCsvButton=el('loadSegmentationCsv'),clearButton=el('clearSegmentation'),file=el('segmentationFile'),tableFile=el('segmentationTableFile'),radius=el('segCellRadius');if(exportButton)exportButton.onclick=exportSelectedRoiForSegmentation;if(startButton)startButton.onclick=startSegmentationForSelectedRoi;if(loadButton&&file)loadButton.onclick=()=>{file.value='';file.click();};if(loadCsvButton&&tableFile)loadCsvButton.onclick=()=>{tableFile.value='';tableFile.click();};if(clearButton)clearButton.onclick=clearSegmentationOverlays;if(radius){radius.oninput=()=>segmentationCellRadius();segmentationCellRadius();}if(file){file.onchange=()=>{const picked=file.files&&file.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addSegmentationGeojson(JSON.parse(reader.result));}catch(e){segmentationStatus('Could not read StarDist GeoJSON: '+e.message);}};reader.readAsText(picked);};}if(tableFile){tableFile.onchange=()=>{const picked=tableFile.files&&tableFile.files[0];if(!picked)return;const reader=new FileReader();reader.onload=()=>{try{addSegmentationCentroidTable(reader.result,picked.name);}catch(e){segmentationStatus('Could not read StarDist centroid table: '+e.message);}};reader.readAsText(picked);};}segmentationStatus('');}\n"
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
    "const el=id=>document.getElementById(id), rois=cfg.rois||[];\n",
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,lastX=0,lastY=0,lastPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0,activeRoiClass='tumour',activeRoiName='',measureStart=null,measures=[],brushing=false,brushPoints=[],brushRadius=80,brushOperation='new',draggingVertex=null,activeVertex=null;\n",
    "function clamp(v,min,max){return Math.max(min,Math.min(max,v));}\n",
    wsi_viewer_menu_js(),
    wsi_viewer_stain_js(),
    wsi_viewer_sync_js(),
    "function setMode(m){mode=m;if(m!=='edit'){draggingVertex=null;activeVertex=null;}canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');canvas.classList.toggle('brushing',m==='brush');canvas.classList.toggle('editing',m==='edit');canvas.classList.toggle('measuring',m==='measure');el('toolPan').classList.toggle('active',m==='pan');el('toolSelect').classList.toggle('active',m==='select');el('toolDraw').classList.toggle('active',m==='draw');el('toolBrush').classList.toggle('active',m==='brush');el('toolEdit').classList.toggle('active',m==='edit');el('toolMeasure').classList.toggle('active',m==='measure');updateButtons();if(canvas.width&&((typeof image==='undefined')||image.complete))draw();}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);fitView();}\n",
    "function fitView(){if(!image.naturalWidth)return;minScale=Math.min(innerWidth/image.naturalWidth,innerHeight/image.naturalHeight);scale=minScale;offsetX=(innerWidth-image.naturalWidth*scale)/2;offsetY=(innerHeight-image.naturalHeight*scale)/2;draw();}\n",
    "function oneToOne(){if(!image.naturalWidth)return;scale=1;offsetX=(innerWidth-image.naturalWidth)/2;offsetY=(innerHeight-image.naturalHeight)/2;draw();}\n",
    "function slideToImage(p){return {x:p.x/cfg.slide_width*image.naturalWidth,y:p.y/cfg.slide_height*image.naturalHeight};}\n",
    "function imageToSlide(p){return {x:p.x/image.naturalWidth*cfg.slide_width,y:p.y/image.naturalHeight*cfg.slide_height};}\n",
    "function slideToCanvas(p){const q=slideToImage(p);return {x:offsetX+q.x*scale,y:offsetY+q.y*scale};}\n",
    "function pointerToSlide(evt){const rect=canvas.getBoundingClientRect();const px=evt.clientX-rect.left;const py=evt.clientY-rect.top;return imageToSlide({x:(px-offsetX)/scale,y:(py-offsetY)/scale});}\n",
    "function pointInsideSlide(p){return p.x>=0&&p.y>=0&&p.x<=cfg.slide_width&&p.y<=cfg.slide_height;}\n",
    "function roiBounds(roi){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointInRing(p,ring){let inside=false;for(let i=0,j=ring.length-1;i<ring.length;j=i++){const xi=ring[i].x,yi=ring[i].y,xj=ring[j].x,yj=ring[j].y;const hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!rois.length)return;selectedRoi=(i+rois.length)%rois.length;const b=roiBounds(rois[selectedRoi]);const p0=slideToImage({x:b.xmin,y:b.ymin}),p1=slideToImage({x:b.xmax,y:b.ymax});const pad=1.35;scale=clamp(Math.min(innerWidth/Math.max(1,(p1.x-p0.x)*pad),innerHeight/Math.max(1,(p1.y-p0.y)*pad)),minScale*0.8,40);offsetX=innerWidth/2-((p0.x+p1.x)/2)*scale;offsetY=innerHeight/2-((p0.y+p1.y)/2)*scale;updateRoiList();draw();}\n",
    "function zoomAt(factor,cx,cy){const beforeX=(cx-offsetX)/scale,beforeY=(cy-offsetY)/scale;scale=clamp(scale*factor,minScale*0.6,80);offsetX=cx-beforeX*scale;offsetY=cy-beforeY*scale;draw();}\n",
    "function draw(){ctx.clearRect(0,0,innerWidth,innerHeight);ctx.imageSmoothingEnabled=true;ctx.drawImage(image,offsetX,offsetY,image.naturalWidth*scale,image.naturalHeight*scale);applyStainToCanvas();drawRois();drawDraft();drawBrushPreview();drawEditHandles();drawMeasurements();drawCrosshair();updateStatus(lastPointer);}\n",
    "function drawRois(){if(!showRois||!rois.length||!image.naturalWidth)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{let label=null;ctx.beginPath();roi.rings.forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(!label)label=q;if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;if(showLabels&&label){const text=roi.name||roi.id;const w=ctx.measureText(text).width+8;ctx.fillStyle='rgba(0,0,0,.68)';ctx.fillRect(label.x,label.y,w,18);ctx.fillStyle=roi.colour;ctx.fillText(text,label.x+4,label.y+3);}});ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!lastPointer||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function updateStatus(p){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x';msg+=stainStatus();msg+=measureStatus();if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(p&&pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg+' | thumbnail preview, full slide not loaded into R';}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){status.textContent='Add at least 3 points before finishing an ROI.';return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;buildRoiList();updateButtons();setMode('select');draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});const cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:cls},class:cls,source:roi.source||null},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(!rois.length&&draft.length<3){status.textContent='Draw an ROI before saving GeoJSON.';return;}const text=geojsonText();const name=cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();status.textContent='Saved '+name;return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);status.textContent='Downloaded '+name;}\n",
    "function updateButtons(){const has=rois.length>0;['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>el(id).disabled=!has);el('finishRoi').disabled=draft.length<3;el('undoPoint').disabled=draft.length<1;el('saveGeojson').disabled=!has&&draft.length<3;el('roiToggle').classList.toggle('active',showRois&&has);el('labelsToggle').classList.toggle('active',showLabels&&has);el('crosshairToggle').classList.toggle('active',showCrosshair);}\n",
    wsi_viewer_geometry_js(),
    wsi_viewer_measure_js(),
    wsi_viewer_segmentation_js(),
    "function copyCoord(){if(!lastPointer||!pointInsideSlide(lastPointer))return;const text=Math.round(lastPointer.x)+','+Math.round(lastPointer.y);if(navigator.clipboard)navigator.clipboard.writeText(text);status.textContent='Copied '+text;}\n",
    "canvas.addEventListener('mousedown',e=>{lastPointer=pointerToSlide(e);if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='brush'){startBrush(lastPointer,e);return;}if(mode==='edit'){activeVertex=findVertexAt(e.clientX,e.clientY);if(activeVertex){selectedRoi=activeVertex.roi;draggingVertex=activeVertex;updateRoiList();draw();return;}selectedRoi=roiAt(lastPointer);updateRoiList();draw();return;}if(mode==='measure'){addMeasurePoint(lastPointer);return;}if(mode==='select'){selectedRoi=roiAt(lastPointer);updateRoiList();draw();return;}dragging=true;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',()=>{if(brushing)finishBrush();if(draggingVertex){draggingVertex=null;buildRoiList();}dragging=false;canvas.classList.remove('dragging');});\n",
    "window.addEventListener('mousemove',e=>{lastPointer=pointerToSlide(e);if(brushing){addBrushPoint(lastPointer);return;}if(draggingVertex){moveActiveVertex(lastPointer);return;}if(dragging){offsetX+=e.clientX-lastX;offsetY+=e.clientY-lastY;lastX=e.clientX;lastY=e.clientY;draw();}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.2:1/1.2,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}if(mode==='edit'){e.preventDefault();insertVertexAt(pointerToSlide(e),e.clientX,e.clientY);return;}zoomAt(2,e.clientX,e.clientY);});\n",
    "el('toolPan').onclick=()=>setMode('pan');el('toolSelect').onclick=()=>setMode('select');el('toolDraw').onclick=()=>setMode('draw');el('toolBrush').onclick=()=>setMode('brush');el('toolEdit').onclick=()=>setMode('edit');el('finishRoi').onclick=finishDraft;el('undoPoint').onclick=()=>{if(mode==='brush'&&brushPoints.length){brushPoints.pop();draw();}else undoDraftPoint();};el('saveGeojson').onclick=saveGeojson;el('zoomIn').onclick=()=>zoomAt(1.25,innerWidth/2,innerHeight/2);el('zoomOut').onclick=()=>zoomAt(1/1.25,innerWidth/2,innerHeight/2);el('fit').onclick=fitView;el('oneToOne').onclick=oneToOne;\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);el('layersToggle').onclick=()=>{toggleRoiPanel();updateButtons();};el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);draw();};el('crosshairToggle').onclick=()=>{showCrosshair=!showCrosshair;updateButtons();draw();};el('copyCoord').onclick=copyCoord;\n",
    "window.addEventListener('keydown',e=>{if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='b')setMode('brush');if(e.key==='e')setMode('edit');if(e.key==='m')setMode('measure');if(e.key==='Enter'&&mode==='draw')finishDraft();if((e.key==='Backspace'||e.key==='Delete')&&mode==='draw'){e.preventDefault();undoDraftPoint();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='edit'&&activeVertex){e.preventDefault();deleteSelectedVertex();}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='c')el('crosshairToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape'){measureStart=null;brushing=false;brushPoints=[];brushOperation='new';draggingVertex=null;activeVertex=null;setMode('pan');draw();}});\n",
    "bindExclusiveMenus();bindStainControls();bindRoiClassControls();bindMeasureControls();bindGeojsonImportControls();bindSegmentationControls();buildRoiList();updateButtons();syncMessage('');setMode('pan');scheduleViewerStateSync('viewer_loaded',{});\n",
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
    "<style>\n",
    wsi_viewer_styles("#101010"),
    "</style>\n",
    "</head>\n",
    "<body>\n",
    wsi_viewer_chrome(config, "Loading Deep Zoom tiles..."),
    "<script>\n",
    "const cfg = ", config_json, ";\n",
    "const canvas = document.getElementById('viewer');\n",
    "const ctx = canvas.getContext('2d');\n",
    "const status = document.getElementById('status');\n",
    "const el=id=>document.getElementById(id), rois=cfg.rois||[];\n",
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,lastX=0,lastY=0,lastPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0,activeRoiClass='tumour',activeRoiName='',measureStart=null,measures=[],brushing=false,brushPoints=[],brushRadius=80,brushOperation='new',draggingVertex=null,activeVertex=null;\n",
    "const cache = new Map();\n",
    "let renderQueued=false,loadingTiles=0;\n",
    "function clamp(v,min,max){return Math.max(min,Math.min(max,v));}\n",
    wsi_viewer_menu_js(),
    wsi_viewer_stain_js(),
    wsi_viewer_sync_js(),
    "function setMode(m){mode=m;if(m!=='edit'){draggingVertex=null;activeVertex=null;}canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');canvas.classList.toggle('brushing',m==='brush');canvas.classList.toggle('editing',m==='edit');canvas.classList.toggle('measuring',m==='measure');el('toolPan').classList.toggle('active',m==='pan');el('toolSelect').classList.toggle('active',m==='select');el('toolDraw').classList.toggle('active',m==='draw');el('toolBrush').classList.toggle('active',m==='brush');el('toolEdit').classList.toggle('active',m==='edit');el('toolMeasure').classList.toggle('active',m==='measure');updateButtons();if(canvas.width&&((typeof image==='undefined')||image.complete))draw();}\n",
    "function tileUrl(level,col,row){return cfg.tile_url_base+'/'+level+'/'+col+'_'+row+'.'+cfg.tile_format;}\n",
    "function requestDraw(){if(renderQueued)return;renderQueued=true;requestAnimationFrame(()=>{renderQueued=false;draw();});}\n",
    "function loadTile(level,col,row){const key=level+'/'+col+'/'+row;if(cache.has(key))return cache.get(key);const img=new Image();const rec={img:img,loaded:false,failed:false};img.onload=()=>{rec.loaded=true;requestDraw();};img.onerror=()=>{rec.failed=true;};img.src=tileUrl(level,col,row);cache.set(key,rec);return rec;}\n",
    "function drawBleed(img,sx,sy,sw,sh,dx,dy,dw,dh){const x=Math.floor(dx),y=Math.floor(dy),w=Math.ceil(dx+dw)-x+1,h=Math.ceil(dy+dh)-y+1;ctx.drawImage(img,sx,sy,sw,sh,x,y,w,h);}\n",
    "function drawAncestorTile(level,col,row,tileLevelW,tileLevelH,dx,dy,dw,dh){for(let a=level-1;a>=0;a--){const factor=Math.pow(2,level-a);const ax=(col*cfg.tile_size)/factor,ay=(row*cfg.tile_size)/factor;const ac=Math.floor(ax/cfg.tile_size),ar=Math.floor(ay/cfg.tile_size);const key=a+'/'+ac+'/'+ar;const rec=cache.get(key)||loadTile(a,ac,ar);if(rec.loaded){const sx=ax-ac*cfg.tile_size,sy=ay-ar*cfg.tile_size,sw=tileLevelW/factor,sh=tileLevelH/factor;ctx.imageSmoothingEnabled=true;drawBleed(rec.img,sx,sy,sw,sh,dx,dy,dw,dh);ctx.imageSmoothingEnabled=false;return true;}}return false;}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);fitView();}\n",
    "function fitView(){minScale=Math.min(innerWidth/cfg.slide_width,innerHeight/cfg.slide_height);scale=minScale;offsetX=(innerWidth-cfg.slide_width*scale)/2;offsetY=(innerHeight-cfg.slide_height*scale)/2;draw();}\n",
    "function oneToOne(){scale=1;offsetX=(innerWidth-cfg.slide_width)/2;offsetY=(innerHeight-cfg.slide_height)/2;draw();}\n",
    "function currentLevel(){return clamp(Math.ceil(cfg.max_level+Math.log2(scale)),0,cfg.max_level);}\n",
    "function draw(){ctx.clearRect(0,0,innerWidth,innerHeight);ctx.fillStyle='#101010';ctx.fillRect(0,0,innerWidth,innerHeight);const level=currentLevel();const down=Math.pow(2,cfg.max_level-level);const levelW=Math.ceil(cfg.slide_width/down);const levelH=Math.ceil(cfg.slide_height/down);const tileSlide=cfg.tile_size*down;const left=Math.max(0,(-offsetX)/scale);const top=Math.max(0,(-offsetY)/scale);const right=Math.min(cfg.slide_width,(innerWidth-offsetX)/scale);const bottom=Math.min(cfg.slide_height,(innerHeight-offsetY)/scale);const c0=clamp(Math.floor(left/tileSlide),0,Math.ceil(levelW/cfg.tile_size)-1);const c1=clamp(Math.floor(right/tileSlide),0,Math.ceil(levelW/cfg.tile_size)-1);const r0=clamp(Math.floor(top/tileSlide),0,Math.ceil(levelH/cfg.tile_size)-1);const r1=clamp(Math.floor(bottom/tileSlide),0,Math.ceil(levelH/cfg.tile_size)-1);ctx.imageSmoothingEnabled=false;loadingTiles=0;for(let row=r0;row<=r1;row++){for(let col=c0;col<=c1;col++){const rec=loadTile(level,col,row);const tileLevelW=Math.min(cfg.tile_size,levelW-col*cfg.tile_size);const tileLevelH=Math.min(cfg.tile_size,levelH-row*cfg.tile_size);const dx=offsetX+col*cfg.tile_size*down*scale;const dy=offsetY+row*cfg.tile_size*down*scale;const dw=tileLevelW*down*scale;const dh=tileLevelH*down*scale;if(rec.loaded){drawBleed(rec.img,0,0,tileLevelW,tileLevelH,dx,dy,dw,dh);}else{loadingTiles++;if(!drawAncestorTile(level,col,row,tileLevelW,tileLevelH,dx,dy,dw,dh)){ctx.fillStyle='#1f1f1f';ctx.fillRect(Math.floor(dx),Math.floor(dy),Math.ceil(dw)+1,Math.ceil(dh)+1);}}}}applyStainToCanvas();drawRois();drawDraft();drawBrushPreview();drawEditHandles();drawMeasurements();drawCrosshair();updateStatus(lastPointer,level);}\n",
    "function slideToCanvas(p){return {x:offsetX+p.x*scale,y:offsetY+p.y*scale};}\n",
    "function pointInsideSlide(p){return p&&p.x>=0&&p.y>=0&&p.x<=cfg.slide_width&&p.y<=cfg.slide_height;}\n",
    "function roiBounds(roi){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}\n",
    "function pointInRing(p,ring){let inside=false;for(let i=0,j=ring.length-1;i<ring.length;j=i++){const xi=ring[i].x,yi=ring[i].y,xj=ring[j].x,yj=ring[j].y;const hit=((yi>p.y)!=(yj>p.y))&&(p.x<(xj-xi)*(p.y-yi)/(yj-yi)+xi);if(hit)inside=!inside;}return inside;}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!rois.length)return;selectedRoi=(i+rois.length)%rois.length;const b=roiBounds(rois[selectedRoi]);const pad=1.35;scale=clamp(Math.min(innerWidth/Math.max(1,(b.xmax-b.xmin)*pad),innerHeight/Math.max(1,(b.ymax-b.ymin)*pad)),minScale*0.8,4);offsetX=innerWidth/2-((b.xmin+b.xmax)/2)*scale;offsetY=innerHeight/2-((b.ymin+b.ymax)/2)*scale;updateRoiList();draw();}\n",
    "function drawRois(){if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{let label=null;ctx.beginPath();roi.rings.forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(!label)label=q;if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;if(showLabels&&label){const text=roi.name||roi.id;const w=ctx.measureText(text).width+8;ctx.fillStyle='rgba(0,0,0,.68)';ctx.fillRect(label.x,label.y,w,18);ctx.fillStyle=roi.colour;ctx.fillText(text,label.x+4,label.y+3);}});ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function zoomAt(factor,cx,cy){const beforeX=(cx-offsetX)/scale,beforeY=(cy-offsetY)/scale;scale=clamp(scale*factor,minScale*0.7,4);offsetX=cx-beforeX*scale;offsetY=cy-beforeY*scale;draw();}\n",
    "function pointerToSlide(evt){const rect=canvas.getBoundingClientRect();const px=evt.clientX-rect.left,py=evt.clientY-rect.top;return {x:(px-offsetX)/scale,y:(py-offsetY)/scale};}\n",
    "function updateStatus(p,level){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x | Deep Zoom level '+level+'/'+cfg.max_level;msg+=stainStatus();msg+=measureStatus();if(loadingTiles)msg+=' | loading '+loadingTiles+' tile'+(loadingTiles===1?'':'s');if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg+' | full-resolution tiled viewer, full slide not loaded into R';}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){status.textContent='Add at least 3 points before finishing an ROI.';return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;buildRoiList();updateButtons();setMode('select');draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});const cls=roi.class||'annotation';return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:cls},class:cls,source:roi.source||null},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(!rois.length&&draft.length<3){status.textContent='Draw an ROI before saving GeoJSON.';return;}const text=geojsonText();const name=cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();status.textContent='Saved '+name;return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);status.textContent='Downloaded '+name;}\n",
    "function updateButtons(){const has=rois.length>0;['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>el(id).disabled=!has);el('finishRoi').disabled=draft.length<3;el('undoPoint').disabled=draft.length<1;el('saveGeojson').disabled=!has&&draft.length<3;el('roiToggle').classList.toggle('active',showRois&&has);el('labelsToggle').classList.toggle('active',showLabels&&has);el('crosshairToggle').classList.toggle('active',showCrosshair);}\n",
    wsi_viewer_geometry_js(),
    wsi_viewer_measure_js(),
    wsi_viewer_segmentation_js(),
    "function copyCoord(){if(!pointInsideSlide(lastPointer))return;const text=Math.round(lastPointer.x)+','+Math.round(lastPointer.y);if(navigator.clipboard)navigator.clipboard.writeText(text);status.textContent='Copied '+text;}\n",
    "canvas.addEventListener('mousedown',e=>{lastPointer=pointerToSlide(e);if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='brush'){startBrush(lastPointer,e);return;}if(mode==='edit'){activeVertex=findVertexAt(e.clientX,e.clientY);if(activeVertex){selectedRoi=activeVertex.roi;draggingVertex=activeVertex;updateRoiList();draw();return;}selectedRoi=roiAt(lastPointer);updateRoiList();draw();return;}if(mode==='measure'){addMeasurePoint(lastPointer);return;}if(mode==='select'){selectedRoi=roiAt(lastPointer);updateRoiList();draw();return;}dragging=true;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',()=>{if(brushing)finishBrush();if(draggingVertex){draggingVertex=null;buildRoiList();}dragging=false;canvas.classList.remove('dragging');});\n",
    "window.addEventListener('mousemove',e=>{lastPointer=pointerToSlide(e);if(brushing){addBrushPoint(lastPointer);return;}if(draggingVertex){moveActiveVertex(lastPointer);return;}if(dragging){offsetX+=e.clientX-lastX;offsetY+=e.clientY-lastY;lastX=e.clientX;lastY=e.clientY;draw();}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.25:1/1.25,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}if(mode==='edit'){e.preventDefault();insertVertexAt(pointerToSlide(e),e.clientX,e.clientY);return;}zoomAt(2,e.clientX,e.clientY);});\n",
    "el('toolPan').onclick=()=>setMode('pan');el('toolSelect').onclick=()=>setMode('select');el('toolDraw').onclick=()=>setMode('draw');el('toolBrush').onclick=()=>setMode('brush');el('toolEdit').onclick=()=>setMode('edit');el('finishRoi').onclick=finishDraft;el('undoPoint').onclick=()=>{if(mode==='brush'&&brushPoints.length){brushPoints.pop();draw();}else undoDraftPoint();};el('saveGeojson').onclick=saveGeojson;el('zoomIn').onclick=()=>zoomAt(1.5,innerWidth/2,innerHeight/2);el('zoomOut').onclick=()=>zoomAt(1/1.5,innerWidth/2,innerHeight/2);el('fit').onclick=fitView;el('oneToOne').onclick=oneToOne;\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);el('layersToggle').onclick=()=>{toggleRoiPanel();updateButtons();};el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);draw();};el('crosshairToggle').onclick=()=>{showCrosshair=!showCrosshair;updateButtons();draw();};el('copyCoord').onclick=copyCoord;\n",
    "window.addEventListener('keydown',e=>{if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='b')setMode('brush');if(e.key==='e')setMode('edit');if(e.key==='m')setMode('measure');if(e.key==='Enter'&&mode==='draw')finishDraft();if((e.key==='Backspace'||e.key==='Delete')&&mode==='draw'){e.preventDefault();undoDraftPoint();}if((e.key==='Backspace'||e.key==='Delete')&&mode==='edit'&&activeVertex){e.preventDefault();deleteSelectedVertex();}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='c')el('crosshairToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape'){measureStart=null;brushing=false;brushPoints=[];brushOperation='new';draggingVertex=null;activeVertex=null;setMode('pan');draw();}});\n",
    "bindExclusiveMenus();bindStainControls();bindRoiClassControls();bindMeasureControls();bindGeojsonImportControls();bindSegmentationControls();buildRoiList();updateButtons();syncMessage('');setMode('pan');scheduleViewerStateSync('viewer_loaded',{});\n",
    "window.addEventListener('resize',resize);\n",
    "resize();\n",
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
#' pan/select modes, fit and 1:1 zoom, ROI visibility and label toggles, ROI
#' opacity, ROI previous/next navigation, an annotation side window listing
#' GeoJSON geometries, browser-side GeoJSON import, crosshair display,
#' coordinate copying, polygon drawing, brush-style annotation painting
#' including selected-ROI extension and `Alt` brush removal,
#' selected-ROI vertex editing, name/class
#' reassignment, deletion, StarDist-oriented selected-ROI export and
#' segmentation GeoJSON import, distance measurement in pixels and micrometres when MPP metadata is
#' available, and browser-based GeoJSON export. Optional `stain = "ihc"` adds an RGB
#' optical-density deconvolution display with selectable stain channels,
#' colours, and gains. Use `channels` to provide multi-IHC channel definitions.
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
#' @param roi Optional ROI overlay. Supply a GeoJSON path or an object returned
#'   by [wsi_read_geojson()]. Coordinates are interpreted as level-0 slide
#'   pixel coordinates, matching QuPath-style GeoJSON exports.
#' @param roi_fill_alpha Fill transparency for ROI polygons.
#' @param stain Optional stain display mode. Use `"ihc"` for interactive
#'   stain-channel color deconvolution in the browser.
#' @param channels Optional stain channels created by [wsi_stain_channels()].
#'   RGB brightfield deconvolution supports up to three independent channels at
#'   a time.
#' @param hematoxylin,hrp RGB optical-density vectors used when
#'   `stain = "ihc"` and `channels` is not supplied.
#' @param hematoxylin_colour,hrp_colour Initial display colours for the
#'   hematoxylin and HRP/DAB channels.
#' @param hematoxylin_strength,hrp_strength Initial display gains for the
#'   hematoxylin and HRP/DAB channels.
#' @param segmentation_run_url Optional HTTP endpoint used by the viewer's
#'   `Start StarDist` button. The browser posts the selected ROI GeoJSON to this
#'   URL and expects StarDist cell GeoJSON in response. Use
#'   [wsi_stardist_server()] to create a local endpoint.
#' @param viewer_state_url Optional HTTP endpoint used to sync annotations,
#'   measurements, segmentation overlays, and display state back to R. Use
#'   [wsi_viewer_live()] or [wsi_viewer_session()] to create this endpoint.
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
                       quality = 90, rebuild = FALSE, roi = NULL,
                       roi_fill_alpha = 0.15, stain = c("none", "ihc"),
                       channels = NULL,
                       hematoxylin = c(0.650, 0.704, 0.286),
                       hrp = c(0.268, 0.570, 0.776),
                       hematoxylin_colour = "#4b3f99",
                       hrp_colour = "#8b5a2b",
                       hematoxylin_strength = 1,
                       hrp_strength = 1,
                       segmentation_run_url = NULL,
                       viewer_state_url = NULL) {
  wsi_check_slide(slide)
  mode <- match.arg(mode)
  tile_format <- match.arg(tile_format)
  stain <- match.arg(stain)
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
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
  rois <- wsi_viewer_roi_features(roi, fill_alpha = roi_fill_alpha)
  mpp <- wsi_mpp(slide)
  mpp_config <- if (all(is.finite(mpp)) && all(mpp > 0)) {
    list(x = unname(mpp[["x"]]), y = unname(mpp[["y"]]))
  } else {
    NULL
  }
  annotation_filename <- paste0(tools::file_path_sans_ext(basename(output)), "_annotations.geojson")
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

  if (identical(mode, "thumbnail")) {
    config <- list(
      title = title,
      subtitle = subtitle,
      slide_width = unname(slide$dimensions[["width"]]),
      slide_height = unname(slide$dimensions[["height"]]),
      mpp = mpp_config,
      image_data_uri = wsi_viewer_thumbnail_data_uri(slide, width = width, height = height),
      annotation_filename = annotation_filename,
      segmentation_run_url = segmentation_run_url,
      viewer_state_url = viewer_state_url,
      stain = stain_config,
      rois = rois
    )
    writeLines(wsi_viewer_html(config), output, useBytes = TRUE)
  } else {
    tile_dir <- tile_dir %||% wsi_default_tile_dir(output)
    tiles <- wsi_create_deepzoom_tiles(
      slide = slide,
      tile_dir = tile_dir,
      tile_size = tile_size,
      tile_format = tile_format,
      quality = quality,
      rebuild = rebuild
    )

    config <- list(
      title = title,
      subtitle = paste0(subtitle, " | Deep Zoom tiles"),
      slide_width = unname(slide$dimensions[["width"]]),
      slide_height = unname(slide$dimensions[["height"]]),
      mpp = mpp_config,
      tile_size = as.integer(tile_size),
      tile_format = tile_format,
      tile_url_base = wsi_tile_base_url(tile_dir, output),
      dzi = basename(tiles$dzi),
      max_level = wsi_dz_max_level(slide$dimensions[["width"]], slide$dimensions[["height"]]),
      annotation_filename = annotation_filename,
      segmentation_run_url = segmentation_run_url,
      viewer_state_url = viewer_state_url,
      stain = stain_config,
      rois = rois
    )
    writeLines(wsi_tiled_viewer_html(config), output, useBytes = TRUE)
  }

  if (isTRUE(open)) {
    utils::browseURL(output)
  }
  invisible(output)
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
