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
    "#viewer.selecting,#viewer.drawing{cursor:crosshair;}\n",
    ".bar{position:fixed;left:12px;right:12px;top:12px;display:flex;gap:8px;align-items:center;pointer-events:none;}\n",
    ".panel{background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;backdrop-filter:blur(6px);pointer-events:auto;}\n",
    ".title{font-weight:600;max-width:34vw;overflow:hidden;text-overflow:ellipsis;white-space:nowrap;}\n",
    ".meta{font-size:12px;color:#d2d2d2;}\n",
    ".spacer{flex:1;}\n",
    ".tools{display:flex;gap:6px;align-items:center;flex-wrap:wrap;justify-content:flex-end;}\n",
    ".sep{width:1px;height:22px;background:rgba(255,255,255,.18);display:inline-block;}\n",
    "button{appearance:none;border:1px solid rgba(255,255,255,.24);background:#252525;color:#f2f2f2;border-radius:5px;padding:6px 9px;font-size:13px;line-height:1;}\n",
    "button:hover{background:#333;}\n",
    "button.active{background:#0f766e;border-color:#5eead4;color:#fff;}\n",
    "button:disabled{opacity:.38;cursor:not-allowed;}\n",
    "label.control{display:flex;gap:6px;align-items:center;color:#d7d7d7;font-size:12px;}\n",
    "input[type=range]{width:82px;accent-color:#5eead4;}\n",
    "input[type=color]{width:28px;height:22px;border:1px solid rgba(255,255,255,.28);border-radius:4px;background:transparent;padding:0;}\n",
    "input[type=checkbox]{accent-color:#5eead4;}\n",
    "#status{position:fixed;left:12px;bottom:12px;font-size:12px;color:#eee;background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;max-width:calc(100vw - 24px);}\n",
    "#roiPanel{position:fixed;right:12px;top:72px;width:380px;max-height:calc(100vh - 132px);overflow:auto;display:none;}\n",
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
    "@media(max-width:900px){.title{max-width:42vw}.bar{align-items:flex-start}.tools{max-width:52vw}#roiPanel{left:12px;right:12px;top:118px;width:auto;}}\n"
  )
}

wsi_viewer_stain_controls <- function(config) {
  if (!isTRUE(config$stain$enabled)) {
    return("")
  }
  paste0(
    "<span class=\"sep\"></span>",
    "<button id=\"stainToggle\" class=\"active\" title=\"Toggle H-DAB deconvolution display\">IHC</button>",
    "<label class=\"control\" title=\"Show hematoxylin channel\"><input id=\"hemaVisible\" type=\"checkbox\" checked>H</label>",
    "<label class=\"control\" title=\"Hematoxylin channel colour\"><input id=\"hemaColor\" type=\"color\" value=\"",
    wsi_html_escape(config$stain$hematoxylin_colour),
    "\"></label>",
    "<label class=\"control\" title=\"Hematoxylin display gain\">H gain <input id=\"hemaStrength\" type=\"range\" min=\"0\" max=\"3\" step=\"0.05\" value=\"",
    wsi_html_escape(config$stain$hematoxylin_strength),
    "\"></label>",
    "<label class=\"control\" title=\"Show HRP/DAB channel\"><input id=\"hrpVisible\" type=\"checkbox\" checked>HRP</label>",
    "<label class=\"control\" title=\"HRP/DAB channel colour\"><input id=\"hrpColor\" type=\"color\" value=\"",
    wsi_html_escape(config$stain$hrp_colour),
    "\"></label>",
    "<label class=\"control\" title=\"HRP/DAB display gain\">HRP gain <input id=\"hrpStrength\" type=\"range\" min=\"0\" max=\"3\" step=\"0.05\" value=\"",
    wsi_html_escape(config$stain$hrp_strength),
    "\"></label>"
  )
}

wsi_viewer_chrome <- function(config, loading_message) {
  paste0(
    "<canvas id=\"viewer\"></canvas>\n",
    "<div class=\"bar\">\n",
    "<div class=\"panel\"><div class=\"title\">", wsi_html_escape(config$title), "</div><div class=\"meta\">",
    wsi_html_escape(config$subtitle), "</div></div>\n",
    "<div class=\"spacer\"></div>\n",
    "<div class=\"panel tools\" role=\"toolbar\" aria-label=\"Viewer tools\">",
    "<button id=\"toolPan\" class=\"active\" title=\"Pan mode\">Pan</button>",
    "<button id=\"toolSelect\" title=\"Select ROI mode\">Select</button>",
    "<button id=\"toolDraw\" title=\"Draw a polygon ROI\">Draw ROI</button>",
    "<button id=\"finishRoi\" title=\"Finish current polygon\">Finish</button>",
    "<button id=\"undoPoint\" title=\"Undo last polygon point\">Undo</button>",
    "<button id=\"saveGeojson\" title=\"Save annotations as GeoJSON\">Save GeoJSON</button>",
    "<span class=\"sep\"></span>",
    "<button id=\"zoomIn\" title=\"Zoom in\">+</button>",
    "<button id=\"zoomOut\" title=\"Zoom out\">-</button>",
    "<button id=\"fit\" title=\"Fit slide to window\">Fit</button>",
    "<button id=\"oneToOne\" title=\"Show image pixels at 1:1\">1:1</button>",
    "<span class=\"sep\"></span>",
    "<button id=\"roiToggle\" title=\"Toggle ROI overlays\">ROI</button>",
    "<button id=\"labelsToggle\" title=\"Toggle ROI labels\">Labels</button>",
    "<button id=\"prevRoi\" title=\"Previous ROI\">Prev</button>",
    "<button id=\"nextRoi\" title=\"Next ROI\">Next</button>",
    "<button id=\"layersToggle\" title=\"Show GeoJSON geometry list\">GeoJSON</button>",
    "<label class=\"control\" title=\"ROI opacity\">Opacity <input id=\"roiOpacity\" type=\"range\" min=\"0\" max=\"1\" step=\"0.05\" value=\"1\"></label>",
    "<span class=\"sep\"></span>",
    "<button id=\"crosshairToggle\" title=\"Toggle crosshair\">Crosshair</button>",
    "<button id=\"copyCoord\" title=\"Copy current coordinates\">Copy XY</button>",
    wsi_viewer_stain_controls(config),
    "</div>\n",
    "</div>\n",
    "<div id=\"roiPanel\" class=\"panel\"><div class=\"sideTitle\">GeoJSON Geometries</div><div id=\"roiSummary\" class=\"sideMeta\"></div><div id=\"roiList\"></div></div>\n",
    "<div id=\"status\">", wsi_html_escape(loading_message), "</div>\n"
  )
}

wsi_viewer_stain_js <- function() {
  paste0(
    "const stainEnabled=!!(cfg.stain&&cfg.stain.enabled);\n",
    "let stainOn=stainEnabled,hemaVisible=true,hrpVisible=true;\n",
    "let hemaColor=stainEnabled?(cfg.stain.hematoxylin_colour||'#4b3f99'):'#4b3f99';\n",
    "let hrpColor=stainEnabled?(cfg.stain.hrp_colour||'#8b5a2b'):'#8b5a2b';\n",
    "let hemaStrength=stainEnabled?Number(cfg.stain.hematoxylin_strength||1):1;\n",
    "let hrpStrength=stainEnabled?Number(cfg.stain.hrp_strength||1):1;\n",
    "let stainInv=null;\n",
    "function rgbHex(hex){const h=String(hex||'#000000').replace('#','');const s=h.length===3?h.split('').map(c=>c+c).join(''):h;const n=parseInt(s,16);return {r:(n>>16)&255,g:(n>>8)&255,b:n&255};}\n",
    "function norm3(v){const n=Math.hypot(Number(v[0]),Number(v[1]),Number(v[2]));return n>0?[Number(v[0])/n,Number(v[1])/n,Number(v[2])/n]:[0,0,0];}\n",
    "function cross3(a,b){return [a[1]*b[2]-a[2]*b[1],a[2]*b[0]-a[0]*b[2],a[0]*b[1]-a[1]*b[0]];}\n",
    "function inv3(m){const a=m[0][0],b=m[0][1],c=m[0][2],d=m[1][0],e=m[1][1],f=m[1][2],g=m[2][0],h=m[2][1],i=m[2][2];const A=e*i-f*h,B=-(d*i-f*g),C=d*h-e*g,D=-(b*i-c*h),E=a*i-c*g,F=-(a*h-b*g),G=b*f-c*e,H=-(a*f-c*d),I=a*e-b*d;const det=a*A+b*B+c*C;if(Math.abs(det)<1e-8)return null;return [[A/det,D/det,G/det],[B/det,E/det,H/det],[C/det,F/det,I/det]];}\n",
    "function initStain(){if(!stainEnabled)return;const h=norm3(cfg.stain.hematoxylin||[0.65,0.704,0.286]);const d=norm3(cfg.stain.hrp||[0.268,0.57,0.776]);let r=norm3(cross3(h,d));if(!isFinite(r[0])||Math.hypot(r[0],r[1],r[2])===0)r=[0.711,0.423,0.561];stainInv=inv3([[h[0],d[0],r[0]],[h[1],d[1],r[1]],[h[2],d[2],r[2]]]);}\n",
    "function applyStainToCanvas(){if(!stainEnabled||!stainOn||!stainInv)return;let img;try{img=ctx.getImageData(0,0,canvas.width,canvas.height);}catch(e){status.textContent='IHC deconvolution cannot read canvas pixels in this browser context.';return;}const data=img.data,hc=rgbHex(hemaColor),dc=rgbHex(hrpColor);for(let p=0;p<data.length;p+=4){const r=data[p],g=data[p+1],b=data[p+2];if(data[p+3]===0||(r<28&&g<28&&b<28))continue;const odR=-Math.log((r+1)/256),odG=-Math.log((g+1)/256),odB=-Math.log((b+1)/256);const h=Math.max(0,stainInv[0][0]*odR+stainInv[0][1]*odG+stainInv[0][2]*odB);const d=Math.max(0,stainInv[1][0]*odR+stainInv[1][1]*odG+stainInv[1][2]*odB);const th=hemaVisible?clamp(1-Math.exp(-h*hemaStrength),0,1):0;const td=hrpVisible?clamp(1-Math.exp(-d*hrpStrength),0,1):0;let rr=255,gg=255,bb=255;rr=rr*(1-th)+hc.r*th;gg=gg*(1-th)+hc.g*th;bb=bb*(1-th)+hc.b*th;rr=rr*(1-td)+dc.r*td;gg=gg*(1-td)+dc.g*td;bb=bb*(1-td)+dc.b*td;data[p]=rr;data[p+1]=gg;data[p+2]=bb;}ctx.putImageData(img,0,0);}\n",
    "function updateStainControls(){if(!stainEnabled)return;el('stainToggle').classList.toggle('active',stainOn);['hemaVisible','hemaColor','hemaStrength','hrpVisible','hrpColor','hrpStrength'].forEach(id=>{el(id).disabled=!stainOn;});}\n",
    "function bindStainControls(){if(!stainEnabled)return;initStain();el('stainToggle').onclick=()=>{stainOn=!stainOn;updateStainControls();draw();};el('hemaVisible').onchange=e=>{hemaVisible=e.target.checked;draw();};el('hrpVisible').onchange=e=>{hrpVisible=e.target.checked;draw();};el('hemaColor').oninput=e=>{hemaColor=e.target.value;draw();};el('hrpColor').oninput=e=>{hrpColor=e.target.value;draw();};el('hemaStrength').oninput=e=>{hemaStrength=Number(e.target.value);draw();};el('hrpStrength').oninput=e=>{hrpStrength=Number(e.target.value);draw();};updateStainControls();}\n"
  )
}

wsi_viewer_geometry_js <- function() {
  paste0(
    "function fmt(v,d=0){return Number.isFinite(Number(v))?Number(v).toFixed(d):'NA';}\n",
    "function isDrawable(roi){return roi&&roi.drawable!==false&&roi.rings&&roi.rings.length>0;}\n",
    "function hasDrawable(){return rois.some(isDrawable);}\n",
    "function boundsFromRing(ring){const xs=ring.map(p=>p.x),ys=ring.map(p=>p.y);return {xmin:Math.min(...xs),ymin:Math.min(...ys),xmax:Math.max(...xs),ymax:Math.max(...ys)};}\n",
    "function ringArea(ring){if(!ring||ring.length<3)return NaN;let a=0;for(let i=0,j=ring.length-1;i<ring.length;j=i++){a+=(ring[j].x*ring[i].y-ring[i].x*ring[j].y);}return Math.abs(a/2);}\n",
    "function polygonArea(rings){if(!rings||!rings.length)return NaN;const outer=ringArea(rings[0]);let holes=0;for(let i=1;i<rings.length;i++)holes+=ringArea(rings[i]);return Math.max(0,outer-holes);}\n",
    "function roiBounds(roi){if(roi&&roi.bbox&&Number.isFinite(Number(roi.bbox.xmin)))return roi.bbox;if(isDrawable(roi)){let xs=[],ys=[];roi.rings.forEach(r=>r.forEach(p=>{xs.push(p.x);ys.push(p.y);}));return {xmin:Math.min(...xs),xmax:Math.max(...xs),ymin:Math.min(...ys),ymax:Math.max(...ys)};}return null;}\n",
    "function geometryType(roi){return roi.geometry_type||roi.geometryType||'Geometry';}\n",
    "function pointCount(roi){if(Number.isFinite(Number(roi.point_count)))return Number(roi.point_count);if(!roi.rings)return 0;let n=0;roi.rings.forEach(r=>{n+=r.length;});return n;}\n",
    "function formatBounds(b){return b?('x '+fmt(b.xmin)+'-'+fmt(b.xmax)+' | y '+fmt(b.ymin)+'-'+fmt(b.ymax)):'NA';}\n",
    "function geometrySummary(){if(!rois.length)return 'No GeoJSON geometries loaded.';const counts={};rois.forEach(r=>{const t=geometryType(r);counts[t]=(counts[t]||0)+1;});return rois.length+' geometr'+(rois.length===1?'y':'ies')+' | '+Object.keys(counts).map(k=>k+' '+counts[k]).join(', ');}\n",
    "function roiAt(p){for(let i=rois.length-1;i>=0;i--){if(isDrawable(rois[i])&&rois[i].rings.some(r=>pointInRing(p,r)))return i;}return -1;}\n",
    "function centerRoi(i){if(!hasDrawable())return;let idx=-1;for(let k=0;k<rois.length;k++){const candidate=(i+rois.length+k)%rois.length;if(isDrawable(rois[candidate])){idx=candidate;break;}}if(idx<0)return;selectedRoi=idx;const b=roiBounds(rois[selectedRoi]);if(!b){status.textContent='This GeoJSON geometry has no drawable bounds.';updateRoiList();draw();return;}let viewW=b.xmax-b.xmin,viewH=b.ymax-b.ymin,centerX=(b.xmin+b.xmax)/2,centerY=(b.ymin+b.ymax)/2;const pad=1.35;if(typeof slideToImage==='function'){const p0=slideToImage({x:b.xmin,y:b.ymin}),p1=slideToImage({x:b.xmax,y:b.ymax});viewW=p1.x-p0.x;viewH=p1.y-p0.y;centerX=(p0.x+p1.x)/2;centerY=(p0.y+p1.y)/2;}const maxScale=(typeof image!=='undefined')?40:4;scale=clamp(Math.min(innerWidth/Math.max(1,viewW*pad),innerHeight/Math.max(1,viewH*pad)),minScale*0.8,maxScale);offsetX=innerWidth/2-centerX*scale;offsetY=innerHeight/2-centerY*scale;updateRoiList();draw();}\n",
    "function drawRois(){if(!showRois||!rois.length)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{if(!isDrawable(roi))return;let label=null;ctx.beginPath();roi.rings.forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(!label)label=q;if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;if(showLabels&&label){const text=roi.name||roi.id;const w=ctx.measureText(text).width+8;ctx.fillStyle='rgba(0,0,0,.68)';ctx.fillRect(label.x,label.y,w,18);ctx.fillStyle=roi.colour;ctx.fillText(text,label.x+4,label.y+3);}});ctx.restore();}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function addDetail(parent,label,value,asCode=false){const l=document.createElement('span');l.textContent=label;const v=document.createElement(asCode?'code':'span');v.textContent=value;parent.append(l,v);}\n",
    "function buildRoiList(){const list=el('roiList'),summary=el('roiSummary');list.innerHTML='';summary.textContent=geometrySummary();rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';b.type='button';const top=document.createElement('div');top.className='roiTop';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour||'#cccccc';const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id||('geometry '+(i+1));const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';top.append(sw,nm,cl);const details=document.createElement('div');details.className='roiDetails';const bb=roiBounds(roi);addDetail(details,'Geometry',geometryType(roi));addDetail(details,'Bounds',formatBounds(bb),true);addDetail(details,'Points',String(pointCount(roi)));const area=Number.isFinite(Number(roi.area))?Number(roi.area):(isDrawable(roi)?polygonArea(roi.rings):NaN);addDetail(details,'Area',Number.isFinite(area)?fmt(area,1):'NA');addDetail(details,'Source',roi.source||'geojson');addDetail(details,'ID',String(roi.id||i+1),true);if(!isDrawable(roi)){const badge=document.createElement('span');badge.className='roiBadge';badge.textContent='listed only';details.append(document.createElement('span'),badge);}b.append(top,details);b.onclick=()=>{if(isDrawable(roi)){centerRoi(i);}else{selectedRoi=i;updateRoiList();draw();status.textContent='Selected '+geometryType(roi)+' geometry in the GeoJSON list; this type is not drawn as a polygon overlay yet.';}};list.appendChild(b);});updateRoiList();}\n",
    "function finishDraft(){if(draft.length<3){status.textContent='Add at least 3 points before finishing an ROI.';return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});const bbox=boundsFromRing(ring);newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',geometry_type:'Polygon',source:'drawn',drawable:true,point_count:ring.length-1,area:polygonArea([ring]),bbox:bbox,colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;buildRoiList();updateButtons();setMode('select');draw();}\n",
    "function roiFeature(roi,i){let geometry=null;if(roi.coordinates){geometry={type:geometryType(roi),coordinates:roi.coordinates};}else if(isDrawable(roi)){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});geometry={type:'Polygon',coordinates:coords};}if(!geometry)return null;return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:roi.class||'annotation'}},geometry:geometry};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature).filter(Boolean)},null,2);}\n",
    "function updateButtons(){const has=rois.length>0,drawable=hasDrawable();['roiToggle','labelsToggle','prevRoi','nextRoi'].forEach(id=>el(id).disabled=!drawable);el('layersToggle').disabled=!has;el('finishRoi').disabled=draft.length<3;el('undoPoint').disabled=draft.length<1;el('saveGeojson').disabled=!has&&draft.length<3;el('roiToggle').classList.toggle('active',showRois&&drawable);el('labelsToggle').classList.toggle('active',showLabels&&drawable);el('crosshairToggle').classList.toggle('active',showCrosshair);}\n"
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
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,lastX=0,lastY=0,lastPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0;\n",
    "function clamp(v,min,max){return Math.max(min,Math.min(max,v));}\n",
    wsi_viewer_stain_js(),
    "function setMode(m){mode=m;canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');el('toolPan').classList.toggle('active',m==='pan');el('toolSelect').classList.toggle('active',m==='select');el('toolDraw').classList.toggle('active',m==='draw');updateButtons();}\n",
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
    "function draw(){ctx.clearRect(0,0,innerWidth,innerHeight);ctx.imageSmoothingEnabled=true;ctx.drawImage(image,offsetX,offsetY,image.naturalWidth*scale,image.naturalHeight*scale);applyStainToCanvas();drawRois();drawDraft();drawCrosshair();updateStatus(lastPointer);}\n",
    "function drawRois(){if(!showRois||!rois.length||!image.naturalWidth)return;ctx.save();ctx.lineWidth=2;ctx.font='12px -apple-system,BlinkMacSystemFont,Segoe UI,sans-serif';ctx.textBaseline='top';rois.forEach((roi,i)=>{let label=null;ctx.beginPath();roi.rings.forEach(ring=>{ring.forEach((p,j)=>{const q=slideToCanvas(p);if(!label)label=q;if(j===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});ctx.closePath();});ctx.globalAlpha=roiOpacity;ctx.fillStyle=roi.fill;ctx.strokeStyle=i===selectedRoi?'#ffffff':roi.colour;ctx.lineWidth=i===selectedRoi?4:2;ctx.fill('evenodd');ctx.stroke();ctx.globalAlpha=1;if(showLabels&&label){const text=roi.name||roi.id;const w=ctx.measureText(text).width+8;ctx.fillStyle='rgba(0,0,0,.68)';ctx.fillRect(label.x,label.y,w,18);ctx.fillStyle=roi.colour;ctx.fillText(text,label.x+4,label.y+3);}});ctx.restore();}\n",
    "function drawDraft(){if(!draft.length)return;ctx.save();ctx.strokeStyle='#facc15';ctx.fillStyle='rgba(250,204,21,.18)';ctx.lineWidth=2;ctx.setLineDash([6,4]);ctx.beginPath();draft.forEach((p,i)=>{const q=slideToCanvas(p);if(i===0)ctx.moveTo(q.x,q.y);else ctx.lineTo(q.x,q.y);});if(mode==='draw'&&lastPointer&&pointInsideSlide(lastPointer)){const q=slideToCanvas(lastPointer);ctx.lineTo(q.x,q.y);}if(draft.length>2){const q=slideToCanvas(draft[0]);ctx.lineTo(q.x,q.y);ctx.fill();}ctx.stroke();ctx.setLineDash([]);draft.forEach(p=>{const q=slideToCanvas(p);ctx.beginPath();ctx.arc(q.x,q.y,4,0,Math.PI*2);ctx.fillStyle='#facc15';ctx.fill();ctx.strokeStyle='#111';ctx.stroke();});ctx.restore();}\n",
    "function drawCrosshair(){if(!showCrosshair||!lastPointer||!pointInsideSlide(lastPointer))return;const q=slideToCanvas(lastPointer);ctx.save();ctx.strokeStyle='rgba(255,255,255,.55)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(q.x,0);ctx.lineTo(q.x,innerHeight);ctx.moveTo(0,q.y);ctx.lineTo(innerWidth,q.y);ctx.stroke();ctx.restore();}\n",
    "function updateStatus(p){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x';if(stainEnabled&&stainOn)msg+=' | IHC H-DAB';if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(p&&pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg+' | thumbnail preview, full slide not loaded into R';}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){status.textContent='Add at least 3 points before finishing an ROI.';return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;buildRoiList();updateButtons();setMode('select');draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:roi.class||'annotation'}},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(!rois.length&&draft.length<3){status.textContent='Draw an ROI before saving GeoJSON.';return;}const text=geojsonText();const name=cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();status.textContent='Saved '+name;return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);status.textContent='Downloaded '+name;}\n",
    "function updateButtons(){const has=rois.length>0;['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>el(id).disabled=!has);el('finishRoi').disabled=draft.length<3;el('undoPoint').disabled=draft.length<1;el('saveGeojson').disabled=!has&&draft.length<3;el('roiToggle').classList.toggle('active',showRois&&has);el('labelsToggle').classList.toggle('active',showLabels&&has);el('crosshairToggle').classList.toggle('active',showCrosshair);}\n",
    wsi_viewer_geometry_js(),
    "function copyCoord(){if(!lastPointer||!pointInsideSlide(lastPointer))return;const text=Math.round(lastPointer.x)+','+Math.round(lastPointer.y);if(navigator.clipboard)navigator.clipboard.writeText(text);status.textContent='Copied '+text;}\n",
    "canvas.addEventListener('mousedown',e=>{lastPointer=pointerToSlide(e);if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='select'){selectedRoi=roiAt(lastPointer);updateRoiList();draw();return;}dragging=true;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',()=>{dragging=false;canvas.classList.remove('dragging');});\n",
    "window.addEventListener('mousemove',e=>{lastPointer=pointerToSlide(e);if(dragging){offsetX+=e.clientX-lastX;offsetY+=e.clientY-lastY;lastX=e.clientX;lastY=e.clientY;draw();}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.2:1/1.2,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}zoomAt(2,e.clientX,e.clientY);});\n",
    "el('toolPan').onclick=()=>setMode('pan');el('toolSelect').onclick=()=>setMode('select');el('toolDraw').onclick=()=>setMode('draw');el('finishRoi').onclick=finishDraft;el('undoPoint').onclick=undoDraftPoint;el('saveGeojson').onclick=saveGeojson;el('zoomIn').onclick=()=>zoomAt(1.25,innerWidth/2,innerHeight/2);el('zoomOut').onclick=()=>zoomAt(1/1.25,innerWidth/2,innerHeight/2);el('fit').onclick=fitView;el('oneToOne').onclick=oneToOne;\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);el('layersToggle').onclick=()=>el('roiPanel').classList.toggle('open');el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);draw();};el('crosshairToggle').onclick=()=>{showCrosshair=!showCrosshair;updateButtons();draw();};el('copyCoord').onclick=copyCoord;\n",
    "window.addEventListener('keydown',e=>{if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='Enter'&&mode==='draw')finishDraft();if((e.key==='Backspace'||e.key==='Delete')&&mode==='draw'){e.preventDefault();undoDraftPoint();}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='c')el('crosshairToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape')setMode('pan');});\n",
    "bindStainControls();buildRoiList();updateButtons();setMode('pan');\n",
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
    "let scale=1,minScale=1,offsetX=0,offsetY=0,dragging=false,lastX=0,lastY=0,lastPointer=null,mode='pan',showRois=true,showLabels=true,showCrosshair=false,selectedRoi=-1,roiOpacity=1,draft=[],newRoiCount=0;\n",
    "const cache = new Map();\n",
    "let renderQueued=false,loadingTiles=0;\n",
    "function clamp(v,min,max){return Math.max(min,Math.min(max,v));}\n",
    wsi_viewer_stain_js(),
    "function setMode(m){mode=m;canvas.classList.toggle('selecting',m==='select');canvas.classList.toggle('drawing',m==='draw');el('toolPan').classList.toggle('active',m==='pan');el('toolSelect').classList.toggle('active',m==='select');el('toolDraw').classList.toggle('active',m==='draw');updateButtons();}\n",
    "function tileUrl(level,col,row){return cfg.tile_url_base+'/'+level+'/'+col+'_'+row+'.'+cfg.tile_format;}\n",
    "function requestDraw(){if(renderQueued)return;renderQueued=true;requestAnimationFrame(()=>{renderQueued=false;draw();});}\n",
    "function loadTile(level,col,row){const key=level+'/'+col+'/'+row;if(cache.has(key))return cache.get(key);const img=new Image();const rec={img:img,loaded:false,failed:false};img.onload=()=>{rec.loaded=true;requestDraw();};img.onerror=()=>{rec.failed=true;};img.src=tileUrl(level,col,row);cache.set(key,rec);return rec;}\n",
    "function drawBleed(img,sx,sy,sw,sh,dx,dy,dw,dh){const x=Math.floor(dx),y=Math.floor(dy),w=Math.ceil(dx+dw)-x+1,h=Math.ceil(dy+dh)-y+1;ctx.drawImage(img,sx,sy,sw,sh,x,y,w,h);}\n",
    "function drawAncestorTile(level,col,row,tileLevelW,tileLevelH,dx,dy,dw,dh){for(let a=level-1;a>=0;a--){const factor=Math.pow(2,level-a);const ax=(col*cfg.tile_size)/factor,ay=(row*cfg.tile_size)/factor;const ac=Math.floor(ax/cfg.tile_size),ar=Math.floor(ay/cfg.tile_size);const key=a+'/'+ac+'/'+ar;const rec=cache.get(key)||loadTile(a,ac,ar);if(rec.loaded){const sx=ax-ac*cfg.tile_size,sy=ay-ar*cfg.tile_size,sw=tileLevelW/factor,sh=tileLevelH/factor;ctx.imageSmoothingEnabled=true;drawBleed(rec.img,sx,sy,sw,sh,dx,dy,dw,dh);ctx.imageSmoothingEnabled=false;return true;}}return false;}\n",
    "function resize(){const dpr=window.devicePixelRatio||1;canvas.width=Math.floor(innerWidth*dpr);canvas.height=Math.floor(innerHeight*dpr);canvas.style.width=innerWidth+'px';canvas.style.height=innerHeight+'px';ctx.setTransform(dpr,0,0,dpr,0,0);fitView();}\n",
    "function fitView(){minScale=Math.min(innerWidth/cfg.slide_width,innerHeight/cfg.slide_height);scale=minScale;offsetX=(innerWidth-cfg.slide_width*scale)/2;offsetY=(innerHeight-cfg.slide_height*scale)/2;draw();}\n",
    "function oneToOne(){scale=1;offsetX=(innerWidth-cfg.slide_width)/2;offsetY=(innerHeight-cfg.slide_height)/2;draw();}\n",
    "function currentLevel(){return clamp(Math.ceil(cfg.max_level+Math.log2(scale)),0,cfg.max_level);}\n",
    "function draw(){ctx.clearRect(0,0,innerWidth,innerHeight);ctx.fillStyle='#101010';ctx.fillRect(0,0,innerWidth,innerHeight);const level=currentLevel();const down=Math.pow(2,cfg.max_level-level);const levelW=Math.ceil(cfg.slide_width/down);const levelH=Math.ceil(cfg.slide_height/down);const tileSlide=cfg.tile_size*down;const left=Math.max(0,(-offsetX)/scale);const top=Math.max(0,(-offsetY)/scale);const right=Math.min(cfg.slide_width,(innerWidth-offsetX)/scale);const bottom=Math.min(cfg.slide_height,(innerHeight-offsetY)/scale);const c0=clamp(Math.floor(left/tileSlide),0,Math.ceil(levelW/cfg.tile_size)-1);const c1=clamp(Math.floor(right/tileSlide),0,Math.ceil(levelW/cfg.tile_size)-1);const r0=clamp(Math.floor(top/tileSlide),0,Math.ceil(levelH/cfg.tile_size)-1);const r1=clamp(Math.floor(bottom/tileSlide),0,Math.ceil(levelH/cfg.tile_size)-1);ctx.imageSmoothingEnabled=false;loadingTiles=0;for(let row=r0;row<=r1;row++){for(let col=c0;col<=c1;col++){const rec=loadTile(level,col,row);const tileLevelW=Math.min(cfg.tile_size,levelW-col*cfg.tile_size);const tileLevelH=Math.min(cfg.tile_size,levelH-row*cfg.tile_size);const dx=offsetX+col*cfg.tile_size*down*scale;const dy=offsetY+row*cfg.tile_size*down*scale;const dw=tileLevelW*down*scale;const dh=tileLevelH*down*scale;if(rec.loaded){drawBleed(rec.img,0,0,tileLevelW,tileLevelH,dx,dy,dw,dh);}else{loadingTiles++;if(!drawAncestorTile(level,col,row,tileLevelW,tileLevelH,dx,dy,dw,dh)){ctx.fillStyle='#1f1f1f';ctx.fillRect(Math.floor(dx),Math.floor(dy),Math.ceil(dw)+1,Math.ceil(dh)+1);}}}}applyStainToCanvas();drawRois();drawDraft();drawCrosshair();updateStatus(lastPointer,level);}\n",
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
    "function updateStatus(p,level){let msg='Mode '+mode+' | Zoom '+(scale/minScale).toFixed(2)+'x | Deep Zoom level '+level+'/'+cfg.max_level;if(stainEnabled&&stainOn)msg+=' | IHC H-DAB';if(loadingTiles)msg+=' | loading '+loadingTiles+' tile'+(loadingTiles===1?'':'s');if(draft.length)msg+=' | drawing '+draft.length+' point'+(draft.length===1?'':'s');if(pointInsideSlide(p))msg+=' | x '+Math.round(p.x)+' y '+Math.round(p.y);if(rois.length)msg+=' | ROIs '+rois.length+(selectedRoi>=0?' | selected '+(rois[selectedRoi].name||rois[selectedRoi].id):'');status.textContent=msg+' | full-resolution tiled viewer, full slide not loaded into R';}\n",
    "function updateRoiList(){document.querySelectorAll('.roiItem').forEach((b,i)=>b.classList.toggle('active',i===selectedRoi));}\n",
    "function buildRoiList(){const list=el('roiList');list.innerHTML='';rois.forEach((roi,i)=>{const b=document.createElement('button');b.className='roiItem';const sw=document.createElement('span');sw.className='swatch';sw.style.background=roi.colour;const nm=document.createElement('span');nm.className='roiName';nm.textContent=roi.name||roi.id;const cl=document.createElement('span');cl.className='roiClass';cl.textContent=roi.class||'';b.append(sw,nm,cl);b.onclick=()=>centerRoi(i);list.appendChild(b);});updateRoiList();}\n",
    "function hexToRgba(hex,a){const h=hex.replace('#','');const n=parseInt(h,16);return 'rgba('+((n>>16)&255)+','+((n>>8)&255)+','+(n&255)+','+a+')';}\n",
    "function addDraftPoint(p){if(!pointInsideSlide(p))return;draft.push({x:p.x,y:p.y});updateButtons();draw();}\n",
    "function undoDraftPoint(){draft.pop();updateButtons();draw();}\n",
    "function finishDraft(){if(draft.length<3){status.textContent='Add at least 3 points before finishing an ROI.';return;}const palette=['#00BFC4','#F8766D','#7CAE00','#C77CFF','#E69F00','#56B4E9','#CC79A7'];const colour=palette[rois.length%palette.length];const ring=draft.map(p=>({x:Math.round(p.x),y:Math.round(p.y)}));ring.push({x:ring[0].x,y:ring[0].y});newRoiCount++;rois.push({id:'drawn_roi_'+newRoiCount,name:'Drawn ROI '+newRoiCount,class:'annotation',colour:colour,fill:hexToRgba(colour,0.18),rings:[ring],drawn:true});selectedRoi=rois.length-1;draft=[];showRois=true;buildRoiList();updateButtons();setMode('select');draw();}\n",
    "function roiFeature(roi,i){const coords=roi.rings.map(r=>{const ring=r.map(p=>[Math.round(p.x),Math.round(p.y)]);const f=ring[0],l=ring[ring.length-1];if(!l||f[0]!==l[0]||f[1]!==l[1])ring.push([f[0],f[1]]);return ring;});return {type:'Feature',id:roi.id||('roi_'+(i+1)),properties:{name:roi.name||('roi_'+(i+1)),classification:{name:roi.class||'annotation'}},geometry:{type:'Polygon',coordinates:coords}};}\n",
    "function geojsonText(){if(draft.length>=3)finishDraft();return JSON.stringify({type:'FeatureCollection',features:rois.map(roiFeature)},null,2);}\n",
    "function downloadText(text,name){const blob=new Blob([text],{type:'application/geo+json'});const a=document.createElement('a');a.href=URL.createObjectURL(blob);a.download=name;document.body.appendChild(a);a.click();setTimeout(()=>{URL.revokeObjectURL(a.href);a.remove();},1000);}\n",
    "async function saveGeojson(){if(!rois.length&&draft.length<3){status.textContent='Draw an ROI before saving GeoJSON.';return;}const text=geojsonText();const name=cfg.annotation_filename||'wsiTools_annotations.geojson';if(window.showSaveFilePicker){try{const h=await window.showSaveFilePicker({suggestedName:name,types:[{description:'GeoJSON',accept:{'application/geo+json':['.geojson'],'application/json':['.json']}}]});const w=await h.createWritable();await w.write(text);await w.close();status.textContent='Saved '+name;return;}catch(e){if(e&&e.name==='AbortError')return;}}downloadText(text,name);status.textContent='Downloaded '+name;}\n",
    "function updateButtons(){const has=rois.length>0;['roiToggle','labelsToggle','prevRoi','nextRoi','layersToggle'].forEach(id=>el(id).disabled=!has);el('finishRoi').disabled=draft.length<3;el('undoPoint').disabled=draft.length<1;el('saveGeojson').disabled=!has&&draft.length<3;el('roiToggle').classList.toggle('active',showRois&&has);el('labelsToggle').classList.toggle('active',showLabels&&has);el('crosshairToggle').classList.toggle('active',showCrosshair);}\n",
    wsi_viewer_geometry_js(),
    "function copyCoord(){if(!pointInsideSlide(lastPointer))return;const text=Math.round(lastPointer.x)+','+Math.round(lastPointer.y);if(navigator.clipboard)navigator.clipboard.writeText(text);status.textContent='Copied '+text;}\n",
    "canvas.addEventListener('mousedown',e=>{lastPointer=pointerToSlide(e);if(mode==='draw'){if(e.detail===1)addDraftPoint(lastPointer);return;}if(mode==='select'){selectedRoi=roiAt(lastPointer);updateRoiList();draw();return;}dragging=true;lastX=e.clientX;lastY=e.clientY;canvas.classList.add('dragging');});\n",
    "window.addEventListener('mouseup',()=>{dragging=false;canvas.classList.remove('dragging');});\n",
    "window.addEventListener('mousemove',e=>{lastPointer=pointerToSlide(e);if(dragging){offsetX+=e.clientX-lastX;offsetY+=e.clientY-lastY;lastX=e.clientX;lastY=e.clientY;draw();}else{draw();}});\n",
    "canvas.addEventListener('wheel',e=>{e.preventDefault();zoomAt(e.deltaY<0?1.25:1/1.25,e.clientX,e.clientY);},{passive:false});\n",
    "canvas.addEventListener('dblclick',e=>{if(mode==='draw'){e.preventDefault();finishDraft();return;}zoomAt(2,e.clientX,e.clientY);});\n",
    "el('toolPan').onclick=()=>setMode('pan');el('toolSelect').onclick=()=>setMode('select');el('toolDraw').onclick=()=>setMode('draw');el('finishRoi').onclick=finishDraft;el('undoPoint').onclick=undoDraftPoint;el('saveGeojson').onclick=saveGeojson;el('zoomIn').onclick=()=>zoomAt(1.5,innerWidth/2,innerHeight/2);el('zoomOut').onclick=()=>zoomAt(1/1.5,innerWidth/2,innerHeight/2);el('fit').onclick=fitView;el('oneToOne').onclick=oneToOne;\n",
    "el('roiToggle').onclick=()=>{showRois=!showRois;updateButtons();draw();};el('labelsToggle').onclick=()=>{showLabels=!showLabels;updateButtons();draw();};el('prevRoi').onclick=()=>centerRoi(selectedRoi<=0?rois.length-1:selectedRoi-1);el('nextRoi').onclick=()=>centerRoi(selectedRoi+1);el('layersToggle').onclick=()=>el('roiPanel').classList.toggle('open');el('roiOpacity').oninput=e=>{roiOpacity=Number(e.target.value);draw();};el('crosshairToggle').onclick=()=>{showCrosshair=!showCrosshair;updateButtons();draw();};el('copyCoord').onclick=copyCoord;\n",
    "window.addEventListener('keydown',e=>{if(e.key==='f')fitView();if(e.key==='1')oneToOne();if(e.key==='d')setMode('draw');if(e.key==='Enter'&&mode==='draw')finishDraft();if((e.key==='Backspace'||e.key==='Delete')&&mode==='draw'){e.preventDefault();undoDraftPoint();}if(e.key==='r'&&rois.length)el('roiToggle').click();if(e.key==='l'&&rois.length)el('labelsToggle').click();if(e.key==='c')el('crosshairToggle').click();if(e.key==='['&&rois.length)el('prevRoi').click();if(e.key===']'&&rois.length)el('nextRoi').click();if(e.key==='Escape')setMode('pan');});\n",
    "bindStainControls();buildRoiList();updateButtons();setMode('pan');\n",
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
#' such as QuPath and napari: pan/select modes, fit and 1:1 zoom, ROI visibility
#' and label toggles, ROI opacity, ROI previous/next navigation, an annotation
#' side window listing GeoJSON geometries, crosshair display, coordinate
#' copying, polygon drawing, and browser-based GeoJSON export. Optional
#' `stain = "ihc"` adds an H-DAB
#' deconvolution display with interactive hematoxylin and HRP/DAB channel
#' colours and gains.
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
#' @param stain Optional stain display mode. Use `"ihc"` for interactive H-DAB
#'   color deconvolution in the browser.
#' @param hematoxylin,hrp RGB optical-density vectors used when
#'   `stain = "ihc"`.
#' @param hematoxylin_colour,hrp_colour Initial display colours for the
#'   hematoxylin and HRP/DAB channels.
#' @param hematoxylin_strength,hrp_strength Initial display gains for the
#'   hematoxylin and HRP/DAB channels.
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
                       hematoxylin = c(0.650, 0.704, 0.286),
                       hrp = c(0.268, 0.570, 0.776),
                       hematoxylin_colour = "#4b3f99",
                       hrp_colour = "#8b5a2b",
                       hematoxylin_strength = 1,
                       hrp_strength = 1) {
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
  annotation_filename <- paste0(tools::file_path_sans_ext(basename(output)), "_annotations.geojson")
  stain_config <- wsi_ihc_stain_config(
    stain = stain,
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
      image_data_uri = wsi_viewer_thumbnail_data_uri(slide, width = width, height = height),
      annotation_filename = annotation_filename,
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
      tile_size = as.integer(tile_size),
      tile_format = tile_format,
      tile_url_base = wsi_tile_base_url(tile_dir, output),
      dzi = basename(tiles$dzi),
      max_level = wsi_dz_max_level(slide$dimensions[["width"]], slide$dimensions[["height"]]),
      annotation_filename = annotation_filename,
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
#' mode so ROI outlines remain aligned while zooming. The viewer toolbar can
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
