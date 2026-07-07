wsi_array_data_uri <- function(image) {
  if (inherits(image, "raster")) {
    raster <- image
  } else {
    raster <- wsi_array_to_raster(image)
  }
  dims <- dim(raster)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp, width = dims[[2L]], height = dims[[1L]], bg = "transparent")
  device_open <- TRUE
  old_par <- graphics::par(mar = c(0, 0, 0, 0))
  on.exit({
    if (isTRUE(device_open)) {
      graphics::par(old_par)
      grDevices::dev.off()
    }
    unlink(tmp)
  }, add = TRUE)
  graphics::plot.new()
  graphics::rasterImage(raster, 0, 0, 1, 1)
  graphics::par(old_par)
  grDevices::dev.off()
  device_open <- FALSE
  wsi_image_data_uri(tmp, mime = "image/png")
}

wsi_file_image_dimensions <- function(path) {
  if (requireNamespace("magick", quietly = TRUE)) {
    info <- magick::image_info(magick::image_read(path))
    return(c(width = info$width[[1L]], height = info$height[[1L]]))
  }
  c(width = 1, height = 1)
}

wsi_compare_source <- function(image, label, width = 1200, height = NULL, roi = NULL, mask = NULL) {
  opened <- NULL
  on.exit(if (!is.null(opened)) wsi_close(opened), add = TRUE)

  if (inherits(image, "wsi_slide")) {
    slide <- image
    uri <- wsi_viewer_thumbnail_data_uri(slide, width = width, height = height)
    dims <- slide$dimensions
    source_label <- label %||% basename(slide$path %||% slide$backend)
  } else if (is.character(image) && length(image) == 1L) {
    path <- wsi_validate_input_path(image)
    ext <- tolower(tools::file_ext(path))
    if (wsi_is_omezarr_path(path)) {
      slide <- open_omezarr(path)
      opened <- slide
      uri <- wsi_omezarr_placeholder_data_uri(slide, width = width)
      dims <- slide$dimensions
      source_label <- label %||% basename(path)
    } else if (ext %in% c("png", "jpg", "jpeg", "gif", "webp")) {
      uri <- wsi_image_data_uri(path, mime = switch(ext, jpg = "image/jpeg", jpeg = "image/jpeg", gif = "image/gif", webp = "image/webp", "image/png"))
      dims <- wsi_file_image_dimensions(path)
      source_label <- label %||% basename(path)
    } else {
      slide <- wsi_open(path, backend = "auto")
      opened <- slide
      uri <- wsi_viewer_thumbnail_data_uri(slide, width = width, height = height)
      dims <- slide$dimensions
      source_label <- label %||% basename(path)
    }
  } else if (inherits(image, "raster") || is.array(image)) {
    uri <- wsi_array_data_uri(image)
    dims <- rev(dim(if (inherits(image, "raster")) image else image[, , 1L]))
    names(dims) <- c("width", "height")
    source_label <- label %||% "image"
  } else {
    wsi_abort("Viewer inputs must be `wsi_slide` objects, image paths, rasters, or arrays.")
  }

  mask_uri <- NULL
  if (!is.null(mask)) {
    if (is.character(mask) && length(mask) == 1L) {
      mask_uri <- wsi_image_data_uri(wsi_validate_input_path(mask), mime = "image/png")
    } else if (inherits(mask, "raster") || is.array(mask)) {
      mask_uri <- wsi_array_data_uri(mask)
    } else {
      wsi_abort("Masks must be image paths, rasters, or arrays.")
    }
  }

  list(
    label = source_label,
    width = unname(as.numeric(dims[["width"]])),
    height = unname(as.numeric(dims[["height"]])),
    image_data_uri = uri,
    rois = wsi_viewer_roi_features(roi),
    mask_data_uri = mask_uri
  )
}

wsi_compare_html <- function(config) {
  config_json <- jsonlite::toJSON(config, auto_unbox = TRUE, null = "null")
  paste0(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>", wsi_html_escape(config$title), "</title><style>",
    "html,body{margin:0;width:100%;height:100%;overflow:hidden;background:#111;color:#f1f1f1;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;}",
    ".top{position:fixed;left:12px;right:12px;top:12px;z-index:5;display:flex;gap:8px;align-items:center;}",
    ".panel{background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;backdrop-filter:blur(6px);}",
    ".spacer{flex:1}button{appearance:none;border:1px solid rgba(255,255,255,.24);background:#252525;color:#f2f2f2;border-radius:5px;padding:6px 9px;font-size:13px;}button.active{background:#0f766e;border-color:#5eead4;}",
    ".wrap{display:grid;grid-template-columns:1fr 1fr;width:100vw;height:100vh;}.pane{position:relative;border-right:1px solid rgba(255,255,255,.18);}.pane:last-child{border-right:0;}",
    "canvas{width:100%;height:100%;display:block;background:#101010;cursor:grab;}.label{position:absolute;left:12px;bottom:12px;background:rgba(0,0,0,.65);padding:6px 8px;border-radius:5px;font-size:12px;}",
    "#status{position:fixed;left:50%;bottom:12px;transform:translateX(-50%);font-size:12px;background:rgba(18,18,18,.86);border:1px solid rgba(255,255,255,.16);border-radius:6px;padding:8px 10px;}",
    "</style></head><body>",
    "<div class=\"top\"><div class=\"panel\"><strong>", wsi_html_escape(config$title), "</strong></div><div class=\"spacer\"></div>",
    "<div class=\"panel\"><button id=\"syncToggle\" class=\"", if (isTRUE(config$sync)) "active" else "", "\">Sync</button><button id=\"fit\">Fit</button><button id=\"oneToOne\">1:1</button></div></div>",
    "<div class=\"wrap\"><div class=\"pane\"><canvas id=\"canvas0\"></canvas><div class=\"label\">", wsi_html_escape(config$sources[[1L]]$label), "</div></div>",
    "<div class=\"pane\"><canvas id=\"canvas1\"></canvas><div class=\"label\">", wsi_html_escape(config$sources[[2L]]$label), "</div></div></div>",
    "<div id=\"status\">Loading comparison viewer...</div><script>",
    "const cfg=", config_json, ";const status=document.getElementById('status');let sync=!!cfg.sync;const panes=[0,1].map(i=>({canvas:document.getElementById('canvas'+i),ctx:null,img:new Image(),mask:null,scale:1,minScale:1,ox:0,oy:0,drag:false,lx:0,ly:0,cursor:null}));",
    "function clamp(v,a,b){return Math.max(a,Math.min(b,v));}function ptCanvas(pane,p){return{x:pane.ox+p.x*pane.scale,y:pane.oy+p.y*pane.scale};}",
    "function resizePane(p){const r=p.canvas.getBoundingClientRect(),dpr=devicePixelRatio||1;p.canvas.width=Math.floor(r.width*dpr);p.canvas.height=Math.floor(r.height*dpr);p.canvas.style.width=r.width+'px';p.canvas.style.height=r.height+'px';p.ctx=p.canvas.getContext('2d');p.ctx.setTransform(dpr,0,0,dpr,0,0);fitPane(p);}",
    "function fitPane(p){const r=p.canvas.getBoundingClientRect();p.minScale=Math.min(r.width/p.img.naturalWidth,r.height/p.img.naturalHeight);p.scale=p.minScale;p.ox=(r.width-p.img.naturalWidth*p.scale)/2;p.oy=(r.height-p.img.naturalHeight*p.scale)/2;}",
    "function fitAll(){panes.forEach(fitPane);drawAll();}function oneToOne(){panes.forEach(p=>{const r=p.canvas.getBoundingClientRect();p.scale=1;p.ox=(r.width-p.img.naturalWidth)/2;p.oy=(r.height-p.img.naturalHeight)/2;});drawAll();}",
    "function drawRois(p,rois){if(!rois||!rois.length)return;const ctx=p.ctx,s=cfg.sources[panes.indexOf(p)];ctx.save();ctx.lineWidth=2;rois.forEach(roi=>{if(!roi.rings)return;ctx.beginPath();roi.rings.forEach(r=>{r.forEach((q,j)=>{const c=ptCanvas(p,{x:q.x/s.width*p.img.naturalWidth,y:q.y/s.height*p.img.naturalHeight});if(j===0)ctx.moveTo(c.x,c.y);else ctx.lineTo(c.x,c.y);});ctx.closePath();});ctx.fillStyle=roi.fill||'rgba(20,184,166,.15)';ctx.strokeStyle=roi.colour||'#5eead4';ctx.fill('evenodd');ctx.stroke();});ctx.restore();}",
    "function drawCursor(p,frac){if(!frac)return;const r=p.canvas.getBoundingClientRect(),x=p.ox+frac.x*p.img.naturalWidth*p.scale,y=p.oy+frac.y*p.img.naturalHeight*p.scale;const ctx=p.ctx;ctx.save();ctx.strokeStyle='rgba(255,255,255,.6)';ctx.setLineDash([5,5]);ctx.beginPath();ctx.moveTo(x,0);ctx.lineTo(x,r.height);ctx.moveTo(0,y);ctx.lineTo(r.width,y);ctx.stroke();ctx.restore();}",
    "function draw(i){const p=panes[i],r=p.canvas.getBoundingClientRect();p.ctx.clearRect(0,0,r.width,r.height);p.ctx.fillStyle='#101010';p.ctx.fillRect(0,0,r.width,r.height);p.ctx.drawImage(p.img,p.ox,p.oy,p.img.naturalWidth*p.scale,p.img.naturalHeight*p.scale);if(p.mask){p.ctx.globalAlpha=.35;p.ctx.drawImage(p.mask,p.ox,p.oy,p.img.naturalWidth*p.scale,p.img.naturalHeight*p.scale);p.ctx.globalAlpha=1;}drawRois(p,cfg.sources[i].rois);const frac=panes.find(x=>x.cursor)?.cursor;drawCursor(p,frac);}function drawAll(){draw(0);draw(1);status.textContent=(sync?'synchronized':'independent')+' zoom/pan | linked cursor';}",
    "function pointer(p,e){const r=p.canvas.getBoundingClientRect();const x=e.clientX-r.left,y=e.clientY-r.top;return{x,y,fx:clamp((x-p.ox)/(p.img.naturalWidth*p.scale),0,1),fy:clamp((y-p.oy)/(p.img.naturalHeight*p.scale),0,1)};}",
    "panes.forEach((p,i)=>{p.img.onload=()=>{if(p.mask&& !p.mask.complete)return;resizePane(p);drawAll();};p.img.src=cfg.sources[i].image_data_uri;if(cfg.sources[i].mask_data_uri){p.mask=new Image();p.mask.src=cfg.sources[i].mask_data_uri;}p.canvas.onmousedown=e=>{p.drag=true;p.lx=e.clientX;p.ly=e.clientY;};window.addEventListener('mouseup',()=>p.drag=false);p.canvas.onmousemove=e=>{const q=pointer(p,e);panes.forEach(pp=>pp.cursor={x:q.fx,y:q.fy});if(p.drag){const dx=e.clientX-p.lx,dy=e.clientY-p.ly;p.lx=e.clientX;p.ly=e.clientY;if(sync){panes.forEach(pp=>{pp.ox+=dx;pp.oy+=dy;});}else{p.ox+=dx;p.oy+=dy;}}drawAll();};p.canvas.onwheel=e=>{e.preventDefault();const factor=e.deltaY<0?1.2:1/1.2;const q=pointer(p,e);const targets=sync?panes:[p];targets.forEach(pp=>{const r=pp.canvas.getBoundingClientRect(),cx=q.fx*pp.img.naturalWidth,cy=q.fy*pp.img.naturalHeight,screenX=sync?r.width*q.fx:e.clientX-pp.canvas.getBoundingClientRect().left,screenY=sync?r.height*q.fy:e.clientY-pp.canvas.getBoundingClientRect().top;pp.scale=clamp(pp.scale*factor,pp.minScale*.5,40);pp.ox=screenX-cx*pp.scale;pp.oy=screenY-cy*pp.scale;});drawAll();};});",
    "document.getElementById('syncToggle').onclick=()=>{sync=!sync;document.getElementById('syncToggle').classList.toggle('active',sync);drawAll();};document.getElementById('fit').onclick=fitAll;document.getElementById('oneToOne').onclick=oneToOne;window.addEventListener('resize',()=>panes.forEach(resizePane));",
    "</script></body></html>"
  )
}

#' Compare two pathology images side by side
#'
#' Creates a lightweight HTML viewer for comparing two slides or images. The
#' first implementation uses backend-generated thumbnails or placeholders for
#' metadata-only sources; it keeps zoom/pan synchronized by default and can show
#' ROI or mask overlays on either side.
#'
#' @param image1,image2 `wsi_slide` objects, image paths, rasters, or arrays.
#' @param sync Whether zoom and pan are synchronized.
#' @param roi1,roi2 Optional ROI overlays for the left and right image.
#' @param mask1,mask2 Optional mask overlays as image paths, rasters, or arrays.
#' @param output Optional HTML output path.
#' @param open Whether to open the viewer with [utils::browseURL()].
#' @param width Thumbnail width for slide inputs.
#' @param title Viewer title.
#' @param overwrite Whether to overwrite `output`.
#'
#' @return The HTML viewer path, invisibly.
#' @export
viewer_compare <- function(image1, image2, sync = TRUE, roi1 = NULL, roi2 = NULL,
                           mask1 = NULL, mask2 = NULL, output = NULL,
                           open = interactive(), width = 1200,
                           title = "wsiTools comparison viewer",
                           overwrite = FALSE) {
  width <- as.integer(wsi_check_scalar_number(width, "width", allow_zero = FALSE))
  if (is.null(output)) {
    output <- tempfile(fileext = ".html")
    overwrite <- TRUE
  }
  output <- wsi_validate_output_path(output, overwrite = overwrite)
  config <- list(
    title = title,
    sync = isTRUE(sync),
    sources = list(
      wsi_compare_source(image1, label = "Image 1", width = width, roi = roi1, mask = mask1),
      wsi_compare_source(image2, label = "Image 2", width = width, roi = roi2, mask = mask2)
    )
  )
  writeLines(wsi_compare_html(config), output, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(output)
  }
  invisible(output)
}

#' @rdname viewer_compare
#' @export
wsi_viewer_compare <- viewer_compare
