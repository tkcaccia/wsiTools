# Faster H&E deconvolution viewer example
#
# This workflow is smoother than live dynamic stain tiles because the base
# Deep Zoom tiles are precomputed once and reused. The viewer is served through
# localhost so the browser can read canvas pixels for H&E stain selection.

library(wsiTools)

slide_path <- Sys.getenv(
  "WSITOOLS_HE_SLIDE",
  "/Users/stefano/Downloads/AP-GY-26-04_HE.svs"
)

out_dir <- Sys.getenv(
  "WSITOOLS_HE_VIEWER_DIR",
  "/Users/stefano/Documents/viewer/he_estimated_fast_viewer"
)

if (!file.exists(slide_path)) {
  stop("Slide not found: ", slide_path, call. = FALSE)
}
if (!requireNamespace("httpuv", quietly = TRUE)) {
  stop("Install the optional package `httpuv` to serve the viewer through localhost.", call. = FALSE)
}

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

slide <- wsi_open(slide_path)
on.exit(wsi_close(slide), add = TRUE)

message("Estimating H&E stain vectors from a low-resolution thumbnail.")
he_channels <- wsi_estimate_he_stain_channels(
  slide,
  method = "macenko",
  thumbnail_width = 2048,
  max_pixels = 20000
)

print(he_channels)

html_file <- file.path(out_dir, "he_estimated_stains.html")
tile_dir <- file.path(out_dir, "he_estimated_stains_tiles")

message("Creating or reusing Deep Zoom tiles. First run may take a while; later runs reuse the tiles.")
wsi_viewer_he(
  slide,
  mode = "tiles",
  channels = he_channels,
  output = html_file,
  tile_dir = tile_dir,
  rebuild = FALSE,
  overwrite = TRUE,
  open = FALSE
)

content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    html = "text/html; charset=UTF-8",
    htm = "text/html; charset=UTF-8",
    js = "application/javascript",
    css = "text/css",
    json = "application/json",
    dzi = "application/xml",
    xml = "application/xml",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    png = "image/png",
    tif = "image/tiff",
    tiff = "image/tiff",
    "application/octet-stream"
  )
}

http_response <- function(status, body, type = "text/plain; charset=UTF-8") {
  if (is.character(body)) {
    body <- charToRaw(paste(body, collapse = "\n"))
  }
  list(
    status = as.integer(status),
    headers = list(
      "Content-Type" = type,
      "Cache-Control" = "no-cache",
      "Access-Control-Allow-Origin" = "*"
    ),
    body = body
  )
}

static_app <- function(root) {
  force(root)
  list(call = function(req) {
    path <- req$PATH_INFO
    if (is.null(path) || !nzchar(path)) {
      path <- "/"
    }
    path <- utils::URLdecode(path)
    path <- sub("^/+", "", path)
    if (!nzchar(path)) {
      path <- basename(html_file)
    }
    file <- normalizePath(file.path(root, path), mustWork = FALSE)
    root_norm <- normalizePath(root, mustWork = TRUE)
    if (!startsWith(file, root_norm) || !file.exists(file) || dir.exists(file)) {
      return(http_response(404, "Not found"))
    }
    size <- file.info(file)$size
    body <- readBin(file, what = "raw", n = size)
    http_response(200, body, content_type(file))
  })
}

find_port <- function(start = 8890, tries = 50) {
  for (port in seq.int(start, start + tries)) {
    server <- try(httpuv::startServer("127.0.0.1", port, list(call = function(req) {
      http_response(200, "ok")
    })), silent = TRUE)
    if (!inherits(server, "try-error")) {
      httpuv::stopServer(server)
      return(port)
    }
  }
  stop("Could not find a free localhost port.", call. = FALSE)
}

port <- find_port()
server <- httpuv::startServer("127.0.0.1", port, static_app(out_dir))
url <- sprintf("http://127.0.0.1:%d/%s", port, basename(html_file))

message("Viewer ready: ", url)
message("Press Esc/Ctrl+C in R to stop the local viewer server.")
utils::browseURL(url)

repeat {
  httpuv::service(100)
}
