#!/usr/bin/env Rscript

usage <- function() {
  cat(paste(
    "Usage: Rscript build_viewer.R --slide SLIDE --out DIRECTORY [options]",
    "Options:",
    "  --geojson FILE          Level-0 slide-pixel GeoJSON (optional)",
    "  --title TEXT            Viewer title (default: slide filename)",
    "  --backend NAME          wsi_open backend (default: auto)",
    "  --tile-size INTEGER     Deep Zoom tile size (default: 512)",
    "  --tile-format jpg|png   Tile format (default: jpg)",
    "  --quality INTEGER       JPEG quality, 1-100 (default: 92)",
    "  --remote-tiff-url URL   Use source-only publication mode instead of Deep Zoom tiles",
    "  --source-identity-manifest FILE  Reviewed local/remote source identity JSON",
    "  --preview-max-px N      Longest preview edge, 256-4096 (default: 1600)",
    "  --display-tissue-mask FILE  Preview-pixel PNG with FILE.json identity sidecar",
    "  --rgb-resample bilinear|nearest  Remote RGB sampling (default: bilinear)",
    "  --label-resample nearest  Fixed label sampling (default: nearest)",
    "  --contours exact|display-smoothed  Viewer contour layer (default: exact)",
    "  --max-displacement-um N  Required bound for display smoothing",
    "  --fragment-navigation   Navigate disconnected display-mask fragments",
    "  --show-tile-progress    Show source tile loading indicator",
    "  --qa-report DIR         Required source-only visual QA output directory",
    "  --force                 Replace an output made by this builder",
    sep = "\n"
  ), "\n")
}

fail <- function(message) stop(message, call. = FALSE)
`%||%` <- function(x, y) if (is.null(x)) y else x

parse_args <- function(args) {
  allowed <- c("slide", "out", "geojson", "title", "backend", "tile-size", "tile-format", "quality",
               "remote-tiff-url", "source-identity-manifest", "preview-max-px", "display-tissue-mask",
               "rgb-resample", "label-resample", "contours", "max-displacement-um", "qa-report")
  result <- list()
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (token == "--help") { usage(); quit(save = "no", status = 0L) }
    if (token %in% c("--force", "--fragment-navigation", "--show-tile-progress")) {
      result[[substring(token, 3L)]] <- TRUE; i <- i + 1L; next
    }
    if (!startsWith(token, "--") || !substring(token, 3L) %in% allowed) fail(paste("Unknown option:", token))
    if (i == length(args) || startsWith(args[[i + 1L]], "--")) fail(paste("Missing value for", token))
    result[[substring(token, 3L)]] <- args[[i + 1L]]
    i <- i + 2L
  }
  for (key in c("slide", "out")) if (is.null(result[[key]])) fail(paste("Missing --", key, sep = ""))
  result
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (!requireNamespace("wsiTools", quietly = TRUE)) fail("Install the wsiTools R package first.")
if (utils::packageVersion("wsiTools") < "0.1.28") fail("wsiTools 0.1.28 or newer is required.")
if (!requireNamespace("jsonlite", quietly = TRUE)) fail("The jsonlite R package is required.")
if (is.null(args[["remote-tiff-url"]]) && !nzchar(Sys.which("vips"))) fail("libvips is required to build Deep Zoom tiles.")

slide_path <- normalizePath(args$slide, winslash = "/", mustWork = TRUE)
geojson_path <- if (!is.null(args$geojson)) normalizePath(args$geojson, winslash = "/", mustWork = TRUE) else NULL
out <- normalizePath(args$out, winslash = "/", mustWork = FALSE)
if (identical(out, "/") || identical(out, normalizePath(getwd(), winslash = "/"))) fail("Choose a dedicated output directory.")
parent <- dirname(out)
if (!dir.exists(parent)) fail(paste("Output parent does not exist:", parent))
if (file.exists(out) && !dir.exists(out)) fail("Output path is an existing file.")
if (dir.exists(out) && !isTRUE(args$force)) fail("Output directory exists. Use --force to replace a viewer made by this builder.")
if (dir.exists(out) && isTRUE(args$force)) {
  marker_file <- file.path(out, "wsi-tissue-viewer.json")
  marker <- if (file.exists(marker_file)) tryCatch(jsonlite::read_json(marker_file), error = function(e) NULL) else NULL
  if (!identical(marker$kind, "wsi-tissue-viewer")) fail("Refusing --force: output directory was not made by this builder.")
}

tile_size <- suppressWarnings(as.integer(args[["tile-size"]] %||% "512"))
quality <- suppressWarnings(as.integer(args$quality %||% "92"))
tile_format <- args[["tile-format"]] %||% "jpg"
backend <- args$backend %||% "auto"
if (!is.finite(tile_size) || tile_size < 128L || tile_size > 2048L) fail("--tile-size must be 128-2048.")
if (!is.finite(quality) || quality < 1L || quality > 100L) fail("--quality must be 1-100.")
if (!tile_format %in% c("jpg", "png")) fail("--tile-format must be jpg or png.")
if (!backend %in% c("auto", "openslide", "vips", "native_czi", "bioformats", "omezarr", "imagemagick")) fail("Invalid --backend.")

script_arg <- grep("^--file=", commandArgs(), value = TRUE)
if (!length(script_arg)) fail("Run this file with Rscript.")
skill_dir <- dirname(dirname(normalizePath(sub("^--file=", "", script_arg[[1L]]), winslash = "/", mustWork = TRUE)))
asset_dir <- file.path(skill_dir, "assets")

if (!is.null(args[["remote-tiff-url"]])) {
  source(file.path(skill_dir, "scripts", "build_remote.R"), local = TRUE)
  build_remote_viewer(args, skill_dir, slide_path, geojson_path, out)
  quit(save = "no", status = 0L)
}
remote_options <- c("source-identity-manifest", "preview-max-px", "display-tissue-mask", "rgb-resample",
                    "label-resample", "contours", "max-displacement-um", "fragment-navigation",
                    "show-tile-progress", "qa-report")
if (any(remote_options %in% names(args))) fail("Remote TIFF options require --remote-tiff-url.")

slide <- wsiTools::wsi_open(slide_path, backend = backend)
on.exit(wsiTools::wsi_close(slide), add = TRUE)
info <- wsiTools::wsi_info(slide)
width <- as.numeric(info$dimensions[["width"]])
height <- as.numeric(info$dimensions[["height"]])
if (!is.finite(width) || !is.finite(height) || width <= 0 || height <= 0) fail("Invalid slide dimensions.")

roi <- NULL
classes <- list()
if (!is.null(geojson_path)) {
  roi <- wsiTools::wsi_read_geojson(geojson_path)
  if (!nrow(roi)) fail("GeoJSON has no features.")
  intersects <- is.finite(roi$xmin) & is.finite(roi$ymin) & is.finite(roi$xmax) & is.finite(roi$ymax) &
    roi$xmax >= 0 & roi$ymax >= 0 & roi$xmin <= width & roi$ymin <= height
  if (!any(intersects)) fail("No GeoJSON feature intersects the slide. Check coordinate registration.")
  if (any(!intersects)) warning(sum(!intersects), " GeoJSON feature(s) lie outside the slide.")
  labels <- trimws(as.character(roi$class))
  labels[is.na(labels) | !nzchar(labels)] <- "annotation"
  palette <- c("#C23B52", "#167D69", "#315FA8", "#9458A8", "#B47520", "#0B8291")
  unique_labels <- unique(labels)
  for (j in seq_along(unique_labels)) {
    label <- unique_labels[[j]]
    color <- palette[[((j - 1L) %% length(palette)) + 1L]]
    if ("color" %in% names(roi)) {
      candidates <- as.character(roi$color[labels == label])
      for (candidate in candidates) {
        valid <- tryCatch({ grDevices::col2rgb(candidate); TRUE }, error = function(e) FALSE)
        if (isTRUE(valid)) { color <- candidate; break }
      }
    }
    classes[[j]] <- list(name = label, color = color)
  }
}

stage <- tempfile(pattern = ".wsi-tissue-viewer-", tmpdir = parent)
dir.create(stage)
on.exit(unlink(stage, recursive = TRUE), add = TRUE)
tile_dir <- file.path(stage, "tiles")
reference_html <- file.path(stage, "wsiTools-reference.html")
invisible(wsiTools::wsi_viewer(
  slide, mode = "tiles", output = reference_html, tile_dir = tile_dir,
  tile_size = tile_size, tile_format = tile_format, quality = quality,
  title = args$title %||% basename(slide_path), open = FALSE,
  overwrite = TRUE, rebuild = FALSE
))
if (!file.exists(file.path(tile_dir, "slide.dzi")) || !dir.exists(file.path(tile_dir, "slide_files"))) {
  fail("wsiTools did not create the expected Deep Zoom tiles.")
}
unlink(reference_html)
unlink(file.path(tile_dir, "slide.wsiTools.json"))
for (name in c("index.html", "viewer.css", "viewer.js")) {
  if (!file.copy(file.path(asset_dir, name), file.path(stage, name))) fail(paste("Could not copy", name))
}
if (!is.null(geojson_path) && !file.copy(geojson_path, file.path(stage, "annotations.geojson"))) fail("Could not copy GeoJSON.")

mpp <- as.numeric(info$mpp[c("x", "y")])
manifest <- list(
  schema_version = 1L,
  title = args$title %||% tools::file_path_sans_ext(basename(slide_path)),
  image = list(dzi = "tiles/slide.dzi", width = width, height = height,
               mpp_x = if (is.finite(mpp[[1L]]) && mpp[[1L]] > 0) mpp[[1L]] else NULL,
               mpp_y = if (is.finite(mpp[[2L]]) && mpp[[2L]] > 0) mpp[[2L]] else NULL),
  overlay = if (is.null(geojson_path)) NULL else list(url = "annotations.geojson", feature_count = nrow(roi), classes = classes, opacity = 0.18)
)
jsonlite::write_json(manifest, file.path(stage, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
jsonlite::write_json(list(kind = "wsi-tissue-viewer", version = 1L), file.path(stage, "wsi-tissue-viewer.json"), auto_unbox = TRUE, pretty = TRUE)

if (dir.exists(out)) {
  backup <- tempfile(pattern = ".wsi-tissue-backup-", tmpdir = parent)
  if (!file.rename(out, backup)) fail("Could not move existing viewer before replacement.")
  if (!file.rename(stage, out)) {
    file.rename(backup, out)
    fail("Could not install new viewer; old viewer restored.")
  }
  unlink(backup, recursive = TRUE)
} else if (!file.rename(stage, out)) {
  fail("Could not move completed viewer into output directory.")
}
cat("Viewer: ", file.path(out, "index.html"), "\n", sep = "")
cat("Slide: ", width, " × ", height, " level-0 pixels\n", sep = "")
if (!is.null(roi)) cat("GeoJSON features: ", nrow(roi), " (exact copy)\n", sep = "")
cat("Serve over HTTP for preview; browsers block local file fetches.\n")
