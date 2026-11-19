#!/usr/bin/env Rscript

# Export an overview or bounded tissue crop with exact level-0 GeoJSON geometry.
fail <- function(message) stop(message, call. = FALSE)
`%||%` <- function(x, y) if (is.null(x)) y else x

usage <- function() {
  cat(paste(
    "Usage: Rscript render_figure.R --slide SLIDE --out FIGURE.png [options]",
    "  --geojson FILE                Exact level-0 GeoJSON (optional)",
    "  --x N --y N --width N --height N   Level-0 crop (omit all for overview)",
    "  --pixels N                   Maximum image side, 256-8192 (default: 2400)",
    "  --opacity N                  Polygon fill opacity, 0-0.65 (default: 0.18)",
    "  --force                      Replace existing PNG",
    sep = "\n"
  ), "\n")
}

parse_args <- function(args) {
  allowed <- c("slide", "out", "geojson", "x", "y", "width", "height", "pixels", "opacity")
  result <- list()
  i <- 1L
  while (i <= length(args)) {
    token <- args[[i]]
    if (token == "--help") { usage(); quit(save = "no", status = 0L) }
    if (token == "--force") { result$force <- TRUE; i <- i + 1L; next }
    if (!startsWith(token, "--") || !substring(token, 3L) %in% allowed) fail(paste("Unknown option:", token))
    if (i == length(args) || startsWith(args[[i + 1L]], "--")) fail(paste("Missing value for", token))
    result[[substring(token, 3L)]] <- args[[i + 1L]]
    i <- i + 2L
  }
  for (key in c("slide", "out")) if (is.null(result[[key]])) fail(paste("Missing --", key, sep = ""))
  result
}

numeric_arg <- function(value, label, default = NULL) {
  if (is.null(value)) return(default)
  result <- suppressWarnings(as.numeric(value))
  if (length(result) != 1L || !is.finite(result)) fail(paste("Invalid --", label, sep = ""))
  result
}

draw_line <- function(points, color, weight) {
  if (length(points) < 2L) return()
  x <- vapply(points, function(point) as.numeric(point[[1L]]), numeric(1))
  y <- vapply(points, function(point) as.numeric(point[[2L]]), numeric(1))
  if (all(is.finite(x)) && all(is.finite(y))) graphics::lines(x, y, col = color, lwd = weight)
}

draw_polygon <- function(rings, color, opacity, weight) {
  xs <- ys <- numeric()
  for (ring in rings) {
    if (length(ring) < 3L) next
    x <- vapply(ring, function(point) as.numeric(point[[1L]]), numeric(1))
    y <- vapply(ring, function(point) as.numeric(point[[2L]]), numeric(1))
    if (all(is.finite(x)) && all(is.finite(y))) {
      xs <- c(xs, x, NA_real_)
      ys <- c(ys, y, NA_real_)
    }
  }
  if (!length(xs)) return()
  xs <- xs[-length(xs)]
  ys <- ys[-length(ys)]
  graphics::polypath(xs, ys, col = grDevices::adjustcolor(color, alpha.f = opacity), border = NA, rule = "evenodd")
  for (ring in rings) draw_line(ring, color, weight)
}

draw_feature <- function(feature, color, opacity, weight, radius) {
  geometry <- feature$geometry %||% list()
  type <- geometry$type %||% ""
  coords <- geometry$coordinates
  if (is.null(coords)) return()
  if (type == "Polygon") draw_polygon(coords, color, opacity, weight)
  else if (type == "MultiPolygon") for (polygon in coords) draw_polygon(polygon, color, opacity, weight)
  else if (type == "LineString") draw_line(coords, color, weight)
  else if (type == "MultiLineString") for (line in coords) draw_line(line, color, weight)
  else if (type %in% c("Point", "MultiPoint")) {
    points <- if (type == "Point") list(coords) else coords
    for (point in points) graphics::symbols(as.numeric(point[[1L]]), as.numeric(point[[2L]]),
                                           circles = radius, inches = FALSE, add = TRUE,
                                           bg = color, fg = color)
  }
}

args <- parse_args(commandArgs(trailingOnly = TRUE))
if (!requireNamespace("wsiTools", quietly = TRUE)) fail("Install wsiTools first.")
if (!requireNamespace("magick", quietly = TRUE)) fail("The optional magick R package is required for PNG export.")
if (!requireNamespace("jsonlite", quietly = TRUE)) fail("The jsonlite R package is required.")
slide_path <- normalizePath(args$slide, mustWork = TRUE)
geojson_path <- if (!is.null(args$geojson)) normalizePath(args$geojson, mustWork = TRUE) else NULL
out <- normalizePath(args$out, mustWork = FALSE)
if (tolower(tools::file_ext(out)) != "png") fail("--out must end in .png")
if (!dir.exists(dirname(out))) fail("Output parent directory does not exist.")
if (file.exists(out) && !isTRUE(args$force)) fail("Output PNG exists. Use --force to replace it.")
pixels <- numeric_arg(args$pixels, "pixels", 2400)
opacity <- numeric_arg(args$opacity, "opacity", 0.18)
if (pixels < 256 || pixels > 8192 || pixels != floor(pixels)) fail("--pixels must be an integer from 256 to 8192.")
if (opacity < 0 || opacity > 0.65) fail("--opacity must be 0-0.65.")
crop_keys <- c("x", "y", "width", "height")
has_crop <- vapply(crop_keys, function(key) !is.null(args[[key]]), logical(1))
if (any(has_crop) && !all(has_crop)) fail("Supply all four crop options: --x --y --width --height.")

slide <- wsiTools::wsi_open(slide_path)
on.exit(wsiTools::wsi_close(slide), add = TRUE)
info <- wsiTools::wsi_info(slide)
slide_w <- as.numeric(info$dimensions[["width"]])
slide_h <- as.numeric(info$dimensions[["height"]])

if (all(has_crop)) {
  x <- numeric_arg(args$x, "x"); y <- numeric_arg(args$y, "y")
  width <- numeric_arg(args$width, "width"); height <- numeric_arg(args$height, "height")
  if (x < 0 || y < 0 || width <= 0 || height <= 0 || x + width > slide_w || y + height > slide_h) fail("Crop must lie within the slide.")
  levels <- wsiTools::wsi_levels(slide)
  target_ds <- max(width, height) / pixels
  eligible <- which(is.finite(levels$downsample) & levels$downsample >= target_ds / 1.15)
  chosen <- if (length(eligible)) eligible[[which.min(levels$downsample[eligible])]] else which.max(levels$downsample)
  ds <- as.numeric(levels$downsample[[chosen]])
  read_w <- as.integer(floor(width / ds)); read_h <- as.integer(floor(height / ds))
  if (read_w < 1L || read_h < 1L || max(read_w, read_h) > 8192L) fail("Crop is too large for available pyramid levels. Choose a smaller crop or an overview.")
  raster <- wsiTools::wsi_read_region(slide, x = x, y = y, width = read_w, height = read_h,
                                     level = levels$level[[chosen]], format = "raster")
  extent <- c(x, y, x + read_w * ds, y + read_h * ds)
} else {
  raster <- wsiTools::wsi_thumbnail(slide, width = as.integer(min(pixels, slide_w)), format = "raster")
  extent <- c(0, 0, slide_w, slide_h)
}
image_h <- nrow(raster); image_w <- ncol(raster)
if (image_w < 1L || image_h < 1L) fail("wsiTools did not return a valid image.")

roi <- NULL
geojson <- NULL
colors <- list()
palette <- c("#C23B52", "#167D69", "#315FA8", "#9458A8", "#B47520", "#0B8291")
if (!is.null(geojson_path)) {
  roi <- wsiTools::wsi_read_geojson(geojson_path)
  geojson <- jsonlite::fromJSON(geojson_path, simplifyVector = FALSE)
  if (length(geojson$features) != nrow(roi)) fail("GeoJSON feature count changed during import.")
  classes <- trimws(as.character(roi$class))
  classes[is.na(classes) | !nzchar(classes)] <- "annotation"
  unique_classes <- unique(classes)
  for (j in seq_along(unique_classes)) {
    label <- unique_classes[[j]]
    color <- palette[[((j - 1L) %% length(palette)) + 1L]]
    if ("color" %in% names(roi)) for (candidate in as.character(roi$color[classes == label])) {
      valid <- tryCatch({ grDevices::col2rgb(candidate); TRUE }, error = function(e) FALSE)
      if (isTRUE(valid)) { color <- candidate; break }
    }
    colors[[tolower(label)]] <- color
  }
}

temp <- tempfile(pattern = ".tissue-figure-", tmpdir = dirname(out), fileext = ".png")
on.exit(unlink(temp), add = TRUE)
grDevices::png(temp, width = image_w, height = image_h, units = "px", res = 144)
device_open <- TRUE
on.exit(if (device_open) grDevices::dev.off(), add = TRUE)
graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
graphics::plot.new()
graphics::plot.window(xlim = extent[c(1, 3)], ylim = extent[c(4, 2)])
graphics::rasterImage(raster, extent[[1]], extent[[4]], extent[[3]], extent[[2]], interpolate = FALSE)

if (!is.null(geojson)) {
  view_w <- extent[[3]] - extent[[1]]
  weight <- max(0.7, min(3, image_w / 1100))
  radius <- 3 * view_w / image_w
  for (i in seq_along(geojson$features)) {
    if (roi$xmax[[i]] < extent[[1]] || roi$xmin[[i]] > extent[[3]] ||
        roi$ymax[[i]] < extent[[2]] || roi$ymin[[i]] > extent[[4]]) next
    color <- colors[[tolower(classes[[i]])]] %||% palette[[1L]]
    draw_feature(geojson$features[[i]], color, opacity, weight, radius)
  }
}

mpp <- as.numeric(info$mpp[["x"]])
if (length(mpp) && is.finite(mpp) && mpp > 0) {
  view_w <- extent[[3]] - extent[[1]]
  view_h <- extent[[4]] - extent[[2]]
  target_um <- view_w * 0.12 * mpp
  step <- 10^floor(log10(target_um))
  options <- c(1, 2, 5, 10) * step
  length_um <- max(options[options <= target_um])
  length_slide <- length_um / mpp
  sx <- extent[[1]] + view_w * 0.04
  sy <- extent[[2]] + view_h * 0.93
  graphics::rect(sx - view_w * 0.015, sy - view_h * 0.04,
                 sx + length_slide + view_w * 0.015, sy + view_h * 0.04,
                 col = grDevices::adjustcolor("white", alpha.f = 0.86), border = NA)
  graphics::segments(sx, sy, sx + length_slide, sy, col = "#17372F", lwd = 2)
  label <- if (length_um >= 1000) paste0(signif(length_um / 1000, 2), " mm") else paste0(signif(length_um, 2), " µm")
  graphics::text(sx + length_slide / 2, sy - view_h * 0.02, labels = label, cex = 0.72, col = "#17372F", font = 2)
}

grDevices::dev.off(); device_open <- FALSE
if (file.exists(out)) unlink(out)
if (!file.rename(temp, out)) fail("Could not write output PNG.")
cat("Figure: ", out, "\nPixels: ", image_w, " × ", image_h, "\n", sep = "")
