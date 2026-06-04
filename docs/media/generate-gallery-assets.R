`%||%` <- function(x, y) if (is.null(x)) y else x

# Generate lightweight screenshot-style assets for the GitHub documentation.
# The images are synthetic and contain no patient data or large image files.

script_file <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
media_dir <- if (is.null(script_file)) {
  file.path(getwd(), "docs", "media")
} else {
  dirname(normalizePath(script_file, mustWork = FALSE))
}
if (!dir.exists(media_dir)) dir.create(media_dir, recursive = TRUE)

asset_path <- function(name) file.path(media_dir, name)

open_png <- function(name, width = 1200, height = 760) {
  grDevices::png(asset_path(name), width = width, height = height, bg = "white", res = 120)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1200), ylim = c(0, 760), asp = 1)
}

close_png <- function() grDevices::dev.off()

col_alpha <- function(col, alpha = 0.5) grDevices::adjustcolor(col, alpha.f = alpha)

rescale01 <- function(x) {
  rng <- range(x, finite = TRUE)
  if (!all(is.finite(rng)) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}

rounded_rect <- function(x0, y0, x1, y1, col, border = NA, r = 10, lwd = 1) {
  # Base R has no rounded rectangle primitive; this intentionally keeps cards
  # square enough for documentation screenshots.
  graphics::rect(x0, y0, x1, y1, col = col, border = border, lwd = lwd)
}

draw_button <- function(x, y, label, w = 78, h = 32, active = FALSE) {
  fill <- if (active) "#2563EB" else "#F8FAFC"
  border <- if (active) "#1D4ED8" else "#CBD5E1"
  text <- if (active) "white" else "#0F172A"
  rounded_rect(x, y, x + w, y + h, fill, border)
  graphics::text(x + w / 2, y + h / 2, label, cex = 0.75, col = text, font = 2)
}

draw_shell <- function(title, subtitle = "", left = TRUE, right = TRUE) {
  graphics::rect(0, 0, 1200, 760, col = "#EEF2F7", border = NA)
  graphics::rect(0, 704, 1200, 760, col = "#0F172A", border = NA)
  graphics::text(28, 733, "wsiTools", adj = c(0, 0.5), col = "white", cex = 1.25, font = 2)
  graphics::text(150, 733, title, adj = c(0, 0.5), col = "#E2E8F0", cex = 0.95)
  draw_button(870, 716, "Project", 82, 28)
  draw_button(960, 716, "Annotations", 112, 28)
  draw_button(1080, 716, "Stains", 78, 28, active = grepl("mIHC|stain", title, ignore.case = TRUE))
  graphics::rect(222, 34, 1048, 690, col = "#FDF7F7", border = "#CBD5E1")
  if (left) {
    rounded_rect(18, 34, 206, 690, col_alpha("#FFFFFF", 0.96), "#CBD5E1")
    graphics::text(36, 666, "Project", adj = c(0, 0.5), cex = 0.95, font = 2, col = "#0F172A")
    graphics::text(36, 638, "Annotations", adj = c(0, 0.5), cex = 0.8, font = 2, col = "#334155")
    graphics::segments(30, 622, 194, 622, col = "#CBD5E1")
  }
  if (right) {
    rounded_rect(1064, 250, 1182, 506, col_alpha("#FFFFFF", 0.96), "#CBD5E1")
    draw_button(1084, 462, "+", 34, 30)
    draw_button(1126, 462, "-", 34, 30)
    draw_button(1084, 424, "Fit", 76, 30)
    draw_button(1084, 386, "1:1", 76, 30)
    graphics::text(1123, 352, "scale", cex = 0.75, col = "#475569")
  }
  graphics::rect(540, 48, 730, 72, col = col_alpha("#FFFFFF", 0.88), border = "#94A3B8")
  graphics::segments(572, 60, 697, 60, lwd = 3, col = "#0F172A")
  graphics::text(635, 84, "500 um", cex = 0.75, col = "#0F172A")
  if (nzchar(subtitle)) graphics::text(238, 674, subtitle, adj = c(0, 0.5), col = "#475569", cex = 0.78)
}

draw_tissue <- function(seed = 1, x0 = 260, y0 = 100, x1 = 1000, y1 = 650, alpha = 1) {
  set.seed(seed)
  graphics::rect(x0, y0, x1, y1, col = "#FFF8F8", border = NA)
  for (i in seq_len(32)) {
    cx <- stats::runif(1, x0 + 60, x1 - 60)
    cy <- stats::runif(1, y0 + 50, y1 - 50)
    rx <- stats::runif(1, 80, 210)
    ry <- stats::runif(1, 35, 105)
    theta <- seq(0, 2 * pi, length.out = 120)
    wobble <- 1 + 0.12 * sin(theta * sample(2:5, 1) + stats::runif(1, 0, pi))
    col <- sample(c("#E9A5B6", "#D58AAE", "#F1C0C5", "#C981A7", "#EDB4A8"), 1)
    graphics::polygon(
      cx + rx * cos(theta) * wobble,
      cy + ry * sin(theta) * wobble,
      col = col_alpha(col, 0.16 * alpha),
      border = NA
    )
  }
  outline_x <- c(338, 420, 560, 706, 865, 970, 1012, 950, 780, 592, 424, 300, 338)
  outline_y <- c(228, 474, 626, 648, 584, 438, 278, 158, 112, 134, 166, 192, 228)
  graphics::polygon(outline_x, outline_y, col = col_alpha("#F3B2C4", 0.24 * alpha), border = "#B85C87", lwd = 2)
}

roi_poly <- function(x, y, col, border, label = NULL) {
  graphics::polygon(x, y, col = col_alpha(col, 0.28), border = border, lwd = 3)
  if (!is.null(label)) graphics::text(mean(range(x)), mean(range(y)), label, col = border, cex = 0.85, font = 2)
}

draw_full_resolution <- function() {
  open_png("full-resolution-viewer.png")
  draw_shell("Full-resolution tiled viewer", "OpenSeadragon tiles with scale bar and navigator")
  draw_tissue(2)
  for (x in seq(300, 980, by = 120)) graphics::segments(x, 112, x, 648, col = col_alpha("#FFFFFF", 0.25), lwd = 1)
  for (y in seq(160, 620, by = 100)) graphics::segments(300, y, 1012, y, col = col_alpha("#FFFFFF", 0.25), lwd = 1)
  graphics::rect(1076, 268, 1168, 348, col = "#F8FAFC", border = "#94A3B8")
  graphics::polygon(c(1084, 1102, 1138, 1160, 1152, 1120, 1088), c(292, 330, 338, 316, 282, 276, 284), col = "#F3B2C4", border = "#B85C87")
  graphics::rect(1108, 296, 1140, 326, border = "#2563EB", lwd = 2)
  close_png()
}

draw_annotation_panel <- function() {
  open_png("annotation-panel.png")
  draw_shell("Annotation manager", "ROI list, classes, brush size, visibility, lock, delete")
  draw_tissue(4)
  roi_poly(c(390, 560, 650, 540, 410), c(260, 310, 450, 530, 430), "#E11D48", "#BE123C", "tumour")
  roi_poly(c(680, 860, 930, 830, 700), c(220, 260, 420, 540, 470), "#2563EB", "#1D4ED8", "stroma")
  y <- 590
  entries <- data.frame(cls = c("tumour", "stroma", "necrosis", "exclusion"), col = c("#E11D48", "#2563EB", "#7C3AED", "#6B7280"))
  for (i in seq_len(nrow(entries))) {
    graphics::rect(34, y - 28 * i, 184, y - 28 * i + 20, col = "#F8FAFC", border = "#CBD5E1")
    graphics::rect(42, y - 28 * i + 5, 54, y - 28 * i + 17, col = entries$col[i], border = NA)
    graphics::text(62, y - 28 * i + 11, entries$cls[i], adj = c(0, 0.5), cex = 0.7, col = "#0F172A")
  }
  graphics::text(36, 454, "Brush size: 32 px", adj = c(0, 0.5), cex = 0.72, col = "#334155")
  graphics::rect(36, 430, 184, 438, col = "#E2E8F0", border = NA)
  graphics::rect(36, 428, 96, 440, col = "#2563EB", border = NA)
  close_png()
}

draw_geojson <- function() {
  open_png("geojson-overlay.png")
  draw_shell("QuPath GeoJSON round trip", "Imported polygons preserve class, colour, object id, and label")
  draw_tissue(7)
  roi_poly(c(342, 506, 634, 590, 408), c(248, 210, 340, 506, 516), "#E11D48", "#BE123C", "Tumour 1")
  roi_poly(c(650, 856, 930, 800, 642), c(200, 240, 390, 576, 482), "#22C55E", "#15803D", "Stroma 2")
  roi_poly(c(504, 620, 662, 574), c(396, 408, 498, 548), "#6B7280", "#374151", "Exclude")
  graphics::text(36, 590, "GeoJSON", adj = c(0, 0.5), cex = 0.85, font = 2)
  graphics::text(36, 556, "3 annotations", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 530, "QuPath compatible", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 504, "Export selected ROIs", adj = c(0, 0.5), cex = 0.72)
  close_png()
}

draw_mihc <- function() {
  open_png("mihc-channel-overlay.png")
  draw_shell("H&E + mIHC channel overlay", "Per-channel visibility, colour, opacity, and registered overlays")
  draw_tissue(8, alpha = 0.75)
  set.seed(8)
  for (col in c("#3B82F6", "#22C55E", "#F97316", "#E11D48", "#A855F7")) {
    x <- stats::runif(80, 340, 940)
    y <- stats::runif(80, 170, 590)
    graphics::points(x, y, pch = 16, cex = stats::runif(80, 0.45, 1.4), col = col_alpha(col, 0.55))
  }
  graphics::text(36, 590, "Stains", adj = c(0, 0.5), cex = 0.85, font = 2)
  labels <- c("H&E base", "DAPI", "CD3", "PanCK", "CD68")
  cols <- c("#B85C87", "#3B82F6", "#22C55E", "#E11D48", "#A855F7")
  for (i in seq_along(labels)) {
    yy <- 560 - i * 28
    graphics::rect(38, yy, 52, yy + 14, col = cols[i], border = NA)
    graphics::text(62, yy + 7, labels[i], adj = c(0, 0.5), cex = 0.7)
  }
  close_png()
}

draw_spots <- function() {
  open_png("spatial-spots.png")
  draw_shell("Seurat / SpatialExperiment spots", "Spatial spots, gene colouring, and reduction plot selection")
  draw_tissue(11)
  xs <- rep(seq(420, 880, length.out = 6), 5)
  ys <- rep(seq(240, 540, length.out = 5), each = 6)
  vals <- seq(0, 1, length.out = length(xs))
  cols <- grDevices::colorRampPalette(c("#1D4ED8", "#FACC15", "#DC2626"))(length(xs))
  graphics::points(xs, ys, pch = 21, bg = cols[rank(vals)], col = "#111827", cex = 1.55, lwd = 1)
  graphics::rect(738, 402, 1000, 648, col = col_alpha("#FFFFFF", 0.94), border = "#94A3B8")
  graphics::text(758, 626, "PCA plot", adj = c(0, 0.5), cex = 0.8, font = 2)
  px <- 766 + 210 * rescale01(sin(seq(0, 2 * pi, length.out = length(xs))))
  py <- 438 + 160 * rescale01(cos(seq(0, 2 * pi, length.out = length(xs))))
  graphics::points(px, py, pch = 16, col = cols[rank(vals)], cex = 0.75)
  graphics::polygon(c(820, 900, 945, 860), c(500, 570, 500, 465), border = "#0F172A", lwd = 2, col = NA)
  graphics::text(36, 590, "Spatial", adj = c(0, 0.5), cex = 0.85, font = 2)
  graphics::text(36, 558, "Gene: DemoGeneA", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 532, "Colour by gene", adj = c(0, 0.5), cex = 0.72)
  close_png()
}

draw_cellphenotyper <- function() {
  open_png("cellphenotyper-cells.png")
  draw_shell("CellPhenotyper cells", "Cell segmentation overlay, GigaTIME channels, and KODAMA groups")
  draw_tissue(12)
  set.seed(12)
  cls <- sample(c("#38BDF8", "#F97316", "#22C55E", "#E11D48"), 260, replace = TRUE)
  graphics::points(stats::runif(260, 340, 950), stats::runif(260, 170, 590), pch = 16, col = col_alpha(cls, 0.78), cex = 0.55)
  graphics::rect(738, 416, 1000, 648, col = col_alpha("#FFFFFF", 0.94), border = "#94A3B8")
  graphics::text(758, 626, "CellPhenotyper plot", adj = c(0, 0.5), cex = 0.75, font = 2)
  graphics::points(stats::runif(120, 766, 970), stats::runif(120, 446, 596), pch = 16, col = col_alpha(sample(c("#38BDF8", "#F97316", "#22C55E", "#E11D48"), 120, TRUE), 0.85), cex = 0.55)
  graphics::text(36, 590, "Cells", adj = c(0, 0.5), cex = 0.85, font = 2)
  graphics::text(36, 558, "Show segmentation", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 532, "Load cell mask", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 506, "KODAMA groups", adj = c(0, 0.5), cex = 0.72)
  close_png()
}

draw_tile_grid <- function() {
  open_png("tile-grid.png")
  draw_shell("Tile grid preview", "Preview ML tiles before extraction and save a manifest")
  draw_tissue(16)
  for (x in seq(340, 860, by = 104)) {
    for (y in seq(190, 514, by = 104)) {
      graphics::rect(x, y, x + 96, y + 96, col = col_alpha("#FACC15", 0.18), border = "#CA8A04", lwd = 1.5)
    }
  }
  graphics::text(36, 590, "Tile export", adj = c(0, 0.5), cex = 0.85, font = 2)
  graphics::text(36, 558, "tile_size = 512", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 532, "stride = 512", adj = c(0, 0.5), cex = 0.72)
  graphics::text(36, 506, "save manifest CSV", adj = c(0, 0.5), cex = 0.72)
  close_png()
}

draw_live_sync <- function() {
  open_png("live-r-synchronization.png")
  graphics::rect(0, 0, 1200, 760, col = "#EEF2F7", border = NA)
  rounded_rect(44, 90, 680, 690, "#FFFFFF", "#CBD5E1")
  rounded_rect(734, 90, 1156, 690, "#0B1220", "#334155")
  graphics::rect(44, 642, 680, 690, col = "#0F172A", border = NA)
  graphics::text(72, 666, "Browser viewer", adj = c(0, 0.5), col = "white", cex = 1.1, font = 2)
  draw_tissue(19, x0 = 82, y0 = 140, x1 = 638, y1 = 608)
  roi_poly(c(180, 340, 422, 330, 206), c(260, 245, 372, 520, 450), "#E11D48", "#BE123C", "ROI")
  graphics::text(758, 650, "R session", adj = c(0, 0.5), col = "#E2E8F0", cex = 1.1, font = 2)
  console <- c(
    "> viewer <- wsi_viewer_live(slide)",
    "> viewer$open()",
    "> viewer$get_rois()",
    "  id        class   area_px",
    "  roi_001   tumour  184392",
    "> viewer$get_measurements()"
  )
  for (i in seq_along(console)) graphics::text(768, 610 - i * 42, console[i], adj = c(0, 0.5), col = "#D1FAE5", family = "mono", cex = 0.78)
  graphics::arrows(680, 444, 734, 444, length = 0.12, lwd = 3, col = "#2563EB")
  graphics::arrows(734, 364, 680, 364, length = 0.12, lwd = 3, col = "#16A34A")
  graphics::text(707, 468, "httpuv", col = "#1E293B", cex = 0.8, font = 2)
  graphics::text(707, 340, "events", col = "#1E293B", cex = 0.8, font = 2)
  close_png()
}

draw_gif_frame <- function(name, step) {
  open_png(name, width = 900, height = 520)
  graphics::rect(0, 0, 900, 520, col = "#EEF2F7", border = NA)
  graphics::rect(0, 470, 900, 520, col = "#0F172A", border = NA)
  graphics::text(28, 495, "Draw ROI -> retrieve in R", adj = c(0, 0.5), col = "white", cex = 1.1, font = 2)
  graphics::rect(40, 48, 548, 446, col = "#FFF8F8", border = "#CBD5E1")
  draw_tissue(21 + step, x0 = 72, y0 = 90, x1 = 516, y1 = 414)
  pts_x <- c(160, 280, 400, 360, 210, 160)
  pts_y <- c(170, 132, 226, 342, 310, 170)
  n <- min(length(pts_x), max(1, step + 1))
  if (step == 1) graphics::points(pts_x[1], pts_y[1], pch = 21, bg = "#E11D48", cex = 1.5)
  if (step > 1 && step < 5) {
    graphics::lines(pts_x[seq_len(n)], pts_y[seq_len(n)], col = "#E11D48", lwd = 4)
    graphics::points(pts_x[seq_len(n)], pts_y[seq_len(n)], pch = 21, bg = "#FCA5A5", cex = 1.2)
  }
  if (step >= 5) roi_poly(pts_x, pts_y, "#E11D48", "#BE123C", "Tumour ROI")
  graphics::rect(588, 48, 860, 446, col = "#0B1220", border = "#334155")
  lines <- switch(
    as.character(step),
    "1" = c("> viewer <- wsi_viewer_live(slide)", "> viewer$open()", "", "# draw first point..."),
    "2" = c("> viewer <- wsi_viewer_live(slide)", "> viewer$open()", "", "# drawing polygon..."),
    "3" = c("> viewer <- wsi_viewer_live(slide)", "> viewer$open()", "", "# closing ROI..."),
    "4" = c("> viewer <- wsi_viewer_live(slide)", "> viewer$open()", "", "event: roi_created"),
    c("> rois <- viewer$get_rois()", "> rois[, c('id','class','area')]", "  roi_001 tumour 184392", "> write_geojson(rois, 'roi.geojson')")
  )
  for (i in seq_along(lines)) graphics::text(610, 410 - i * 42, lines[i], adj = c(0, 0.5), col = "#D1FAE5", family = "mono", cex = 0.72)
  close_png()
}

draw_full_resolution()
draw_annotation_panel()
draw_geojson()
draw_mihc()
draw_spots()
draw_cellphenotyper()
draw_tile_grid()
draw_live_sync()

frame_dir <- file.path(tempdir(), "wsiTools-roi-roundtrip-frames")
if (!dir.exists(frame_dir)) dir.create(frame_dir, recursive = TRUE)
frame_files <- file.path(frame_dir, sprintf("frame-%02d.png", 1:5))
old_media_dir <- media_dir
media_dir <- frame_dir
for (i in seq_along(frame_files)) draw_gif_frame(basename(frame_files[[i]]), i)
media_dir <- old_media_dir

magick <- Sys.which("magick")
if (nzchar(magick)) {
  args <- c("-delay", "85", "-loop", "0", frame_files, asset_path("roi-roundtrip.gif"))
  status <- system2(magick, args = args)
  if (!identical(status, 0L)) warning("ImageMagick failed to create roi-roundtrip.gif")
} else {
  warning("ImageMagick `magick` command not found; GIF was not generated.")
}

message("Gallery assets written to: ", media_dir)
