remote_sha256 <- function(path) digest::digest(file = path, algo = "sha256")
remote_fail <- function(message) stop(message, call. = FALSE)

smooth_ring <- function(ring) {
  if (!is.matrix(ring) || ncol(ring) != 2L || nrow(ring) < 5L) remote_fail("Display smoothing requires closed 2D polygon rings.")
  points <- ring[-nrow(ring), , drop = FALSE]
  n <- nrow(points)
  if (n < 4L) remote_fail("Display smoothing requires at least four distinct ring points.")
  next_points <- points[c(2:n, 1L), , drop = FALSE]
  output <- matrix(0, nrow = 2L * n + 1L, ncol = 2L)
  output[seq(1L, 2L * n, by = 2L), ] <- .75 * points + .25 * next_points
  output[seq(2L, 2L * n, by = 2L), ] <- .25 * points + .75 * next_points
  output[nrow(output), ] <- output[1L, ]
  output
}

geometry_structure <- function(geometry) {
  if (inherits(geometry, "POLYGON")) return(list(type = "POLYGON", rings = length(geometry), parts = 1L))
  if (inherits(geometry, "MULTIPOLYGON")) return(list(type = "MULTIPOLYGON", rings = vapply(geometry, length, integer(1)), parts = length(geometry)))
  remote_fail("Display smoothing supports Polygon and MultiPolygon only.")
}

smooth_geometry <- function(geometry) {
  if (inherits(geometry, "POLYGON")) return(sf::st_polygon(lapply(geometry, smooth_ring)))
  if (inherits(geometry, "MULTIPOLYGON")) return(sf::st_multipolygon(lapply(geometry, function(polygon) lapply(polygon, smooth_ring))))
  remote_fail("Display smoothing supports Polygon and MultiPolygon only.")
}

make_display_contours <- function(exact_path, display_path, max_um, mpp_x, mpp_y) {
  if (!requireNamespace("sf", quietly = TRUE)) remote_fail("The sf R package is required for validated display smoothing.")
  original <- sf::st_read(exact_path, quiet = TRUE)
  # GeoJSON readers often assume EPSG:4326; these are explicitly level-0 pixels.
  sf::st_crs(original) <- NA
  if (!nrow(original) || !all(sf::st_is_valid(original))) remote_fail("Exact contours are empty or invalid; refusing smoothing.")
  original_geom <- sf::st_geometry(original)
  before <- lapply(original_geom, geometry_structure)
  display_geom <- sf::st_sfc(lapply(original_geom, smooth_geometry), crs = sf::st_crs(original))
  after <- lapply(display_geom, geometry_structure)
  if (!identical(before, after) || !all(sf::st_is_valid(display_geom)) || any(sf::st_is_empty(display_geom))) {
    remote_fail("Display smoothing changed polygon topology, holes, or component count.")
  }
  distances_px <- as.numeric(sf::st_distance(original_geom, display_geom, which = "Hausdorff", by_element = TRUE))
  displacement_um <- max(distances_px * max(mpp_x, mpp_y))
  if (!is.finite(displacement_um) || displacement_um > max_um) {
    remote_fail(sprintf("Display contour displacement %.3f um exceeds %.3f um.", displacement_um, max_um))
  }
  display <- original
  sf::st_geometry(display) <- display_geom
  sf::st_write(display, display_path, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  reread <- sf::st_read(display_path, quiet = TRUE)
  if (nrow(reread) != nrow(original) || !identical(before, lapply(sf::st_geometry(reread), geometry_structure))) {
    remote_fail("Display GeoJSON did not retain class/component structure.")
  }
  for (name in intersect(c("class", "assigned_group", "classification"), names(original))) {
    if (!identical(as.character(original[[name]]), as.character(reread[[name]]))) remote_fail("Display contour class identity changed.")
  }
  list(max_displacement_um = displacement_um, feature_count = nrow(display))
}

build_remote_viewer <- function(args, skill_dir, slide_path, geojson_path, out) {
  if (is.null(args[["source-identity-manifest"]])) remote_fail("--source-identity-manifest is required for source-only mode.")
  if (is.null(args[["qa-report"]])) remote_fail("--qa-report is required before source-only publication.")
  if (!requireNamespace("magick", quietly = TRUE) || !requireNamespace("digest", quietly = TRUE)) {
    remote_fail("Source-only mode requires the magick and digest R packages.")
  }
  if (any(c("tile-size", "tile-format", "quality") %in% names(args))) remote_fail("Deep Zoom tile options cannot be used with --remote-tiff-url.")
  preview_max <- suppressWarnings(as.integer(args[["preview-max-px"]] %||% "1600"))
  if (!is.finite(preview_max) || preview_max < 256L || preview_max > 4096L) remote_fail("--preview-max-px must be 256-4096.")
  rgb_resample <- args[["rgb-resample"]] %||% "bilinear"
  if (!rgb_resample %in% c("bilinear", "nearest")) remote_fail("--rgb-resample must be bilinear or nearest.")
  if (!identical(args[["label-resample"]] %||% "nearest", "nearest")) remote_fail("Label resampling is fixed to nearest.")
  contours <- args$contours %||% "exact"
  if (!contours %in% c("exact", "display-smoothed")) remote_fail("--contours must be exact or display-smoothed.")
  if (contours == "display-smoothed" && is.null(geojson_path)) remote_fail("Display smoothing requires --geojson.")
  max_displacement <- suppressWarnings(as.numeric(args[["max-displacement-um"]] %||% NA_real_))
  if (contours == "display-smoothed" && (!is.finite(max_displacement) || max_displacement <= 0)) {
    remote_fail("--max-displacement-um must be positive for display smoothing.")
  }
  if (isTRUE(args[["fragment-navigation"]]) && is.null(args[["display-tissue-mask"]])) remote_fail("Fragment navigation requires a display tissue mask.")
  qa_dir <- normalizePath(args[["qa-report"]], winslash = "/", mustWork = FALSE)
  if (file.exists(qa_dir)) remote_fail("--qa-report must name a new directory.")
  identity_path <- normalizePath(args[["source-identity-manifest"]], winslash = "/", mustWork = TRUE)
  identity <- jsonlite::read_json(identity_path, simplifyVector = FALSE)
  if (!is.list(identity) || !identical(identity$annotation_approved, TRUE) && !is.null(geojson_path)) {
    remote_fail("Source identity must declare annotation_approved: true for GeoJSON publication.")
  }
  if (!is.null(args[["display-tissue-mask"]]) && !identical(identity$mask_approved, TRUE)) {
    remote_fail("Source identity must declare mask_approved: true for display-mask publication.")
  }
  landmarks <- identity$landmarks
  if (is.null(landmarks) || length(landmarks) < 2L) remote_fail("Source identity needs at least two reviewed registration landmarks for QA.")
  if (!is.list(landmarks) || !all(vapply(landmarks, is.list, logical(1)))) remote_fail("Landmarks must be a JSON array of objects.")
  for (landmark in landmarks) {
    a <- as.numeric(unlist(landmark$annotation_xy)); b <- as.numeric(unlist(landmark$source_xy))
    if (length(a) != 2L || length(b) != 2L || any(!is.finite(c(a, b))) || sqrt(sum((a - b)^2)) > 3) {
      remote_fail("Registration landmark is invalid or displaced by more than 3 source pixels.")
    }
  }

  stage <- tempfile(pattern = ".wsi-tissue-source-", tmpdir = dirname(out))
  dir.create(stage)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)
  source_check <- file.path(stage, "source-check.json")
  command <- c("--slide", slide_path, "--identity", identity_path,
               "--url", args[["remote-tiff-url"]], "--out", source_check)
  result <- suppressWarnings(system2("python3", c(shQuote(file.path(skill_dir, "scripts", "check_source.py")), shQuote(command)), stdout = TRUE, stderr = TRUE))
  if (!identical(attr(result, "status") %||% 0L, 0L)) remote_fail(paste(result, collapse = "\n"))

  slide <- wsiTools::wsi_open(slide_path, backend = args$backend %||% "auto")
  on.exit(wsiTools::wsi_close(slide), add = TRUE)
  info <- wsiTools::wsi_info(slide)
  width <- as.numeric(info$dimensions[["width"]]); height <- as.numeric(info$dimensions[["height"]])
  if (!isTRUE(all.equal(c(width, height), as.numeric(unlist(identity[c("width", "height")]))))) remote_fail("Local slide dimensions differ from source identity.")
  target_width <- max(1L, as.integer(floor(preview_max * width / max(width, height))))
  preview <- wsiTools::wsi_thumbnail(slide, width = target_width, format = "raster")
  preview_path <- file.path(stage, "preview.jpg")
  magick::image_write(magick::image_read(preview), path = preview_path, format = "jpeg", quality = 88)
  preview_info <- magick::image_info(magick::image_read(preview_path))
  preview_width <- preview_info$width[[1L]]; preview_height <- preview_info$height[[1L]]
  if (max(preview_width, preview_height) > preview_max) remote_fail("Preview exceeds --preview-max-px.")

  mask <- NULL
  if (!is.null(args[["display-tissue-mask"]])) {
    mask_path <- normalizePath(args[["display-tissue-mask"]], winslash = "/", mustWork = TRUE)
    if (tolower(tools::file_ext(mask_path)) != "png") remote_fail("Display tissue mask must be a PNG.")
    sidecar_path <- paste0(mask_path, ".json")
    if (!file.exists(sidecar_path)) remote_fail("Display mask needs FILE.json source-identity sidecar.")
    mask <- jsonlite::read_json(sidecar_path, simplifyVector = TRUE)
    mask_info <- magick::image_info(magick::image_read(mask_path))
    if (!identical(mask$coordinate_system, "preview_pixel_xy") ||
        !identical(mask$source_sha256, identity$source_sha256) ||
        !isTRUE(all.equal(c(mask$source_width, mask$source_height), c(width, height))) ||
        !isTRUE(all.equal(c(mask$width, mask$height), c(preview_width, preview_height))) ||
        !isTRUE(all.equal(c(mask_info$width[[1L]], mask_info$height[[1L]]), c(preview_width, preview_height))) ||
        !identical(mask$mask_sha256, remote_sha256(mask_path))) remote_fail("Display mask dimensions, coordinates, source identity, or SHA256 differ.")
    if (!file.copy(mask_path, file.path(stage, "display-mask.png"))) remote_fail("Could not copy display tissue mask.")
  }

  roi <- NULL; classes <- list(); display_info <- NULL
  if (!is.null(geojson_path)) {
    roi <- wsiTools::wsi_read_geojson(geojson_path)
    if (!nrow(roi)) remote_fail("GeoJSON has no features.")
    intersects <- is.finite(roi$xmin) & is.finite(roi$ymin) & is.finite(roi$xmax) & is.finite(roi$ymax) &
      roi$xmax >= 0 & roi$ymax >= 0 & roi$xmin <= width & roi$ymin <= height
    if (!all(intersects)) remote_fail("Every exact GeoJSON feature must intersect the source dimensions.")
    if (!file.copy(geojson_path, file.path(stage, "annotations.geojson"))) remote_fail("Could not copy exact GeoJSON.")
    labels <- trimws(as.character(roi$class)); labels[is.na(labels) | !nzchar(labels)] <- "annotation"
    palette <- c("#C23B52", "#167D69", "#315FA8", "#9458A8", "#B47520", "#0B8291")
    for (j in seq_along(unique(labels))) {
      name <- unique(labels)[[j]]
      classes[[j]] <- list(name = name, color = palette[[((j - 1L) %% length(palette)) + 1L]])
    }
    if (contours == "display-smoothed") {
      display_info <- make_display_contours(geojson_path, file.path(stage, "display-contours.geojson"),
                                            max_displacement, identity$mpp_x, identity$mpp_y)
    }
  }

  asset_dir <- file.path(skill_dir, "assets")
  for (name in c("index.html", "viewer.css", "viewer.js", "remote.js")) {
    if (!file.copy(file.path(asset_dir, name), file.path(stage, name))) remote_fail(paste("Could not copy", name))
  }
  html_path <- file.path(stage, "index.html")
  html <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  html <- sub("<!--REMOTE_TIFF_SCRIPT-->", '<script defer src="vendor/geotiff.js"></script>\n  <script defer src="remote.js"></script>', html, fixed = TRUE)
  writeLines(html, html_path, useBytes = TRUE)
  dir.create(file.path(stage, "vendor"))
  for (name in c("geotiff.js", "LICENSE")) {
    if (!file.copy(file.path(asset_dir, "vendor", name), file.path(stage, "vendor", name))) remote_fail(paste("Could not copy", name))
  }
  manifest <- list(
    schema_version = 2L, mode = "remote-tiff", title = args$title %||% tools::file_path_sans_ext(basename(slide_path)),
    image = list(url = args[["remote-tiff-url"]], width = width, height = height,
                 mpp_x = identity$mpp_x, mpp_y = identity$mpp_y,
                 preview = "preview.jpg", preview_width = preview_width, preview_height = preview_height,
                 source_sha256 = identity$source_sha256, remote_sha256_verified = FALSE,
                 rgb_resample = rgb_resample, label_resample = "nearest"),
    source_identity = list(coordinate_system = "level0_pixel_xy", registration = identity$registration,
                           publish_origin = identity$publish_origin, remote_byte_length = file.info(slide_path)$size,
                           publication_approved = TRUE, preview_approved = TRUE),
    overlay = if (is.null(roi)) NULL else list(exact_url = "annotations.geojson",
      display_url = if (is.null(display_info)) NULL else "display-contours.geojson",
      feature_count = nrow(roi), classes = classes, opacity = 0.18,
      contours = contours, max_displacement_um = if (is.null(display_info)) NULL else display_info$max_displacement_um,
      approved_for_publication = TRUE),
    display_mask = if (is.null(mask)) NULL else list(url = "display-mask.png", coordinate_system = "preview_pixel_xy",
      width = preview_width, height = preview_height, source_sha256 = identity$source_sha256,
      fragment_navigation = isTRUE(args[["fragment-navigation"]]), label_resample = "nearest", approved_for_publication = TRUE),
    show_tile_progress = isTRUE(args[["show-tile-progress"]]),
    qa = list(landmarks = identity$landmarks, required = TRUE),
    integrity = list()
  )
  assets <- c("index.html", "viewer.css", "viewer.js", "remote.js", "vendor/geotiff.js", "vendor/LICENSE",
              "preview.jpg", "source-check.json")
  if (!is.null(roi)) assets <- c(assets, "annotations.geojson")
  if (!is.null(display_info)) assets <- c(assets, "display-contours.geojson")
  if (!is.null(mask)) assets <- c(assets, "display-mask.png")
  for (name in assets) manifest$integrity[[name]] <- remote_sha256(file.path(stage, name))
  jsonlite::write_json(manifest, file.path(stage, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null")
  jsonlite::write_json(list(kind = "wsi-tissue-viewer", version = 2L), file.path(stage, "wsi-tissue-viewer.json"), auto_unbox = TRUE, pretty = TRUE)

  verify <- suppressWarnings(system2("python3", c(shQuote(file.path(skill_dir, "scripts", "verify_bundle.py")), shQuote(stage)), stdout = TRUE, stderr = TRUE))
  if (!identical(attr(verify, "status") %||% 0L, 0L)) remote_fail(paste(verify, collapse = "\n"))
  qa_args <- c("--bundle", stage, "--out", qa_dir)
  qa <- suppressWarnings(system2("python3", c(shQuote(file.path(skill_dir, "scripts", "qa_viewer.py")), shQuote(qa_args)), stdout = TRUE, stderr = TRUE))
  if (!identical(attr(qa, "status") %||% 0L, 0L)) remote_fail(paste(qa, collapse = "\n"))
  verify_final <- suppressWarnings(system2("python3", c(shQuote(file.path(skill_dir, "scripts", "verify_bundle.py")), shQuote(stage),
    "--qa-report", shQuote(qa_dir)), stdout = TRUE, stderr = TRUE))
  if (!identical(attr(verify_final, "status") %||% 0L, 0L)) remote_fail(paste(verify_final, collapse = "\n"))

  if (dir.exists(out)) {
    backup <- tempfile(pattern = ".wsi-tissue-backup-", tmpdir = dirname(out))
    if (!file.rename(out, backup)) remote_fail("Could not move existing viewer before replacement.")
    if (!file.rename(stage, out)) { file.rename(backup, out); remote_fail("Could not install new viewer; old viewer restored.") }
    unlink(backup, recursive = TRUE)
  } else if (!file.rename(stage, out)) remote_fail("Could not move completed viewer into output directory.")
  cat("Source-only viewer: ", file.path(out, "index.html"), "\n", sep = "")
  cat("Browser QA: ", file.path(qa_dir, "report.json"), "\n", sep = "")
}
