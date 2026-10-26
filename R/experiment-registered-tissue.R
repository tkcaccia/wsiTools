# Exact 0.5-µm tissue-frame attachments to a 0.25-µm full-slide cell project.
wsi_experiment_registered_tissue_validate <- function(item, root, cell_image, verify_hashes = TRUE) {
  if (!is.list(item) || is.null(item$registration) || is.null(item$image))
    wsi_abort("Registered tissue requires an exact sidecar and 0.5-µm image.")
  registration <- wsi_experiment_file(item$registration, root, "dual-MPP registration", TRUE)
  if (file.info(registration$resolved_path)$size > 2 * 1024^2)
    wsi_abort("Dual-MPP registration metadata exceeds its bounded size.")
  sidecar <- jsonlite::fromJSON(registration$resolved_path, simplifyVector = FALSE)
  if (!identical(sidecar$format, "cellphenotyper-dual-mpp-registration-v1") ||
      !identical(sidecar$verified, TRUE) ||
      !identical(sidecar$coordinate_convention, "pixel-edge-origin_centres-in-native-pixels") ||
      !grepl("^[a-f0-9]{64}$", sidecar$source_scene_sha256 %||% ""))
    wsi_abort("Verified dual-MPP source-scene registration is required.")
  tissue_image <- wsi_experiment_file(item$image, root, "registered 0.5-um tissue image", TRUE)
  same <- function(left, right, fields, label) {
    for (key in fields) if (is.null(left[[key]]) || is.null(right[[key]]) ||
        !isTRUE(all.equal(left[[key]], right[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort(paste("Dual-MPP", label, "differs:", key))
  }
  fields <- c("sha256", "width", "height", "mpp_x", "mpp_y", "frame_id",
              "coordinate_space", "coordinate_convention", "origin_x", "origin_y")
  same(sidecar$cell_full_image, cell_image, fields, "full cell image")
  same(sidecar$image, tissue_image, fields, "tissue image")
  width <- wsi_experiment_number(sidecar$image$width, "tissue width", integer = TRUE)
  height <- wsi_experiment_number(sidecar$image$height, "tissue height", integer = TRUE)
  cell_width <- wsi_experiment_number(cell_image$width, "full cell width", integer = TRUE)
  cell_height <- wsi_experiment_number(cell_image$height, "full cell height", integer = TRUE)
  sx <- cell_width / width; sy <- cell_height / height
  affine <- function(value, expected, label) {
    actual <- tryCatch(do.call(rbind, lapply(value, function(row) as.numeric(unlist(row, use.names = FALSE)))),
      error = function(error) NULL)
    if (!is.matrix(actual) || !identical(dim(actual), c(3L, 3L)) ||
        any(!is.finite(actual)) || !isTRUE(all.equal(actual, expected, tolerance = 1e-9, check.attributes = FALSE)))
      wsi_abort(paste("Dual-MPP", label, "differs from exact image dimensions/crop bounds."))
    actual
  }
  crop <- as.numeric(unlist(sidecar$cell_crop_bbox_in_cell_full_xyxy, use.names = FALSE))
  if (length(crop) != 4L || any(!is.finite(crop)) || any(crop != floor(crop)) ||
      crop[[1L]] < 0 || crop[[2L]] < 0 || crop[[3L]] > cell_width || crop[[4L]] > cell_height ||
      crop[[1L]] >= crop[[3L]] || crop[[2L]] >= crop[[4L]])
    wsi_abort("Dual-MPP prepared-cell crop bounds are invalid.")
  affine(sidecar$tissue_to_cell_full_affine,
    rbind(c(sx, 0, 0), c(0, sy, 0), c(0, 0, 1)), "tissue_to_cell_full_affine")
  affine(sidecar$tissue_to_cell_project_affine,
    rbind(c(sx, 0, -crop[[1L]]), c(0, sy, -crop[[2L]]), c(0, 0, 1)), "tissue_to_cell_project_affine")
  affine(sidecar$cell_project_to_tissue_affine,
    rbind(c(1 / sx, 0, crop[[1L]] / sx), c(0, 1 / sy, crop[[2L]] / sy), c(0, 0, 1)),
    "cell_project_to_tissue_affine")
  for (axis in c("x", "y")) {
    c_extent <- cell_image[[paste0(if (axis == "x") "width" else "height")]] * cell_image[[paste0("mpp_", axis)]]
    t_extent <- sidecar$image[[paste0(if (axis == "x") "width" else "height")]] * sidecar$image[[paste0("mpp_", axis)]]
    if (!isTRUE(all.equal(c_extent, t_extent, tolerance = 1e-6)))
      wsi_abort("Dual-MPP converted images do not share the same physical extent.")
  }
  assets <- item$assets %||% list()
  if (!is.list(assets) || length(assets) > 32L || (length(assets) && !is.null(names(assets))))
    wsi_abort("Registered tissue assets must be a bounded explicit array.")
  allowed <- list(virchow2_tiles = c("csv", "csv.gz"), kodama_map = c("csv", "csv.gz"),
    bora_geometry = "geojson")
  ids <- character()
  for (i in seq_along(assets)) {
    asset <- assets[[i]]
    if (!is.list(asset) || !is.character(asset$role) || length(asset$role) != 1L ||
        !is.character(asset$format) || length(asset$format) != 1L ||
        !asset$role %in% names(allowed))
      wsi_abort("Registered tissue asset has an unsupported role/format.")
    id <- wsi_experiment_text(asset$asset_id, "registered tissue asset_id")
    if (id %in% ids || !asset$format %in% allowed[[asset$role]] ||
        !identical(asset$frame_id, sidecar$image$frame_id) ||
        !identical(asset$image_sha256, sidecar$image$sha256))
      wsi_abort("Registered tissue asset has duplicate ID, unsupported role/format or wrong 0.5-um frame.")
    ids <- c(ids, id)
    assets[[i]] <- wsi_experiment_file(asset, root, paste("registered tissue", id), TRUE)
  }
  item$registration <- registration
  item$image <- tissue_image
  item$assets <- assets
  item$sidecar <- sidecar
  item
}

#' Query registered KODAMA points in the full 0.25-um cell frame.
#' Reads one bounded CSV chunk at a time; returns no feature vectors.
#' @export
wsi_experiment_registered_tissue_viewport <- function(experiment, tissue_id, asset_id, bbox, limit = 2000L) {
  experiment <- wsi_read_experiment(experiment)
  ids <- vapply(experiment$tissues, `[[`, character(1), "tissue_id")
  tissue <- experiment$tissues[[match(tissue_id, ids)]]
  if (is.null(tissue)) wsi_abort("Unknown registered tissue.")
  registered <- tissue$registered_tissue
  if (is.null(registered)) wsi_abort("Tissue has no verified dual-MPP registration.")
  assets <- Filter(function(asset) identical(asset$asset_id, asset_id) &&
    identical(asset$role, "kodama_map"), registered$assets)
  if (length(assets) != 1L) wsi_abort("Select one registered KODAMA map asset.")
  asset <- assets[[1L]]
  if (!identical(wsi_experiment_sha256(asset$resolved_path), tolower(asset$sha256)))
    wsi_abort("Registered KODAMA asset changed since Open Project; reopen a verified project.")
  box <- as.numeric(unlist(bbox[c("xmin", "ymin", "xmax", "ymax")], use.names = FALSE))
  if (length(box) != 4L || any(!is.finite(box)) || box[[1L]] >= box[[3L]] || box[[2L]] >= box[[4L]])
    wsi_abort("KODAMA viewport requires a finite ordered cell-frame bbox.")
  limit <- wsi_experiment_number(limit, "KODAMA viewport limit", integer = TRUE)
  if (limit > 2000L) wsi_abort("KODAMA viewport limit exceeds 2000 points.")
  open <- if (identical(asset$format, "csv.gz")) gzfile else file
  stream <- open(asset$resolved_path, "rt")
  on.exit(close(stream), add = TRUE)
  header <- readLines(stream, n = 1L, warn = FALSE)
  if (length(header) != 1L) wsi_abort("KODAMA CSV has no header.")
  names <- names(utils::read.csv(text = header, check.names = FALSE, nrows = 0L))
  keys <- c(asset$id_column, asset$x_column, asset$y_column,
    unlist(asset$dimension_columns, use.names = FALSE))
  if (length(keys) != 5L || anyDuplicated(keys) || !all(keys %in% names))
    wsi_abort("KODAMA map lacks its declared ID/native XY/reduction columns.")
  affine <- registered$sidecar$tissue_to_cell_full_affine
  sx <- affine[[1L]][[1L]]; sy <- affine[[2L]][[2L]]
  found <- list(); total <- 0L
  repeat {
    lines <- readLines(stream, n = 256L, warn = FALSE)
    if (!length(lines)) break
    table <- utils::read.csv(text = paste(c(header, lines), collapse = "\n"), check.names = FALSE,
      colClasses = "character", stringsAsFactors = FALSE)
    if (nrow(table) != length(lines)) wsi_abort("KODAMA CSV contains unsupported multiline records.")
    for (i in seq_len(nrow(table))) {
      x <- suppressWarnings(as.numeric(table[[asset$x_column]][[i]])) * sx
      y <- suppressWarnings(as.numeric(table[[asset$y_column]][[i]])) * sy
      if (!is.finite(x) || !is.finite(y)) wsi_abort("KODAMA map contains nonfinite tissue coordinates.")
      kodama_1 <- suppressWarnings(as.numeric(table[[keys[[4L]]]][[i]]))
      kodama_2 <- suppressWarnings(as.numeric(table[[keys[[5L]]]][[i]]))
      if (!is.finite(kodama_1) || !is.finite(kodama_2))
        wsi_abort("KODAMA map contains nonfinite reduction coordinates.")
      if (box[[1L]] <= x && x < box[[3L]] && box[[2L]] <= y && y < box[[4L]]) {
        total <- total + 1L
        if (length(found) < limit) found[[length(found) + 1L]] <- list(
          id = table[[asset$id_column]][[i]], x = x, y = y,
          kodama_1 = kodama_1, kodama_2 = kodama_2)
      }
    }
  }
  list(tissue_id = tissue_id, asset_id = asset_id, total = total, truncated = total > limit,
    points = found, coordinate_frame = tissue$image$frame_id,
    source_frame_id = registered$sidecar$image$frame_id,
    affine = registered$sidecar$tissue_to_cell_full_affine)
}

# Metadata only: the browser never receives a source image, a feature matrix,
# or the complete full-slide tissue population at Open Project.
wsi_experiment_registered_tissue_catalog <- function(experiment) {
  lapply(Filter(function(tissue) !is.null(tissue$registered_tissue), experiment$tissues), function(tissue) {
    registered <- tissue$registered_tissue
    list(tissue_id = tissue$tissue_id, cell_frame_id = tissue$image$frame_id,
      tissue_frame_id = registered$sidecar$image$frame_id,
      tissue_image_sha256 = registered$sidecar$image$sha256,
      registration_sha256 = registered$registration$sha256,
      source_scene_sha256 = registered$sidecar$source_scene_sha256,
      source_mpp = list(x = registered$sidecar$image$mpp_x, y = registered$sidecar$image$mpp_y),
      display_mpp = list(x = tissue$image$mpp_x, y = tissue$image$mpp_y),
      tissue_to_cell_full_affine = registered$sidecar$tissue_to_cell_full_affine,
      assets = lapply(registered$assets, function(asset) list(
        asset_id = asset$asset_id, role = asset$role, format = asset$format,
        sha256 = asset$sha256, row_count = asset$row_count %||% NULL,
        id_column = asset$id_column %||% NULL,
        dimension_columns = asset$dimension_columns %||% NULL)))
  })
}

wsi_experiment_registered_tissue_query <- function(experiment, tissue_id, asset_id, bbox,
                                                   limit = 2000L) {
  experiment <- wsi_read_experiment(experiment)
  tissue <- Filter(function(item) identical(item$tissue_id, tissue_id), experiment$tissues)
  if (length(tissue) != 1L || is.null(tissue[[1L]]$registered_tissue))
    wsi_abort("Unknown or unregistered tissue.")
  tissue <- tissue[[1L]]
  registered <- tissue$registered_tissue
  matches <- Filter(function(asset) identical(asset$asset_id, asset_id), registered$assets)
  if (length(matches) != 1L) wsi_abort("Select one registered tissue asset.")
  asset <- matches[[1L]]
  if (!identical(wsi_experiment_sha256(asset$resolved_path), tolower(asset$sha256)))
    wsi_abort("Registered tissue asset changed since Open Project; reopen the verified project.")
  box <- as.numeric(unlist(bbox[c("xmin", "ymin", "xmax", "ymax")], use.names = FALSE))
  if (length(box) != 4L || any(!is.finite(box)) || box[[1L]] >= box[[3L]] || box[[2L]] >= box[[4L]])
    wsi_abort("Registered tissue viewport requires a finite ordered cell-frame bbox.")
  limit <- wsi_experiment_number(limit, "registered tissue viewport limit", integer = TRUE)
  if (limit > 2000L) wsi_abort("Registered tissue viewport exceeds 2000 observations.")
  affine <- registered$sidecar$tissue_to_cell_full_affine
  sx <- affine[[1L]][[1L]]; sy <- affine[[2L]][[2L]]
  response <- list(tissue_id = tissue_id, asset_id = asset_id, role = asset$role,
    source_sha256 = asset$sha256, registration_sha256 = registered$registration$sha256,
    source_frame_id = registered$sidecar$image$frame_id,
    coordinate_frame = tissue$image$frame_id,
    source_mpp = list(x = registered$sidecar$image$mpp_x, y = registered$sidecar$image$mpp_y),
    affine = affine, bbox = as.list(stats::setNames(box, c("xmin", "ymin", "xmax", "ymax"))))
  if (asset$role == "kodama_map") {
    result <- wsi_experiment_registered_tissue_viewport(experiment, tissue_id, asset_id, bbox, limit)
    response$total <- result$total
    response$truncated <- result$truncated
    response$items <- result$points
    return(response)
  }
  if (asset$role == "virchow2_tiles") {
    open <- if (identical(asset$format, "csv.gz")) gzfile else file
    stream <- open(asset$resolved_path, "rt")
    on.exit(close(stream), add = TRUE)
    header <- readLines(stream, n = 1L, warn = FALSE)
    if (length(header) != 1L) wsi_abort("Virchow2 tile CSV has no header.")
    names <- names(utils::read.csv(text = header, check.names = FALSE, nrows = 0L))
    keys <- c(asset$id_column, asset$x_column, asset$y_column)
    if (length(keys) != 3L || anyDuplicated(keys) || !all(keys %in% names))
      wsi_abort("Virchow2 tile CSV lacks its declared native ID/X/Y columns.")
    items <- list(); total <- 0L
    repeat {
      lines <- readLines(stream, n = 256L, warn = FALSE)
      if (!length(lines)) break
      rows <- utils::read.csv(text = paste(c(header, lines), collapse = "\n"),
        check.names = FALSE, colClasses = "character", stringsAsFactors = FALSE)
      if (nrow(rows) != length(lines)) wsi_abort("Virchow2 tile CSV contains unsupported multiline records.")
      for (i in seq_len(nrow(rows))) {
        x <- suppressWarnings(as.numeric(rows[[asset$x_column]][[i]])) * sx
        y <- suppressWarnings(as.numeric(rows[[asset$y_column]][[i]])) * sy
        if (!is.finite(x) || !is.finite(y)) wsi_abort("Virchow2 tile has nonfinite native coordinates.")
        if (box[[1L]] <= x && x < box[[3L]] && box[[2L]] <= y && y < box[[4L]]) {
          total <- total + 1L
          if (length(items) < limit) items[[length(items) + 1L]] <- list(
            id = rows[[asset$id_column]][[i]], x = x, y = y)
        }
      }
    }
    response$total <- total
    response$truncated <- total > limit
    response$items <- items
    return(response)
  }
  # Bora GeoJSON remains the authoritative 0.5-um vector. Only intersecting
  # polygons are transformed for display, with an explicit complexity ceiling.
  if (file.info(asset$resolved_path)$size > 64 * 1024^2)
    wsi_abort("Bora GeoJSON exceeds the bounded viewer size; export an indexed derivative.")
  geo <- jsonlite::fromJSON(asset$resolved_path, simplifyVector = FALSE)
  if (!identical(geo$type, "FeatureCollection") || !is.list(geo$features))
    wsi_abort("Registered Bora geometry is not a GeoJSON FeatureCollection.")
  native_box <- box / c(sx, sy, sx, sy)
  selected <- list(); total <- 0L; vertices <- 0L
  scale_coordinates <- function(value) {
    if (is.list(value) && length(value) >= 2L && is.numeric(value[[1L]]) &&
        length(value[[1L]]) == 1L && is.numeric(value[[2L]]) && length(value[[2L]]) == 1L)
      return(list(value[[1L]] * sx, value[[2L]] * sy))
    lapply(value, scale_coordinates)
  }
  for (feature in geo$features) {
    if (!is.list(feature) || !feature$geometry$type %in% c("Polygon", "MultiPolygon"))
      wsi_abort("Registered Bora feature requires Polygon or MultiPolygon geometry.")
    points <- wsi_collect_points(feature$geometry$coordinates)
    if (!nrow(points) || any(!is.finite(points))) wsi_abort("Bora geometry has invalid coordinates.")
    if (max(points[, 1L]) < native_box[[1L]] || min(points[, 1L]) > native_box[[3L]] ||
        max(points[, 2L]) < native_box[[2L]] || min(points[, 2L]) > native_box[[4L]]) next
    total <- total + 1L
    if (total > min(limit, 128L)) next
    vertices <- vertices + nrow(points)
    if (vertices > 20000L) wsi_abort("Visible Bora geometry exceeds 20,000 vertices; zoom in to inspect a smaller region.")
    feature$geometry$coordinates <- scale_coordinates(feature$geometry$coordinates)
    feature$properties <- list(classification = feature$properties$classification %||%
      feature$properties$class %||% "Predicted Bora", locked = TRUE,
      source_type = "bora_predicted", tissue_annotation = TRUE)
    selected[[length(selected) + 1L]] <- feature
  }
  if (total > min(limit, 128L))
    wsi_abort("Visible Bora geometry exceeds 128 polygons; zoom in to inspect a smaller region.")
  transformed <- list(type = "FeatureCollection", features = selected)
  rois <- wsi_roi_from_geojson(transformed)
  items <- if (nrow(rois)) wsi_viewer_roi_features(rois) else list()
  response$total <- total
  response$truncated <- FALSE
  response$items <- lapply(items, function(item) {
    item$locked <- TRUE
    item$source_type <- "bora_predicted"
    item$source <- "Predicted Bora"
    item$coordinates <- NULL; item$geometry <- NULL; item$feature <- NULL
    item$properties <- list(predicted = TRUE, source_sha256 = asset$sha256,
      source_frame_id = registered$sidecar$image$frame_id)
    item
  })
  response
}
