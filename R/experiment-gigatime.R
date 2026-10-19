# Typed GigaTIME artifacts remain separate from Virchow predictors and human labels.
wsi_experiment_gigatime_semantics <- "uncalibrated_virtual_marker_score"

wsi_experiment_gigatime_array <- function(value, label) {
  if (!is.list(value) || !length(value) || !is.null(names(value)) ||
      any(!vapply(value, is.list, logical(1)))) wsi_abort(paste(label, "must be a nonempty array of records."))
  value
}

wsi_experiment_gigatime_affine <- function(value) {
  if (is.list(value)) value <- tryCatch(do.call(rbind, lapply(value, unlist, use.names = FALSE)), error = function(e) NULL)
  if (!is.matrix(value) || !is.numeric(value) || !identical(dim(value), c(3L, 3L)) ||
      any(!is.finite(value)) || any(abs(value[3, ] - c(0, 0, 1)) > 1e-10) || abs(det(value)) < 1e-12)
    wsi_abort("GigaTIME source_to_image_affine must be an invertible finite 3x3 affine with final row [0,0,1].")
  value
}

wsi_experiment_gigatime_validate <- function(experiment) {
  result <- list(images = list(), masks = list(), scores = list())
  roles <- c("gigatime_marker_image", "cell_segmentation_mask", "gigatime_cell_scores")
  for (i in seq_along(experiment$tissues)) {
    tissue <- experiment$tissues[[i]]
    artifacts <- tissue$artifacts %||% list()
    selected <- Filter(function(a) isTRUE(a$role %in% roles), artifacts)
    if (!length(selected)) next
    all_ids <- vapply(artifacts, function(a) a$artifact_id %||% "", character(1))
    ids <- vapply(selected, function(a) wsi_experiment_text(a$artifact_id, "GigaTIME artifact_id"), character(1))
    if (anyDuplicated(ids) || any(vapply(ids, function(id) sum(all_ids == id) != 1L, logical(1))))
      wsi_abort("GigaTIME artifact IDs must be unique within a tissue.")
    resolved <- list()
    for (a in selected) {
      if (!identical(a$frame_id, tissue$image$frame_id) || !identical(a$image_sha256, tolower(tissue$image$sha256)))
        wsi_abort("GigaTIME artifact H&E image SHA-256/frame differs from its tissue.")
      a <- wsi_experiment_file(a, experiment$root, paste("GigaTIME", a$artifact_id), TRUE)
      if (a$role != "cell_segmentation_mask" && !identical(a$score_semantics, wsi_experiment_gigatime_semantics))
        wsi_abort("GigaTIME artifacts require score_semantics='uncalibrated_virtual_marker_score'.")
      if (!is.null(a$value_encoding) && !identical(a$value_encoding, "native_export_values"))
        wsi_abort("GigaTIME value_encoding must be native_export_values; no silent value conversion is supported.")
      if (a$role == "gigatime_marker_image") {
        if (!identical(a$format, "ome-tiff")) wsi_abort("GigaTIME marker images require format='ome-tiff'.")
        channels <- wsi_experiment_gigatime_array(a$channels, "GigaTIME channels")
        markers <- vapply(channels, function(channel) wsi_experiment_text(channel$marker, "channel.marker"), character(1))
        pages <- vapply(channels, function(channel) wsi_experiment_number(channel$page, "channel.page", FALSE, TRUE), numeric(1))
        if (anyDuplicated(markers) || anyDuplicated(pages) || any(pages < 0))
          wsi_abort("GigaTIME channels require unique markers and unique nonnegative zero-based pages.")
        extent <- a$extent
        if (!is.list(extent) || !setequal(names(extent), c("x", "y", "width", "height")))
          wsi_abort("GigaTIME image extent must explicitly declare native H&E x/y/width/height edges.")
        for (key in c("x", "y", "width", "height")) wsi_experiment_number(extent[[key]], paste("extent", key), key %in% c("width", "height"))
        if (extent$x < 0 || extent$y < 0 || extent$x + extent$width > tissue$image$width + 1e-8 ||
            extent$y + extent$height > tissue$image$height + 1e-8)
          wsi_abort("GigaTIME image extent lies outside its declared native H&E image.")
        if (wsi_has_vips()) {
          properties <- wsi_vips_properties(a$resolved_path)
          page_count <- suppressWarnings(as.numeric(properties[["n-pages"]] %||% 1))
          if (any(pages >= page_count)) wsi_abort("GigaTIME channel page exceeds the actual TIFF page count.")
          description <- wsi_vips_field(a$resolved_path, "image-description", collapse = TRUE)
          actual_markers <- wsi_ome_channel_names(a$resolved_path, pages = seq_len(page_count) - 1L)
          if (is.na(description) || !grepl("<(?:[A-Za-z_][A-Za-z0-9_.-]*:)?OME\\b", description, perl = TRUE) ||
              !identical(actual_markers[pages + 1L], markers))
            wsi_abort("GigaTIME marker names/pages must match the explicit OME channel identities.")
          size <- function(key) {
            match <- regmatches(description, regexec(paste0("\\b", key, "=['\"]([0-9]+)['\"]"), description, perl = TRUE))[[1L]]
            if (length(match) != 2L) NA_real_ else as.numeric(match[[2L]])
          }
          pixel_blocks <- regmatches(description, gregexpr("<(?:[A-Za-z_][A-Za-z0-9_.-]*:)?Pixels\\b", description, perl = TRUE))[[1L]]
          if (length(pixel_blocks) != 1L || !identical(size("SizeZ"), 1) || !identical(size("SizeT"), 1) ||
              !identical(size("SizeC"), page_count))
            wsi_abort("GigaTIME OME-TIFF must contain one channel-only image series with singleton Z/T axes.")
          first_dimensions <- wsi_vips_image_dimensions(a$resolved_path, page = pages[[1]])
          a$source_width <- unname(first_dimensions[["width"]]); a$source_height <- unname(first_dimensions[["height"]])
          a$display_max <- numeric(length(pages))
          for (j in seq_along(pages)) {
            page <- pages[[j]]
            bands <- suppressWarnings(as.numeric(wsi_vips_field(paste0(a$resolved_path, "[page=", page, "]"), "bands")))
            if (!is.finite(bands) || bands != 1) wsi_abort("Each GigaTIME marker TIFF page must contain exactly one scalar band.")
            dimensions <- wsi_vips_image_dimensions(a$resolved_path, page = page)
            if (!identical(as.numeric(dimensions), as.numeric(first_dimensions)))
              wsi_abort("All declared GigaTIME marker pages must share the same source image dimensions.")
            dtype <- wsi_vips_properties(paste0(a$resolved_path, "[page=", page, "]"))$format
            if (!dtype %in% c("uchar", "ushort", "float", "double"))
              wsi_abort("GigaTIME marker TIFF pages support uint8, uint16, float32 or float64 scalar storage.")
            storage_max <- switch(dtype, uchar = 255, char = 127, ushort = 65535, short = 32767,
              uint = 4294967295, int = 2147483647, NULL)
            display_max <- channels[[j]]$display_max %||% storage_max
            if (is.null(display_max)) wsi_abort("Floating GigaTIME marker pages require explicit channel.display_max; display scaling is never guessed.")
            a$display_max[[j]] <- wsi_experiment_number(display_max, "channel.display_max")
          }
        }
        a$marker_names <- markers
        a$pages <- pages
      } else if (a$role == "cell_segmentation_mask") {
        if (!is.character(a$format) || !a$format %in% c("tiff", "ome-tiff") ||
            !is.character(a$segmentation_unit) || !a$segmentation_unit %in% c("cell", "nucleus"))
          wsi_abort("Cell segmentation masks require TIFF format and explicit segmentation_unit cell or nucleus.")
        wsi_experiment_text(a$mask_name, "mask_name")
        for (key in c("width", "height")) {
          wsi_experiment_number(a[[key]], paste("mask", key), integer = TRUE)
          if (a[[key]] != tissue$image[[key]]) wsi_abort("Cell mask dimensions must match the native H&E image grid; no scaling is inferred.")
        }
        if (wsi_has_vips()) {
          properties <- wsi_vips_properties(a$resolved_path)
          if (as.numeric(properties$width) != a$width || as.numeric(properties$height) != a$height ||
              as.numeric(properties[["n-pages"]] %||% 1) != 1 || as.numeric(properties$bands) != 1 ||
              !properties$format %in% c("uchar", "char", "ushort", "short", "uint", "int"))
            wsi_abort("Cell mask TIFF header must describe one native-size integer label plane.")
          a$max_label <- switch(properties$format, uchar = 255, char = 127, ushort = 65535, short = 32767,
            uint = 4294967295, int = 2147483647)
        }
      } else {
        if (!is.character(a$format) || !a$format %in% c("csv", "csv.gz")) wsi_abort("GigaTIME cell scores require csv/csv.gz format.")
        if (identical(a$format, "csv.gz") != grepl("\\.csv\\.gz$", a$resolved_path, ignore.case = TRUE))
          wsi_abort("GigaTIME score filename and CSV/CSV.GZ format differ.")
        wsi_experiment_number(a$row_count, "GigaTIME row_count", integer = TRUE)
        identity <- vapply(c("id_column", "x_column", "y_column"), function(key) wsi_experiment_text(a[[key]], key), character(1))
        columns <- wsi_experiment_gigatime_array(a$marker_columns, "GigaTIME marker_columns")
        markers <- vapply(columns, function(column) wsi_experiment_text(column$marker, "score marker"), character(1))
        predictors <- vapply(columns, function(column) wsi_experiment_text(column$column, "score column"), character(1))
        if (anyDuplicated(markers) || anyDuplicated(c(identity, predictors)))
          wsi_abort("GigaTIME marker columns must be unique and exclude ID/coordinate columns.")
        if (!is.character(a$source_coordinate_convention) ||
            !a$source_coordinate_convention %in% c("zero-based-pixel-index", "native-image-pixel-centres"))
          wsi_abort("GigaTIME scores must declare source_coordinate_convention explicitly; pixel-centre shifts are never guessed.")
        if (!is.null(a$coordinate_convention) && !identical(a$coordinate_convention, "pixel-edge-origin_centres-in-native-pixels"))
          wsi_abort("GigaTIME output coordinates require the native H&E pixel-centre convention.")
        affine <- wsi_experiment_gigatime_affine(a$source_to_image_affine)
        if (a$source_coordinate_convention == "native-image-pixel-centres" && any(abs(affine - diag(3)) > 1e-10))
          wsi_abort("Native image pixel-centres require an identity affine.")
        a$affine <- affine; a$marker_names <- markers; a$score_columns <- predictors
        for (key in c("mask_name", "mask_artifact_id", "source_artifact_id")) wsi_experiment_text(a[[key]], key)
      }
      resolved[[a$artifact_id]] <- a
    }
    for (a in resolved) {
      entry <- list(tissue = tissue, image_index = i - 1L, artifact = a)
      if (a$role == "gigatime_marker_image") result$images[[length(result$images) + 1L]] <- entry
      else if (a$role == "cell_segmentation_mask") result$masks[[length(result$masks) + 1L]] <- entry
      else {
        source <- resolved[[a$source_artifact_id]]; mask <- resolved[[a$mask_artifact_id]]
        if (!identical(source$role, "gigatime_marker_image") || !identical(mask$role, "cell_segmentation_mask") ||
            !identical(mask$mask_name, a$mask_name) || any(!a$marker_names %in% source$marker_names))
          wsi_abort("GigaTIME scores require an explicit matching marker-image and segmentation-mask link in the same tissue.")
        if (identical(a$source_coordinate_convention, "zero-based-pixel-index") && !is.null(source$source_width)) {
          sx <- source$extent$width / source$source_width; sy <- source$extent$height / source$source_height
          expected <- rbind(c(sx, 0, source$extent$x + .5 * sx), c(0, sy, source$extent$y + .5 * sy), c(0, 0, 1))
          if (any(abs(a$affine - expected) > 1e-8))
            wsi_abort("Raw GigaTIME pixel-index affine must match its marker-image extent, including the scaled +0.5 pixel-centre shift.")
        }
        entry$mask <- mask; entry$source <- source
        result$scores[[length(result$scores) + 1L]] <- entry
      }
    }
  }
  result
}

wsi_experiment_gigatime_scores <- function(experiment, entry) {
  a <- entry$artifact
  if (a$row_count > getOption("wsiTools.experiment_max_marker_rows", 10000L))
    wsi_abort("GigaTIME cell scores exceed wsiTools.experiment_max_marker_rows; choose a bounded export or explicitly increase the limit. No rows were sampled.")
  if (file.info(a$resolved_path)$size > getOption("wsiTools.experiment_max_marker_bytes", 64 * 1024^2))
    wsi_abort("GigaTIME CSV exceeds wsiTools.experiment_max_marker_bytes; use a bounded export.")
  connection <- function() if (a$format == "csv.gz") gzfile(a$resolved_path, "rt") else file(a$resolved_path, "rt")
  columns <- c(a$id_column, a$x_column, a$y_column, a$score_columns)
  stream <- connection()
  header <- tryCatch(names(utils::read.csv(stream, nrows = 0L, check.names = FALSE)), finally = close(stream))
  if (anyDuplicated(header) || any(!columns %in% header)) wsi_abort("GigaTIME score CSV declared columns are missing or duplicated.")
  columns <- union(columns, intersect("mask_name", header))
  stream <- connection()
  table <- tryCatch(utils::read.csv(stream, colClasses = ifelse(header %in% columns, "character", "NULL"), nrows = a$row_count + 1L,
    check.names = FALSE, stringsAsFactors = FALSE, na.strings = character(), fill = FALSE), finally = close(stream))
  if (nrow(table) != a$row_count || anyDuplicated(names(table)) || any(!columns %in% names(table)))
    wsi_abort("GigaTIME score CSV row count or declared columns differ.")
  local <- table[[a$id_column]]
  if (any(!grepl("^[1-9][0-9]*$", local)) || anyDuplicated(local))
    wsi_abort("GigaTIME cell IDs must be unique positive integer mask labels, never row positions.")
  labels <- suppressWarnings(as.numeric(local))
  if (any(!is.finite(labels)) || (!is.null(entry$mask$max_label) && any(labels > entry$mask$max_label)))
    wsi_abort("GigaTIME cell IDs are not representable by the explicitly linked mask's integer storage.")
  if ("mask_name" %in% names(table) && any(!nzchar(trimws(table$mask_name)) | table$mask_name != a$mask_name))
    wsi_abort("GigaTIME CSV mask_name rows differ from the explicitly linked segmentation mask.")
  x <- wsi_experiment_review_numeric(table[[a$x_column]], "GigaTIME source X")
  y <- wsi_experiment_review_numeric(table[[a$y_column]], "GigaTIME source Y")
  xy <- cbind(x, y, 1) %*% t(a$affine)
  if (identical(a$source_coordinate_convention, "zero-based-pixel-index") &&
      (any(x < 0 | x >= entry$source$source_width) || any(y < 0 | y >= entry$source$source_height)))
    wsi_abort("Raw GigaTIME centroids lie outside the linked marker raster.")
  image <- entry$tissue$image
  if (any(xy[, 1] < 0 | xy[, 1] >= image$width | xy[, 2] < 0 | xy[, 2] >= image$height))
    wsi_abort("GigaTIME transformed cell centres lie outside the declared native H&E image.")
  extent <- entry$source$extent
  if (any(xy[, 1] < extent$x | xy[, 1] >= extent$x + extent$width | xy[, 2] < extent$y | xy[, 2] >= extent$y + extent$height))
    wsi_abort("GigaTIME cell centres lie outside the linked marker-image extent.")
  scores <- lapply(a$score_columns, function(column) {
    values <- wsi_experiment_review_numeric(table[[column]], paste("GigaTIME marker", column))
    if (any(values < 0)) wsi_abort("GigaTIME marker scores must be finite nonnegative exported values.")
    values
  })
  names(scores) <- a$marker_names
  list(id = wsi_experiment_namespace_id(experiment$experiment_id, entry$tissue$tissue_id, a$artifact_id, local),
    original_id = local, x = xy[, 1], y = xy[, 2], scores = scores)
}

wsi_experiment_gigatime_viewer <- function(experiment) {
  catalog <- wsi_experiment_gigatime_validate(experiment)
  sources <- list(); layers <- list()
  point_count <- sum(vapply(catalog$scores, function(entry) entry$artifact$row_count * length(entry$artifact$marker_names), numeric(1)))
  item_budget_exceeded <- point_count > getOption("wsiTools.experiment_max_marker_items", 250000L)
  if (length(catalog$images) && !wsi_has_vips())
    wsi_abort("GigaTIME experiment marker overlays require libvips (`vips` and `vipsheader`); no inference is required.")
  for (entry in catalog$images) {
    a <- entry$artifact; tissue <- entry$tissue
    for (j in seq_along(a$marker_names)) {
      marker <- a$marker_names[[j]]
      identity <- list(experiment_id = experiment$experiment_id, tissue_id = tissue$tissue_id,
        artifact_id = a$artifact_id, marker = marker, page = a$pages[[j]])
      id <- paste0("gigatime_image_", wsi_review_digest(identity))
      source <- wsi_dynamic_image_tile_source(a$resolved_path, source_id = id,
        name = paste(tissue$name, marker, "GigaTIME", sep = " / "), page = a$pages[[j]],
        visible = FALSE, colour = wsi_channel_palette(length(a$marker_names))[[j]],
        gain = 1,
        extent = unlist(a$extent[c("x", "y", "width", "height")]),
        metadata = list(target_path = tissue$image$resolved_path, base_slide_path = tissue$image$resolved_path,
          target_project_item_id = tissue$tissue_id,
          gigatime = c(list(marker = marker, mode = "image", tissue_id = tissue$tissue_id,
            artifact_id = a$artifact_id, display_max = a$display_max[[j]]), list(score_semantics = a$score_semantics))))
      sources[[length(sources) + 1L]] <- source
    }
  }
  for (entry in catalog$scores) {
    a <- entry$artifact; tissue <- entry$tissue
    reason <- if (a$row_count > getOption("wsiTools.experiment_max_marker_rows", 10000L))
      "Cell display was not loaded: this score table exceeds wsiTools.experiment_max_marker_rows. Use a bounded derivative or explicitly raise the reviewed limit. No cells were sampled."
    else if (file.info(a$resolved_path)$size > getOption("wsiTools.experiment_max_marker_bytes", 64 * 1024^2))
      "Cell display was not loaded: this CSV exceeds wsiTools.experiment_max_marker_bytes. Use a bounded derivative. No cells were sampled."
    else if (item_budget_exceeded)
      "Cell display was not loaded: registered marker layers exceed wsiTools.experiment_max_marker_items. Use a bounded experiment derivative. No cells were sampled."
    else NULL
    # Integrity/schema checks above remain strict. Only known display budgets
    # produce unavailable descriptors, and their CSV rows are never parsed.
    data <- if (is.null(reason)) wsi_experiment_gigatime_scores(experiment, entry) else NULL
    for (marker in a$marker_names) {
      values <- if (is.null(data)) numeric() else data$scores[[marker]]
      range <- if (length(values)) range(values) else c(NA_real_, NA_real_)
      fraction <- if (length(values) && diff(range) > 0) (values - range[[1]]) / diff(range) else rep(.5, length(values))
      colours <- grDevices::rgb(.15 + .85 * fraction, .35 * (1 - fraction), 1 - .85 * fraction)
      layer <- wsi_viewer_layer_payload(paste(tissue$name, marker, a$mask_name, "GigaTIME cells", sep = " / "),
        data.frame(id = data$id %||% character(), original_id = data$original_id %||% character(),
          x = data$x %||% numeric(), y = data$y %||% numeric(), colour = colours,
          image_id = rep(tissue$tissue_id, length(values)), project_image_index = rep(entry$image_index, length(values))),
        type = "points", visible = FALSE, radius = 4)
      layer$id <- paste0("gigatime_cells_", wsi_review_digest(list(experiment$experiment_id, tissue$tissue_id, a$artifact_id, marker)))
      layer$source_type <- "gigatime_cell_marker"
      meta <- list(marker = marker, mode = "cells", tissue_id = tissue$tissue_id, artifact_id = a$artifact_id,
        source_artifact_id = a$source_artifact_id, mask_name = a$mask_name,
        source_column = a$score_columns[[match(marker, a$marker_names)]],
        segmentation_unit = entry$mask$segmentation_unit, score_semantics = a$score_semantics,
        available = is.null(reason), reason = reason, row_count = a$row_count,
        min = if (length(values)) unname(range[[1]]) else NULL, max = if (length(values)) unname(range[[2]]) else NULL)
      layer$metadata <- list(gigatime = meta)
      layer$items <- lapply(seq_along(layer$items), function(j) {
        item <- layer$items[[j]]
        item$tissue_id <- tissue$tissue_id; item$value <- values[[j]]
        item$metadata <- c(meta, list(scores = stats::setNames(list(values[[j]]), marker)))
        item
      })
      layers[[length(layers) + 1L]] <- layer
    }
  }
  list(channel_sources = sources, layers = layers)
}
