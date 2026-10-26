# Immutable cell model outputs and feature descriptors, independent of human review.
wsi_cell_feature_definition <- function(f) {
  descriptor <- f$descriptor
  if (!is.list(descriptor)) wsi_abort("Cell features require an explicit descriptor; width alone is not a representation.")
  for (key in c("backbone", "checkpoint_sha256", "runtime_version", "feature_definition", "coordinate_frame"))
    wsi_experiment_text(descriptor[[key]], paste0("cell descriptor.", key))
  morphology <- identical(f$kind, "cell-morphology")
  if ((!morphology && !descriptor$backbone %in% c("HIPT", "SAM", "Virchow2")) ||
      (morphology && !grepl("^HoVer-NeXt-ConvNeXtV2-(Tiny|Base|Large)$", descriptor$backbone)) ||
      !grepl("^[a-fA-F0-9]{64}$", descriptor$checkpoint_sha256) ||
      !identical(descriptor$coordinate_frame, "image"))
    wsi_abort("Cell descriptor requires its supported backbone, checkpoint SHA-256 and image coordinate frame.")
  sampling <- descriptor$sampling
  if (!is.list(sampling)) wsi_abort("Cell descriptor requires the actual normalized model sampling contract.")
  wsi_experiment_number(sampling$model_input_mpp, "descriptor.sampling.model_input_mpp")
  preprocessing <- descriptor$preprocessing
  if (morphology) {
    if (!identical(descriptor$detector, "HoVer-NeXt") ||
        !identical(descriptor$feature_definition, "hovernext-native-instance-morphology-rgb-v1") ||
        !grepl("^[a-f0-9]{40}$", descriptor$upstream_source_commit %||% "") ||
        !grepl("^[a-f0-9]{32}$", descriptor$checkpoint_archive_md5 %||% "") ||
        !identical(sampling$resampling, "none") || !is.list(preprocessing))
      wsi_abort("HoVer-NeXt morphology requires immutable source/checkpoint provenance and its exact feature definition.")
    for (key in c("mask_geometry", "colour")) wsi_experiment_text(preprocessing[[key]], paste0("descriptor.preprocessing.", key))
    columns <- c("area_um2", "perimeter_um", "bbox_width_um", "bbox_height_um", "major_axis_um", "minor_axis_um",
      "eccentricity", "extent", "mean_red", "mean_green", "mean_blue")
    if (!identical(unlist(f$feature_columns, use.names = FALSE), columns) || !identical(as.integer(descriptor$feature_count), 11L))
      wsi_abort("HoVer-NeXt morphology requires its exact ordered 11 measurements; these are not CellViT embeddings.")
  } else {
    for (key in c("patch_size_px", "token_size_px")) wsi_experiment_number(sampling[[key]], paste0("descriptor.sampling.", key), integer = TRUE)
    overlap <- wsi_experiment_number(sampling$patch_overlap_px, "descriptor.sampling.patch_overlap_px", FALSE, TRUE)
    if (overlap < 0 || overlap >= sampling$patch_size_px) wsi_abort("Cell patch overlap must be nonnegative and smaller than its patch size.")
    if (!is.list(preprocessing) || !grepl("^[a-fA-F0-9]{64}$", preprocessing$runtime_wheel_sha256 %||% ""))
      wsi_abort("Cell descriptor requires the immutable runtime wheel SHA-256 and preprocessing contract.")
    for (key in c("normalization", "resampling", "token_provenance"))
      wsi_experiment_text(preprocessing[[key]], paste0("descriptor.preprocessing.", key))
  }
  count <- wsi_experiment_number(descriptor$feature_count, "cell descriptor.feature_count", integer = TRUE)
  if (count > getOption("wsiTools.experiment_max_cell_features", 4096L)) wsi_abort("Cell feature width exceeds the configured bound.")
  expected_encoder <- list(name = if (morphology) "HoVer-NeXt" else "CellViT++", revision = descriptor$runtime_version,
    checkpoint_sha256 = tolower(descriptor$checkpoint_sha256), backbone = descriptor$backbone)
  if (!is.null(f$encoder) && (!identical(f$encoder$name, expected_encoder$name) ||
      !identical(tolower(f$encoder$checkpoint_sha256 %||% ""), expected_encoder$checkpoint_sha256) ||
      !identical(f$encoder$revision, expected_encoder$revision) ||
      (!is.null(f$encoder$backbone) && !identical(f$encoder$backbone, expected_encoder$backbone))))
    wsi_abort("Cell encoder and explicit descriptor disagree.")
  f$encoder <- expected_encoder
  if (!is.null(f$preprocessing$feature_definition) &&
      !identical(f$preprocessing$feature_definition, descriptor$feature_definition))
    wsi_abort("Cell preprocessing and descriptor feature_definition disagree.")
  f$preprocessing <- f$preprocessing %||% list()
  f$preprocessing$feature_definition <- descriptor$feature_definition
  f
}

wsi_experiment_validate_feature <- function(f) {
  cell <- f$kind %in% c("cellvit-cell", "cell-morphology")
  if (cell) f <- wsi_cell_feature_definition(f)
  expected <- if (cell) f$descriptor$feature_count else switch(f$kind,
    "virchow2-pooled" = 2560L, "virchow2-spatial" = 1280L, NULL)
  if (is.null(expected)) wsi_abort("Unsupported feature kind; declare virchow2-pooled, virchow2-spatial, cellvit-cell or cell-morphology.")
  unit <- if (cell) "cell" else if (f$kind == "virchow2-pooled") "tile" else "token"
  if (!identical(f$observation_unit, unit)) wsi_abort("Feature kind and observation_unit disagree.")
  if (!identical(f$coordinate_frame, "image")) wsi_abort("Feature coordinates must already be in the declared image frame.")
  if (!f$format %in% c("csv", "csv.gz", if (identical(f$kind, "cellvit-cell")) "partitioned-sqlite"))
    wsi_abort("Only explicit csv/csv.gz or indexed partitioned cell features are supported.")
  columns <- unlist(f$feature_columns, use.names = FALSE)
  if (!is.character(columns) || length(columns) != expected || anyNA(columns) || any(!nzchar(columns)) || anyDuplicated(columns))
    wsi_abort(sprintf("%s requires %d unique ordered feature_columns.", f$kind, expected))
  for (field in c("id_column", "x_column", "y_column")) wsi_experiment_text(f[[field]], field)
  if (any(columns %in% c(f$id_column, f$x_column, f$y_column)) || length(unique(c(f$id_column, f$x_column, f$y_column))) != 3L)
    wsi_abort("Predictor allowlist must exclude identity and coordinate columns.")
  if (cell && any(grepl("(^|_)(id|label|class|prediction|predicted|confidence|probability|reviewer|split|fold|patient|specimen|source)(_|$)", columns, ignore.case = TRUE)))
    wsi_abort("Cell predictor allowlist cannot include identities, labels, predictions, confidence or split metadata.")
  wsi_experiment_number(f$row_count, "row_count", positive = !cell, integer = TRUE)
  if (f$row_count < 0) wsi_abort("row_count cannot be negative.")
  if (!cell) {
    for (field in c("name", "revision", "checkpoint_sha256")) wsi_experiment_text(f$encoder[[field]], paste0("encoder.", field))
    if (!identical(tolower(f$encoder$name), "virchow2") || !grepl("^[a-fA-F0-9]{64}$", f$encoder$checkpoint_sha256))
      wsi_abort("Feature encoder must identify Virchow2 and a checkpoint SHA-256.")
    definition <- if (unit == "tile") "CLS+mean_patch" else "spatial_patch_tokens"
    if (!identical(f$preprocessing$feature_definition, definition)) wsi_abort("Feature definition disagrees with the declared feature kind.")
    for (field in c("tile_width_px", "tile_height_px", "spacing_x_px", "spacing_y_px")) wsi_experiment_number(f$grid[[field]], paste0("grid.", field))
    for (field in c("origin_x_px", "origin_y_px")) wsi_experiment_number(f$grid[[field]], paste0("grid.", field), positive = FALSE)
    if (!is.null(f$grid$representative_policy) && !identical(f$grid$representative_policy, "logical-centre-clipped-to-image"))
      wsi_abort("Unknown grid representative_policy.")
    if (!is.null(f$grid$representative_policy) && !identical(f$grid$edge_policy, "white-padded"))
      wsi_abort("Clipped logical centres require edge_policy='white-padded'.")
  }
  f$feature_columns <- columns
  f
}

wsi_experiment_feature_geometry <- function(f, image, x, y) {
  if (any(x < 0 | x >= image$width | y < 0 | y >= image$height)) wsi_abort("Feature centres fall outside the declared image dimensions.")
  if (identical(f$observation_unit, "cell")) return(data.frame(row.names = seq_along(x)))
  grid <- f$grid
  clipped <- identical(grid$representative_policy, "logical-centre-clipped-to-image")
  if (clipped) {
    dimensions <- unlist(grid[c("origin_x_px", "origin_y_px", "spacing_x_px", "spacing_y_px",
      "tile_width_px", "tile_height_px")], use.names = FALSE)
    if (!identical(f$observation_unit, "tile") || !identical(grid$edge_policy, "white-padded") ||
        any(dimensions != floor(dimensions)) || grid$spacing_x_px != grid$tile_width_px ||
        grid$spacing_y_px != grid$tile_height_px)
      wsi_abort("Unsupported clipped native grid geometry.")
  }
  index <- function(v, origin, spacing, width, limit) {
    raw <- (v - origin - width / 2) / spacing
    if (clipped) {
      # Producer representatives use integer pixel indices; footprints below keep their geometric centres.
      raw <- round((v - origin - width %/% 2) / spacing)
      clipped_edge <- v == limit - 1
      raw[clipped_edge] <- floor((limit - 1 - origin) / spacing)
      start <- origin + raw * spacing
      expected <- pmin(start + width %/% 2, limit - 1)
      if (any(raw < 0 | start < 0 | start >= limit | abs(v - expected) > 1e-5))
        wsi_abort("Feature centres disagree with the declared clipped native grid.")
    } else if (any(abs(raw - round(raw)) > 1e-5)) wsi_abort("Feature centres disagree with the declared grid (origin is its top-left edge).")
    if (any(raw < -1e-5)) wsi_abort("Feature grid indices cannot be negative.")
    round(raw)
  }
  col <- index(x, grid$origin_x_px, grid$spacing_x_px, grid$tile_width_px, image$width)
  row <- index(y, grid$origin_y_px, grid$spacing_y_px, grid$tile_height_px, image$height)
  lx <- grid$origin_x_px + col * grid$spacing_x_px + grid$tile_width_px / 2
  ly <- grid$origin_y_px + row * grid$spacing_y_px + grid$tile_height_px / 2
  data.frame(logical_x = lx, logical_y = ly, footprint_width_px = grid$tile_width_px,
    footprint_height_px = grid$tile_height_px, valid_x0 = pmax(0, lx - grid$tile_width_px / 2),
    valid_y0 = pmax(0, ly - grid$tile_height_px / 2), valid_x1 = pmin(image$width, lx + grid$tile_width_px / 2),
    valid_y1 = pmin(image$height, ly + grid$tile_height_px / 2),
    padded = lx + grid$tile_width_px / 2 > image$width | ly + grid$tile_height_px / 2 > image$height)
}

wsi_experiment_cell_registration <- function(source, target, registration) {
  for (key in c("sha256", "width", "height", "mpp_x", "mpp_y", "coordinate_convention")) {
    if (is.null(source[[key]]) || is.null(target[[key]]) ||
        !isTRUE(all.equal(source[[key]], target[[key]], check.attributes = FALSE)))
      wsi_abort(paste("Cell classification image identity/calibration differs:", key))
  }
  source_frame <- wsi_experiment_text(source$frame_id, "cell image.frame_id")
  target_frame <- wsi_experiment_text(target$frame_id, "tissue image.frame_id")
  if (identical(source_frame, target_frame) && is.null(registration)) return(NULL)
  if (!is.list(registration) ||
      !identical(registration$source_frame_id, source_frame) ||
      !identical(registration$target_frame_id, target_frame))
    wsi_abort("Different cell/image frames require an explicit verified identity registration.")
  affine <- registration$source_to_image_affine
  if (!is.list(affine) || length(affine) != 3L ||
      any(vapply(affine, function(row) length(unlist(row, use.names = FALSE)) != 3L, logical(1))) ||
      !isTRUE(all.equal(unlist(affine, use.names = FALSE), as.numeric(diag(3)), tolerance = 0, check.attributes = FALSE)))
    wsi_abort("Cell frame registration must declare an exact 3x3 identity affine.")
  proof <- registration$identity_verified
  if (!is.list(proof)) wsi_abort("Cell frame registration requires explicit verified image identity evidence.")
  for (key in c("sha256", "width", "height", "mpp_x", "mpp_y", "coordinate_convention", "coordinate_space", "origin_x", "origin_y")) {
    if (is.null(source[[key]]) || is.null(target[[key]]) || is.null(proof[[key]]) ||
        !isTRUE(all.equal(source[[key]], target[[key]], check.attributes = FALSE)) ||
        !isTRUE(all.equal(source[[key]], proof[[key]], check.attributes = FALSE)))
      wsi_abort(paste("Cell identity registration evidence is missing or differs:", key))
  }
  if (!identical(proof$coordinate_space, "crop-native-pixels") ||
      !identical(proof$coordinate_convention, "pixel-edge-origin_centres-in-native-pixels") ||
      !isTRUE(all.equal(proof$origin_x, 0, check.attributes = FALSE)) ||
      !isTRUE(all.equal(proof$origin_y, 0, check.attributes = FALSE)))
    wsi_abort("Cell identity registration requires native image coordinates and zero origin.")
  registration
}

wsi_cell_geometry_quarantine_reason <- function(reasons, policy) {
  codes <- intersect(reasons, c("invalid_original_geometry", "overlapping_original_geometry"))
  if (length(codes) != 1L)
    wsi_abort("Unavailable geometry must declare exactly one invalid_original_geometry or overlapping_original_geometry reason.")
  flag <- if (codes == "invalid_original_geometry") "quarantine_invalid_geometry" else "quarantine_overlapping_geometry"
  if (!isTRUE(policy[[flag]])) wsi_abort(paste("Unavailable", codes, "requires explicit", paste0(flag, "=true.")))
  codes
}

wsi_experiment_cell_segmentation_review <- function(ref, classification, feature, cells, root) {
  review <- ref$segmentation_review
  if (is.null(review)) {
    cells <- lapply(cells, function(cell) {
      evidence <- cell[["segmentation"]] %||% cell[["disagreement"]] %||% list()
      if (!is.list(evidence)) evidence <- list()
      available <- any(vapply(list(cell$segmentation_disagreement, cell$segmentation_review_required,
        evidence$segmentation_disagreement, evidence$segmentation_review_required),
        function(value) is.logical(value) && length(value) == 1L && !is.na(value), logical(1)))
      explicit_status <- cell$segmentation_review_status %||% evidence$segmentation_review_status
      cell$segmentation_review_status <- if (is.character(explicit_status) && length(explicit_status) == 1L &&
        explicit_status %in% c("available", "unavailable")) explicit_status else if (available) "available" else "unavailable"
      if (identical(cell$segmentation_review_status, "unavailable") && is.null(cell$segmentation_geometry_unavailable_reason))
        cell$segmentation_review_unavailable_reason <- "No StarDist segmentation review is registered for this cell."
      cell
    })
    return(list(cells = cells, source = NULL))
  }
  read_checked_json <- function(entry, label) {
    entry <- wsi_experiment_file(entry, root, label, TRUE)
    if (file.info(entry$resolved_path)$size > getOption("wsiTools.experiment_max_cell_json_bytes", 64 * 1024^2))
      wsi_abort(paste(label, "exceeds the configured bounded JSON limit."))
    list(reference = entry, data = jsonlite::fromJSON(entry$resolved_path, simplifyVector = FALSE))
  }
  checked <- read_checked_json(review, "cell segmentation review")
  report <- checked$data
  if (!identical(report$schema, "cellphenotyper.segmentation-review/v1") ||
      !identical(report$reference_quality, "model_agreement_only") ||
      !identical(report$cell_type_evidence_changed, FALSE))
    wsi_abort("Cell segmentation review must preserve cell-type evidence and declare model-agreement-only semantics.")
  image <- classification$image
  if (!identical(review$source_frame_id, image$frame_id) ||
      !identical(review$coordinate_convention, image$coordinate_convention))
    wsi_abort("Cell segmentation review source frame/convention differs from the classification image.")
  frame <- report$frame
  for (key in c("sha256", "width", "height", "mpp_x", "mpp_y", "coordinate_space", "origin_x", "origin_y")) {
    value <- frame[[if (key == "sha256") "source_image_sha256" else key]]
    if (is.null(value) || is.null(image[[key]]) || !isTRUE(all.equal(value, image[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort(paste("Cell segmentation review frame/image identity differs:", key))
  }
  if (is.null(ref$feature_manifest)) wsi_abort("Cell segmentation review requires a checked feature_manifest lineage reference.")
  manifest <- read_checked_json(ref$feature_manifest, "cell feature manifest")
  source <- manifest$data
  if (!identical(source$schema, "cellphenotyper-cell-features") || !identical(as.integer(source$schema_version), 1L) ||
      !identical(classification$provenance$feature_manifest_sha256, manifest$reference$sha256))
    wsi_abort("Cell segmentation review feature manifest is not the classification's checked source.")
  for (key in c("sha256", "frame_id", "width", "height", "mpp_x", "mpp_y", "coordinate_convention", "coordinate_space", "origin_x", "origin_y"))
    if (is.null(source$image[[key]]) || !isTRUE(all.equal(source$image[[key]], image[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort(paste("Cell segmentation review feature manifest image differs:", key))
  definition <- wsi_experiment_validate_feature(source$feature_set)
  if (!identical(definition$sha256, feature$sha256) || !identical(definition$feature_set_id, feature$feature_set_id) ||
      !identical(wsi_experiment_feature_signature(definition), wsi_experiment_feature_signature(feature)))
    wsi_abort("Cell segmentation review feature manifest does not describe the registered cell features.")
  normalized <- source$source$normalized_segmentation
  input_hash <- if (is.null(normalized)) source$source$cells_sha256 else normalized$sha256
  if (!grepl("^[0-9a-f]{64}$", input_hash %||% "") ||
      !identical(report$inputs$cellvit_cells$sha256, input_hash) ||
      !identical(report$inputs$source_image$sha256, image$sha256))
    wsi_abort("Cell segmentation review original CellViT cells/image hashes do not match the feature manifest lineage.")
  rows <- wsi_review_rows(report$cells, "segmentation review cells")
  count <- function(value, label) {
    value <- wsi_experiment_number(value, paste("segmentation review", label), positive = FALSE, integer = TRUE)
    if (value < 0) wsi_abort("Cell segmentation review counts must be nonnegative integers.")
    value
  }
  if (length(rows) != length(cells) || count(report$original_cell_count, "original_cell_count") != length(cells) ||
      count(report$candidate_cell_count, "candidate_cell_count") != length(cells))
    wsi_abort("Cell segmentation review must preserve the complete original cell population.")
  statuses <- vapply(rows, function(row) {
    status <- row$segmentation_review_status %||% "available"
    if (!is.character(status) || length(status) != 1L || is.na(status) || !status %in% c("available", "unavailable"))
      wsi_abort("Cell segmentation review status must be available or unavailable.")
    status
  }, character(1))
  unavailable_count <- sum(statuses == "unavailable")
  for (flag in c("quarantine_invalid_geometry", "quarantine_overlapping_geometry"))
    if (!is.null(report[[flag]]) && (!is.logical(report[[flag]]) || length(report[[flag]]) != 1L || is.na(report[[flag]])))
      wsi_abort(paste("Cell segmentation review", flag, "must be an explicit boolean."))
  geometry_codes <- vapply(seq_along(rows), function(index) if (statuses[[index]] == "unavailable")
    wsi_cell_geometry_quarantine_reason(unlist(rows[[index]]$segmentation_review_reason, use.names = FALSE), report) else "", character(1))
  geometry_reason_counts <- c(invalid_geometry_unavailable_cell_count = sum(geometry_codes == "invalid_original_geometry"),
    overlapping_geometry_unavailable_cell_count = sum(geometry_codes == "overlapping_original_geometry"))
  if (!is.null(report$quarantine_overlapping_geometry) || any(names(geometry_reason_counts) %in% names(report)))
    for (key in names(geometry_reason_counts))
      if (count(report[[key]], key) != geometry_reason_counts[[key]])
        wsi_abort("Cell segmentation review invalid/overlapping reason counts do not match canonical cell statuses.")
  geometry_counts <- c("rasterized_cell_count", "geometry_unavailable_cell_count")
  if (unavailable_count || any(geometry_counts %in% names(report)) || !is.null(report$quarantine_invalid_geometry)) {
    rasterized <- count(report$rasterized_cell_count, "rasterized_cell_count")
    unavailable <- count(report$geometry_unavailable_cell_count, "geometry_unavailable_cell_count")
    if (rasterized + unavailable != length(cells) || unavailable != unavailable_count || rasterized != sum(statuses == "available"))
      wsi_abort("Cell segmentation review rasterized/unavailable counts must match all canonical cell statuses without omissions.")
  }
  cell_ids <- vapply(cells, `[[`, character(1), "cell_id")
  source_ids <- vapply(cells, `[[`, character(1), "source_id")
  if (anyDuplicated(source_ids)) wsi_abort("Cell segmentation review requires unique source IDs for a bijective identity join.")
  row_ids <- vapply(rows, function(row) wsi_experiment_text(row$cell_id, "segmentation review.cell_id"), character(1))
  if (anyDuplicated(row_ids)) wsi_abort("Cell segmentation review cell IDs must be unique.")
  used <- integer()
  for (row_index in seq_along(rows)) {
    row <- rows[[row_index]]
    row_id <- wsi_experiment_text(row$cell_id, "segmentation review.cell_id")
    original <- row$original_cell %||% list()
    original_source <- original$source_id
    if (!is.null(original_source)) original_source <- wsi_experiment_text(original_source, "segmentation review.original_cell.source_id")
    index <- match(row_id, cell_ids)
    if (is.na(index)) index <- match(original_source %||% row_id, source_ids)
    if (is.na(index) || index %in% used ||
        (!is.null(original_source) && !identical(original_source, source_ids[[index]])) ||
        (!row_id %in% c(cell_ids[[index]], source_ids[[index]]) && is.null(original_source)) ||
        (!is.null(original$cell_id) && !original$cell_id %in% c(cell_ids[[index]], source_ids[[index]])))
      wsi_abort("Cell segmentation review IDs do not join one-to-one to the original canonical/source cell IDs.")
    for (key in c("segmentation_disagreement", "segmentation_review_required", "segmentation_split_signal", "segmentation_merge_signal"))
      if (!is.logical(row[[key]]) || length(row[[key]]) != 1L || is.na(row[[key]]))
        wsi_abort(paste("Cell segmentation review requires explicit boolean", key))
    if (!row$segmentation_action %in% c("retain_original", "adjust_boundary") ||
        !identical(row$segmentation_reference_quality, "model_agreement_only") ||
        !identical(row$segmentation_review_required, row$segmentation_disagreement || row$segmentation_action == "adjust_boundary"))
      wsi_abort("Cell segmentation review action/required flags or reference semantics are inconsistent.")
    reasons <- unlist(row$segmentation_review_reason, use.names = FALSE)
    if ((length(reasons) && (!is.character(reasons) || anyNA(reasons) || any(!nzchar(reasons)))) ||
        !identical(length(reasons) > 0L, row$segmentation_disagreement))
      wsi_abort("Cell segmentation review reasons and disagreement flag differ.")
    status <- statuses[[row_index]]
    metrics <- c("segmentation_iou", "segmentation_cellvit_coverage", "segmentation_stardist_coverage")
    if (identical(status, "unavailable")) {
      reason <- wsi_experiment_text(row$segmentation_geometry_unavailable_reason, "segmentation_geometry_unavailable_reason")
      wsi_cell_geometry_quarantine_reason(reasons, report)
      if (!nzchar(trimws(reason)) ||
          !isTRUE(row$segmentation_disagreement) || !isTRUE(row$segmentation_review_required) ||
          !identical(row$segmentation_action, "retain_original") || !identical(row$segmentation_auto_adjustment_eligible, FALSE) ||
          !identical(row$segmentation_split_signal, FALSE) || !identical(row$segmentation_merge_signal, FALSE) ||
          (!is.null(row$auto_adjustment_eligible) && !identical(row$auto_adjustment_eligible, FALSE)))
        wsi_abort("Unavailable cell segmentation geometry must retain the original, explain its supported invalid/overlapping geometry reason and require human review without automatic adjustment.")
      if (!all(metrics %in% names(row)) || any(!vapply(row[metrics], is.null, logical(1))))
        wsi_abort("Unavailable cell segmentation agreement metrics must be explicit JSON null, never zero or missing.")
    } else {
      if (any(c("invalid_original_geometry", "overlapping_original_geometry") %in% reasons) || (!is.null(row$segmentation_geometry_unavailable_reason) &&
          !identical(row$segmentation_geometry_unavailable_reason, "")))
        wsi_abort("Available cell segmentation geometry cannot declare a geometry-unavailable reason.")
      for (key in metrics) {
        value <- wsi_experiment_number(row[[key]], key, positive = FALSE)
        if (value < 0 || value > 1) wsi_abort("Cell segmentation agreement fractions must be between zero and one.")
      }
    }
    fields <- row[grepl("^segmentation_", names(row)) | names(row) == "stardist_label"]
    cells[[index]][names(fields)] <- fields
    cells[[index]]$segmentation_review_status <- status
    cells[[index]]$segmentation_review_source_sha256 <- checked$reference$sha256
    used <- c(used, index)
  }
  list(cells = cells, source = list(sha256 = checked$reference$sha256, feature_manifest_sha256 = manifest$reference$sha256,
    source_cells_sha256 = input_hash, reference_quality = "model_agreement_only",
    quarantine_invalid_geometry = isTRUE(report$quarantine_invalid_geometry),
    quarantine_overlapping_geometry = isTRUE(report$quarantine_overlapping_geometry),
    invalid_geometry_unavailable_cell_count = unname(geometry_reason_counts[[1L]]),
    overlapping_geometry_unavailable_cell_count = unname(geometry_reason_counts[[2L]]),
    rasterized_cell_count = length(cells) - unavailable_count, geometry_unavailable_cell_count = unavailable_count))
}

# Tissue segmentation supplies descriptive context, never a replacement cell label.
wsi_experiment_cell_tissue_context <- function(ref, classification, feature, cells, root) {
  entry <- ref$tissue_context
  if (is.null(entry)) return(list(cells = cells, source = NULL))
  read_checked <- function(value, label) {
    checked <- wsi_experiment_file(value, root, label, TRUE)
    if (file.info(checked$resolved_path)$size > getOption("wsiTools.experiment_max_cell_json_bytes", 64 * 1024^2))
      wsi_abort(paste(label, "exceeds the configured bounded JSON limit."))
    list(reference = checked, data = jsonlite::fromJSON(checked$resolved_path, simplifyVector = FALSE))
  }
  checked <- read_checked(entry, "cell tissue context")
  report <- checked$data
  if (!identical(report$schema, "cellphenotyper-cell-tissue-context") ||
      !identical(as.integer(report$schema_version), 1L) || !identical(report$labels_changed, FALSE) ||
      length(report$predictor_fields) ||
      !identical(report$context_semantics, "descriptive_tissue_model_context_not_cell_type_evidence"))
    wsi_abort("Cell tissue context must declare descriptive-only semantics and no labels or predictors changed.")
  image <- classification$image
  if (!identical(entry$source_frame_id, image$frame_id) || !identical(entry$coordinate_convention, image$coordinate_convention))
    wsi_abort("Cell tissue context registration differs from its classification source frame.")
  for (key in c("sha256", "frame_id", "width", "height", "mpp_x", "mpp_y", "coordinate_convention", "coordinate_space", "origin_x", "origin_y"))
    if (is.null(image[[key]]) || !isTRUE(all.equal(report$image[[key]], image[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort(paste("Cell tissue context image identity differs:", key))
  if (is.null(ref$feature_manifest)) wsi_abort("Cell tissue context requires its checked feature manifest.")
  manifest <- read_checked(ref$feature_manifest, "cell context feature manifest")
  definition <- wsi_experiment_validate_feature(manifest$data$feature_set)
  if (!identical(manifest$data$schema, "cellphenotyper-cell-features") ||
      !identical(as.integer(manifest$data$schema_version), 1L) ||
      !identical(classification$provenance$feature_manifest_sha256, manifest$reference$sha256) ||
      !identical(definition$sha256, feature$sha256) ||
      !identical(wsi_experiment_feature_signature(definition), wsi_experiment_feature_signature(feature)))
    wsi_abort("Cell tissue context feature manifest is not the classification's checked feature source.")
  for (key in c("sha256", "frame_id", "width", "height", "mpp_x", "mpp_y", "coordinate_convention", "coordinate_space", "origin_x", "origin_y"))
    if (!isTRUE(all.equal(manifest$data$image[[key]], image[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort(paste("Cell tissue context feature manifest image differs:", key))
  normalized <- manifest$data$source$normalized_segmentation$sha256
  expected <- list(classification = ref$sha256, feature_manifest = manifest$reference$sha256,
    normalized_segmentation = normalized, image = image$sha256)
  for (key in names(expected))
    if (!grepl("^[a-f0-9]{64}$", expected[[key]] %||% "") || !identical(report$inputs[[key]]$sha256, expected[[key]]))
      wsi_abort(paste("Cell tissue context input lineage differs:", key))
  for (key in c("bora_geojson", "bora_frame"))
    if (!grepl("^[a-f0-9]{64}$", report$inputs[[key]]$sha256 %||% ""))
      wsi_abort(paste("Cell tissue context requires recorded source hash:", key))
  rows <- wsi_review_rows(report$cells, "cell tissue context cells")
  ids <- vapply(cells, `[[`, character(1), "cell_id")
  row_ids <- vapply(rows, function(row) wsi_experiment_text(row$cell_id, "context.cell_id"), character(1))
  if (length(rows) != length(cells) || anyDuplicated(row_ids) || !setequal(row_ids, ids))
    wsi_abort("Cell tissue context requires the complete canonical cell-ID population; numeric mask IDs are not a join.")
  fraction <- function(value, label) {
    number <- wsi_experiment_number(value, label, positive = FALSE)
    if (number < 0 || number > 1) wsi_abort(paste(label, "must be between zero and one."))
    number
  }
  boolean <- function(value) is.logical(value) && length(value) == 1L && !is.na(value)
  for (row in rows) {
    i <- match(row$cell_id, ids)
    if (!identical(row$source_id, cells[[i]]$source_id) ||
        !isTRUE(all.equal(row$x, cells[[i]]$x, tolerance = 0, check.attributes = FALSE)) ||
        !isTRUE(all.equal(row$y, cells[[i]]$y, tolerance = 0, check.attributes = FALSE)))
      wsi_abort("Cell tissue context source ID/coordinate evidence contradicts its canonical cell ID.")
    context <- row$tissue_context
    for (key in c("coverage_fraction", "uncovered_fraction", "ambiguous_overlap_fraction")) fraction(context[[key]], paste0("context.", key))
    if (abs(context$coverage_fraction + context$uncovered_fraction - 1) > 1e-6 ||
        context$ambiguous_overlap_fraction > context$coverage_fraction + 1e-6 || !boolean(context$boundary_crossing))
      wsi_abort("Cell tissue context coverage, ambiguity or boundary flags are inconsistent.")
    if (!is.null(context$overlap_denominator)) {
      if (!identical(context$overlap_denominator, "visible_original_contour_area") ||
          !identical(context$cell_boundary_distance_geometry, "original_contour") || !boolean(context$partial_image_boundary))
        wsi_abort("Cell tissue context must declare visible original contour support at the image boundary.")
      original_area <- wsi_experiment_number(context$original_area_um2, "context.original_area_um2")
      visible_area <- wsi_experiment_number(context$visible_area_um2, "context.visible_area_um2")
      visible_fraction <- fraction(context$visible_area_fraction, "context.visible_area_fraction")
      if (visible_area > original_area + 1e-6 || abs(visible_fraction - visible_area / original_area) > 1e-6)
        wsi_abort("Cell tissue context visible support disagrees with original contour area.")
    }
    regions <- wsi_review_rows(context$regions, "cell tissue regions")
    if (length(regions) > 256L) wsi_abort("Cell tissue context exceeds the bounded tissue-region limit.")
    region_ids <- character()
    for (region in regions) {
      id <- region$tissue_id
      if (is.numeric(id)) {
        id <- wsi_experiment_number(id, "context region.tissue_id", positive = FALSE, integer = TRUE)
        if (id < 0) wsi_abort("Cell tissue region IDs cannot be negative.")
        id <- as.character(id)
      } else id <- wsi_experiment_text(id, "context region.tissue_id")
      wsi_experiment_text(region$label, "context region.label")
      if (id %in% region_ids) wsi_abort("Cell tissue context regions require unique tissue IDs.")
      region_ids <- c(region_ids, id)
      fraction(region$overlap_fraction, "region.overlap_fraction")
      wsi_experiment_number(region$signed_centroid_boundary_distance_um, "region.signed_centroid_boundary_distance_um", FALSE)
      distance <- wsi_experiment_number(region$unsigned_cell_boundary_distance_um, "region.unsigned_cell_boundary_distance_um", FALSE)
      if (distance < 0 || !boolean(region$centroid_inside)) wsi_abort("Cell tissue context boundary distance/inside flag is invalid.")
    }
    positive <- Filter(function(region) region$overlap_fraction > 0, regions)
    if (length(positive)) {
      order <- order(-vapply(positive, `[[`, numeric(1), "overlap_fraction"), vapply(positive, function(region) as.character(region$tissue_id), character(1)))
      context$label <- positive[[order[[1L]]]]$label
      context$dominant_tissue_id <- positive[[order[[1L]]]]$tissue_id
    } else context$label <- "outside tissue regions"
    context$score_semantics <- report$context_semantics
    context$source_sha256 <- checked$reference$sha256
    cells[[i]]$tissue_context <- context
  }
  list(cells = cells, source = list(sha256 = checked$reference$sha256,
    context_semantics = report$context_semantics, inputs = report$inputs))
}

wsi_experiment_cell_classification <- function(experiment, tissue) {
  ref <- tissue$cell_classification
  if (is.null(ref)) return(NULL)
  if (!is.null(ref$index)) return(wsi_experiment_cell_index_catalog(experiment, tissue))
  ref <- wsi_experiment_file(ref, experiment$root, "cell_classification", TRUE)
  if (file.info(ref$resolved_path)$size > getOption("wsiTools.experiment_max_cell_json_bytes", 64 * 1024^2))
    wsi_abort("Cell classification JSON exceeds the configured bounded display limit; use a bounded derivative.")
  data <- jsonlite::fromJSON(ref$resolved_path, simplifyVector = FALSE)
  if (!identical(data$schema, "cellphenotyper-cell-classification") || !identical(as.integer(data$schema_version), 1L))
    wsi_abort("Unsupported cell classification schema/version.")
  registration <- wsi_experiment_cell_registration(data$image, tissue$image, ref$registration)
  feature <- wsi_experiment_validate_feature(data$feature_set)
  if (!feature$kind %in% c("cellvit-cell", "cell-morphology") ||
      (!is.null(feature$frame_id) && !identical(feature$frame_id, data$image$frame_id)) ||
      (!is.null(registration) && is.null(feature$frame_id)))
    wsi_abort("Cell classification requires typed cell features in its exact image frame.")
  feature <- wsi_experiment_file(feature, dirname(ref$resolved_path), "cell features", TRUE)
  registered <- Filter(function(f) identical(f$feature_set_id, feature$feature_set_id), tissue$feature_sets)
  if (length(registered) != 1L || !identical(registered[[1L]]$sha256, feature$sha256) ||
      !identical(wsi_experiment_feature_signature(registered[[1L]]), wsi_experiment_feature_signature(feature)))
    wsi_abort("Cell classification feature_set must be registered exactly once with its identical typed descriptor and hash.")
  if (!is.null(registration) && (!identical(registered[[1L]]$frame_id, tissue$image$frame_id) ||
      !identical(registered[[1L]]$source_frame_id, data$image$frame_id) ||
      !identical(registered[[1L]]$registration, registration)))
    wsi_abort("Registered cell features must retain the source frame and the same verified identity registration.")
  if (is.null(registration) && (!is.null(registered[[1L]]$registration) ||
      (!is.null(registered[[1L]]$source_frame_id) && !identical(registered[[1L]]$source_frame_id, data$image$frame_id))))
    wsi_abort("Registered cell feature source frame/registration contradicts its classification image.")
  cells <- wsi_review_rows(data$cells, "classification cells")
  if (length(cells) > getOption("wsiTools.experiment_max_cell_rows", 10000L))
    wsi_abort("Cell classification exceeds the configured bounded row limit; no cells were sampled.")
  if (length(cells) != feature$row_count) wsi_abort("Cell classification and feature row_count differ.")
  ids <- vapply(cells, function(cell) wsi_experiment_text(cell$cell_id, "cell_id"), character(1))
  if (anyDuplicated(ids)) wsi_abort("Cell classification IDs must be unique and stable within a tissue.")
  models <- wsi_review_rows(data$models, "cell models")
  if (length(models) > getOption("wsiTools.experiment_max_cell_models", 64L)) wsi_abort("Cell model catalog exceeds the bounded model limit.")
  model_ids <- vapply(models, function(model) wsi_experiment_text(model$model_id, "model_id"), character(1))
  if (anyDuplicated(model_ids)) wsi_abort("Cell model IDs must be unique.")
  for (model in models) {
    for (key in c("backbone", "taxonomy", "status")) wsi_experiment_text(model[[key]], paste0("model.", key))
    if (!model$status %in% c("available", "unavailable")) wsi_abort("Cell model availability must be explicit.")
    classes <- unlist(model$class_names, use.names = FALSE)
    if (model$status == "available" && (!length(classes) || length(classes) > 1024L || anyNA(classes) || any(!nzchar(classes)) || anyDuplicated(classes) ||
        !grepl("^[a-fA-F0-9]{64}$", model$checkpoint_sha256 %||% "") ||
        !model$score_semantics %in% c("uncalibrated_model_probability", "uncalibrated_pixel_type_vote_fraction", "uncalibrated_softmax_cell_class_vote_counts")))
      wsi_abort("Available cell models need ordered classes, checkpoint SHA-256 and explicit uncalibrated probability semantics.")
    if (model$status == "available" && (!identical(model$backbone, feature$descriptor$backbone) ||
        (feature$kind == "cell-morphology" && (!identical(model$kind, "native_instance_head") ||
          !identical(model$score_semantics, "uncalibrated_softmax_cell_class_vote_counts"))) ||
        (feature$kind == "cellvit-cell" && identical(model$score_semantics, "uncalibrated_softmax_cell_class_vote_counts"))))
      wsi_abort("Available cell model backbone/kind differs from its exact feature representation.")
    if (model$status == "unavailable") wsi_experiment_text(model$reason, "unavailable model.reason")
  }
  for (i in seq_along(cells)) {
    cell <- cells[[i]]
    for (key in c("source_id", "source")) wsi_experiment_text(cell[[key]], paste0("cell.", key))
    x <- wsi_experiment_number(cell$x, "cell.x", FALSE); y <- wsi_experiment_number(cell$y, "cell.y", FALSE)
    if (x < 0 || y < 0 || x >= tissue$image$width || y >= tissue$image$height) wsi_abort("Cell classification coordinate lies outside the image frame.")
    if (length(setdiff(names(cell$predictions %||% list()), model_ids))) wsi_abort("Cell prediction refers to an unknown model.")
    for (model in models) {
      prediction <- cell$predictions[[model$model_id]]
      if (is.null(prediction)) {
        if (model$status == "available") wsi_abort("Available cell model is missing a cell prediction.")
        next
      }
      if (!prediction$status %in% c("predicted", "unavailable", "abstained")) wsi_abort("Unknown cell prediction status.")
      if (prediction$status == "predicted") {
        probabilities <- unlist(prediction$probabilities, use.names = FALSE)
        index <- wsi_experiment_number(prediction$class_id, "prediction.class_id", FALSE, TRUE) + 1L
        classes <- unlist(model$class_names, use.names = FALSE)
        native_vote <- identical(model$score_semantics, "uncalibrated_pixel_type_vote_fraction")
        missing_native_score <- identical(model$score_semantics, "uncalibrated_softmax_cell_class_vote_counts") &&
          is.null(prediction$probability) && is.null(prediction$probabilities) &&
          identical(prediction$probability_vector_status, "not_exported_by_upstream")
        valid_vector <- if (native_vote) is.null(prediction$probabilities) &&
          identical(prediction$probability_vector_status, "not_exported_by_upstream") else
          missing_native_score || (length(probabilities) == length(classes) && is.numeric(probabilities) &&
          all(is.finite(probabilities)) && all(probabilities >= 0 & probabilities <= 1) && abs(sum(probabilities) - 1) <= 1e-5
          )
        if (model$status != "available" || !valid_vector || index < 1L || index > length(classes) ||
            !identical(prediction$label, classes[[index]]) || (!missing_native_score && (
            !is.numeric(prediction$probability) || length(prediction$probability) != 1L || !is.finite(prediction$probability) ||
            prediction$probability < 0 || prediction$probability > 1 ||
            (!native_vote && abs(prediction$probability - probabilities[[index]]) > 1e-6))))
          wsi_abort("Cell prediction class/probability vector is inconsistent with its ordered taxonomy.")
      }
    }
    cell$id <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, feature$feature_set_id, cell$cell_id)
    cell$tissue_id <- tissue$tissue_id; cell$frame_id <- tissue$image$frame_id
    cell$source_frame_id <- data$image$frame_id
    cells[[i]] <- cell
  }
  # Read only identity/coordinate columns here, never the feature matrix.
  table <- wsi_experiment_review_table(registered[[1L]], experiment,
    c(feature$id_column, feature$x_column, feature$y_column), "cell identity table", getOption("wsiTools.experiment_max_cell_rows", 10000L))$table
  feature_ids <- table[[feature$id_column]]
  if (anyNA(feature_ids) || any(!nzchar(feature_ids))) wsi_abort("Cell feature IDs must be nonmissing.")
  feature_x <- wsi_experiment_review_numeric(table[[feature$x_column]], "Cell feature X")
  feature_y <- wsi_experiment_review_numeric(table[[feature$y_column]], "Cell feature Y")
  match <- match(ids, feature_ids)
  if (anyNA(match) || anyDuplicated(table[[feature$id_column]]) || nrow(table) != length(ids) ||
      any(abs(feature_x[match] - vapply(cells, `[[`, numeric(1), "x")) > 1e-8) ||
      any(abs(feature_y[match] - vapply(cells, `[[`, numeric(1), "y")) > 1e-8))
    wsi_abort("Cell classification and feature identities/coordinates do not join exactly.")
  segmentation <- wsi_experiment_cell_segmentation_review(ref, data, feature, cells, experiment$root)
  context <- wsi_experiment_cell_tissue_context(ref, data, feature, segmentation$cells, experiment$root)
  list(tissue_id = tissue$tissue_id, name = tissue$name, frame_id = tissue$image$frame_id,
    source_frame_id = data$image$frame_id, registration = registration,
    source_sha256 = ref$sha256, feature_set_id = feature$feature_set_id, descriptor = feature$descriptor,
    segmentation_review = segmentation$source, tissue_context = context$source, models = models, cells = context$cells)
}

wsi_experiment_cell_catalog <- function(experiment) {
  Filter(Negate(is.null), lapply(experiment$tissues, function(tissue) wsi_experiment_cell_classification(experiment, tissue)))
}

wsi_experiment_cell_review_rows <- function(rows, catalog) {
  if (is.null(catalog)) return(rows)
  cells <- stats::setNames(catalog$cells, vapply(catalog$cells, `[[`, character(1), "id"))
  lapply(rows, function(row) {
    cell <- cells[[row$id]]
    if (is.null(cell)) return(row)
    row$predictions <- cell$predictions
    row$model_sources <- list(classification_sha256 = catalog$source_sha256,
      model_ids = as.list(vapply(catalog$models, `[[`, character(1), "model_id")))
    row$source_id <- cell$source_id; row$source <- cell$source
    row$source_frame_id <- cell$source_frame_id
    row$segmentation_review_status <- cell$segmentation_review_status
    row$segmentation_review_unavailable_reason <- cell$segmentation_review_unavailable_reason
    row$segmentation_review_source_sha256 <- cell$segmentation_review_source_sha256
    row$tissue_context <- cell$tissue_context
    row$segmentation <- cell[["segmentation"]] %||% cell[["disagreement"]]
    for (key in c("segmentation_disagreement", "segmentation_review_required", "segmentation_review_reason", "segmentation_action", "segmentation_reference_quality", "segmentation_iou", "segmentation_cellvit_coverage", "segmentation_stardist_coverage", "segmentation_split_signal", "segmentation_merge_signal", "segmentation_geometry_unavailable_reason", "segmentation_auto_adjustment_eligible"))
      if (key %in% names(cell)) row[key] <- cell[key]
    for (model in catalog$models) {
      prediction <- cell$predictions[[model$model_id]]
      row$cluster_values[[paste0("cellvit:", model$model_id)]] <- if (identical(prediction$status, "predicted")) prediction$label else prediction$status %||% "unavailable"
    }
    if (!is.null(cell$tissue_context$label)) row$cluster_values$tissue_context <- cell$tissue_context$label
    if (!is.null(cell$segmentation_disagreement)) row$cluster_values$segmentation_disagreement <- as.character(cell$segmentation_disagreement)
    if (!is.null(cell$segmentation_review_required)) row$cluster_values$segmentation_review_required <- as.character(cell$segmentation_review_required)
    row
  })
}

# Fit only explicit confirmed review targets; predictions/RNA clusters never supply labels.
wsi_experiment_train_reviewed_response <- function(context, state, payload) {
  if (is.null(state$review_context) || is.null(state$review_checkpoint)) wsi_abort("Open the review context and confirm training cells first.")
  review <- state$review_context
  if (!identical(payload$context_id, review$context_id) || !identical(payload$source_signature, review$source_signature))
    wsi_abort("Cell training review context changed; reopen the current review workspace.")
  checkpoint <- if (is.null(payload$checkpoint)) state$review_checkpoint else wsi_review_commit_checkpoint(state, payload$checkpoint)
  source <- wsi_experiment_text(payload$feature_source, "feature_source")
  if (!startsWith(source, "experiment:")) wsi_abort("Reviewed fitting requires an explicit typed experiment feature set.")
  fid <- substring(source, 12L)
  train <- as.character(unlist(payload$train_ids, use.names = FALSE))
  model <- wsi_experiment_train_reviewed_classifier(context$experiment, review, checkpoint, fid, train,
    ncomp = payload$ncomp %||% 2L, scaling = payload$scaling %||% "autoscaling", max_features = payload$max_features %||% 256L)
  directory <- wsi_experiment_derivative_dir(context$experiment, "reviewed-cell-classifiers")
  model_path <- wsi_write_experiment_classifier(model, file.path(directory, "classifier.rds"))
  wsi_experiment_register_classifier(state, context$experiment, model_path, model)
  state$review_checkpoint <- checkpoint
  state$experiment_classifier <- model
  state$experiment_classifier_path <- model_path
  result <- wsi_experiment_apply_saved_classifier(context, state,
    list(model_path = model_path, feature_source = source, tissue_ids = payload$tissue_ids))
  attr(result, "source_name") <- "Explicit human-reviewed classifier (uncalibrated)"
  result
}
