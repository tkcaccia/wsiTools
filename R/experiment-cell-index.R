# Immutable, read-only cell indexes. SQL selects bounded rows before JSON parsing.
wsi_cell_index_hash_cache <- new.env(parent = emptyenv())

wsi_cell_index_checked_file <- function(entry, root, label) {
  file <- wsi_experiment_file(entry, root, label, FALSE)
  info <- file.info(file$resolved_path)
  stamp <- paste(file$sha256, info$size, as.numeric(info$mtime), as.numeric(info$ctime), sep = ":")
  previous <- wsi_cell_index_hash_cache[[file$resolved_path]]
  if (!identical(previous, stamp)) {
    if (!identical(wsi_experiment_sha256(file$resolved_path), tolower(file$sha256)))
      wsi_abort(paste("SHA-256 mismatch for", label))
    wsi_cell_index_hash_cache[[file$resolved_path]] <- stamp
  }
  file
}

wsi_cell_index_connection <- function(reference) {
  if (!requireNamespace("DBI", quietly = TRUE) || !requireNamespace("RSQLite", quietly = TRUE))
    wsi_abort("Indexed whole-slide cells require the optional DBI and RSQLite packages.")
  reference <- wsi_cell_index_checked_file(reference, ".", "cell SQLite index")
  for (suffix in c("-wal", "-journal")) {
    companion <- paste0(reference$resolved_path, suffix)
    if (file.exists(companion) && file.info(companion)$size > 0)
      wsi_abort("Cell index has an active journal/WAL; publish a checkpointed immutable SQLite file before opening it.")
  }
  connection <- DBI::dbConnect(RSQLite::SQLite(), dbname = reference$resolved_path, flags = RSQLite::SQLITE_RO)
  tryCatch({
    DBI::dbExecute(connection, "PRAGMA query_only=ON")
    DBI::dbExecute(connection, "PRAGMA trusted_schema=OFF")
    DBI::dbExecute(connection, "PRAGMA busy_timeout=1000")
  }, error = function(error) { DBI::dbDisconnect(connection); stop(error) })
  connection
}

wsi_cell_index_metadata <- function(connection) {
  hidden <- DBI::dbGetQuery(connection, "SELECT name,type FROM sqlite_master WHERE type IN ('view','trigger') LIMIT 1")
  if (nrow(hidden)) wsi_abort("Cell indexes must contain data tables only; SQLite views and triggers are not supported.")
  tables <- DBI::dbListTables(connection)
  if (!all(c("metadata", "cells", "predictions", "features", "bbox_rtree") %in% tables))
    wsi_abort("Cell index is missing required tables. Rebuild it with SQLite RTree support for bounded viewport queries.")
  sizes <- DBI::dbGetQuery(connection, "SELECT key,length(CAST(value AS BLOB)) AS bytes FROM metadata LIMIT 129")
  if (nrow(sizes) > 128L || anyDuplicated(sizes$key) || any(sizes$bytes > 2 * 1024^2))
    wsi_abort("Cell index metadata exceeds its bounded schema.")
  raw <- DBI::dbGetQuery(connection, "SELECT key,value FROM metadata LIMIT 129")
  metadata <- stats::setNames(lapply(raw$value, jsonlite::fromJSON, simplifyVector = FALSE), raw$key)
  version <- suppressWarnings(as.integer(metadata$schema_version))
  if (!identical(metadata$schema, "cellphenotyper-cell-index") ||
      length(version) != 1L || is.na(version) || !version %in% c(1L, 2L, 3L) ||
      (version %in% c(2L, 3L) &&
       !identical(metadata$source_format, "cellphenotyper-cellvit-shards-v1")))
    wsi_abort("Unsupported cell SQLite index schema/version.")
  metadata
}

wsi_cell_index_review_catalog <- function(connection, metadata, ref, root, image, feature, header) {
  if (!is.null(ref$tissue_context))
    wsi_abort("This indexed source has a separate tissue-context sidecar; indexed context support is not implemented.")
  sidecars <- wsi_review_rows(metadata$sidecars %||% list(), "indexed sidecars")
  if (length(sidecars) > 1L || any(vapply(sidecars, function(value)
      !identical(value$role, "segmentation_review") || !identical(value$indexed_table, "segmentation_review"), logical(1))))
    wsi_abort("Unsupported indexed review/context sidecar metadata.")
  if (!length(sidecars)) {
    if (!is.null(ref$segmentation_review) || "segmentation_review" %in% DBI::dbListTables(connection))
      wsi_abort("Cell segmentation review requires an explicitly bound indexed sidecar and data table.")
    return(NULL)
  }
  if (is.null(ref$segmentation_review) || is.null(ref$feature_manifest))
    wsi_abort("Indexed segmentation review requires registered review and feature_manifest references.")
  review <- wsi_cell_index_checked_file(ref$segmentation_review, root, "indexed segmentation review")
  manifest_ref <- wsi_cell_index_checked_file(ref$feature_manifest, root, "indexed cell feature manifest")
  source <- sidecars[[1L]]
  if (!identical(source$sha256, review$sha256) ||
      !identical(source$source_frame_id, image$frame_id) || !identical(review$source_frame_id, image$frame_id) ||
      !identical(source$coordinate_convention, image$coordinate_convention) ||
      !identical(review$coordinate_convention, image$coordinate_convention) ||
      !identical(source$schema, "cellphenotyper.segmentation-review/v1") ||
      !identical(source$reference_quality, "model_agreement_only") || !identical(source$cell_type_evidence_changed, FALSE))
    wsi_abort("Indexed segmentation review source hash/frame/convention or semantics differs from its registered evidence.")
  if (file.info(manifest_ref$resolved_path)$size > 8 * 1024^2)
    wsi_abort("Indexed feature manifest exceeds its bounded header size.")
  manifest <- jsonlite::fromJSON(manifest_ref$resolved_path, simplifyVector = FALSE)
  if (!identical(manifest$schema, "cellphenotyper-cell-features") || !identical(as.integer(manifest$schema_version), 1L) ||
      !identical(source$feature_manifest_sha256, manifest_ref$sha256) ||
      !identical(metadata$feature_manifest$sha256, manifest_ref$sha256) ||
      !identical(header$provenance$feature_manifest_sha256, manifest_ref$sha256))
    wsi_abort("Indexed segmentation review feature-manifest lineage differs from the classification.")
  for (key in c("sha256", "frame_id", "width", "height", "mpp_x", "mpp_y", "coordinate_convention", "coordinate_space", "origin_x", "origin_y"))
    if (is.null(manifest$image[[key]]) || !isTRUE(all.equal(manifest$image[[key]], image[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort(paste("Indexed segmentation review feature-manifest image differs:", key))
  definition <- wsi_experiment_validate_feature(manifest$feature_set)
  if (!identical(definition$sha256, feature$sha256) || !identical(definition$feature_set_id, feature$feature_set_id) ||
      !identical(wsi_experiment_feature_signature(definition), wsi_experiment_feature_signature(feature)))
    wsi_abort("Indexed segmentation review does not describe the exact registered cell feature representation.")
  input_hash <- manifest$source$normalized_segmentation$sha256 %||% manifest$source$cells_sha256
  if (!grepl("^[0-9a-f]{64}$", input_hash %||% "") || !identical(source$source_cells_sha256, input_hash))
    wsi_abort("Indexed segmentation review original cell input differs from verified feature lineage.")
  expected_frame <- image[c("width", "height", "mpp_x", "mpp_y", "coordinate_space", "origin_x", "origin_y")]
  expected_frame$source_image_sha256 <- image$sha256
  if (!setequal(names(source$frame), names(expected_frame)) || any(vapply(names(expected_frame), function(key)
      !isTRUE(all.equal(source$frame[[key]], expected_frame[[key]], tolerance = 0, check.attributes = FALSE)), logical(1))))
    wsi_abort("Indexed segmentation review native image frame differs from classification.")
  counts <- vapply(c("cell_count", "original_cell_count", "candidate_cell_count", "rasterized_cell_count", "geometry_unavailable_cell_count"),
    function(key) wsi_experiment_number(source[[key]], paste("indexed review", key), FALSE, TRUE), numeric(1))
  if (any(counts < 0) || any(counts[1:3] != metadata$cell_count) || sum(counts[4:5]) != metadata$cell_count ||
      !is.logical(source$quarantine_invalid_geometry) || length(source$quarantine_invalid_geometry) != 1L || is.na(source$quarantine_invalid_geometry))
    wsi_abort("Indexed segmentation review counts/quarantine do not preserve the complete canonical population.")
  overlap_flag <- source$quarantine_overlapping_geometry %||% FALSE
  if (!is.logical(overlap_flag) || length(overlap_flag) != 1L || is.na(overlap_flag))
    wsi_abort("Indexed overlap quarantine requires an explicit boolean.")
  reason_fields <- c("invalid_geometry_unavailable_cell_count", "overlapping_geometry_unavailable_cell_count")
  reason_counts <- if (any(reason_fields %in% names(source)) || overlap_flag)
    vapply(reason_fields, function(key) wsi_experiment_number(source[[key]], key, FALSE, TRUE), numeric(1)) else c(counts[[5L]], 0)
  if (any(reason_counts < 0) || sum(reason_counts) != counts[[5L]] ||
      (reason_counts[[1L]] > 0 && !isTRUE(source$quarantine_invalid_geometry)) || (reason_counts[[2L]] > 0 && !overlap_flag))
    wsi_abort("Indexed invalid/overlapping geometry counts require their separate explicit quarantine opt-ins.")
  columns <- DBI::dbGetQuery(connection, "PRAGMA table_info(segmentation_review)")$name
  if (!setequal(columns, c("row_id", "cell_id", "source_id", "qc_json")))
    wsi_abort("Indexed segmentation review lacks its checked scalar data table.")
  population <- DBI::dbGetQuery(connection, "SELECT COUNT(*) AS n FROM segmentation_review")$n[[1L]]
  mismatch <- DBI::dbGetQuery(connection, "SELECT 1 FROM cells c LEFT JOIN segmentation_review q USING(row_id) WHERE q.row_id IS NULL OR q.cell_id!=c.cell_id OR q.source_id!=c.source_id LIMIT 1")
  if (population != metadata$cell_count || nrow(mismatch))
    wsi_abort("Indexed segmentation review does not join every canonical/source cell exactly once.")
  source
}

wsi_experiment_cell_index_catalog <- function(experiment, tissue) {
  ref <- tissue$cell_classification
  if (!identical(ref$index$format, "sqlite")) wsi_abort("Cell index must explicitly declare format='sqlite'.")
  source <- wsi_cell_index_checked_file(ref, experiment$root, "cell classification")
  index <- wsi_cell_index_checked_file(ref$index, experiment$root, "cell SQLite index")
  # Internal operations use an absolute reference; client metadata contains no connection.
  index$path <- index$resolved_path
  connection <- wsi_cell_index_connection(index)
  on.exit(DBI::dbDisconnect(connection))
  metadata <- wsi_cell_index_metadata(connection)
  partitioned <- as.integer(metadata$schema_version) %in% c(2L, 3L)
  if (partitioned) {
    if (!is.null(ref$segmentation_review) || !identical(ref$feature_manifest$sha256, source$sha256))
      wsi_abort("Partitioned CellViT source requires one checked manifest and no legacy review sidecar.")
    if (file.info(source$resolved_path)$size > 8 * 1024^2)
      wsi_abort("CellViT partition manifest exceeds the bounded header size.")
    partitions <- jsonlite::fromJSON(source$resolved_path, simplifyVector = FALSE)
    if (!identical(partitions$schema, "cellphenotyper-cellvit-shards-v1") ||
        !identical(partitions$image, metadata$image) ||
        !identical(partitions$cell_count, metadata$cell_count) ||
        length(partitions$partitions) != partitions$partition_count)
      wsi_abort("CellViT partition manifest differs from indexed global image or population.")
  }
  if (!identical(metadata$classification$sha256 %||% metadata$classification_sha256, source$sha256))
    wsi_abort("Cell index classification source hash differs from the registered source.")
  data <- metadata$classification_header %||% list(image = metadata$image, feature_set = metadata$feature_set, models = metadata$models)
  if (!is.null(data$schema) && (!identical(data$schema, "cellphenotyper-cell-classification") ||
      !identical(as.integer(data$schema_version), 1L))) wsi_abort("Cell index header has an unsupported classification schema.")
  if (length(data$cells %||% list())) wsi_abort("Indexed classification header must not contain cell rows.")
  registration <- wsi_experiment_cell_registration(data$image, tissue$image, ref$registration)
  feature <- wsi_experiment_validate_feature(data$feature_set)
  if (!feature$kind %in% c("cellvit-cell", "cell-morphology") ||
      !identical(feature$frame_id, data$image$frame_id)) wsi_abort("Indexed cells require exact typed cell feature/frame metadata.")
  feature <- wsi_cell_index_checked_file(feature, dirname(index$resolved_path), "indexed cell features")
  if (partitioned) {
    if (!identical(feature$format, "partitioned-sqlite") ||
        file.info(feature$resolved_path)$size > 8 * 1024^2)
      wsi_abort("Partitioned feature pointer must be a small checked JSON document.")
    pointer <- jsonlite::fromJSON(feature$resolved_path, simplifyVector = FALSE)
    expected_pointer <- if (identical(as.integer(metadata$schema_version), 3L))
      "cellphenotyper-partitioned-cell-features-v2" else
      "cellphenotyper-partitioned-cell-features-v1"
    expected_storage <- if (identical(as.integer(metadata$schema_version), 3L))
      "partition-row-reference-v1" else
      "ordered-server-only-JSON-vector-per-cell"
    if ((identical(as.integer(metadata$schema_version), 3L) &&
         !identical(metadata$feature_storage, expected_storage)) ||
        !identical(pointer$schema, expected_pointer) ||
        !identical(pointer$partition_manifest_sha256, source$sha256) ||
        !identical(pointer$image, metadata$image) ||
        !identical(pointer$row_count, metadata$cell_count) ||
        !identical(pointer$feature_set_id, feature$feature_set_id) ||
        !identical(unlist(pointer$feature_columns, use.names = FALSE), feature$feature_columns))
      wsi_abort("Partitioned feature pointer differs from the registered CellViT source.")
  }
  if (!identical(metadata$feature_csv_sha256 %||% metadata$feature_set$sha256, feature$sha256))
    wsi_abort("Cell index feature artifact hash differs from its classification feature source.")
  registered <- Filter(function(f) identical(f$feature_set_id, feature$feature_set_id), tissue$feature_sets)
  if (length(registered) != 1L || !identical(registered[[1L]]$sha256, feature$sha256) ||
      !identical(wsi_experiment_feature_signature(registered[[1L]]), wsi_experiment_feature_signature(feature)))
    wsi_abort("Indexed feature representation is not registered exactly once with the matching signature.")
  if (!is.null(registration) && (!identical(registered[[1L]]$frame_id, tissue$image$frame_id) ||
      !identical(registered[[1L]]$source_frame_id, data$image$frame_id) ||
      !identical(registered[[1L]]$registration, registration))) wsi_abort("Indexed feature frame alias differs from its checked registration.")
  count <- wsi_experiment_number(metadata$cell_count, "cell index cell_count", positive = FALSE, integer = TRUE)
  if (count < 0 || count != feature$row_count) wsi_abort("Cell index count differs from the feature representation.")
  actual <- DBI::dbGetQuery(connection, "SELECT (SELECT COUNT(*) FROM cells) AS cells,(SELECT COUNT(*) FROM features) AS features")
  if (actual$cells[[1L]] != count || actual$features[[1L]] != count) wsi_abort("Cell index identity/feature table counts contradict its declared population.")
  review <- wsi_cell_index_review_catalog(connection, metadata, ref, experiment$root, data$image, feature, data)
  models <- wsi_review_rows(data$models, "indexed cell models")
  ids <- vapply(models, function(model) wsi_experiment_text(model$model_id, "model_id"), character(1))
  if (length(models) > 64L || anyDuplicated(ids)) wsi_abort("Cell index model catalog is invalid or exceeds its bound.")
  for (model in models) {
    if (!model$status %in% c("available", "unavailable")) wsi_abort("Indexed model availability must be explicit.")
    if (model$status == "unavailable") wsi_experiment_text(model$reason, "model.reason")
    else if (!identical(model$backbone, feature$descriptor$backbone) ||
      !grepl("^[a-fA-F0-9]{64}$", model$checkpoint_sha256 %||% "") ||
      !length(model$class_names) || length(model$class_names) > 1024L || anyDuplicated(unlist(model$class_names)) ||
      !model$score_semantics %in% c("uncalibrated_model_probability", "uncalibrated_pixel_type_vote_fraction", "uncalibrated_softmax_cell_class_vote_counts"))
      wsi_abort("Indexed available model requires its exact backbone, checkpoint, ordered classes and score semantics.")
  }
  list(tissue_id = tissue$tissue_id, name = tissue$name, indexed = TRUE, cells = list(), cell_count = count,
    frame_id = tissue$image$frame_id, source_frame_id = data$image$frame_id, source_sha256 = source$sha256,
    feature_set_id = feature$feature_set_id, descriptor = feature$descriptor, models = models,
    registration = registration, index_reference = index, index_metadata = metadata,
    segmentation_review = review,
    tissue_context = NULL)
}

wsi_cell_index_hydrate_review <- function(cell, row, source) {
  if (!is.list(row) || !row$cell_id %in% c(cell$cell_id, cell$source_id))
    wsi_abort("Indexed segmentation review has a contradictory canonical/source cell ID.")
  original <- row$original_cell %||% list()
  if (!is.list(original) || (!is.null(row$source_id) && !identical(row$source_id, cell$source_id)) ||
      (!is.null(original$cell_id) && !identical(original$cell_id, cell$cell_id)) ||
      (!is.null(original$source_id) && !identical(original$source_id, cell$source_id)) ||
      (isTRUE(source$explicit_geometry_status) && (!identical(original$cell_id, cell$cell_id) || !identical(original$source_id, cell$source_id))))
    wsi_abort("Indexed segmentation review changed immutable original cell IDs.")
  for (key in intersect(c("source", "x", "y", "contour", "bbox", "label", "predictions", "native_type_id", "native_type_probability"), intersect(names(original), names(cell))))
    if (!isTRUE(all.equal(original[[key]], cell[[key]], tolerance = 0, check.attributes = FALSE)))
      wsi_abort("Indexed segmentation review changed original geometry or classification evidence.")
  if (!is.null(original$centroid) && !isTRUE(all.equal(unlist(original$centroid), c(cell$x, cell$y), tolerance = 0, check.attributes = FALSE)))
    wsi_abort("Indexed segmentation review changed the original centroid.")
  status <- row$segmentation_review_status %||% "available"
  if (!is.character(status) || length(status) != 1L || is.na(status) || !status %in% c("available", "unavailable") ||
      (isTRUE(source$explicit_geometry_status) && is.null(row$segmentation_review_status)))
    wsi_abort("Indexed segmentation review requires explicit available/unavailable status.")
  for (key in c("segmentation_disagreement", "segmentation_review_required", "segmentation_split_signal", "segmentation_merge_signal"))
    if (!is.logical(row[[key]]) || length(row[[key]]) != 1L || is.na(row[[key]]))
      wsi_abort(paste("Indexed segmentation review requires boolean", key))
  reasons <- unlist(row$segmentation_review_reason, use.names = FALSE)
  if ((length(reasons) && (!is.character(reasons) || anyNA(reasons) || any(!nzchar(trimws(reasons))))) ||
      anyDuplicated(reasons) || !identical(length(reasons) > 0L, row$segmentation_disagreement) ||
      !row$segmentation_action %in% c("retain_original", "adjust_boundary") ||
      !identical(row$segmentation_reference_quality, "model_agreement_only") ||
      !identical(row$segmentation_review_required, row$segmentation_disagreement || row$segmentation_action == "adjust_boundary"))
    wsi_abort("Indexed segmentation review reasons/action/reference semantics are inconsistent.")
  metrics <- c("segmentation_iou", "segmentation_cellvit_coverage", "segmentation_stardist_coverage")
  if (status == "unavailable") {
    reason <- wsi_experiment_text(row$segmentation_geometry_unavailable_reason, "segmentation_geometry_unavailable_reason")
    wsi_cell_geometry_quarantine_reason(reasons, source)
    if (!nzchar(trimws(reason)) ||
        !isTRUE(row$segmentation_disagreement) || !isTRUE(row$segmentation_review_required) ||
        !identical(row$segmentation_action, "retain_original") || !identical(row$segmentation_auto_adjustment_eligible, FALSE) ||
        !identical(row$segmentation_split_signal, FALSE) || !identical(row$segmentation_merge_signal, FALSE) ||
        (!is.null(row$auto_adjustment_eligible) && !identical(row$auto_adjustment_eligible, FALSE)) ||
        !all(metrics %in% names(row)) || any(!vapply(row[metrics], is.null, logical(1))))
      wsi_abort("Unavailable indexed segmentation geometry must preserve original evidence, quarantine/reasons and explicit null metrics.")
  } else {
    if (any(c("invalid_original_geometry", "overlapping_original_geometry") %in% reasons) || !is.null(row$segmentation_geometry_unavailable_reason))
      wsi_abort("Available indexed segmentation review cannot declare unavailable original geometry.")
    for (key in metrics) {
      value <- wsi_experiment_number(row[[key]], key, FALSE)
      if (value < 0 || value > 1) wsi_abort("Indexed segmentation agreement fractions must be between zero and one.")
    }
  }
  fields <- row[grepl("^segmentation_", names(row)) | names(row) == "stardist_label"]
  cell[names(fields)] <- fields
  cell$segmentation_review_status <- status
  cell$segmentation_review_source_sha256 <- source$sha256
  cell
}

wsi_cell_index_runtime_rows <- function(table, experiment, tissue, catalog) {
  if (!nrow(table)) return(list())
  maximum <- getOption("wsiTools.cell_index_max_row_bytes", 1024^2)
  if (any(nchar(table$raw_json, type = "bytes") > maximum)) wsi_abort("Indexed cell geometry exceeds the bounded per-cell payload.")
  if (!is.null(catalog$segmentation_review) && (!"qc_json" %in% names(table) || anyNA(table$qc_json) ||
      any(nchar(table$qc_json, type = "bytes") > maximum)))
    wsi_abort("Indexed segmentation review exceeds its bounded per-cell payload or is missing.")
  lapply(seq_len(nrow(table)), function(i) {
    cell <- jsonlite::fromJSON(table$raw_json[[i]], simplifyVector = FALSE)
    if (!identical(cell$cell_id, table$cell_id[[i]]) || !identical(cell$source_id, table$source_id[[i]]) ||
        !isTRUE(all.equal(cell$x, table$x[[i]], check.attributes = FALSE, tolerance = 0)) ||
        !isTRUE(all.equal(cell$y, table$y[[i]], check.attributes = FALSE, tolerance = 0)) ||
        !is.finite(cell$x) || !is.finite(cell$y) || cell$x < 0 || cell$x >= tissue$image$width || cell$y < 0 || cell$y >= tissue$image$height)
      wsi_abort("Indexed cell payload contradicts its canonical identity or native coordinates.")
    for (model in catalog$models) {
      prediction <- cell$predictions[[model$model_id]]
      if (is.null(prediction)) {
        if (model$status == "available") wsi_abort("Indexed available model has a missing cell prediction.")
        next
      }
      if (!prediction$status %in% c("predicted", "abstained", "unavailable")) wsi_abort("Indexed prediction status is invalid.")
      if (prediction$status == "predicted") {
        index <- wsi_experiment_number(prediction$class_id, "class_id", FALSE, TRUE) + 1L
        classes <- unlist(model$class_names, use.names = FALSE)
        if (model$status != "available" || index < 1 || index > length(classes) || !identical(prediction$label, classes[[index]]))
          wsi_abort("Indexed class differs from its exact ordered taxonomy.")
        score <- prediction$probability
        missing <- is.null(score) && identical(model$score_semantics, "uncalibrated_softmax_cell_class_vote_counts")
        if (!missing && (!is.numeric(score) || length(score) != 1L || !is.finite(score) || score < 0 || score > 1))
          wsi_abort("Indexed cell score is not a finite uncalibrated fraction.")
        vector <- unlist(prediction$probabilities, use.names = FALSE)
        native_vote <- identical(model$score_semantics, "uncalibrated_pixel_type_vote_fraction")
        if ((native_vote && (!is.null(prediction$probabilities) ||
            !identical(prediction$probability_vector_status, "not_exported_by_upstream"))) ||
            (identical(model$score_semantics, "uncalibrated_model_probability") && !length(vector)) ||
            (missing && !identical(prediction$probability_vector_status, "not_exported_by_upstream")))
          wsi_abort("Indexed cell score/vector availability contradicts its declared model semantics.")
        if (length(vector) && (length(vector) != length(classes) || !is.numeric(vector) || any(!is.finite(vector)) ||
            any(vector < 0 | vector > 1) || abs(sum(vector) - 1) > 1e-5 || abs(vector[[index]] - score) > 1e-6))
          wsi_abort("Indexed cell probability vector is inconsistent.")
      }
    }
    if (!is.null(catalog$segmentation_review))
      cell <- wsi_cell_index_hydrate_review(cell, jsonlite::fromJSON(table$qc_json[[i]], simplifyVector = FALSE), catalog$segmentation_review)
    cell$id <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, catalog$feature_set_id, cell$cell_id)
    cell$tissue_id <- tissue$tissue_id; cell$frame_id <- tissue$image$frame_id; cell$source_frame_id <- catalog$source_frame_id
    if (is.null(cell$segmentation_review_status)) {
      cell$segmentation_review_status <- if (!is.null(cell$segmentation_disagreement)) "available" else "unavailable"
      if (cell$segmentation_review_status == "unavailable") cell$segmentation_review_unavailable_reason <- "No checked StarDist review is indexed for this cell."
    }
    cell
  })
}

wsi_cell_index_local_id <- function(id, experiment, tissue, catalog) {
  id <- wsi_experiment_text(id, "cell_id")
  prefix <- paste0(wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, catalog$feature_set_id, ""))
  if (startsWith(id, prefix)) {
    decoded <- utils::URLdecode(substring(id, nchar(prefix) + 1L))
    if (!identical(wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, catalog$feature_set_id, decoded), id))
      wsi_abort("Invalid indexed cell namespace encoding.")
    decoded
  } else id
}

# The display stream is separate from rich review rows. SQLite extracts only
# bounded scalar evidence; neither contours nor feature vectors enter R here.
wsi_cell_index_display_points <- function(experiment, tissue, catalog, payload) {
  model_id <- wsi_experiment_text(payload$model_id, "display model_id")
  model_ids <- vapply(catalog$models, `[[`, character(1), "model_id")
  if (!model_id %in% model_ids) wsi_abort("Unknown indexed display model.")
  model <- catalog$models[[match(model_id, model_ids)]]
  limit <- wsi_experiment_number(payload$display_limit %||% 5000L, "display_limit", integer = TRUE)
  if (limit > 10000L) wsi_abort("Cell display_limit exceeds its bounded 10000-point page size.")
  maximum <- wsi_experiment_number(getOption("wsiTools.cell_index_display_max_response_bytes", 8 * 1024^2),
    "cell display response budget", integer = TRUE)
  maximum <- min(maximum, 8 * 1024^2)
  scalar_max <- 4096L
  expected <- list(source_sha256 = catalog$source_sha256, index_sha256 = catalog$index_reference$sha256,
    feature_set_id = catalog$feature_set_id, frame_id = catalog$frame_id, source_frame_id = catalog$source_frame_id)
  repeated_text <- c(experiment$experiment_id, tissue$tissue_id, model_id, unlist(expected, use.names = FALSE),
    if (model$status == "unavailable") model$reason)
  if (anyNA(repeated_text) || any(nchar(repeated_text, type = "bytes") > scalar_max))
    wsi_abort("Indexed display identity/model evidence exceeds its bounded scalar schema.")
  for (key in names(expected)) if (!is.null(payload[[key]]) && !identical(payload[[key]], expected[[key]]))
    wsi_abort(paste("Cell display request has a stale", key, "; reopen this view."))
  threshold <- wsi_experiment_number(payload$threshold %||% 0, "display threshold", FALSE)
  if (threshold < 0 || threshold > 1) wsi_abort("Display threshold must be between zero and one.")
  class_name <- payload$class_name
  if (!is.null(class_name)) {
    class_name <- wsi_experiment_text(class_name, "display class_name")
    if (class_name %in% c("*", "all")) class_name <- NULL
  }
  classes <- unlist(model$class_names, use.names = FALSE)
  if (!is.null(class_name) && class_name != "__review__" && !class_name %in% classes)
    wsi_abort("Unknown display class for the selected taxonomy.")
  bounds <- NULL
  if (!is.null(payload$bbox)) {
    bounds <- vapply(c("xmin", "ymin", "xmax", "ymax"), function(key)
      wsi_experiment_number(payload$bbox[[key]], paste0("bbox.", key), FALSE), numeric(1))
    if (bounds[[1L]] >= bounds[[3L]] || bounds[[2L]] >= bounds[[4L]]) wsi_abort("Display bbox must have positive area.")
    bounds <- as.list(stats::setNames(bounds, c("xmin", "ymin", "xmax", "ymax")))
  }
  scope <- wsi_review_digest(c(list(mode = "display_points", experiment_id = experiment$experiment_id,
    tissue_id = tissue$tissue_id, model_id = model_id, bbox = bounds, class_name = class_name,
    threshold = threshold), expected))
  after <- 0
  if (!is.null(payload$cursor)) {
    if (!is.character(payload$cursor) || length(payload$cursor) != 1L || is.na(payload$cursor) ||
        nchar(payload$cursor, type = "bytes") > 4096L) wsi_abort("Invalid cell display page cursor.")
    cursor <- tryCatch(jsonlite::fromJSON(payload$cursor, simplifyVector = FALSE),
      error = function(error) wsi_abort("Invalid cell display page cursor."))
    if (!identical(cursor$scope, scope)) wsi_abort("Cell display cursor belongs to a different source/filter; restart this view.")
    after <- wsi_experiment_number(cursor$row_id, "display cursor.row_id", integer = TRUE)
  }
  connection <- wsi_cell_index_connection(catalog$index_reference)
  on.exit(DBI::dbDisconnect(connection))
  tryCatch(DBI::dbGetQuery(connection, "SELECT json_extract('{\"ok\":1}', '$.ok') AS ok"),
    error = function(error) wsi_abort("Compact cell display requires SQLite JSON scalar extraction support; install a JSON-enabled RSQLite build."))
  if (nrow(DBI::dbGetQuery(connection, "SELECT row_id FROM cells WHERE row_id<=0 LIMIT 1")))
    wsi_abort("Indexed display requires positive immutable row identities for keyset paging.")
  join <- " LEFT JOIN predictions p ON p.row_id=c.row_id AND p.model_id=?"
  review_json <- "c.raw_json"
  if (!is.null(catalog$segmentation_review)) {
    join <- paste0(join, " LEFT JOIN segmentation_review q ON q.row_id=c.row_id")
    review_json <- "q.qc_json"
  }
  params <- list(model_id); where <- character()
  if (!is.null(bounds)) {
    join <- paste0(join, " JOIN bbox_rtree b ON b.row_id=c.row_id")
    where <- c(where, "b.xmin<=? AND b.xmax>=? AND b.ymin<=? AND b.ymax>=?",
      "c.xmin<=? AND c.xmax>=? AND c.ymin<=? AND c.ymax>=?")
    params <- c(params, rep(bounds[c("xmax", "xmin", "ymax", "ymin")], 2L))
  }
  if (!is.null(class_name)) {
    if (class_name == "__review__") {
      where <- c(where, "(p.status IS NULL OR p.status!='predicted' OR p.probability IS NULL OR p.probability<?)")
      params <- c(params, list(threshold))
    } else {
      where <- c(where, "p.status='predicted'", "p.label=?")
      params <- c(params, list(class_name))
    }
  }
  clause <- function(parts) if (length(parts)) paste(" WHERE", paste(parts, collapse = " AND ")) else ""
  total <- as.numeric(DBI::dbGetQuery(connection, paste0("SELECT COUNT(*) AS n FROM cells c", join, clause(where)),
    params = unname(params))$n[[1L]])
  # JSON strings and IDs are cut at the SQL boundary before crossing into R.
  scalar <- function(paths, object = "c.raw_json") paste0("COALESCE(", paste(vapply(paths, function(path)
    paste0("json_extract(", object, ",'", path, "')"), character(1)), collapse = ","), ",NULL)")
  kind <- function(paths, object = "c.raw_json") paste0("COALESCE(", paste(vapply(paths, function(path)
    paste0("NULLIF(json_type(", object, ",'", path, "'),'null')"), character(1)), collapse = ","), ",NULL)")
  text <- function(expression) paste0("substr(", expression, ",1,", scalar_max + 1L, ")")
  number <- function(object, path) paste0("CASE WHEN json_type(", object, ",'", path,
    "') IN ('integer','real') THEN json_extract(", object, ",'", path, "') ELSE NULL END")
  flag <- function(paths, object = "c.raw_json") {
    types <- vapply(paths, function(path) paste0("json_type(", object, ",'", path, "')"), character(1))
    values <- vapply(paths, function(path) paste0("json_extract(", object, ",'", path, "')"), character(1))
    paste0("CASE WHEN ", paste(paste0(types, " NOT IN ('true','false','null')"), collapse = " OR "),
      " THEN -1 WHEN ", paste(paste0(values, "=1"), collapse = " OR "), " THEN 1 ELSE 0 END")
  }
  status_paths <- c("$.segmentation_review_status", "$.segmentation.segmentation_review_status", "$.disagreement.segmentation_review_status")
  reason_paths <- c("$.segmentation_geometry_unavailable_reason", "$.segmentation.segmentation_geometry_unavailable_reason", "$.disagreement.segmentation_geometry_unavailable_reason")
  disagreement_paths <- c("$.segmentation_disagreement", "$.segmentation.segmentation_disagreement", "$.disagreement.segmentation_disagreement")
  required_paths <- c("$.segmentation_review_required", "$.segmentation.segmentation_review_required", "$.disagreement.segmentation_review_required")
  fields <- c("c.row_id", paste0(text("c.cell_id"), " AS cell_id"), paste0(text("c.source_id"), " AS source_id"), "c.x", "c.y",
    paste0(text("p.status"), " AS status"), "p.class_id", paste0(text("p.label"), " AS label"), "p.probability",
    paste0(text(scalar("$.cell_id")), " AS raw_cell_id"), paste0(text(scalar("$.source_id")), " AS raw_source_id"),
    paste0(number("c.raw_json", "$.x"), " AS raw_x"), paste0(number("c.raw_json", "$.y"), " AS raw_y"),
    paste0(text(scalar(status_paths, review_json)), " AS review_status"), paste0(kind(status_paths, review_json), " AS review_status_type"),
    paste0(text(scalar(reason_paths, review_json)), " AS geometry_reason"), paste0(kind(reason_paths, review_json), " AS geometry_reason_type"),
    paste0(flag(disagreement_paths, review_json), " AS disagreement"), paste0(flag(required_paths, review_json), " AS review_required"),
    paste0(kind(c(disagreement_paths, required_paths), review_json), " AS review_flag_type"),
    paste0(text(scalar("$.tissue_context.label")), " AS context_label"), paste0(kind("$.tissue_context.label"), " AS context_label_type"),
    # j stays inside SQLite: only scalar checks, never its prediction JSON or vector, enter R.
    paste0(text("json_extract(j.value,'$.status')"), " AS raw_status"),
    paste0(number("j.value", "$.class_id"), " AS raw_class_id"),
    paste0(text("json_extract(j.value,'$.label')"), " AS raw_label"),
    paste0(number("j.value", "$.probability"), " AS raw_probability"),
    paste0(text("json_extract(j.value,'$.probability_vector_status')"), " AS vector_status"),
    "json_type(j.value,'$.probabilities') AS vector_type", "json_array_length(j.value,'$.probabilities') AS vector_length",
    "(SELECT total(v.value) FROM json_each(j.value,'$.probabilities') v) AS vector_sum",
    "(SELECT COUNT(*) FROM json_each(j.value,'$.probabilities') v WHERE v.type NOT IN ('integer','real') OR v.value<0 OR v.value>1) AS vector_invalid",
    "(SELECT CASE WHEN v.type IN ('integer','real') THEN v.value ELSE NULL END FROM json_each(j.value,'$.probabilities') v WHERE v.key=p.class_id LIMIT 1) AS vector_selected")
  table <- tryCatch(DBI::dbGetQuery(connection, paste0("SELECT ", paste(fields, collapse = ","), " FROM cells c", join,
    " LEFT JOIN json_each(c.raw_json,'$.predictions') j ON j.key=p.model_id",
    clause(c(where, "c.row_id>?")), " ORDER BY c.row_id ASC LIMIT ?"),
    params = unname(c(params, list(after, as.integer(limit + 1L))))),
    error = function(error) wsi_abort(paste("Compact cell display could not extract checked JSON scalar evidence:", conditionMessage(error))))
  truncated <- nrow(table) > limit
  if (truncated) table <- table[seq_len(limit), , drop = FALSE]
  coordinates <- c("x", "y", "raw_x", "raw_y")
  if (nrow(table) && (!all(vapply(table[coordinates], is.numeric, logical(1))) ||
      any(!is.finite(unlist(table[coordinates], use.names = FALSE))) ||
      any(table$x != table$raw_x | table$y != table$raw_y) ||
      any(table$x < 0 | table$x >= tissue$image$width | table$y < 0 | table$y >= tissue$image$height)))
    wsi_abort("Indexed display identity or finite native image coordinates contradict its canonical scalar evidence.")
  check_text <- function(value, label, required = FALSE) {
    if (is.na(value)) { if (required) wsi_abort(paste("Indexed display", label, "is missing.")); return(NULL) }
    if (!is.character(value) || (required && !nzchar(trimws(value))) || nchar(value, type = "bytes") > scalar_max)
      wsi_abort(paste("Indexed display", label, "exceeds its bounded scalar schema."))
    value
  }
  scalar_equal <- function(left, right) (is.na(left) && is.na(right)) || isTRUE(left == right)
  # The encoded namespace is invariant across this page; only the canonical
  # cell identifier needs encoding for each point.
  id_prefix <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, catalog$feature_set_id, "")
  # A lower bound on the returned string bytes rejects obviously oversized
  # pages before constructing/serializing nested point records. The exact
  # serialized response is checked again below, including escaping and keys.
  repeated_bytes <- sum(nchar(c(id_prefix, tissue$tissue_id, catalog$frame_id, catalog$source_frame_id, model_id,
    if (model$status == "unavailable") model$reason), type = "bytes"))
  scalar_bytes <- sum(vapply(table[c("cell_id", "source_id", "status", "label", "geometry_reason", "context_label")],
    function(value) sum(nchar(value, type = "bytes"), na.rm = TRUE), numeric(1))) +
    sum(nchar(table$cell_id, type = "bytes"), na.rm = TRUE)
  if (nrow(table) * repeated_bytes + scalar_bytes > maximum)
    wsi_abort("Compact cell display page exceeds its bounded response budget; request a smaller display_limit.")
  points <- lapply(seq_len(nrow(table)), function(i) {
    row <- lapply(table, `[[`, i)
    cell_id <- check_text(row$cell_id[[1L]], "cell_id", TRUE)
    source_id <- check_text(row$source_id[[1L]], "source_id", TRUE)
    if (!identical(cell_id, row$raw_cell_id[[1L]]) || !identical(source_id, row$raw_source_id[[1L]]))
      wsi_abort("Indexed display identity or finite native image coordinates contradict its canonical scalar evidence.")
    status <- check_text(row$status[[1L]], "prediction status")
    if (is.null(status) && model$status == "available") wsi_abort("Indexed available model has a missing display prediction.")
    if (!is.null(status) && !status %in% c("predicted", "abstained", "unavailable")) wsi_abort("Indexed display prediction status is invalid.")
    class_id <- row$class_id[[1L]]; label <- check_text(row$label[[1L]], "prediction label")
    probability <- row$probability[[1L]]
    if (model$status == "available" && (!identical(status, row$raw_status[[1L]]) ||
        !scalar_equal(class_id, row$raw_class_id[[1L]]) || !scalar_equal(probability, row$raw_probability[[1L]]) ||
        !identical(label, check_text(row$raw_label[[1L]], "raw prediction label"))))
      wsi_abort("Indexed display prediction contradicts its original scalar model evidence.")
    if (identical(status, "predicted")) {
      if (model$status != "available" || !is.numeric(class_id) || !is.finite(class_id) || class_id != floor(class_id) ||
          class_id < 0 || class_id >= length(classes) || !identical(label, classes[[class_id + 1L]]))
        wsi_abort("Indexed display class differs from its exact ordered taxonomy.")
      missing <- is.na(probability) && identical(model$score_semantics, "uncalibrated_softmax_cell_class_vote_counts") &&
        identical(row$vector_status[[1L]], "not_exported_by_upstream")
      if (!missing && (!is.numeric(probability) || length(probability) != 1L || !is.finite(probability) || probability < 0 || probability > 1))
        wsi_abort("Indexed display score is not a finite uncalibrated fraction or declared unavailable native score.")
      has_vector <- identical(row$vector_type[[1L]], "array")
      if (!is.na(row$vector_type[[1L]]) && !row$vector_type[[1L]] %in% c("array", "null"))
        wsi_abort("Indexed display probability evidence has an invalid JSON vector type.")
      if (identical(model$score_semantics, "uncalibrated_pixel_type_vote_fraction") &&
          ((!is.na(row$vector_type[[1L]]) && row$vector_type[[1L]] != "null") ||
            !identical(row$vector_status[[1L]], "not_exported_by_upstream")))
        wsi_abort("Indexed native vote display evidence must retain its unavailable probability-vector semantics.")
      if (identical(model$score_semantics, "uncalibrated_model_probability") && !has_vector)
        wsi_abort("Indexed probability display evidence requires its original probability vector.")
      if (has_vector && (row$vector_length != length(classes) || row$vector_invalid != 0L ||
          !is.finite(row$vector_sum) || abs(row$vector_sum - 1) > 1e-5 ||
          !is.finite(row$vector_selected) || is.na(probability) || abs(row$vector_selected - probability) > 1e-6))
        wsi_abort("Indexed display probability vector differs from its ordered taxonomy or selected score.")
      prediction <- list(status = status, class_id = class_id, label = label, probability = if (missing) NULL else probability)
    } else {
      if (!is.na(class_id) || !is.null(label) || !is.na(probability)) wsi_abort("Unavailable or abstained display evidence cannot contain a biological assignment.")
      prediction <- list(status = if (model$status == "unavailable") "unavailable" else status,
        class_id = NULL, label = NULL, probability = NULL)
      if (model$status == "unavailable") prediction$reason <- model$reason
    }
    review_status <- check_text(row$review_status[[1L]], "segmentation review status")
    geometry_reason <- check_text(row$geometry_reason[[1L]], "geometry reason")
    context_label <- check_text(row$context_label[[1L]], "tissue context label")
    for (key in c("review_status_type", "geometry_reason_type", "context_label_type"))
      if (!is.na(row[[key]][[1L]]) && row[[key]][[1L]] != "text") wsi_abort("Indexed display context/review scalar has an invalid JSON type.")
    if ((!is.null(review_status) && !review_status %in% c("available", "unavailable")) ||
        row$disagreement < 0 || row$review_required < 0) wsi_abort("Indexed display review evidence has an invalid status or boolean.")
    if (is.null(review_status)) review_status <- if (is.na(row$review_flag_type[[1L]])) "unavailable" else "available"
    list(id = paste0(id_prefix, utils::URLencode(cell_id, reserved = TRUE)),
      cell_id = cell_id, source_id = source_id, tissue_id = tissue$tissue_id, frame_id = catalog$frame_id,
      source_frame_id = catalog$source_frame_id, x = row$x[[1L]], y = row$y[[1L]],
      predictions = stats::setNames(list(prediction), model_id), segmentation_review_status = review_status,
      segmentation_disagreement = as.logical(row$disagreement[[1L]]), segmentation_review_required = as.logical(row$review_required[[1L]]),
      segmentation_geometry_unavailable_reason = geometry_reason, tissue_context = list(label = context_label))
  })
  if (anyDuplicated(vapply(points, `[[`, character(1), "id")) || anyDuplicated(vapply(points, `[[`, character(1), "source_id")))
    wsi_abort("Indexed display page contains duplicate canonical or source identities.")
  result <- c(list(mode = "display_points", indexed = TRUE, cells = list(), points = points,
    cell_count = catalog$cell_count, total = total, truncated = truncated,
    next_cursor = if (truncated && nrow(table)) as.character(jsonlite::toJSON(list(scope = scope, row_id = tail(table$row_id, 1L)),
      auto_unbox = TRUE, null = "null", digits = 17)) else NULL,
    query_scope = scope, model_id = model_id, bbox = bounds), expected)
  bytes <- nchar(jsonlite::toJSON(list(ok = TRUE, cell_index = result), auto_unbox = TRUE, null = "null", digits = 17), type = "bytes")
  if (bytes > maximum) wsi_abort("Compact cell display page exceeds its bounded response budget; request a smaller display_limit.")
  result
}

wsi_cell_index_select <- function(experiment, tissue, catalog, payload) {
  if (identical(payload$mode, "display_points")) return(wsi_cell_index_display_points(experiment, tissue, catalog, payload))
  if (!is.null(payload$class_name)) {
    if (!is.character(payload$class_name) || length(payload$class_name) != 1L || is.na(payload$class_name))
      wsi_abort("Cell class filter must be a single class name or '*'.")
    if (payload$class_name %in% c("", "*", "all")) payload$class_name <- NULL
  }
  mode <- payload$mode %||% "metadata"
  if (!mode %in% c("metadata", "summary", "viewport", "queue", "cell", "review_subset")) wsi_abort("Unknown cell index query mode.")
  limit <- wsi_experiment_number(payload$limit %||% 500L, "cell query limit", integer = TRUE)
  if (limit > min(2000L, getOption("wsiTools.cell_index_query_limit", 500L))) wsi_abort("Cell query limit exceeds its configured bounded page size.")
  connection <- wsi_cell_index_connection(catalog$index_reference)
  on.exit(DBI::dbDisconnect(connection))
  model_ids <- vapply(catalog$models, `[[`, character(1), "model_id")
  available <- model_ids[vapply(catalog$models, function(m) m$status == "available", logical(1))]
  model <- payload$model_id %||% if (length(available)) available[[1L]] else NULL
  if (!is.null(model) && (length(model) != 1L || is.na(model) || !model %in% model_ids)) wsi_abort("Unknown indexed cell model.")
  summary <- function() {
    if (is.null(model)) return(list())
    result <- DBI::dbGetQuery(connection, "SELECT model_id,status,label,COUNT(*) AS count FROM predictions WHERE model_id=? GROUP BY status,label LIMIT 2049", params = list(model))
    if (nrow(result) > 2048L) wsi_abort("Cell class summary exceeds the bounded catalog.")
    lapply(seq_len(nrow(result)), function(i) as.list(result[i,,drop = FALSE]))
  }
  result <- list(mode = mode, indexed = TRUE, cells = list(), summary = summary(), cell_count = catalog$cell_count,
    total = catalog$cell_count, truncated = FALSE, next_cursor = NULL, source_sha256 = catalog$source_sha256,
    index_sha256 = catalog$index_reference$sha256, feature_set_id = catalog$feature_set_id,
    descriptor = catalog$descriptor, frame_id = catalog$frame_id, models = catalog$models)
  if (mode %in% c("metadata", "summary")) return(result)
  where <- character(); params <- list(); join <- ""; sort <- "c.row_id ASC"; score <- "NULL AS queue_score"
  if (mode == "viewport") {
    bbox <- payload$bbox
    bounds <- vapply(c("xmin", "ymin", "xmax", "ymax"), function(key) wsi_experiment_number(bbox[[key]], paste0("bbox.", key), FALSE), numeric(1))
    if (bounds[[1L]] >= bounds[[3L]] || bounds[[2L]] >= bounds[[4L]]) wsi_abort("Viewport bbox must have positive area.")
    join <- " JOIN bbox_rtree b ON b.row_id=c.row_id"
    where <- c(where, "b.xmin<=? AND b.xmax>=? AND b.ymin<=? AND b.ymax>=?", "c.xmin<=? AND c.xmax>=? AND c.ymin<=? AND c.ymax>=?")
    params <- as.list(rep(bounds[c(3,1,4,2)], 2))
    if (!is.null(payload$class_name) && nzchar(payload$class_name)) {
      if (is.null(model)) wsi_abort("Choose a model for class-filtered viewport queries.")
      if (identical(payload$class_name, "__review__")) {
        threshold <- wsi_experiment_number(payload$threshold %||% 0, "review threshold", FALSE)
        if (threshold < 0 || threshold > 1) wsi_abort("Review threshold must be between zero and one.")
        join <- paste0(join, " LEFT JOIN predictions p ON p.row_id=c.row_id AND p.model_id=?")
        where <- c(where, "(p.status IS NULL OR p.status!='predicted' OR p.probability IS NULL OR p.probability<?)")
        params <- c(list(model), params, list(threshold))
      } else {
        classes <- unlist(catalog$models[[match(model, model_ids)]]$class_names, use.names = FALSE)
        if (!payload$class_name %in% classes) wsi_abort("Unknown viewport class for the selected taxonomy.")
        join <- paste0(join, " JOIN predictions p ON p.row_id=c.row_id")
        where <- c(where, "p.model_id=?", "p.status='predicted'", "p.label=?"); params <- c(params, list(model, payload$class_name))
      }
    }
  }
  if (mode == "queue") {
    if (is.null(model)) wsi_abort("Choose an available model for the cell review queue.")
    threshold <- wsi_experiment_number(payload$threshold %||% 1, "queue threshold", FALSE)
    if (threshold < 0 || threshold > 1) wsi_abort("Queue threshold must be between zero and one.")
    if (identical(payload$class_name, "__review__")) {
      join <- " LEFT JOIN predictions p ON p.row_id=c.row_id AND p.model_id=?"
      where <- c(where, "(p.status IS NULL OR p.status!='predicted' OR p.probability IS NULL OR p.probability<?)")
      params <- list(model, threshold)
    } else {
      # All classes and a named class use threshold only for UI review hints.
      # The explicit __review__ sentinel alone filters by that threshold.
      join <- " LEFT JOIN predictions p ON p.row_id=c.row_id AND p.model_id=?"
      params <- list(model)
    }
    if (!is.null(payload$class_name) && nzchar(payload$class_name) && payload$class_name != "__review__") {
      classes <- unlist(catalog$models[[match(model, model_ids)]]$class_names, use.names = FALSE)
      if (!payload$class_name %in% classes) wsi_abort("Unknown class in the selected cell taxonomy.")
      where <- c(where, "p.status='predicted'", "p.label=?"); params <- c(params, list(payload$class_name))
    }
    sort <- "COALESCE(p.probability,-1) ASC,c.row_id ASC"; score <- "COALESCE(p.probability,-1) AS queue_score"
  }
  if (mode %in% c("cell", "review_subset")) {
    ids <- if (mode == "cell") payload$cell_id else unlist(payload$cell_ids %||% payload$selected_cell_ids, use.names = FALSE)
    if (!length(ids) || length(ids) > limit || anyNA(ids) || anyDuplicated(ids)) wsi_abort("Choose explicit unique cell IDs within the bounded page size.")
    ids <- vapply(ids, wsi_cell_index_local_id, character(1), experiment = experiment, tissue = tissue, catalog = catalog)
    where <- c(where, paste0("c.cell_id IN (", paste(rep("?", length(ids)), collapse = ","), ")")); params <- as.list(ids)
  }
  clause <- function(parts) if (length(parts)) paste(" WHERE", paste(parts, collapse = " AND ")) else ""
  if (!is.null(catalog$segmentation_review)) join <- paste0(join, " LEFT JOIN segmentation_review q ON q.row_id=c.row_id")
  base_where <- where; base_params <- params
  result$total <- as.numeric(DBI::dbGetQuery(connection, paste0("SELECT COUNT(*) AS n FROM cells c", join, clause(where)), params = if (length(params)) unname(params) else NULL)$n[[1L]])
  scope <- wsi_review_digest(list(index = catalog$index_reference$sha256, mode = mode, model_id = model,
    bbox = payload$bbox, threshold = payload$threshold, class_name = payload$class_name))
  if (!is.null(payload$cursor)) {
    if (!is.character(payload$cursor) || length(payload$cursor) != 1L || nchar(payload$cursor) > 4096) wsi_abort("Invalid cell page cursor.")
    cursor <- tryCatch(jsonlite::fromJSON(payload$cursor, simplifyVector = FALSE), error = function(e) wsi_abort("Invalid cell page cursor."))
    if (!identical(cursor$scope, scope)) wsi_abort("Cell page cursor belongs to a different source/filter; restart this view.")
    row <- wsi_experiment_number(cursor$row_id, "cursor.row_id", integer = TRUE)
    if (mode == "queue") {
      probability <- wsi_experiment_number(cursor$score, "cursor.score", FALSE)
      where <- c(where, "(COALESCE(p.probability,-1)>? OR (COALESCE(p.probability,-1)=? AND c.row_id>?))")
      params <- c(params, list(probability, probability, row))
    } else { where <- c(where, "c.row_id>?"); params <- c(params, list(row)) }
  }
  payload_size <- if (is.null(catalog$segmentation_review)) "length(CAST(c.raw_json AS BLOB))" else
    "length(CAST(c.raw_json AS BLOB))+COALESCE(length(CAST(q.qc_json AS BLOB)),0)"
  table <- DBI::dbGetQuery(connection, paste0("SELECT c.row_id,c.cell_id,c.source_id,c.x,c.y,", payload_size, " AS payload_bytes,", score,
    " FROM cells c", join, clause(where), " ORDER BY ", sort, " LIMIT ?"), params = unname(c(params, list(as.integer(limit + 1L)))))
  result$truncated <- nrow(table) > limit
  if (result$truncated) table <- table[seq_len(limit),,drop = FALSE]
  if (nrow(table)) {
    if (any(table$payload_bytes > getOption("wsiTools.cell_index_max_row_bytes", 1024^2)) ||
        sum(table$payload_bytes) > getOption("wsiTools.cell_index_max_response_bytes", 32 * 1024^2))
      wsi_abort("Visible cell geometry exceeds the bounded response budget; zoom further or request fewer cells.")
    payload_columns <- if (is.null(catalog$segmentation_review)) "c.row_id,c.raw_json" else "c.row_id,c.raw_json,q.qc_json"
    payload_join <- if (is.null(catalog$segmentation_review)) "" else " LEFT JOIN segmentation_review q ON q.row_id=c.row_id"
    payloads <- DBI::dbGetQuery(connection, paste0("SELECT ", payload_columns, " FROM cells c", payload_join, " WHERE c.row_id IN (",
      paste(rep("?", nrow(table)), collapse = ","), ")"), params = as.list(table$row_id))
    table$raw_json <- payloads$raw_json[match(table$row_id, payloads$row_id)]
    if (!is.null(catalog$segmentation_review)) table$qc_json <- payloads$qc_json[match(table$row_id, payloads$row_id)]
  } else table$raw_json <- character()
  if (mode %in% c("cell", "review_subset") && nrow(table) != length(ids)) wsi_abort("One or more requested cell IDs are absent from this indexed tissue.")
  result$cells <- wsi_cell_index_runtime_rows(table, experiment, tissue, catalog)
  if (mode == "viewport" && result$truncated) {
    density <- DBI::dbGetQuery(connection, paste0(
      "SELECT MIN(31,MAX(0,CAST((c.x-?)/? AS INTEGER))) AS bin_column,",
      "MIN(31,MAX(0,CAST((c.y-?)/? AS INTEGER))) AS bin_row,COUNT(*) AS count FROM cells c", join,
      clause(base_where), " GROUP BY bin_column,bin_row LIMIT 1025"),
      params = unname(c(as.list(c(bounds[[1L]], (bounds[[3L]]-bounds[[1L]])/32,
        bounds[[2L]], (bounds[[4L]]-bounds[[2L]])/32)), base_params)))
    if (nrow(density) > 1024L) wsi_abort("Viewport density exceeds its fixed grid.")
    result$density <- list(bounds = as.list(stats::setNames(bounds, c("xmin","ymin","xmax","ymax"))), columns = 32L, rows = 32L,
      bins = lapply(seq_len(nrow(density)), function(i) list(column = density$bin_column[[i]], row = density$bin_row[[i]], count = density$count[[i]])),
      total = sum(density$count), semantics = "Exact counts of indexed cell bounding boxes intersecting this viewport; bins show aggregate counts, not individual cells.")
  }
  if (result$truncated && nrow(table)) {
    last <- table[nrow(table),,drop = FALSE]
    result$next_cursor <- as.character(jsonlite::toJSON(list(scope = scope, row_id = last$row_id[[1L]],
      score = if (mode == "queue") last$queue_score[[1L]] else NULL), auto_unbox = TRUE, null = "null", digits = 17))
  }
  result
}

wsi_cell_index_review_rows <- function(cells, experiment, tissue, catalog) {
  base <- lapply(cells, function(cell) list(id = cell$id, original_id = cell$cell_id,
    tissue_id = tissue$tissue_id, frame_id = tissue$image$frame_id, observation_unit = "cell",
    x = cell$x, y = cell$y, dataset_id = paste0("experiment:", catalog$feature_set_id),
    feature_set_id = catalog$feature_set_id, cluster_values = list()))
  local <- catalog; local$cells <- cells
  wsi_experiment_cell_review_rows(base, local)
}

wsi_experiment_cell_index_response <- function(context, state, payload) {
  experiment <- context$experiment
  if (!inherits(experiment, "wsi_experiment")) wsi_abort("Open an experiment with an indexed cell source first.")
  ids <- vapply(experiment$tissues, `[[`, character(1), "tissue_id")
  tissue_id <- wsi_experiment_text(payload$tissue_id, "tissue_id")
  if (!tissue_id %in% ids) wsi_abort("Unknown indexed tissue.")
  tissue <- experiment$tissues[[match(tissue_id, ids)]]
  if (is.null(tissue$cell_classification$index)) wsi_abort("This tissue has no indexed cell source.")
  catalog <- wsi_experiment_cell_index_catalog(experiment, tissue)
  result <- wsi_cell_index_select(experiment, tissue, catalog, payload)
  reviewed <- state$experiment_classifier
  if (inherits(reviewed, "wsi_experiment_classifier") && identical(reviewed$feature_set_id, catalog$feature_set_id) &&
      length(result$cells) && !isTRUE(result$truncated)) {
    features <- wsi_experiment_index_features(experiment, catalog$feature_set_id,
      vapply(result$cells, `[[`, character(1), "id"))
    predicted <- wsi_experiment_predict_classifier(reviewed, features)
    for (i in seq_along(result$cells)) result$cells[[i]]$reviewed_prediction <- list(status = "predicted",
      label = predicted$predicted[[i]], score_semantics = "uncalibrated_human_reviewed_PLS", uncertainty_status = "not_calibrated",
      model_path = state$experiment_classifier_path %||% NULL)
    result$reviewed_classifier <- list(feature_set_id = catalog$feature_set_id, classes = as.list(reviewed$classes),
      label_origin = "explicit_human_review", score_semantics = "uncalibrated_human_reviewed_PLS")
  }
  response <- list(ok = TRUE, cell_index = result)
  if (identical(result$mode, "review_subset")) {
    previous_context <- state$review_context %||% wsi_experiment_review_context(experiment, strict = FALSE)
    checkpoint <- if (!is.null(payload$checkpoint)) wsi_review_commit_checkpoint(state, payload$checkpoint) else
      state$review_checkpoint %||% wsi_review_checkpoint(previous_context)
    checkpoint <- wsi_validate_review_checkpoint(checkpoint, previous_context)
    rows <- wsi_cell_index_review_rows(result$cells, experiment, tissue, catalog)
    existing <- previous_context$observations
    existing_ids <- vapply(existing, `[[`, character(1), "id")
    rows <- rows[!vapply(rows, function(row) row$id %in% existing_ids, logical(1))]
    all <- c(existing, rows)
    if (length(all) > getOption("wsiTools.cell_index_review_limit", 2000L))
      wsi_abort("Review subset plus prior decisions exceeds the bounded review workspace. Save this review before explicitly starting another workspace; no decisions were discarded.")
    candidate <- wsi_review_context(previous_context$images, all, previous_context$reductions,
      previous_context$context_id, previous_context$experiment_id, previous_context$provenance)
    checked <- wsi_review_checkpoint(candidate, selected_ids = vapply(result$cells, `[[`, character(1), "id"),
      review_records = checkpoint$review_records, revision = checkpoint$revision, history = checkpoint$history)
    state$review_context <- candidate; state$review_checkpoint <- checked
    response$review_context <- candidate; response$review_checkpoint <- checked
  }
  response
}

wsi_cell_index_partition_features <- function(catalog, cell_ids, feature_columns) {
  python <- Sys.which("python3")
  script <- system.file("python", "cellvit_partition_features.py", package = "wsiTools")
  if (!nzchar(python) || !nzchar(script) || !file.exists(script))
    wsi_abort("Indexed CellViT float32 features require Python 3 and the installed wsiTools partition decoder.")
  index <- catalog$index_reference
  source <- catalog$index_metadata$classification
  if (is.null(source$path) || !identical(source$sha256, catalog$source_sha256))
    wsi_abort("CellViT partition manifest reference differs from the indexed source.")
  request_path <- tempfile("cellvit-selected-", fileext = ".json")
  output_path <- tempfile("cellvit-selected-", fileext = ".json")
  on.exit(unlink(c(request_path, output_path)), add = TRUE)
  jsonlite::write_json(list(
    index_sha256 = index$sha256,
    partition_manifest_path = source$path,
    partition_manifest_sha256 = source$sha256,
    cell_ids = unname(as.list(cell_ids)),
    feature_columns = unname(as.list(feature_columns))
  ), request_path, auto_unbox = TRUE, null = "null", digits = 17)
  messages <- suppressWarnings(system2(python,
    args = c("-I", shQuote(script), "--index", shQuote(index$resolved_path),
             "--request", shQuote(request_path), "--output", shQuote(output_path)),
    stdout = TRUE, stderr = TRUE))
  status <- attr(messages, "status") %||% 0L
  if (status != 0L || !file.exists(output_path))
    wsi_abort(paste("Could not verify selected CellViT partition vectors:",
      paste(tail(messages, 3L), collapse = " ")))
  if (file.info(output_path)$size > 128 * 1024^2)
    wsi_abort("Selected CellViT feature response exceeds its bounded byte budget.")
  result <- jsonlite::fromJSON(output_path, simplifyVector = FALSE)
  if (!identical(result$schema, "wsitools-cellvit-selected-features-v1") ||
      !identical(unlist(result$cell_ids, use.names = FALSE), unname(cell_ids)) ||
      !identical(unlist(result$feature_columns, use.names = FALSE), unname(feature_columns)) ||
      length(result$rows) != length(cell_ids))
    wsi_abort("Selected CellViT partition vectors have an unexpected ID or predictor schema.")
  lapply(result$rows, function(row) {
    values <- unlist(row, use.names = FALSE)
    if (!is.numeric(values) || length(values) != length(feature_columns) ||
        any(!is.finite(values)))
      wsi_abort("Selected CellViT vector width or numeric values differ from its descriptor.")
    values
  })
}

wsi_experiment_index_features <- function(experiment, feature_set_id, global_ids) {
  if (!length(global_ids) || anyNA(global_ids) || anyDuplicated(global_ids) ||
      length(global_ids) > getOption("wsiTools.cell_index_review_limit", 2000L)) wsi_abort("Indexed feature lookup requires bounded explicit unique reviewed IDs.")
  matrices <- observations <- list(); signature <- NULL
  for (tissue in experiment$tissues) {
    if (is.null(tissue$cell_classification$index)) next
    matches <- Filter(function(f) identical(f$feature_set_id, feature_set_id), tissue$feature_sets)
    if (length(matches) != 1L) next
    f <- matches[[1L]]; catalog <- wsi_experiment_cell_index_catalog(experiment, tissue)
    prefix <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, feature_set_id, "")
    selected <- global_ids[startsWith(global_ids, prefix)]
    if (!length(selected)) next
    local <- vapply(selected, wsi_cell_index_local_id, character(1), experiment = experiment, tissue = tissue, catalog = catalog)
    s <- wsi_experiment_feature_signature(f)
    if (!is.null(signature) && !identical(signature, s)) wsi_abort("Indexed training feature representations differ across tissues.")
    signature <- s
    connection <- wsi_cell_index_connection(catalog$index_reference)
    table <- tryCatch(DBI::dbGetQuery(connection, paste0("SELECT c.cell_id,c.x,c.y,f.values_json FROM cells c JOIN features f ON f.row_id=c.row_id WHERE c.cell_id IN (",
      paste(rep("?", length(local)), collapse = ","), ")"), params = unname(as.list(local))), finally = DBI::dbDisconnect(connection))
    if (nrow(table) != length(local) || anyDuplicated(table$cell_id)) wsi_abort("Indexed cell/feature rows do not join exactly once.")
    table <- table[match(local, table$cell_id),,drop = FALSE]
    vectors <- if (identical(as.integer(catalog$index_metadata$schema_version), 3L)) {
      wsi_cell_index_partition_features(catalog, local, f$feature_columns)
    } else lapply(table$values_json, function(value) {
      if (nchar(value, type = "bytes") > 1024^2) wsi_abort("Indexed feature vector exceeds its bounded size.")
      row <- unlist(jsonlite::fromJSON(value, simplifyVector = FALSE), use.names = FALSE)
      if (!is.numeric(row) || length(row) != length(f$feature_columns) || any(!is.finite(row))) wsi_abort("Indexed feature vector does not match its declared predictor allowlist.")
      row
    })
    matrix <- do.call(rbind, vectors); colnames(matrix) <- f$feature_columns; rownames(matrix) <- selected
    matrices[[length(matrices)+1L]] <- matrix
    observations[[length(observations)+1L]] <- data.frame(id = selected, original_id = local, label = local, x = table$x, y = table$y,
      tissue_id = tissue$tissue_id, unit = "cell", feature_set_id = feature_set_id, project_image = tissue$name,
      image_id = tissue$tissue_id, sample_id = tissue$tissue_id,
      project_image_index = match(tissue$tissue_id, vapply(experiment$tissues, `[[`, character(1), "tissue_id")) - 1L,
      project_section_index = -1L, stringsAsFactors = FALSE)
  }
  if (!length(matrices)) wsi_abort("No indexed feature rows matched the reviewed selection.")
  matrix <- do.call(rbind, matrices); obs <- do.call(rbind, observations)
  if (!setequal(obs$id, global_ids)) wsi_abort("One or more reviewed IDs are not in the exact indexed feature representation.")
  order <- match(global_ids, obs$id)
  structure(list(x = matrix[order,,drop = FALSE], observations = obs[order,,drop = FALSE], signature = signature,
    grids = list(), experiment_id = experiment$experiment_id, feature_set_id = feature_set_id), class = "wsi_experiment_features")
}

wsi_experiment_index_apply_classifier <- function(model, experiment, tissue_ids = NULL, model_path = NULL) {
  tissues <- experiment$tissues
  if (!is.null(tissue_ids)) {
    known <- vapply(tissues, `[[`, character(1), "tissue_id")
    if (anyNA(tissue_ids) || anyDuplicated(tissue_ids) || any(!tissue_ids %in% known)) wsi_abort("Unknown or duplicate indexed prediction tissue IDs.")
    tissues <- tissues[match(tissue_ids, known)]
  }
  run <- wsi_experiment_derivative_dir(experiment, "indexed-classifier-applications")
  destination <- file.path(run, "predictions.csv")
  temporary <- tempfile(".predictions-", tmpdir = run)
  on.exit(unlink(temporary), add = TRUE)
  preview <- list(); displayed <- total <- 0L; classes <- list()
  page <- min(500L, getOption("wsiTools.cell_index_query_limit", 500L))
  for (tissue in tissues) {
    found <- Filter(function(f) identical(f$feature_set_id, model$feature_set_id), tissue$feature_sets)
    if (length(found) != 1L || is.null(tissue$cell_classification$index))
      wsi_abort("Whole-slide indexed application requires the same indexed feature representation in every selected tissue.")
    if (!identical(wsi_experiment_feature_signature(found[[1L]]), model$signature))
      wsi_abort("Indexed prediction features differ from the saved classifier's encoder/sampler signature.")
    catalog <- wsi_experiment_cell_index_catalog(experiment, tissue)
    connection <- wsi_cell_index_connection(catalog$index_reference)
    tryCatch({
      last <- 0L; written <- 0L
      repeat {
        ids <- DBI::dbGetQuery(connection, "SELECT row_id,cell_id FROM cells WHERE row_id>? ORDER BY row_id LIMIT ?", params = list(last, as.integer(page)))
        if (!nrow(ids)) break
        global <- wsi_experiment_namespace_id(experiment$experiment_id, tissue$tissue_id, model$feature_set_id, ids$cell_id)
        features <- wsi_experiment_index_features(experiment, model$feature_set_id, global)
        result <- wsi_experiment_predict_classifier(model, features)
        result$set <- "application"; result$train_annotation_id <- NA_character_; result$test_annotation_id <- NA_character_
        result$observed <- NA_character_; result$predicted_pls_lda <- result$predicted; result$svm_refined <- FALSE
        result$feature_source <- paste0("experiment:", model$feature_set_id)
        utils::write.table(as.data.frame(result), temporary, sep = ",", row.names = FALSE, quote = TRUE,
          col.names = total == 0L, append = total > 0L, na = "")
        count <- min(nrow(result), max(0L, 500L-displayed))
        if (count) { preview[[length(preview)+1L]] <- result[seq_len(count),,drop = FALSE]; displayed <- displayed+count }
        total <- total+nrow(result); written <- written+nrow(result); last <- tail(ids$row_id, 1L)
      }
      if (written != catalog$cell_count) wsi_abort("Indexed prediction population changed or differs from its immutable metadata.")
    }, finally = DBI::dbDisconnect(connection))
  }
  if (!total || !file.exists(temporary)) wsi_abort("Selected indexed tissues contain no cells to predict.")
  if (!file.rename(temporary, destination)) wsi_abort("Could not atomically publish indexed classifier predictions.")
  out <- do.call(rbind, preview)
  class(out) <- c("wsi_prediction_result", class(out))
  attr(out, "classifier_model") <- model; attr(out, "classifier_path") <- model_path
  attr(out, "result_path") <- destination; attr(out, "total_count") <- total
  attr(out, "display_policy") <- "bounded preview; full slide predictions saved to CSV; viewport cells are queried on demand"
  attr(out, "point_unit") <- "cell"; attr(out, "feature_count") <- length(model$preprocessing$feature_columns)
  attr(out, "ncomp") <- model$ncomp
  out
}
