# Source-bound, anonymous three-group KODAMA/Bora predictions. These are a
# locked reference layer; reviewer corrections live in separate editable ROIs.
wsi_experiment_bora_three_group_review_layer <- function(experiment, tissue,
                                                          image_index, artifact) {
  provenance <- artifact$provenance %||% list()
  smoothed <- identical(provenance$producer,
                        "bora-kodama-three-group-topology-guarded-review-v3")
  if (!identical(artifact$role, "bora_exploratory_boundary_review") ||
      !identical(artifact$format, "geojson") ||
      !identical(artifact$review_only, TRUE) ||
      !identical(artifact$training_eligible, FALSE) ||
      !identical(artifact$predicted_not_expert_annotation, TRUE) ||
      !identical(artifact$source_frame_id, tissue$image$frame_id) ||
      !identical(artifact$image_sha256, tissue$image$sha256) ||
      !isTRUE(provenance$producer %in% c(
        "bora-kodama-three-group-he-diagnostic-review-v1",
        "bora-kodama-three-group-topology-guarded-review-v3")) ||
      !identical(provenance$prediction_unit,
                 "KODAMA_three_group_token_field_native_HE_refinement") ||
      !identical(as.integer(provenance$group_count), 3L) ||
      !identical(provenance$accepted_annotation, FALSE) ||
      !identical(provenance$biological_accuracy_qualified, FALSE) ||
      !is.numeric(provenance$minimum_area_um2) ||
      length(provenance$minimum_area_um2) != 1L ||
      provenance$minimum_area_um2 != 100)
    wsi_abort("Three-group Bora review identity or diagnostic status is invalid.")
  artifact <- wsi_experiment_file(artifact, experiment$root,
                                  "three-group Bora review", TRUE)
  receipt_names <- c("run_started", "mask", "diagnostic", "vector",
                     "kodama_binding")
  receipts <- lapply(receipt_names, function(name) {
    bound <- provenance[[paste0(name, "_receipt")]]
    if (!is.list(bound)) wsi_abort(paste("Three-group Bora", name, "receipt is missing."))
    bound <- wsi_experiment_file(bound, experiment$root,
                                 paste("three-group Bora", name, "receipt"), TRUE)
    if (file.info(bound$resolved_path)$size > 8 * 1024^2)
      wsi_abort("Three-group Bora receipt exceeds 8 MiB.")
    jsonlite::fromJSON(bound$resolved_path, simplifyVector = FALSE)
  })
  names(receipts) <- receipt_names
  if (smoothed) {
    if (!identical(provenance$topology_guarded_review, TRUE) ||
        !is.list(provenance$base_review_geojson))
      wsi_abort("Topology-guarded Bora review lacks its source geometry.")
    base_review <- wsi_experiment_file(provenance$base_review_geojson,
      experiment$root, "unsmoothed Bora review", TRUE)
    extra_names <- c("smoothing", "smoothing_binding")
    extra <- lapply(extra_names, function(name) {
      bound <- provenance[[paste0(name, "_receipt")]]
      if (!is.list(bound)) wsi_abort(paste("Bora", name, "receipt is missing."))
      bound <- wsi_experiment_file(bound, experiment$root,
                                   paste("Bora", name, "receipt"), TRUE)
      if (file.info(bound$resolved_path)$size > 8 * 1024^2)
        wsi_abort("Topology-guarded Bora receipt exceeds 8 MiB.")
      jsonlite::fromJSON(bound$resolved_path, simplifyVector = FALSE)
    })
    names(extra) <- extra_names
    receipts <- c(receipts, extra)
  }
  started <- receipts$run_started
  mask <- receipts$mask; diagnostic <- receipts$diagnostic
  vector <- receipts$vector; kodama <- receipts$kodama_binding
  receipt_hash <- function(name) provenance[[paste0(name, "_receipt")]]$sha256
  geo_size <- file.info(artifact$resolved_path)$size
  if (is.na(geo_size) || geo_size > 32 * 1024^2)
    wsi_abort("Three-group Bora review GeoJSON exceeds 32 MiB; use an indexed derivative.")
  geo <- jsonlite::fromJSON(artifact$resolved_path, simplifyVector = FALSE)
  expected_shape <- c(tissue$image$height, tissue$image$width)
  expected_mpp <- c(tissue$image$mpp_x, tissue$image$mpp_y)
  pair <- function(x) as.numeric(unlist(x, use.names = FALSE))
  same_pair <- function(x, y) length(pair(x)) == 2L &&
    isTRUE(all.equal(pair(x), as.numeric(y), tolerance = 1e-8,
                     check.attributes = FALSE))
  cluster_sha <- kodama$files$kodama_three_group_clusters$sha256
  if (!identical(kodama$schema,
                 "cellphenotyper-pm016-kodama-three-group-binding") ||
      !identical(kodama$status, "exploratory_unlabeled_three_group_review") ||
      !identical(as.integer(kodama$observation_count), 1103L) ||
      !identical(provenance$kodama_cluster_sha256, cluster_sha) ||
      !identical(provenance$kodama_binding_sha256,
                 receipt_hash("kodama_binding")) ||
      !identical(started$format,
                 "pm016-kodama-three-group-recovered-bora-diagnostic-v2") ||
      !identical(started$status, "RUNNING_DIAGNOSTIC") ||
      !identical(started$input_files$native_he$sha256,
                 tissue$image$sha256) ||
      !identical(started$input_files$three_csv$sha256, cluster_sha) ||
      !identical(started$three_group_join$new_csv_sha256, cluster_sha) ||
      !identical(started$expert_annotation_used, FALSE) ||
      !identical(mask$format, started$format) ||
      !identical(mask$status,
                 "complete_mask_diagnostic_not_expert_annotation") ||
      !identical(mask$three_group_join, started$three_group_join) ||
      !identical(mask$three_group_join$joined_by_label, TRUE) ||
      !identical(mask$refinement$mask_sha256,
                 geo$source_semantic_mask_sha256) ||
      !identical(mask$refinement$global_all_three_classes_preserved, TRUE) ||
      !identical(mask$token$seam_gate_pass,
                 mask$technical_seam_gate_pass) ||
      !identical(mask$biological_accuracy_qualified, FALSE) ||
      !identical(diagnostic$format, started$format) ||
      !identical(diagnostic$status,
                 "complete_diagnostic_not_biological_annotation") ||
      !identical(diagnostic$three_group_join, mask$three_group_join) ||
      !identical(diagnostic$model, mask$model) ||
      !identical(diagnostic$token, mask$token) ||
      !identical(diagnostic$refinement, mask$refinement) ||
      !identical(diagnostic$vectorization$receipt_sha256,
                 receipt_hash("vector")) ||
      !identical(diagnostic$technical_seam_gate_pass,
                 mask$technical_seam_gate_pass) ||
      !identical(diagnostic$biological_accuracy_qualified, FALSE) ||
      !identical(vector$format, "bora-wsi-global-vector-v1") ||
      !identical(vector$source_image_sha256, tissue$image$sha256) ||
      !identical(vector$source_frame_id, tissue$image$frame_id) ||
      !same_pair(vector$native_shape_yx, expected_shape) ||
      !same_pair(vector$native_mpp_xy, expected_mpp) ||
      !identical(vector$source_semantic_mask_sha256,
                 geo$source_semantic_mask_sha256) ||
      !identical(vector$global_model_binding_sha256,
                 geo$global_model_binding_sha256) ||
      !identical(vector$output_sha256[["review/mask.geojson"]],
                 if (smoothed) base_review$sha256 else artifact$sha256) ||
      !is.numeric(vector$minimum_area_um2) ||
      length(vector$minimum_area_um2) != 1L ||
      vector$minimum_area_um2 != 100 ||
      !identical(vector$minimum_area_strictly_less_removed, TRUE) ||
      !identical(vector$global_contours, TRUE) ||
      !identical(vector$no_macroregion_cuts, TRUE) ||
      !identical(vector$independently_fitted_vector_windows, FALSE) ||
      !identical(geo$type, "FeatureCollection") ||
      !identical(geo$coordinate_space, "native_image_pixel_edges") ||
      !identical(geo$source_image_sha256, tissue$image$sha256) ||
      !identical(geo$source_frame_id, tissue$image$frame_id) ||
      !same_pair(geo$native_shape_yx, expected_shape) ||
      !same_pair(geo$native_mpp_xy, expected_mpp) ||
      !identical(geo$predicted_not_expert_annotation, TRUE) ||
      !identical(geo$vector_export$format, "bora-wsi-global-vector-v1") ||
      !identical(geo$vector_export$global_contours, TRUE) ||
      !identical(geo$vector_export$independently_fitted_windows, FALSE) ||
      !is.numeric(geo$vector_export$minimum_part_and_hole_area_um2) ||
      length(geo$vector_export$minimum_part_and_hole_area_um2) != 1L ||
      geo$vector_export$minimum_part_and_hole_area_um2 != 100)
    wsi_abort("Three-group Bora review disagrees with its native image and receipts.")
  if (smoothed) {
    smoothing <- receipts$smoothing; binding <- receipts$smoothing_binding
    if (!identical(binding$format,
                   "pm016-three-class-bora-review-smoothing-v3") ||
        !identical(binding$status,
                   "diagnostic_review_only_seam_gate_failed") ||
        !identical(binding$review_geojson_sha256, artifact$sha256) ||
        !identical(binding$smoothing_receipt_sha256,
                   receipt_hash("smoothing")) ||
        !identical(binding$v2_diagnostic_receipt_sha256,
                   receipt_hash("diagnostic")) ||
        !identical(binding$v2_mask_receipt_sha256, receipt_hash("mask")) ||
        !identical(binding$v2_vector_receipt_sha256,
                   receipt_hash("vector")) ||
        !identical(binding$v2_exact_geojson_sha256,
                   vector$output_sha256[["exact/mask.geojson"]]) ||
        !identical(as.integer(binding$component_count), length(geo$features)) ||
        !identical(as.integer(unlist(binding$classes, use.names = FALSE)), 1:3) ||
        !identical(binding$technical_seam_gate_pass, FALSE) ||
        !identical(binding$biological_accuracy_qualified, FALSE) ||
        !identical(smoothing$format, "bora-wsi-review-smoothing-v1") ||
        !identical(smoothing$status, "REVIEW_ONLY_NOT_ACCEPTED") ||
        !identical(smoothing$review_geojson_sha256, artifact$sha256) ||
        !identical(smoothing$selected$status, "PASSED_DIAGNOSTIC_REVIEW") ||
        !identical(as.numeric(smoothing$selected$tolerance_um),
                   as.numeric(binding$selected_tolerance_um)) ||
        !identical(as.numeric(provenance$review_smoothing_tolerance_um),
                   as.numeric(binding$selected_tolerance_um)) ||
        !identical(as.integer(smoothing$selected$full_native_raster_agreement$
                             new_background_in_exact_tissue), 0L) ||
        !identical(smoothing$technical_r4_seam_failure_remains, TRUE) ||
        !identical(smoothing$biological_accuracy_qualified, FALSE) ||
        !identical(smoothing$exact_cleaned_mask_and_geojson_unchanged, TRUE))
      wsi_abort("Topology-guarded Bora review disagrees with its v3 binding.")
  }
  features <- geo$features
  if (!is.list(features) || !length(features) || length(features) > 5000L)
    wsi_abort("Three-group Bora review needs 1–5,000 bounded features; use an indexed derivative for larger results.")
  vertices <- 0L; groups <- integer()
  for (i in seq_along(features)) {
    f <- features[[i]]; p <- f$properties %||% list()
    group <- as.integer(p$value %||% NA_integer_)
    expected_name <- if (length(group) == 1L && !is.na(group) &&
                         group %in% 1:3) paste("KODAMA Group", group) else ""
    if (!nzchar(expected_name) || !identical(p$name, expected_name) ||
        !identical(p$predicted_tissue, expected_name) ||
        !identical(p$predicted_not_expert_annotation, TRUE) ||
        !identical(p$tissue_identity_status, "exploratory_transfer") ||
        !is.numeric(p$area_um2) || length(p$area_um2) != 1L ||
        p$area_um2 < 100 ||
        !f$geometry$type %in% c("Polygon", "MultiPolygon"))
      wsi_abort("Three-group Bora review has an invalid anonymous component.")
    polygons <- if (identical(f$geometry$type, "Polygon"))
      list(f$geometry$coordinates) else f$geometry$coordinates
    if (!is.list(polygons) || !length(polygons))
      wsi_abort("Three-group Bora component has empty geometry.")
    for (polygon in polygons) for (ring in polygon) {
      xy <- do.call(rbind, lapply(ring, function(point)
        as.numeric(unlist(point, use.names = FALSE))))
      if (!is.matrix(xy) || ncol(xy) != 2L || nrow(xy) < 4L ||
          any(!is.finite(xy)) ||
          any(xy[, 1L] < 0 | xy[, 1L] > tissue$image$width) ||
          any(xy[, 2L] < 0 | xy[, 2L] > tissue$image$height) ||
          !identical(unname(xy[1L, ]), unname(xy[nrow(xy), ])))
        wsi_abort("Three-group Bora polygon is outside the native image frame.")
      vertices <- vertices + nrow(xy)
      if (vertices > 100000L)
        wsi_abort("Three-group Bora review exceeds 100,000 vertices; use an indexed derivative.")
    }
    groups <- c(groups, group)
    component_id <- p$component_id %||% paste0("bora_three_", i)
    features[[i]] <- list(type = "Feature", id = component_id,
      geometry = f$geometry, properties = list(
        name = paste(expected_name, "· Bora prediction · click to edit"),
        classification = list(name = expected_name), value = group,
        isLocked = TRUE, source_type = "bora_candidate_review",
        tissue_annotation = TRUE, review_only = TRUE,
        predicted_not_expert_annotation = TRUE,
        predicted_tissue = expected_name))
  }
  if (!identical(sort(unique(groups)), 1:3))
    wsi_abort("Three-group Bora review must contain all three anonymous domains.")
  geo$features <- features
  rois <- wsi_roi_from_geojson(geo)
  items <- wsi_viewer_roi_features(rois, fill_alpha = 0.02)
  colours <- c("#3B82F6", "#F43F5E", "#F59E0B")
  for (i in seq_along(items)) {
    colour <- colours[[groups[[i]]]]
    items[[i]]$colour <- colour
    items[[i]]$fill <- wsi_viewer_hex_to_rgba(colour, alpha = 0.02)
    items[[i]]$locked <- TRUE
    items[[i]]$tissue_annotation <- TRUE
    items[[i]]$source <- "Bora tissue annotation · KODAMA prediction · click to edit"
    items[[i]]$project_image_index <- image_index
    items[[i]]$image_id <- tissue$tissue_id
    # The checked rings are sufficient for rendering and manual-copy editing.
    # Avoid retaining duplicate full-slide GeoJSON arrays in browser state.
    items[[i]]$coordinates <- NULL
    items[[i]]$geometry <- NULL
    items[[i]]$feature <- NULL
    items[[i]]$measurements <- NULL
  }
  list(id = paste0("experiment_bora_three_", tissue$tissue_id),
    name = paste("Bora tissue annotations · 3 KODAMA groups ·",
      if (smoothed) "topology-guarded prediction ·" else "diagnostic prediction ·",
      if (isTRUE(diagnostic$technical_seam_gate_pass))
        "seam gate passed" else "seam gate failed", "· click to edit"),
    type = "vector", source_type = "bora_candidate_review",
    colour = "#FBBF24", visible = TRUE, opacity = 1,
    outline = TRUE, project_image_index = image_index,
    image_id = tissue$tissue_id, project_managed = TRUE,
    metadata = list(review_only = TRUE, training_eligible = FALSE,
      group_count = 3L, source_sha256 = artifact$sha256,
      source_frame_id = artifact$source_frame_id,
      image_sha256 = artifact$image_sha256,
      producer = provenance$producer,
      prediction_unit = provenance$prediction_unit,
      biological_accuracy_qualified = FALSE,
      accepted_annotation = FALSE,
      technical_seam_gate_pass = isTRUE(diagnostic$technical_seam_gate_pass),
      diagnostic_only = TRUE, kodama_cluster_sha256 = cluster_sha,
      kodama_binding_sha256 = receipt_hash("kodama_binding"),
      mask_receipt_sha256 = receipt_hash("mask"),
      diagnostic_receipt_sha256 = receipt_hash("diagnostic"),
      run_started_receipt_sha256 = receipt_hash("run_started"),
      vector_receipt_sha256 = receipt_hash("vector"),
      topology_guarded_review = smoothed,
      smoothing_receipt_sha256 = if (smoothed) receipt_hash("smoothing") else NULL,
      smoothing_binding_sha256 = if (smoothed) receipt_hash("smoothing_binding") else NULL),
    count = length(items), items = items)
}
