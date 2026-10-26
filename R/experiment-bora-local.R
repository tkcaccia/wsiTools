# Recovered token-local Bora predictions are source-bound review assets. They
# must never enter editable tissue annotations or classifier training ROIs.
wsi_experiment_bora_local_review_layer <- function(experiment, tissue, image_index) {
  required <- c("bora_local_tumor_probability", "bora_local_uncertainty",
    "bora_local_exact_semantic_mask", "bora_local_review_geometry",
    "bora_local_review_regions", "bora_local_prediction_provenance",
    "bora_local_exact_semantic_vector")
  assets <- Filter(function(a) a$role %in% required, tissue$artifacts %||% list())
  if (!length(assets)) return(NULL)
  roles <- vapply(assets, `[[`, character(1), "role")
  if (anyDuplicated(roles) || !setequal(roles, required))
    wsi_abort("Bora local review requires one complete prediction bundle.")
  bundle <- stats::setNames(assets, roles)
  image <- tissue$image
  hash <- function(x) is.character(x) && length(x) == 1L && !is.na(x) &&
    grepl("^[0-9a-f]{64}$", x)
  digest <- function(x) tolower(wsi_experiment_text(x, "Bora local digest"))
  frame <- function(a) identical(a$source_frame_id, image$frame_id) &&
    identical(a$image_sha256, image$sha256) &&
    identical(as.numeric(unlist(a$native_shape_yx, use.names = FALSE)),
      c(as.numeric(image$height), as.numeric(image$width))) &&
    identical(a$coordinate_space, "native_image_pixel_edges") &&
    identical(a$origin, "top_left")
  prediction_id <- bundle[[required[[1L]]]]$prediction_id
  proof_id <- bundle$bora_local_prediction_provenance$artifact_id
  if (!is.character(prediction_id) || length(prediction_id) != 1L ||
      is.na(prediction_id) || !nzchar(prediction_id) ||
      any(!vapply(assets, function(a) identical(a$prediction_id, prediction_id) &&
        identical(a$provenance_artifact_id, proof_id) && frame(a) &&
        identical(a$review_only, TRUE) && identical(a$training_eligible, FALSE) &&
        identical(a$predicted_not_expert_annotation, TRUE), logical(1))))
    wsi_abort("Bora local review identity, frame or locked status disagrees with its image.")
  checked <- lapply(bundle, function(a) wsi_experiment_file(a, experiment$root,
    paste("Bora local", a$role), TRUE))
  proof <- jsonlite::fromJSON(checked$bora_local_prediction_provenance$resolved_path,
    simplifyVector = FALSE)
  mpp <- as.numeric(unlist(proof$native_mpp_xy, use.names = FALSE))
  classes <- proof$class_order
  ids <- vapply(classes, function(a) as.integer(a$id), integer(1))
  names <- vapply(classes, function(a) as.character(a$name), character(1))
  if (!identical(proof$format, "cellphenotyper-bora-local-prediction-v1") ||
      !identical(proof$prediction_id, prediction_id) ||
      !identical(proof$source_frame_id, image$frame_id) ||
      !identical(proof$image_sha256, image$sha256) ||
      !identical(as.numeric(unlist(proof$native_shape_yx, use.names = FALSE)),
        c(as.numeric(image$height), as.numeric(image$width))) ||
      length(mpp) != 2L || any(!is.finite(mpp)) ||
      !isTRUE(all.equal(mpp, c(image$mpp_x, image$mpp_y), tolerance = 1e-8,
        check.attributes = FALSE)) ||
      !identical(proof$exact_semantic_mask_authoritative, TRUE) ||
      !identical(proof$review_geometry_non_authoritative, TRUE) ||
      !identical(proof$reference_used_for_inference, FALSE) ||
      !identical(proof$independent_patient_holdout, FALSE) ||
      !identical(proof$probability_calibrated, FALSE) ||
      !identical(proof$scientific_status, "EXPLORATORY_SAME_SPECIMEN_REVIEW_ONLY") ||
      !hash(proof$local_model_sha256) || !hash(proof$source_encoder_sha256) ||
      length(ids) != 2L || anyNA(ids) || anyDuplicated(ids) ||
      any(!nzchar(names)) || anyDuplicated(names) ||
      !identical(bundle$bora_local_exact_semantic_mask$measurement_authority, TRUE))
    wsi_abort("Bora local review provenance is incomplete or overstates validation.")
  declared <- proof$artifact_sha256
  observed_roles <- setdiff(roles, "bora_local_prediction_provenance")
  if (!is.list(declared) || !setequal(names(declared), observed_roles) ||
      any(!vapply(observed_roles, function(role) hash(declared[[role]]) &&
        identical(digest(declared[[role]]), digest(checked[[role]]$sha256)), logical(1))))
    wsi_abort("Bora local review assets differ from prediction provenance.")
  if (!identical(bundle$bora_local_review_geometry$format, "geojson") ||
      !identical(bundle$bora_local_review_regions$format, "geojson") ||
      !identical(bundle$bora_local_prediction_provenance$format, "json") ||
      !identical(bundle$bora_local_exact_semantic_mask$format, "ome-tiff"))
    wsi_abort("Bora local review artifact formats are incompatible.")
  exact <- bundle$bora_local_exact_semantic_vector
  if (!identical(exact$source_mask_artifact_id,
        bundle$bora_local_exact_semantic_mask$artifact_id) ||
      !identical(exact$source_mask_sha256,
        bundle$bora_local_exact_semantic_mask$sha256) ||
      !identical(exact$derived_from_exact_mask, TRUE) ||
      !identical(exact$geometry_simplified, FALSE))
    wsi_abort("Bora local exact vector is not bound to the measurement mask.")
  geo_path <- checked$bora_local_review_geometry$resolved_path
  size <- file.info(geo_path)$size
  if (is.na(size) || size > 32 * 1024^2)
    wsi_abort("Bora local fitted review geometry exceeds the bounded viewer size.")
  geo <- jsonlite::fromJSON(geo_path, simplifyVector = FALSE)
  shape <- as.numeric(unlist(geo$native_shape_yx, use.names = FALSE))
  geo_mpp <- as.numeric(unlist(geo$native_mpp_xy, use.names = FALSE))
  semantic <- geo$semantic_boundary
  if (!identical(geo$type, "FeatureCollection") ||
      !identical(geo$coordinate_space, "native_image_pixel_edges") ||
      !identical(geo$origin, "top_left") ||
      !identical(geo$source_frame_id, image$frame_id) ||
      !identical(geo$image_sha256, image$sha256) ||
      !identical(shape, c(as.numeric(image$height), as.numeric(image$width))) ||
      length(geo_mpp) != 2L || any(!is.finite(geo_mpp)) ||
      !isTRUE(all.equal(geo_mpp, mpp, tolerance = 1e-8, check.attributes = FALSE)) ||
      !identical(semantic$role, "fitted_review") ||
      !identical(semantic$predicted_not_expert_annotation, TRUE) ||
      !identical(semantic$geometry_simplified, TRUE) ||
      !identical(semantic$source_exact_geojson_sha256,
        checked$bora_local_exact_semantic_vector$sha256) ||
      !is.list(geo$features) || length(geo$features) != length(ids))
    wsi_abort("Bora local fitted geometry disagrees with the prediction frame or exact source.")
  vertices <- 0L
  for (i in seq_along(geo$features)) {
    feature <- geo$features[[i]]
    props <- feature$properties
    value <- as.integer(props$value)
    if (!identical(feature$type, "Feature") || length(value) != 1L ||
        is.na(value) || !value %in% ids ||
        !identical(props$name, names[[match(value, ids)]]) ||
        !identical(props$predicted_tissue, props$name) ||
        !identical(as.integer(props$predicted_tissue_id), value))
      wsi_abort("Bora local fitted geometry has an unknown predicted class.")
    geometry <- feature$geometry
    polygons <- if (identical(geometry$type, "Polygon"))
      list(geometry$coordinates) else if (identical(geometry$type, "MultiPolygon"))
      geometry$coordinates else NULL
    if (!is.list(polygons) || !length(polygons))
      wsi_abort("Bora local fitted geometry must contain polygons.")
    for (polygon in polygons) for (ring in polygon) {
      if (!is.list(ring) || length(ring) < 4L)
        wsi_abort("Bora local fitted geometry contains an incomplete ring.")
      xy <- do.call(rbind, lapply(ring, function(p) unlist(p, use.names = FALSE)))
      if (!is.numeric(xy) || ncol(xy) != 2L || any(!is.finite(xy)) ||
          any(xy[, 1L] < 0 | xy[, 1L] > image$width |
              xy[, 2L] < 0 | xy[, 2L] > image$height) ||
          !identical(unname(xy[1L, ]), unname(xy[nrow(xy), ])))
        wsi_abort("Bora local fitted geometry has invalid native-pixel vertices.")
      vertices <- vertices + nrow(xy)
      if (vertices > 100000L)
        wsi_abort("Bora local fitted geometry exceeds the bounded vertex count.")
    }
    props$name <- paste("Bora predicted", props$name, "· exploratory")
    props$classification <- list(name = paste("Predicted", props$predicted_tissue))
    props$isLocked <- TRUE
    props$tissue_annotation <- TRUE
    props$review_only <- TRUE
    props$training_eligible <- FALSE
    props$source_type <- "bora_candidate_review"
    feature$properties <- props
    geo$features[[i]] <- feature
  }
  rois <- wsi_roi_from_geojson(geo)
  items <- wsi_viewer_roi_features(rois, fill_alpha = 0.02)
  for (i in seq_along(items)) {
    colour <- switch(geo$features[[i]]$properties$predicted_tissue,
      tumor = "#F43F5E", stroma = "#3B82F6", "#FBBF24")
    items[[i]]$colour <- colour
    items[[i]]$fill <- wsi_viewer_hex_to_rgba(colour, alpha = 0.02)
    items[[i]]$locked <- TRUE
    items[[i]]$tissue_annotation <- TRUE
    items[[i]]$source <- "Bora tissue annotation · local prediction · click to edit"
    items[[i]]$predicted_tissue <- geo$features[[i]]$properties$predicted_tissue
    items[[i]]$project_image_index <- image_index
    items[[i]]$image_id <- tissue$tissue_id
    items[[i]]$coordinates <- NULL
    items[[i]]$geometry <- NULL
    items[[i]]$feature <- NULL
    items[[i]]$measurements <- NULL
  }
  list(id = paste0("experiment_bora_local_", tissue$tissue_id),
    name = "Bora tissue annotations · local predictions · exploratory",
    type = "vector", source_type = "bora_candidate_review", colour = "#FBBF24",
    visible = TRUE, opacity = 1, outline = TRUE, project_image_index = image_index,
    image_id = tissue$tissue_id, project_managed = TRUE,
    metadata = list(review_only = TRUE, training_eligible = FALSE,
      predicted_not_expert_annotation = TRUE, biological_accuracy_qualified = FALSE,
      producer = "cellphenotyper-bora-local-prediction-v1",
      measurement_authority = "bora_local_exact_semantic_mask",
      review_geometry_non_authoritative = TRUE, prediction_id = prediction_id,
      source_sha256 = checked$bora_local_review_geometry$sha256,
      exact_mask_sha256 = checked$bora_local_exact_semantic_mask$sha256,
      model_sha256 = proof$local_model_sha256,
      source_frame_id = image$frame_id, image_sha256 = image$sha256),
    items = items, count = length(items))
}
