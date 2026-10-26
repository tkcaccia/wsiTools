test_that("recovered Bora local prediction opens as a locked review layer", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  digest <- wsi_experiment_sha256
  image <- file.path(root, "image.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  image_sha <- digest(image)
  frame <- "native-frame"
  prediction <- "bora-local-synthetic"
  common <- list(prediction_id = prediction, provenance_artifact_id = "local-proof",
    source_frame_id = frame, image_sha256 = image_sha,
    native_shape_yx = c(40L, 60L), coordinate_space = "native_image_pixel_edges",
    origin = "top_left", review_only = TRUE, training_eligible = FALSE,
    predicted_not_expert_annotation = TRUE)
  roles <- c("bora_local_tumor_probability", "bora_local_uncertainty",
    "bora_local_exact_semantic_mask", "bora_local_exact_semantic_vector",
    "bora_local_review_geometry", "bora_local_review_regions")
  files <- stats::setNames(file.path(root, paste0(roles, ".dat")), roles)
  for (path in files) writeBin(as.raw(c(4, 5, 6)), path)
  exact_sha <- digest(files[["bora_local_exact_semantic_vector"]])
  geo <- list(type = "FeatureCollection", coordinate_space = "native_image_pixel_edges",
    origin = "top_left", native_shape_yx = c(40L, 60L), native_mpp_xy = c(.25, .25),
    source_frame_id = frame, image_sha256 = image_sha,
    semantic_boundary = list(role = "fitted_review", geometry_simplified = TRUE,
      predicted_not_expert_annotation = TRUE, source_exact_geojson_sha256 = exact_sha),
    features = list(
      list(type = "Feature", properties = list(value = 1L, name = "tumor",
        predicted_tissue = "tumor", predicted_tissue_id = 1L),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(2, 2), c(20, 2), c(20, 20), c(2, 20), c(2, 2))))),
      list(type = "Feature", properties = list(value = 2L, name = "stroma",
        predicted_tissue = "stroma", predicted_tissue_id = 2L),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(22, 2), c(40, 2), c(40, 20), c(22, 20), c(22, 2)))))))
  jsonlite::write_json(geo, files[["bora_local_review_geometry"]], auto_unbox = TRUE)
  hashes <- lapply(files, digest)
  proof <- list(format = "cellphenotyper-bora-local-prediction-v1",
    prediction_id = prediction, source_frame_id = frame, image_sha256 = image_sha,
    native_shape_yx = c(40L, 60L), native_mpp_xy = c(.25, .25),
    exact_semantic_mask_authoritative = TRUE, review_geometry_non_authoritative = TRUE,
    reference_used_for_inference = FALSE, independent_patient_holdout = FALSE,
    probability_calibrated = FALSE,
    scientific_status = "EXPLORATORY_SAME_SPECIMEN_REVIEW_ONLY",
    local_model_sha256 = paste(rep("a", 64L), collapse = ""),
    source_encoder_sha256 = paste(rep("b", 64L), collapse = ""),
    class_order = list(list(id = 1L, name = "tumor"), list(id = 2L, name = "stroma")),
    artifact_sha256 = hashes)
  proof_path <- file.path(root, "proof.json")
  jsonlite::write_json(proof, proof_path, auto_unbox = TRUE)
  artifacts <- lapply(roles, function(role) {
    a <- c(common, list(role = role, artifact_id = role, path = basename(files[[role]]),
      sha256 = hashes[[role]], format = if (role %in% c("bora_local_exact_semantic_vector",
        "bora_local_review_geometry", "bora_local_review_regions")) "geojson" else "ome-tiff"))
    if (role == "bora_local_exact_semantic_mask") a$measurement_authority <- TRUE
    if (role == "bora_local_exact_semantic_vector") {
      a$source_mask_artifact_id <- "bora_local_exact_semantic_mask"
      a$source_mask_sha256 <- hashes[["bora_local_exact_semantic_mask"]]
      a$derived_from_exact_mask <- TRUE
      a$geometry_simplified <- FALSE
    }
    a
  })
  artifacts[[length(artifacts) + 1L]] <- c(common, list(role =
    "bora_local_prediction_provenance", artifact_id = "local-proof",
    path = basename(proof_path), sha256 = digest(proof_path), format = "json"))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    experiment_id = "local-test", name = "Bora local review",
    tissues = list(list(tissue_id = "t1", name = "Tissue",
      image = list(path = basename(image), sha256 = image_sha, width = 60L,
        height = 40L, mpp_x = .25, mpp_y = .25, frame_id = frame,
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = artifacts)))
  path <- file.path(root, "experiment.cellphenotyper.json")
  open <- function(value) {
    jsonlite::write_json(value, path, auto_unbox = TRUE)
    wsi_experiment_images(wsi_read_experiment(path))
  }
  images <- open(manifest)
  expect_length(images[[1L]]$rois, 0L)
  expect_length(images[[1L]]$layers, 1L)
  layer <- images[[1L]]$layers[[1L]]
  expect_identical(layer$source_type, "bora_candidate_review")
  expect_true(layer$metadata$review_only)
  expect_false(layer$metadata$training_eligible)
  expect_true(layer$metadata$review_geometry_non_authoritative)
  expect_length(layer$items, 2L)
  expect_true(all(vapply(layer$items, `[[`, logical(1), "locked")))
  expect_true(all(vapply(layer$items, `[[`, logical(1), "tissue_annotation")))
  expect_setequal(vapply(layer$items, `[[`, character(1), "predicted_tissue"),
    c("tumor", "stroma"))

  changed <- manifest
  changed$tissues[[1L]]$artifacts[[1L]]$training_eligible <- TRUE
  expect_error(open(changed), "locked status")
  changed <- manifest
  changed$tissues[[1L]]$artifacts[[1L]]$sha256 <- paste(rep("c", 64L), collapse = "")
  expect_error(open(changed), "SHA-256 mismatch")
  changed <- manifest
  changed$tissues[[1L]]$artifacts[[4L]]$source_mask_sha256 <-
    paste(rep("c", 64L), collapse = "")
  expect_error(open(changed), "not bound")
})
