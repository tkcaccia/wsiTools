test_that("expert-trained Bora semantic output reopens only as a locked exploratory review", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "semantic-review.geojson")
  digest <- wsi_experiment_sha256
  h <- function(letter) paste(rep(letter, 64L), collapse = "")
  geo <- list(type = "FeatureCollection", coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(40L, 60L), native_mpp_xy = c(0.25, 0.25),
    source_frame_id = "native-frame", source_image_sha256 = digest(image),
    source_semantic_mask_sha256 = h("a"), global_model_binding_sha256 = h("b"),
    predicted_not_expert_annotation = TRUE, tissue_identity_status = "exploratory_transfer",
    vector_export = list(format = "bora-wsi-global-vector-v1", global_contours = TRUE,
      independently_fitted_windows = FALSE, minimum_part_and_hole_area_um2 = 100,
      geometry_role = "smoothed_review", smoothing_tolerance_um = 4),
    features = list(list(type = "Feature", properties = list(value = 1L,
      name = "tumor", classification = list(name = "tumor"),
      predicted_tissue_id = 1L, predicted_tissue = "tumor",
      predicted_not_expert_annotation = TRUE,
      tissue_identity_status = "exploratory_transfer"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(2, 2), c(20, 2), c(20, 20), c(2, 20), c(2, 2)))))))
  write_geo <- function(value) jsonlite::write_json(value, geometry,
    auto_unbox = TRUE, pretty = TRUE)
  write_geo(geo)
  artifact <- list(artifact_id = "bora-semantic-review-synthetic",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = digest(geometry), format = "geojson", review_only = TRUE,
    training_eligible = FALSE, predicted_not_expert_annotation = TRUE,
    source_frame_id = "native-frame", image_sha256 = digest(image),
    provenance = list(producer = "bora-wsi-semantic-chain-v1",
      prediction_unit = "semantic_virchow2_spatial_token_native_probability",
      identity_status = "exploratory_same_slide_holdout",
      evidence_class = "expert_trained_same_slide_holdout_not_independent_patient_validated",
      predicted_not_expert_annotation = TRUE,
      native_he_boundary_refinement_applied = FALSE,
      biological_accuracy_qualified = FALSE,
      chain_complete_sha256 = h("c"), roi_spec_sha256 = h("d"),
      expert_annotation_sha256 = h("e"),
      same_slide_holdout_receipt_sha256 = h("f"), model_sha256 = h("1"),
      token_receipt_sha256 = h("2"), registration_sha256 = h("3"),
      registered_support_source_sha256 = h("4"), tissue_support_sha256 = h("5"),
      semantic_mask_sha256 = h("a"), exact_mask_sha256 = h("6"),
      exact_geojson_sha256 = h("7"), review_geojson_sha256 = digest(geometry),
      global_model_binding_sha256 = h("b"), native_shape_yx = c(40L, 60L),
      native_mpp_xy = c(0.25, 0.25), minimum_area_um2 = 100,
      exact_vector_matches_cleaned_mask = TRUE))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    experiment_id = "synthetic:semantic", name = "Semantic Bora review",
    tissues = list(list(tissue_id = "t1", name = "Native tissue",
      image = list(path = basename(image), sha256 = digest(image), width = 60L,
        height = 40L, mpp_x = 0.25, mpp_y = 0.25,
        frame_id = "native-frame",
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = list(artifact))))
  path <- file.path(root, "experiment.cellphenotyper.json")
  open_images <- function(value) {
    jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
    wsi_experiment_images(wsi_read_experiment(path, verify_hashes = TRUE))
  }
  images <- open_images(manifest)
  expect_length(images[[1L]]$rois, 0L)
  expect_length(images[[1L]]$layers, 1L)
  layer <- images[[1L]]$layers[[1L]]
  expect_match(layer$name, "same-slide holdout")
  expect_identical(layer$source_type, "bora_candidate_review")
  expect_true(layer$metadata$review_only)
  expect_false(layer$metadata$training_eligible)
  expect_identical(layer$metadata$producer, "bora-wsi-semantic-chain-v1")
  expect_identical(layer$metadata$model_sha256, h("1"))
  expect_identical(layer$metadata$same_slide_holdout_receipt_sha256, h("f"))
  expect_length(layer$items, 1L)
  expect_true(layer$items[[1L]]$locked)
  expect_true(layer$items[[1L]]$tissue_annotation)
  expect_identical(layer$items[[1L]]$predicted_tissue, "tumor")
  expect_true(grepl("Bora predicted tumor", layer$items[[1L]]$name, fixed = TRUE))
  expect_true(grepl("exploratory", layer$items[[1L]]$name, fixed = TRUE))
  expect_identical(unname(layer$items[[1L]]$class), "Predicted tumor")
  expect_match(layer$items[[1L]]$source, "tissue annotation")

  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$role <- "tissue_annotations"
  expect_error(open_images(wrong), "cannot be tissue_annotations")
  wrong$tissues[[1L]]$artifacts[[1L]]$provenance <- list()
  expect_error(open_images(wrong), "cannot be tissue_annotations")
  for (change in list(
    function(a) {a$review_only <- FALSE; a},
    function(a) {a$training_eligible <- TRUE; a},
    function(a) {a$provenance$identity_status <- "failed_holdout"; a},
    function(a) {a$provenance$prediction_unit <- "global_anonymous_Bora-AF_group"; a},
    function(a) {a$provenance$model_sha256 <- NULL; a},
    function(a) {a$provenance$same_slide_holdout_receipt_sha256 <- h("g"); a},
    function(a) {a$provenance$native_he_boundary_refinement_applied <- TRUE; a},
    function(a) {a$provenance$review_geojson_sha256 <- h("8"); a})) {
    wrong <- manifest
    wrong$tissues[[1L]]$artifacts[[1L]] <- change(artifact)
    expect_error(open_images(wrong), "Bora candidate review format, status")
  }
  for (field in c("source_semantic_mask_sha256", "global_model_binding_sha256",
                  "predicted_not_expert_annotation", "tissue_identity_status")) {
    altered <- geo
    altered[[field]] <- switch(field,
      source_semantic_mask_sha256 = h("8"), global_model_binding_sha256 = h("8"),
      predicted_not_expert_annotation = FALSE, tissue_identity_status = "validated")
    write_geo(altered)
    wrong <- manifest
    wrong$tissues[[1L]]$artifacts[[1L]]$sha256 <- digest(geometry)
    wrong$tissues[[1L]]$artifacts[[1L]]$provenance$review_geojson_sha256 <- digest(geometry)
    expect_error(open_images(wrong), "Semantic Bora review provenance")
  }
  altered <- geo
  altered$features[[1L]]$properties$predicted_not_expert_annotation <- FALSE
  write_geo(altered)
  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$sha256 <- digest(geometry)
  wrong$tissues[[1L]]$artifacts[[1L]]$provenance$review_geojson_sha256 <- digest(geometry)
  expect_error(open_images(wrong), "Semantic Bora review feature")
})
