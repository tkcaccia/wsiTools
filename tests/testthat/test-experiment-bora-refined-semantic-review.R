test_that("native H&E-refined Bora semantic output opens only as locked predicted review", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "refined-review.geojson")
  h <- function(letter) paste(rep(letter, 64L), collapse = "")
  digest <- wsi_experiment_sha256
  feature <- function(name, y) list(type = "Feature",
    properties = list(predicted_tissue = name,
      predicted_not_expert_annotation = TRUE,
      tissue_identity_status = "exploratory_transfer"),
    geometry = list(type = "Polygon", coordinates = list(list(
      c(2, y), c(110, y), c(110, y + 20), c(2, y + 20), c(2, y)))))
  geo <- list(type = "FeatureCollection",
    coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(80L, 120L), native_mpp_xy = c(0.25, 0.25),
    source_frame_id = "native-frame", source_image_sha256 = digest(image),
    source_semantic_mask_sha256 = h("a"), global_model_binding_sha256 = h("b"),
    predicted_not_expert_annotation = TRUE,
    tissue_identity_status = "exploratory_transfer",
    vector_export = list(format = "bora-wsi-global-vector-v1",
      geometry_role = "smoothed_review", global_contours = TRUE,
      independently_fitted_windows = FALSE,
      minimum_part_and_hole_area_um2 = 100),
    features = list(feature("tumor", 2), feature("stroma", 42)))
  jsonlite::write_json(geo, geometry, auto_unbox = TRUE, pretty = TRUE)
  artifact <- list(artifact_id = "bora-refined-semantic-review-synthetic",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = digest(geometry), format = "geojson", review_only = TRUE,
    training_eligible = FALSE, predicted_not_expert_annotation = TRUE,
    source_frame_id = "native-frame", image_sha256 = digest(image),
    provenance = list(
      producer = "bora-wsi-native-he-refined-semantic-chain-v1",
      prediction_unit = "semantic_virchow2_spatial_token_native_he_refined_probability",
      identity_status = "exploratory_same_slide_holdout",
      evidence_class = "expert_trained_same_slide_holdout_not_independent_patient_validated",
      predicted_not_expert_annotation = TRUE,
      native_he_boundary_refinement_applied = TRUE,
      biological_accuracy_qualified = FALSE,
      chain_complete_sha256 = h("c"), roi_spec_sha256 = h("d"),
      expert_annotation_sha256 = h("e"),
      same_slide_holdout_receipt_sha256 = h("f"),
      model_sha256 = h("1"), token_receipt_sha256 = h("2"),
      token_materialization_receipt_sha256 = h("9"),
      native_he_source_sha256 = digest(image),
      registration_sha256 = h("3"),
      registered_support_source_sha256 = h("4"),
      tissue_support_sha256 = h("5"), semantic_mask_sha256 = h("a"),
      exact_mask_sha256 = h("6"), exact_geojson_sha256 = h("7"),
      review_geojson_sha256 = digest(geometry),
      global_model_binding_sha256 = h("b"),
      native_shape_yx = c(80L, 120L), native_mpp_xy = c(0.25, 0.25),
      minimum_area_um2 = 100, exact_vector_matches_cleaned_mask = TRUE))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    experiment_id = "synthetic:refined-semantic", name = "Refined review",
    tissues = list(list(tissue_id = "t1", name = "Native tissue",
      image = list(path = basename(image), sha256 = digest(image), width = 120L,
        height = 80L, mpp_x = 0.25, mpp_y = 0.25,
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
  layer <- images[[1L]]$layers[[1L]]
  expect_match(layer$name, "H&E-refined")
  expect_identical(layer$source_type, "bora_candidate_review")
  expect_true(layer$metadata$review_only)
  expect_false(layer$metadata$training_eligible)
  expect_true(layer$metadata$native_he_boundary_refinement_applied)
  expect_false(layer$metadata$biological_accuracy_qualified)
  expect_identical(vapply(layer$items, `[[`, character(1), "predicted_tissue"),
                   c("tumor", "stroma"))
  names <- unname(vapply(layer$items, `[[`, character(1), "name"))
  expect_true(grepl("Bora predicted tumor", names[[1L]], fixed = TRUE))
  expect_true(grepl("Bora predicted stroma", names[[2L]], fixed = TRUE))
  expect_true(all(grepl("exploratory", names, fixed = TRUE)))
  expect_identical(unname(vapply(layer$items, `[[`, character(1), "class")),
                   c("Predicted tumor", "Predicted stroma"))
  expect_true(all(vapply(layer$items, `[[`, logical(1), "locked")))
  expect_true(all(vapply(layer$items, `[[`, logical(1), "tissue_annotation")))

  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$role <- "tissue_annotations"
  expect_error(open_images(wrong), "cannot be tissue_annotations")
  for (change in list(
    function(a) {a$training_eligible <- TRUE; a},
    function(a) {a$provenance$native_he_boundary_refinement_applied <- FALSE; a},
    function(a) {a$provenance$native_he_source_sha256 <- h("8"); a},
    function(a) {a$provenance$token_materialization_receipt_sha256 <- NULL; a},
    function(a) {a$provenance$prediction_unit <- "semantic_virchow2_spatial_token_native_probability"; a},
    function(a) {a$provenance$biological_accuracy_qualified <- TRUE; a})) {
    wrong <- manifest
    wrong$tissues[[1L]]$artifacts[[1L]] <- change(artifact)
    expect_error(open_images(wrong), "Bora candidate review format, status")
  }
})
