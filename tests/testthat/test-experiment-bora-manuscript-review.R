test_that("manuscript Bora transfer remains a locked exploratory prediction", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "review.geojson")
  h <- function(letter) paste(rep(letter, 64L), collapse = "")
  sha <- wsi_experiment_sha256
  feature <- function(name, y) list(type = "Feature",
    properties = list(predicted_tissue = name,
      predicted_not_expert_annotation = TRUE,
      tissue_identity_status = "exploratory_transfer"),
    geometry = list(type = "Polygon", coordinates = list(list(
      c(2, y), c(110, y), c(110, y + 20), c(2, y + 20), c(2, y)))))
  geo <- list(type = "FeatureCollection",
    coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(80L, 120L), native_mpp_xy = c(0.25, 0.25),
    source_frame_id = "native-frame", source_image_sha256 = sha(image),
    source_semantic_mask_sha256 = h("a"),
    global_model_binding_sha256 = h("b"),
    predicted_not_expert_annotation = TRUE,
    tissue_identity_status = "exploratory_transfer",
    vector_export = list(geometry_role = "smoothed_review",
      global_contours = TRUE, independently_fitted_windows = FALSE,
      minimum_part_and_hole_area_um2 = 100),
    features = list(feature("stroma", 2), feature("inflamed_stroma", 42)))
  jsonlite::write_json(geo, geometry, auto_unbox = TRUE, pretty = TRUE)
  artifact <- list(artifact_id = "bora-manuscript-review-synthetic",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = sha(geometry), format = "geojson", review_only = TRUE,
    training_eligible = FALSE, predicted_not_expert_annotation = TRUE,
    source_frame_id = "native-frame", image_sha256 = sha(image),
    provenance = list(producer = "bora-wsi-global-v1",
      algorithm_id = "bora-global-af-neighbor-context-v4-review",
      prediction_unit = "global_bora_neighbor_context_classification",
      identity_status = "exploratory_transfer",
      biological_accuracy_qualified = FALSE,
      chain_complete_sha256 = h("c"),
      source_vector_complete_sha256 = h("d"),
      review_complete_sha256 = h("e"),
      source_exact_mask_sha256 = h("f"),
      source_exact_geojson_sha256 = h("1"),
      semantic_mask_sha256 = h("a"),
      global_model_binding_sha256 = h("b"),
      review_geojson_sha256 = sha(geometry),
      native_shape_yx = c(80L, 120L), native_mpp_xy = c(0.25, 0.25),
      minimum_area_um2 = 100,
      exact_vector_matches_cleaned_mask = TRUE))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    experiment_id = "synthetic:manuscript-review", name = "Manuscript review",
    tissues = list(list(tissue_id = "t1", name = "Native tissue",
      image = list(path = basename(image), sha256 = sha(image), width = 120L,
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
  expect_equal(layer$count, 2L)
  expect_match(layer$name, "manuscript model transfer")
  expect_true(layer$metadata$review_only)
  expect_false(layer$metadata$training_eligible)
  expect_identical(vapply(layer$items, `[[`, character(1), "predicted_tissue"),
                   c("stroma", "inflamed_stroma"))
  expect_match(layer$items[[2L]]$class, "Predicted inflamed stroma")
  expect_true(all(vapply(layer$items, `[[`, logical(1), "locked")))
  expect_true(all(vapply(layer$items, `[[`, logical(1), "tissue_annotation")))

  for (change in list(
    function(a) { a$training_eligible <- TRUE; a },
    function(a) { a$source_frame_id <- "wrong-frame"; a },
    function(a) { a$provenance$review_complete_sha256 <- NULL; a },
    function(a) { a$provenance$biological_accuracy_qualified <- TRUE; a })) {
    wrong <- manifest
    wrong$tissues[[1L]]$artifacts[[1L]] <- change(artifact)
    expect_error(open_images(wrong), "Bora candidate review format, status")
  }
})
