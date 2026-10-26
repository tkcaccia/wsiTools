test_that("unknown-aware Bora transfer imports only as a locked review layer", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "review.geojson")
  h <- function(letter) paste(rep(letter, 64L), collapse = "")
  sha <- wsi_experiment_sha256
  geo <- list(type = "FeatureCollection",
    format = "bora-exploratory-transfer-unknown-aware-global-vector-v1",
    coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(80L, 120L), native_mpp_xy = c(0.25, 0.25),
    source_frame_id = "native-frame", source_image_sha256 = sha(image),
    source_label_sha256 = h("a"), source_unknown_sha256 = h("b"),
    registered_support_source_sha256 = h("c"),
    identity_status = "exploratory_manuscript_transfer_unvalidated",
    biological_accuracy_qualified = FALSE,
    unknown_semantics = "abstention_not_background_or_tissue_class",
    vector_export = list(geometry_role = "smoothed_review_unknown_preserved",
      global_contours = TRUE, independently_fitted_windows = FALSE,
      minimum_part_and_unprotected_hole_area_um2 = 100,
      unknown_holes_exempt_from_minimum = TRUE,
      unsupported_holes_exempt_from_minimum = TRUE,
      smoothing_tolerance_um = 0.75),
    features = list(list(type = "Feature",
      properties = list(predicted_tissue = "tumor",
        predicted_not_expert_annotation = TRUE,
        tissue_identity_status = "exploratory_manuscript_transfer_unvalidated"),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(2, 2), c(110, 2), c(110, 50), c(2, 50), c(2, 2)))))))
  jsonlite::write_json(geo, geometry, auto_unbox = TRUE, pretty = TRUE)
  artifact <- list(artifact_id = "bora-v5-review-synthetic",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = sha(geometry), format = "geojson", review_only = TRUE,
    source_frame_id = "native-frame", image_sha256 = sha(image),
    provenance = list(producer = geo$format,
      prediction_unit = "Virchow2 spatial token; native H&E boundary refinement",
      identity_status = geo$identity_status,
      biological_accuracy_qualified = FALSE,
      independent_pm016_validation = FALSE,
      unknown_semantics = geo$unknown_semantics,
      global_contours = TRUE, no_stair_step_fallback = TRUE,
      minimum_area_um2 = 100, smoothing_tolerance_um = 0.75,
      model_sha256 = h("d"), policy_sha256 = h("e"),
      materialization_receipt_sha256 = h("f"), registration_sha256 = h("1"),
      registered_support_source_sha256 = h("c"), tissue_support_sha256 = h("2"),
      native_label_sha256 = h("a"), native_unknown_sha256 = h("b"),
      global_vector_receipt_sha256 = h("3")))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    experiment_id = "synthetic:bora-v5-review", name = "Bora v5 review",
    tissues = list(list(tissue_id = "t1", name = "Native tissue",
      image = list(path = basename(image), sha256 = sha(image), width = 120L,
        height = 80L, mpp_x = 0.25, mpp_y = 0.25,
        frame_id = "native-frame",
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = list(artifact))))
  project <- file.path(root, "experiment.cellphenotyper.json")
  open_images <- function(value) {
    jsonlite::write_json(value, project, auto_unbox = TRUE, pretty = TRUE)
    wsi_experiment_images(wsi_read_experiment(project, verify_hashes = TRUE))
  }
  images <- open_images(manifest)
  expect_length(images[[1L]]$rois, 0L)
  review <- images[[1L]]$layers[[1L]]
  expect_equal(review$count, 1L)
  expect_match(review$name, "manuscript model transfer")
  expect_identical(review$items[[1L]]$predicted_tissue, "tumor")
  expect_true(review$items[[1L]]$locked)
  expect_true(review$items[[1L]]$tissue_annotation)
  expect_false(review$metadata$training_eligible)

  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$provenance$native_unknown_sha256 <- h("4")
  expect_error(open_images(bad), "Unknown-aware Bora transfer review")
})
