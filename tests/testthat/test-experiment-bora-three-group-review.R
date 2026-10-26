test_that("three-group Bora predictions remain locked while manual ROIs stay separate", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  sha <- wsi_experiment_sha256
  h <- function(letter) paste(rep(letter, 64L), collapse = "")
  save_json <- function(name, value) {
    path <- file.path(root, name)
    jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
    list(path = basename(path), sha256 = sha(path))
  }
  frame <- "pm016-native"
  feature <- function(group, x) {
    label <- paste("KODAMA Group", group)
    list(type = "Feature", properties = list(value = group, name = label,
      predicted_tissue = label, predicted_not_expert_annotation = TRUE,
      tissue_identity_status = "exploratory_transfer", area_um2 = 100),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(x, 1), c(x + 40, 1), c(x + 40, 41), c(x, 41), c(x, 1)))))
  }
  geo <- save_json("review.geojson", list(type = "FeatureCollection",
    coordinate_space = "native_image_pixel_edges", native_shape_yx = c(100, 200),
    native_mpp_xy = c(0.25, 0.25), source_frame_id = frame,
    source_image_sha256 = sha(image), source_semantic_mask_sha256 = h("a"),
    global_model_binding_sha256 = h("b"),
    predicted_not_expert_annotation = TRUE,
    vector_export = list(format = "bora-wsi-global-vector-v1",
      global_contours = TRUE, independently_fitted_windows = FALSE,
      minimum_part_and_hole_area_um2 = 100),
    features = list(feature(1, 1), feature(2, 60), feature(3, 120))))
  kodama <- save_json("binding.json", list(
    schema = "cellphenotyper-pm016-kodama-three-group-binding",
    status = "exploratory_unlabeled_three_group_review",
    observation_count = 1103,
    files = list(kodama_three_group_clusters = list(sha256 = h("c")))))
  join <- list(new_csv_sha256 = h("c"), joined_by_label = TRUE)
  started <- save_json("RUN_STARTED.json", list(
    format = "pm016-kodama-three-group-recovered-bora-diagnostic-v2",
    status = "RUNNING_DIAGNOSTIC", three_group_join = join,
    input_files = list(native_he = list(sha256 = sha(image)),
                       three_csv = list(sha256 = h("c"))),
    expert_annotation_used = FALSE))
  model <- list(model_sha256 = h("d"))
  token <- list(seam_gate_pass = FALSE)
  refinement <- list(mask_sha256 = h("a"),
                     global_all_three_classes_preserved = TRUE)
  mask <- save_json("MASK_DIAGNOSTIC_READY.json", list(
    format = "pm016-kodama-three-group-recovered-bora-diagnostic-v2",
    status = "complete_mask_diagnostic_not_expert_annotation",
    three_group_join = join, model = model, token = token,
    refinement = refinement, technical_seam_gate_pass = FALSE,
    biological_accuracy_qualified = FALSE))
  vector <- save_json("GLOBAL_VECTOR_COMPLETE.json", list(
    format = "bora-wsi-global-vector-v1", source_image_sha256 = sha(image),
    source_frame_id = frame, native_shape_yx = c(100, 200),
    native_mpp_xy = c(0.25, 0.25), source_semantic_mask_sha256 = h("a"),
    global_model_binding_sha256 = h("b"),
    output_sha256 = list("review/mask.geojson" = geo$sha256),
    minimum_area_um2 = 100, minimum_area_strictly_less_removed = TRUE,
    global_contours = TRUE, no_macroregion_cuts = TRUE,
    independently_fitted_vector_windows = FALSE))
  diagnostic <- save_json("DIAGNOSTIC_COMPLETE.json", list(
    format = "pm016-kodama-three-group-recovered-bora-diagnostic-v2",
    status = "complete_diagnostic_not_biological_annotation",
    three_group_join = join, model = model, token = token,
    refinement = refinement,
    vectorization = list(receipt_sha256 = vector$sha256),
    biological_accuracy_qualified = FALSE,
    technical_seam_gate_pass = FALSE))
  artifact <- list(role = "bora_exploratory_boundary_review",
    path = geo$path, sha256 = geo$sha256, format = "geojson",
    review_only = TRUE, training_eligible = FALSE,
    predicted_not_expert_annotation = TRUE, source_frame_id = frame,
    image_sha256 = sha(image), provenance = list(
      producer = "bora-kodama-three-group-he-diagnostic-review-v1",
      prediction_unit = "KODAMA_three_group_token_field_native_HE_refinement",
      group_count = 3L, accepted_annotation = FALSE,
      biological_accuracy_qualified = FALSE, minimum_area_um2 = 100,
      kodama_cluster_sha256 = h("c"), kodama_binding_sha256 = kodama$sha256,
      run_started_receipt = started, mask_receipt = mask,
      diagnostic_receipt = diagnostic,
      vector_receipt = vector, kodama_binding_receipt = kodama))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1,
    experiment_id = "synthetic:pm016-three", name = "three-group review",
    tissues = list(list(tissue_id = "PM016", name = "PM016",
      image = list(path = basename(image), sha256 = sha(image),
        width = 200, height = 100, mpp_x = 0.25, mpp_y = 0.25,
        frame_id = frame,
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = list(artifact))))
  project <- file.path(root, "experiment.cellphenotyper.json")
  open_images <- function(value) {
    jsonlite::write_json(value, project, auto_unbox = TRUE, pretty = TRUE)
    wsi_experiment_images(wsi_read_experiment(project, verify_hashes = TRUE))
  }
  legacy <- manifest
  legacy$settings <- list(bora_annotations_mode = "legacy_review_layer")
  images <- open_images(legacy)
  expect_length(images[[1L]]$rois, 0L)
  layer <- images[[1L]]$layers[[1L]]
  expect_equal(layer$count, 3L)
  expect_equal(layer$metadata$group_count, 3L)
  expect_false(layer$metadata$training_eligible)
  expect_false(layer$metadata$technical_seam_gate_pass)
  expect_true(all(vapply(layer$items, `[[`, logical(1), "locked")))
  expect_identical(vapply(layer$items, function(item) item$properties$value, integer(1)),
                   1:3)

  ordinary <- manifest
  ordinary$settings <- list(bora_annotations_mode = "ordinary_editable")
  editable <- open_images(ordinary)[[1L]]
  expect_length(editable$layers, 0L)
  expect_length(editable$rois, 3L)
  expect_true(all(vapply(editable$rois, `[[`, logical(1), "tissue_annotation")))
  expect_true(all(!vapply(editable$rois, `[[`, logical(1), "locked")))
  expect_true(all(vapply(editable$rois, function(item)
    identical(item$properties$review_only, FALSE), logical(1))))
  expect_true(all(vapply(editable$rois, function(item)
    identical(item$properties$bora_prediction_sha256, geo$sha256), logical(1))))

  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$provenance$group_count <- 2L
  expect_error(open_images(bad), "three-group Bora review identity|Three-group Bora review identity")
  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$provenance$kodama_cluster_sha256 <- h("d")
  expect_error(open_images(bad), "native image and receipts")
})
