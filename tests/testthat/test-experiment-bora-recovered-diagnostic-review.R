test_that("recovered KODAMA Bora corner review is visibly diagnostic and locked", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  sha <- wsi_experiment_sha256
  h <- function(letter) paste(rep(letter, 64L), collapse = "")
  write_receipt <- function(name, value) {
    path <- file.path(root, paste0(name, ".json"))
    jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
    list(path = basename(path), sha256 = sha(path))
  }
  r6_review <- file.path(root, "r6-review.geojson")
  writeLines("r6 review identity", r6_review)
  geometry <- file.path(root, "rounded-review.geojson")
  feature <- function(group, y) {
    name <- paste("KODAMA Group", group)
    list(type = "Feature", properties = list(value = group, name = name,
      predicted_tissue = name, predicted_not_expert_annotation = TRUE,
      tissue_identity_status = "exploratory_transfer",
      boundary_role = "corner_rounded_review_only", area_um2 = 150),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(2, y), c(110, y), c(110, y + 20), c(2, y + 20), c(2, y)))))
  }
  geo <- list(type = "FeatureCollection",
    coordinate_space = "native_image_pixel_edges", origin = "top_left",
    native_shape_yx = c(80, 120), native_mpp_xy = c(0.25, 0.25),
    source_frame_id = "native-frame", source_image_sha256 = sha(image),
    source_semantic_mask_sha256 = h("a"), global_model_binding_sha256 = h("b"),
    predicted_not_expert_annotation = TRUE,
    tissue_identity_status = "exploratory_transfer",
    vector_export = list(format = "bora-wsi-global-vector-v1",
      geometry_role = "corner_rounded_review_only", global_contours = TRUE,
      independently_fitted_windows = FALSE,
      minimum_part_and_hole_area_um2 = 100),
    features = list(feature(1, 2), feature(2, 42)))
  jsonlite::write_json(geo, geometry, auto_unbox = TRUE, pretty = TRUE)
  mask <- write_receipt("MASK_DIAGNOSTIC_READY", list(
    format = "pm016-recovered-bora-full-slide-diagnostic-v1",
    algorithm = paste("Frozen r2 complete token field -> unchanged",
      "COLON_BOUNDARY_SPEC native H&E refinement -> optional global vectorizer"),
    accepted_annotation = FALSE, technical_seam_gate_pass = FALSE,
    source_files_sha256 = list(native_he = sha(image)),
    r2_token_field_sha256 = h("c"), r2_model_receipt_sha256 = h("d"),
    native_refinement = list(mask_sha256 = h("a"))))
  vector <- write_receipt("GLOBAL_VECTOR_COMPLETE", list(
    format = "bora-wsi-global-vector-v1", source_image_sha256 = sha(image),
    source_frame_id = "native-frame", native_shape_yx = c(80, 120),
    native_mpp_xy = c(0.25, 0.25), source_semantic_mask_sha256 = h("a"),
    global_model_binding_sha256 = h("b"),
    output_sha256 = list("exact/mask.geojson" = h("e")),
    minimum_area_um2 = 100, minimum_area_strictly_less_removed = TRUE,
    exact_vector_matches_cleaned_mask = TRUE, global_contours = TRUE,
    no_macroregion_cuts = TRUE, independently_fitted_vector_windows = FALSE))
  diagnostic <- write_receipt("DIAGNOSTIC_COMPLETE", list(
    format = "pm016-recovered-bora-full-slide-diagnostic-v1",
    status = "DIAGNOSTIC_ONLY_NOT_ACCEPTED", accepted_annotation = FALSE,
    technical_seam_gate_pass = FALSE, biological_accuracy_qualified = FALSE,
    mask_receipt_sha256 = mask$sha256,
    vector = list(receipt_sha256 = vector$sha256)))
  smoothing <- write_receipt("SMOOTHING_COMPLETE", list(
    format = "bora-wsi-review-smoothing-v1", status = "REVIEW_ONLY_NOT_ACCEPTED",
    technical_r4_seam_failure_remains = TRUE,
    exact_cleaned_mask_and_geojson_unchanged = TRUE,
    new_tissue_classifier_used = FALSE, biological_accuracy_qualified = FALSE,
    selected = list(status = "PASSED_DIAGNOSTIC_REVIEW"),
    review_geojson_sha256 = sha(r6_review),
    source_sha256 = list(image = sha(image), vector = vector$sha256,
                         exact_geojson = h("e"))))
  corner <- write_receipt("CORNER_ROUNDING_COMPLETE", list(
    format = "bora-wsi-corner-rounding-v1", status = "REVIEW_ONLY_NOT_ACCEPTED",
    technical_r4_seam_failure_remains = TRUE,
    long_straight_model_seam_cannot_be_removed_by_local_rounding = TRUE,
    exact_r4_and_r6_unchanged = TRUE, biological_accuracy_qualified = FALSE,
    review_geojson_sha256 = sha(geometry),
    source_sha256 = list(image = sha(image), vector = vector$sha256,
                         exact_geojson = h("e"), r6_review = sha(r6_review)),
    attempts = list(list(status = "PASSED_REVIEW_ONLY"))))
  artifact <- list(artifact_id = "bora-recovered-corner-review-synthetic",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = sha(geometry), format = "geojson", review_only = TRUE,
    training_eligible = FALSE, predicted_not_expert_annotation = TRUE,
    source_frame_id = "native-frame", image_sha256 = sha(image),
    provenance = list(producer = "bora-recovered-kodama-he-diagnostic-review-v1",
      prediction_unit = "KODAMA_two_group_token_field_native_HE_refinement",
      identity_status = "exploratory_transfer",
      biological_accuracy_qualified = FALSE, accepted_annotation = FALSE,
      technical_seam_gate_pass = FALSE, minimum_area_um2 = 100,
      r2_token_field_sha256 = h("c"), r2_model_receipt_sha256 = h("d"),
      source_semantic_mask_sha256 = h("a"),
      global_model_binding_sha256 = h("b"),
      source_exact_geojson_sha256 = h("e"),
      r6_review_geojson_sha256 = sha(r6_review),
      review_geojson_sha256 = sha(geometry),
      mask_receipt = mask, diagnostic_receipt = diagnostic,
      vector_receipt = vector, smoothing_receipt = smoothing,
      corner_receipt = corner))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1,
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    experiment_id = "synthetic:recovered-bora-review", name = "Recovered review",
    tissues = list(list(tissue_id = "t1", name = "Native tissue",
      image = list(path = basename(image), sha256 = sha(image), width = 120,
        height = 80, mpp_x = 0.25, mpp_y = 0.25,
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
  layer <- images[[1L]]$layers[[1L]]
  expect_equal(layer$count, 2L)
  expect_match(layer$name, "seam gate failed")
  expect_true(layer$metadata$diagnostic_only)
  expect_false(layer$metadata$training_eligible)
  expect_true(all(vapply(layer$items, `[[`, logical(1), "locked")))
  expect_true(all(vapply(layer$items, `[[`, logical(1), "tissue_annotation")))
  expect_identical(layer$items[[1L]]$colour, "#3B82F6")
  expect_identical(layer$items[[2L]]$colour, "#F43F5E")

  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$training_eligible <- TRUE
  expect_error(open_images(bad), "Bora candidate review format, status")
  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$provenance$technical_seam_gate_pass <- TRUE
  expect_error(open_images(bad), "Bora candidate review format, status")
  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$provenance$corner_receipt$sha256 <- h("f")
  expect_error(open_images(bad), "SHA-256 mismatch")
  bad <- manifest
  bad$tissues[[1L]]$artifacts[[1L]]$provenance$r6_review_geojson_sha256 <- h("f")
  expect_error(open_images(bad), "source-bound diagnostic chain")

  # A failed r7 attempt may leave the already reviewed r6 geometry available.
  # It has its own signed fallback provenance and no corner-rounding claim.
  fallback_geo <- geo
  fallback_geo$vector_export$geometry_role <- "smoothed_review_only"
  for (i in seq_along(fallback_geo$features))
    fallback_geo$features[[i]]$properties$boundary_role <- "smoothed_review_only"
  fallback_path <- file.path(root, "smoothed-review.geojson")
  jsonlite::write_json(fallback_geo, fallback_path, auto_unbox = TRUE, pretty = TRUE)
  smoothing_value <- jsonlite::fromJSON(file.path(root, smoothing$path),
                                        simplifyVector = FALSE)
  smoothing_value$review_geojson_sha256 <- sha(fallback_path)
  smoothing <- write_receipt("SMOOTHING_COMPLETE", smoothing_value)
  fallback <- manifest
  a <- fallback$tissues[[1L]]$artifacts[[1L]]
  a$path <- basename(fallback_path)
  a$sha256 <- sha(fallback_path)
  a$provenance$producer <- "bora-recovered-kodama-he-smoothed-diagnostic-review-v1"
  a$provenance$review_geojson_sha256 <- a$sha256
  a$provenance$r6_review_geojson_sha256 <- a$sha256
  a$provenance$smoothing_receipt <- smoothing
  a$provenance$corner_receipt <- NULL
  fallback$tissues[[1L]]$artifacts[[1L]] <- a
  fallback_images <- open_images(fallback)
  expect_match(fallback_images[[1L]]$layers[[1L]]$name, "smoothed diagnostic")
  expect_length(fallback_images[[1L]]$rois, 0L)
})
