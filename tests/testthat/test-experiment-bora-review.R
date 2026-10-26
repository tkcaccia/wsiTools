test_that("exploratory Bora boundaries stay checked, anonymous, and outside annotations", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "image.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "candidate.geojson")
  polygon <- list(list(c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1)))
  original <- list(type = "FeatureCollection", coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(20L, 20L), native_mpp_xy = c(0.5, 0.5),
    source_frame_id = "frame1", source_image_sha256 = wsi_experiment_sha256(image),
    features = list(list(
    type = "Feature", id = "tumor-secret", predicted_tissue = "tumor",
    properties = list(name = "tumor", class = "tumor",
      classification = list(name = "tumor")),
    geometry = list(type = "Polygon", coordinates = polygon))))
  jsonlite::write_json(original, geometry, auto_unbox = TRUE)
  sha <- wsi_experiment_sha256
  # Match CellPhenotyper/bin/export_bora_global_project.py: the anonymous
  # prediction unit and identity status live inside artifact provenance.
  artifact <- list(artifact_id = "bora-global-exact-synthetic",
    role = "bora_exploratory_boundary_review",
    label = "Bora anonymous-group boundaries (exploratory review; exact)",
    path = basename(geometry), sha256 = sha(geometry), format = "geojson",
    review_only = TRUE, source_frame_id = "frame1", image_sha256 = sha(image),
    provenance = list(producer = "bora-wsi-global-v1", algorithm_id = "synthetic",
      algorithm_evidence_receipt_sha256 = paste(rep("c", 64), collapse = ""),
      prediction_unit = "global_anonymous_Bora-AF_group",
      chain_complete_sha256 = paste(rep("a", 64), collapse = ""),
      manifest_sha256 = paste(rep("b", 64), collapse = ""),
      identity_status = "exploratory_transfer", evidence_class = "synthetic",
      geometry = "exact mask-to-vector, 100 um2 minimum part/hole area"))
  experiment <- list(root = root, experiment_id = "synthetic:bora-review",
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    tissues = list(list(tissue_id = "t1", name = "Synthetic tissue",
      image = list(resolved_path = image, sha256 = sha(image), frame_id = "frame1",
        width = 20L, height = 20L, mpp_x = 0.5, mpp_y = 0.5),
      artifacts = list(artifact))))
  images <- wsi_experiment_images(experiment)
  expect_length(images[[1L]]$rois, 0L)
  expect_length(images[[1L]]$layers, 1L)
  layer <- images[[1L]]$layers[[1L]]
  expect_identical(layer$source_type, "bora_candidate_review")
  expect_true(layer$metadata$review_only)
  expect_false(layer$metadata$training_eligible)
  expect_equal(layer$metadata$source_sha256, sha(geometry))
  expect_identical(layer$metadata$identity_status, "exploratory_transfer")
  expect_identical(layer$metadata$chain_complete_sha256,
                   artifact$provenance$chain_complete_sha256)
  expect_equal(layer$project_image_index, 0L)
  expect_length(layer$items, 1L)
  expect_true(layer$items[[1L]]$locked)
  expect_true(layer$items[[1L]]$tissue_annotation)
  expect_identical(layer$items[[1L]]$class, "Candidate boundary")
  expect_false(grepl("tumor", jsonlite::toJSON(layer$items[[1L]], auto_unbox = TRUE),
                     fixed = TRUE))

  # A vector can fit the working image's bounds yet still belong to a
  # different native frame. Earlier exporters mislabeled precisely this case.
  wrong_frame <- original
  wrong_frame$source_frame_id <- "native-0.25um"
  wrong_frame$native_shape_yx <- c(40L, 40L)
  wrong_frame$native_mpp_xy <- c(0.25, 0.25)
  jsonlite::write_json(wrong_frame, geometry, auto_unbox = TRUE)
  mislabeled <- experiment
  mislabeled$tissues[[1L]]$artifacts[[1L]]$sha256 <- sha(geometry)
  expect_error(wsi_experiment_images(mislabeled), "GeoJSON native frame")
  # The same mismatch must be caught for legacy tissue_annotations review.
  mislabeled$tissues[[1L]]$artifacts[[1L]]$role <- "tissue_annotations"
  mislabeled$tissues[[1L]]$artifacts[[1L]]$provenance$algorithm_id <- "bora-global-hard-group-v1"
  expect_error(wsi_experiment_images(mislabeled), "GeoJSON native frame")
  jsonlite::write_json(original, geometry, auto_unbox = TRUE)
  experiment$tissues[[1L]]$artifacts[[1L]]$sha256 <- sha(geometry)

  for (field in c("source_image_sha256", "native_shape_yx", "native_mpp_xy", "coordinate_space")) {
    changed <- original
    changed[[field]] <- switch(field,
      source_image_sha256 = paste(rep("b", 64), collapse = ""),
      native_shape_yx = c(21L, 20L), native_mpp_xy = c(0.25, 0.5),
      coordinate_space = "working_image_pixel_edges")
    jsonlite::write_json(changed, geometry, auto_unbox = TRUE)
    mismatch <- experiment
    mismatch$tissues[[1L]]$artifacts[[1L]]$sha256 <- sha(geometry)
    expect_error(wsi_experiment_images(mismatch), "GeoJSON native frame")
  }
  missing <- original
  missing$native_mpp_xy <- NULL
  jsonlite::write_json(missing, geometry, auto_unbox = TRUE)
  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$sha256 <- sha(geometry)
  expect_error(wsi_experiment_images(mismatch), "GeoJSON native frame")
  jsonlite::write_json(original, geometry, auto_unbox = TRUE)
  experiment$tissues[[1L]]$artifacts[[1L]]$sha256 <- sha(geometry)

  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$source_frame_id <- "another-frame"
  expect_error(wsi_experiment_images(mismatch), "frame or image identity")
  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$image_sha256 <- paste(rep("a", 64), collapse = "")
  expect_error(wsi_experiment_images(mismatch), "frame or image identity")
  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$sha256 <- paste(rep("a", 64), collapse = "")
  expect_error(wsi_experiment_images(mismatch), "SHA-256 mismatch")
  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$review_only <- FALSE
  expect_error(wsi_experiment_images(mismatch), "format, status, frame or image identity")
  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$provenance$prediction_unit <- "pixel"
  expect_error(wsi_experiment_images(mismatch), "format, status, frame or image identity")
  mismatch <- experiment
  mismatch$tissues[[1L]]$artifacts[[1L]]$provenance$identity_status <- "semantic_tumor"
  expect_error(wsi_experiment_images(mismatch), "format, status, frame or image identity")

  withr::local_options(wsiTools.experiment_max_bora_review_bytes = 1)
  capped <- wsi_experiment_images(experiment)[[1L]]$layers[[1L]]
  expect_false(capped$visible)
  expect_match(capped$name, "unavailable")
  expect_match(capped$metadata$unavailable_reason, "indexed derivative")
})

test_that("PLS-LDA refuses caller-supplied exploratory Bora training ROIs", {
  square <- list(list(c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1)))
  feature <- function(id, properties) list(type = "Feature", id = id,
    properties = c(list(name = id, classification = list(name = id)), properties),
    geometry = list(type = "Polygon", coordinates = square))
  rois <- wsi_roi_from_geojson(list(type = "FeatureCollection", features = list(
    feature("review", list(review_only = TRUE, source_type = "bora_candidate_review")),
    feature("manual", list()))))
  expect_error(wsi_prediction_reject_review_training(rois, 1L),
    "cannot be used for PLS-LDA training")
  expect_invisible(wsi_prediction_reject_review_training(rois, 2L))
  points <- data.frame(id = "token-1", x = 5, y = 5,
    tissue_id = "t1", project_image = "t1", project_image_index = 0L)
  expect_error(wsi_experiment_roi_training(points, rois, "roi:review"),
    "cannot be used for PLS-LDA training")
})

test_that("qualified tissue annotations retain the established ROI import", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "image.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  dir.create(file.path(root, "vectors", "exact"), recursive = TRUE)
  geometry <- file.path(root, "vectors", "exact", "mask.geojson")
  jsonlite::write_json(list(type = "FeatureCollection", features = list(list(
    type = "Feature", id = "qualified", properties = list(name = "accepted",
      classification = list(name = "tissue")), geometry = list(type = "Polygon",
      coordinates = list(list(c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1))))))),
    geometry, auto_unbox = TRUE)
  experiment <- list(root = root, experiment_id = "synthetic:accepted",
    tissues = list(list(tissue_id = "t1", name = "Synthetic tissue",
      image = list(resolved_path = image, sha256 = wsi_experiment_sha256(image),
        frame_id = "frame1", width = 20L, height = 20L, mpp_x = 0.5, mpp_y = 0.5),
      artifacts = list(list(role = "tissue_annotations", path = "vectors/exact/mask.geojson",
        sha256 = wsi_experiment_sha256(geometry), frame_id = "frame1")))))
  images <- wsi_experiment_images(experiment)
  expect_length(images[[1L]]$rois, 1L)
  expect_length(images[[1L]]$layers, 0L)
  expect_identical(images[[1L]]$rois[[1L]]$properties$source_role, "tissue_annotations")
  expect_identical(images[[1L]]$rois[[1L]]$class, "tissue")
})

test_that("an overlarge Bora feature count leaves the project open as an unavailable review layer", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "image.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  sha <- wsi_experiment_sha256
  geometry <- file.path(root, "many-candidates.geojson")
  feature <- list(type = "Feature", properties = list(class = "tumor"),
    geometry = list(type = "Polygon", coordinates = list(list(
      c(1, 1), c(2, 1), c(2, 2), c(1, 1)))))
  jsonlite::write_json(list(type = "FeatureCollection",
    coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(20L, 20L), native_mpp_xy = c(0.5, 0.5),
    source_frame_id = "native", source_image_sha256 = sha(image),
    features = replicate(5001L, feature, simplify = FALSE)),
    geometry, auto_unbox = TRUE)
  expect_lt(file.info(geometry)$size, 32 * 1024^2)
  artifact <- list(artifact_id = "bora-global-exact-many",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = sha(geometry), format = "geojson", review_only = TRUE,
    source_frame_id = "native", image_sha256 = sha(image), provenance = list(
      prediction_unit = "global_anonymous_Bora-AF_group",
      chain_complete_sha256 = paste(rep("a", 64), collapse = ""),
      manifest_sha256 = paste(rep("b", 64), collapse = ""),
      identity_status = "exploratory_transfer"))
  experiment <- list(root = root, experiment_id = "synthetic:many-bora-features",
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    tissues = list(list(tissue_id = "t1", name = "Synthetic tissue",
      image = list(resolved_path = image, sha256 = sha(image), frame_id = "native",
        width = 20L, height = 20L, mpp_x = 0.5, mpp_y = 0.5),
      artifacts = list(artifact))))
  images <- wsi_experiment_images(experiment)
  expect_length(images[[1L]]$rois, 0L)
  expect_length(images[[1L]]$layers, 1L)
  layer <- images[[1L]]$layers[[1L]]
  expect_identical(layer$source_type, "bora_candidate_review")
  expect_false(layer$visible)
  expect_length(layer$items, 0L)
  expect_false(layer$metadata$training_eligible)
  expect_true(layer$metadata$review_only)
  expect_match(layer$metadata$unavailable_reason, "5,000 features")
  expect_match(layer$metadata$unavailable_reason, "indexed derivative")
})

test_that("CellPhenotyper exporter-shaped JSON reopens as a review layer, not ROIs", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "image.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "mask.geojson")
  jsonlite::write_json(list(type = "FeatureCollection",
    coordinate_space = "native_image_pixel_edges", native_shape_yx = c(20L, 20L),
    native_mpp_xy = c(0.5, 0.5), source_frame_id = "synthetic-native-frame",
    source_image_sha256 = wsi_experiment_sha256(image), features = list(list(
    type = "Feature", id = "source-tumor", properties = list(
      class = "tumor", predicted_tissue = "tumor"),
    geometry = list(type = "Polygon", coordinates = list(list(
      c(1, 1), c(10, 1), c(10, 10), c(1, 10), c(1, 1))))))),
    geometry, auto_unbox = TRUE)
  digest <- wsi_experiment_sha256
  producer_artifact <- list(artifact_id = "bora-global-exact-synthetic",
    role = "bora_exploratory_boundary_review",
    label = "Bora anonymous-group boundaries (exploratory review; exact)",
    path = basename(geometry), sha256 = digest(geometry), format = "geojson",
    review_only = TRUE, source_frame_id = "synthetic-native-frame",
    image_sha256 = digest(image), provenance = list(
      producer = "bora-wsi-global-v1", algorithm_id = "synthetic-algorithm",
      algorithm_evidence_receipt_sha256 = paste(rep("c", 64), collapse = ""),
      prediction_unit = "global_anonymous_Bora-AF_group",
      chain_complete_sha256 = paste(rep("a", 64), collapse = ""),
      manifest_sha256 = paste(rep("b", 64), collapse = ""),
      identity_status = "exploratory_transfer", evidence_class = "synthetic",
      geometry = "exact mask-to-vector, 100 um2 minimum part/hole area"))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "synthetic:exporter-consumer", name = "Tiny cross-repo fixture",
    settings = list(bora_status = "withheld_pending_corrected_algorithm",
      bora_annotations_mode = "legacy_review_layer"),
    provenance = list(interpretation = "Anonymous boundaries require review."),
    tissues = list(list(tissue_id = "synthetic", name = "Synthetic image",
      image = list(path = basename(image), sha256 = digest(image), width = 20L,
        height = 20L, mpp_x = 0.5, mpp_y = 0.5,
        frame_id = "synthetic-native-frame",
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = list(producer_artifact))))
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  experiment <- wsi_read_experiment(path)
  project_images <- wsi_experiment_images(experiment)
  expect_length(project_images[[1L]]$rois, 0L)
  expect_length(project_images[[1L]]$layers, 1L)
  expect_identical(project_images[[1L]]$layers[[1L]]$source_type, "bora_candidate_review")
  expect_true(project_images[[1L]]$layers[[1L]]$items[[1L]]$locked)
  expect_identical(project_images[[1L]]$layers[[1L]]$metadata$identity_status,
                   "exploratory_transfer")
  expect_false(grepl("tumor", jsonlite::toJSON(project_images[[1L]]$layers[[1L]],
                                             auto_unbox = TRUE), fixed = TRUE))

  legacy <- manifest
  legacy_artifact <- producer_artifact
  legacy_artifact$role <- "tissue_annotations"
  legacy_artifact$review_only <- NULL
  legacy_artifact$provenance$algorithm_id <- "bora-global-hard-group-v1"
  legacy$tissues[[1L]]$artifacts <- list(legacy_artifact)
  jsonlite::write_json(legacy, path, auto_unbox = TRUE, pretty = TRUE)
  legacy_images <- wsi_experiment_images(wsi_read_experiment(path))
  expect_length(legacy_images[[1L]]$rois, 0L)
  expect_length(legacy_images[[1L]]$layers, 1L)
  expect_true(legacy_images[[1L]]$layers[[1L]]$items[[1L]]$locked)
  expect_true(legacy_images[[1L]]$layers[[1L]]$items[[1L]]$tissue_annotation)
  expect_false(grepl("tumor", jsonlite::toJSON(legacy_images[[1L]]$layers[[1L]],
                                             auto_unbox = TRUE), fixed = TRUE))

  # A suspicious old tissue_annotations artifact with missing provenance must
  # fail closed instead of becoming an editable PLS-LDA training annotation.
  legacy$tissues[[1L]]$artifacts[[1L]]$provenance <- list()
  jsonlite::write_json(legacy, path, auto_unbox = TRUE, pretty = TRUE)
  expect_error(wsi_experiment_images(wsi_read_experiment(path)),
               "Legacy anonymous Bora tissue_annotations lack complete source binding")
})

test_that("continuous Bora receipt imports only as a source-bound anonymous review", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  image <- file.path(root, "native.ppm")
  writeBin(as.raw(c(1, 2, 3)), image)
  geometry <- file.path(root, "review.geojson")
  digest <- wsi_experiment_sha256
  h <- function(letter) paste(rep(letter, 64), collapse = "")
  geo <- list(type = "FeatureCollection", coordinate_space = "native_image_pixel_edges",
    native_shape_yx = c(40L, 60L), native_mpp_xy = c(0.25, 0.25),
    source_frame_id = "native-frame", source_image_sha256 = digest(image),
    source_semantic_mask_sha256 = h("a"), global_model_binding_sha256 = h("b"),
    predicted_not_expert_annotation = TRUE, tissue_identity_status = "exploratory_transfer",
    vector_export = list(format = "bora-wsi-global-vector-v1", global_contours = TRUE,
      independently_fitted_windows = FALSE, minimum_part_and_hole_area_um2 = 100,
      geometry_role = "smoothed_review", smoothing_tolerance_um = 4),
    features = list(list(type = "Feature", properties = list(name = "tumor",
      predicted_tissue = "tumor", classification = list(name = "tumor")),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(2, 2), c(20, 2), c(20, 20), c(2, 20), c(2, 2)))))))
  write_geo <- function(value) jsonlite::write_json(value, geometry,
    auto_unbox = TRUE, pretty = TRUE)
  write_geo(geo)
  artifact <- list(artifact_id = "bora-continuous-review-synthetic",
    role = "bora_exploratory_boundary_review", path = basename(geometry),
    sha256 = digest(geometry), format = "geojson", review_only = TRUE,
    source_frame_id = "native-frame", image_sha256 = digest(image),
    provenance = list(producer = "bora-wsi-continuous-chain-v1",
      prediction_unit = "continuous_rgb9_rgb19_native_probability",
      identity_status = "anonymous_exploratory_review",
      chain_complete_sha256 = h("c"), semantic_mask_sha256 = h("a"),
      exact_mask_sha256 = h("d"), exact_geojson_sha256 = h("e"),
      review_geojson_sha256 = digest(geometry), global_model_binding_sha256 = h("b"),
      registration_sha256 = h("1"), registered_support_source_sha256 = h("2"),
      tissue_support_sha256 = h("3"), native_shape_yx = c(40L, 60L),
      native_mpp_xy = c(0.25, 0.25), minimum_area_um2 = 100,
      biological_accuracy_qualified = FALSE,
      exact_vector_matches_cleaned_mask = TRUE))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "synthetic:continuous", name = "Continuous Bora review",
    settings = list(bora_annotations_mode = "legacy_review_layer"),
    tissues = list(list(tissue_id = "t1", name = "Native tissue",
      image = list(path = basename(image), sha256 = digest(image), width = 60L,
        height = 40L, mpp_x = 0.25, mpp_y = 0.25,
        frame_id = "native-frame",
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = list(artifact))))
  path <- file.path(root, "experiment.cellphenotyper.json")
  open_images <- function(value) {
    jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
    wsi_experiment_images(wsi_read_experiment(path))
  }
  images <- open_images(manifest)
  expect_length(images[[1L]]$rois, 0L)
  expect_length(images[[1L]]$layers, 1L)
  layer <- images[[1L]]$layers[[1L]]
  expect_match(layer$name, "continuous")
  expect_identical(layer$source_type, "bora_candidate_review")
  expect_true(layer$metadata$review_only)
  expect_false(layer$metadata$training_eligible)
  expect_identical(layer$metadata$semantic_mask_sha256, h("a"))
  expect_identical(layer$metadata$exact_mask_sha256, h("d"))
  expect_identical(layer$metadata$exact_geojson_sha256, h("e"))
  expect_length(layer$items, 1L)
  expect_true(layer$items[[1L]]$locked)
  expect_true(layer$items[[1L]]$tissue_annotation)
  expect_false(grepl("tumor", jsonlite::toJSON(layer, auto_unbox = TRUE), fixed = TRUE))

  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$role <- "tissue_annotations"
  expect_error(open_images(wrong), "cannot be tissue_annotations")
  wrong$tissues[[1L]]$artifacts[[1L]]$provenance <- list()
  expect_error(open_images(wrong), "cannot be tissue_annotations")
  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$provenance$review_geojson_sha256 <- h("f")
  expect_error(open_images(wrong), "format, status, frame or image identity")
  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$provenance$identity_status <- "semantic_tumor"
  expect_error(open_images(wrong), "format, status, frame or image identity")
  for (field in c("source_semantic_mask_sha256", "global_model_binding_sha256",
                  "predicted_not_expert_annotation", "tissue_identity_status")) {
    altered <- geo
    altered[[field]] <- switch(field,
      source_semantic_mask_sha256 = h("f"), global_model_binding_sha256 = h("f"),
      predicted_not_expert_annotation = FALSE, tissue_identity_status = "validated")
    write_geo(altered)
    wrong <- manifest
    wrong$tissues[[1L]]$artifacts[[1L]]$sha256 <- digest(geometry)
    wrong$tissues[[1L]]$artifacts[[1L]]$provenance$review_geojson_sha256 <- digest(geometry)
    expect_error(open_images(wrong), "Continuous Bora review provenance")
  }
  for (field in c("geometry_role", "global_contours", "minimum_part_and_hole_area_um2")) {
    altered <- geo
    altered$vector_export[[field]] <- switch(field,
      geometry_role = "native_exact_after_area_cleanup", global_contours = FALSE,
      minimum_part_and_hole_area_um2 = 50)
    write_geo(altered)
    wrong <- manifest
    wrong$tissues[[1L]]$artifacts[[1L]]$sha256 <- digest(geometry)
    wrong$tissues[[1L]]$artifacts[[1L]]$provenance$review_geojson_sha256 <- digest(geometry)
    expect_error(open_images(wrong), "Continuous Bora review provenance")
  }
  write_geo(geo)
  wrong <- manifest
  wrong$tissues[[1L]]$artifacts[[1L]]$sha256 <- digest(geometry)
  wrong$tissues[[1L]]$artifacts[[1L]]$provenance$review_geojson_sha256 <- digest(geometry)
  wrong$tissues[[1L]]$image$width <- 30L
  expect_error(open_images(wrong), "GeoJSON native frame")
})
