# Deliberately synthetic calibrated images and feature tables, never biological evidence.
experiment_fixture <- function(tissues = 2L, kind = "virchow2-pooled", rows = 8L) {
  root <- tempfile("synthetic-experiment-")
  dir.create(root)
  width <- if (kind == "virchow2-pooled") 2560L else 1280L
  feature_names <- paste0("feat_", seq_len(width))
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "synthetic:test", name = "Synthetic contract fixture",
    settings = list(), provenance = list(synthetic = TRUE), tissues = list())
  for (i in seq_len(tissues)) {
    image <- file.path(root, paste0("image", i, ".ppm"))
    image_width <- rows * 8L
    writeBin(c(charToRaw(sprintf("P6\n%d 32\n255\n", image_width)), as.raw(rep(128L, image_width * 32L * 3L))), image)
    x <- outer(seq_len(rows), seq_len(width), function(row, col) sin(row / 2 + col / 13) + row * (col %% 3) / 10)
    colnames(x) <- feature_names
    table <- cbind(data.frame(tile_id = sprintf("%03d", seq_len(rows)), x = seq(4, image_width - 4, 8), y = 4), as.data.frame(x))
    features <- file.path(root, paste0("features", i, ".csv"))
    utils::write.csv(table, features, row.names = FALSE)
    manifest$tissues[[i]] <- list(tissue_id = paste0("t", i), name = paste("Synthetic tissue", i),
      specimen_id = paste0("s", i), image = list(path = basename(image), sha256 = wsi_experiment_sha256(image),
        width = image_width, height = 32L, mpp_x = 0.5, mpp_y = 0.5, frame_id = paste0("frame", i),
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"), artifacts = list(),
      feature_sets = list(list(feature_set_id = "virchow", kind = kind,
        observation_unit = if (kind == "virchow2-pooled") "tile" else "token",
        path = basename(features), sha256 = wsi_experiment_sha256(features), format = "csv",
        row_count = rows, feature_columns = as.list(feature_names), id_column = "tile_id", x_column = "x", y_column = "y",
        coordinate_frame = "image", encoder = list(name = "Virchow2", revision = "synthetic-fixed",
          checkpoint_sha256 = paste(rep("a", 64), collapse = "")),
        preprocessing = list(feature_definition = if (kind == "virchow2-pooled") "CLS+mean_patch" else "spatial_patch_tokens"),
        grid = list(tile_width_px = 8, tile_height_px = 8, spacing_x_px = 8, spacing_y_px = 8,
          origin_x_px = 0, origin_y_px = 0))))
  }
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE)
  list(root = root, path = path, manifest = manifest)
}

test_that("experiment providers keep tile/token identity and native coordinates", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture()
  exp <- wsi_read_experiment(fixture$root)
  features <- wsi_experiment_features(exp, "virchow")
  expect_s3_class(exp, "wsi_experiment")
  expect_equal(dim(features$x), c(16L, 2560L))
  expect_equal(features$observations$original_id[1], "001")
  expect_equal(length(unique(features$observations$id)), 16L)
  expect_true(all(features$observations$unit == "tile"))
  expect_equal(features$observations$x[1], 4)
  expect_equal(features$observations$project_image_index, rep(0:1, each = 8))
  expect_equal(colnames(features$x), unlist(fixture$manifest$tissues[[1]]$feature_sets[[1]]$feature_columns))
  spatial <- wsi_experiment_features(wsi_read_experiment(experiment_fixture(1L, "virchow2-spatial")$path), "virchow")
  expect_equal(dim(spatial$x), c(8L, 1280L))
  expect_true(all(spatial$observations$unit == "token"))
  context <- wsi_experiment_prediction_context(exp)
  expect_true(wsi_prediction_context_enabled(context))
  expect_equal(wsi_prediction_config(experiment = exp)$sources[[1]]$unit, "tile")
  expect_equal(wsi_prediction_points(context, "experiment:virchow")$id, features$observations$id)
})

test_that("registered viewing pyramids keep source identity and an explicit reader", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture(1L)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  original <- fixture$manifest$tissues[[1]]$image
  viewing <- file.path(fixture$root, "viewing.ppm")
  file.copy(file.path(fixture$root, original$path), viewing)
  manifest <- fixture$manifest
  manifest$tissues[[1]]$image$display <- list(path = basename(viewing),
    sha256 = wsi_experiment_sha256(viewing), format = "native-pixel-pyramid",
    backend = "vips", source_sha256 = original$sha256,
    source_frame_id = original$frame_id)
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE)
  experiment <- wsi_read_experiment(fixture$path)
  expect_identical(experiment$tissues[[1]]$image$sha256, original$sha256)
  expect_equal(experiment$tissues[[1]]$image$resolved_path,
               normalizePath(file.path(fixture$root, original$path), winslash = "/"))
  display <- wsi_experiment_images(experiment)[[1]]
  expect_equal(display$path, normalizePath(viewing, winslash = "/"))
  expect_identical(display$backend, "vips")
  expect_identical(display$frame_id, original$frame_id)
  manifest$tissues[[1]]$image$display$source_frame_id <- "another-frame"
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE)
  expect_error(wsi_read_experiment(fixture$path), "preserve the analyzed image identity")
  manifest$tissues[[1]]$image$display$source_frame_id <- original$frame_id
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE)
  cat("tampered", file = viewing, append = TRUE)
  expect_error(wsi_read_experiment(fixture$path), "SHA-256 mismatch.*display pyramid")
})

test_that("experiment files fail closed on identity, order, unit and grid errors", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture(1L)
  exp <- wsi_read_experiment(fixture$path)
  features_path <- exp$tissues[[1]]$feature_sets[[1]]$resolved_path
  cat("\n", file = features_path, append = TRUE)
  expect_error(wsi_experiment_features(exp, "virchow"), "SHA-256")
  bad <- fixture$manifest
  bad$tissues[[1]]$feature_sets[[1]]$observation_unit <- "cell"
  jsonlite::write_json(bad, fixture$path, auto_unbox = TRUE)
  expect_error(wsi_read_experiment(fixture$path, verify_hashes = FALSE), "observation_unit")
  bad <- fixture$manifest
  bad$tissues[[1]]$feature_sets[[1]]$feature_columns <- rev(bad$tissues[[1]]$feature_sets[[1]]$feature_columns)
  bad$tissues[[1]]$feature_sets[[1]]$sha256 <- wsi_experiment_sha256(features_path)
  jsonlite::write_json(bad, fixture$path, auto_unbox = TRUE)
  expect_error(wsi_experiment_features(wsi_read_experiment(fixture$path), "virchow"), "different order")
  bad <- fixture$manifest
  bad$tissues[[1]]$feature_sets[[1]]$sha256 <- wsi_experiment_sha256(features_path)
  bad$tissues[[1]]$feature_sets[[1]]$grid$origin_x_px <- 1
  jsonlite::write_json(bad, fixture$path, auto_unbox = TRUE)
  expect_error(wsi_experiment_features(wsi_read_experiment(fixture$path), "virchow"), "grid")
})

test_that("project save and reopen preserve the experiment source identity", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture()
  project_dir <- tempfile("synthetic.wsiproject")
  project <- wsi_experiment_project(fixture$path, output = project_dir)
  restored <- wsi_read_project(project_dir)
  expect_equal(restored$metadata$cellphenotyper_experiment$experiment_id, "synthetic:test")
  expect_length(restored$metadata$experiment_images, 2L)
  expect_equal(wsi_read_experiment(restored)$manifest_path, project$experiment$manifest_path)
  cat("\n", file = fixture$path, append = TRUE)
  expect_error(wsi_read_experiment(restored), "identity changed")
})

test_that("classifier preprocessing is invariant to held-out features and persists", {
  skip_if_not_installed("openssl")
  skip_if_not_installed("fastPLS")
  f <- wsi_experiment_features(wsi_read_experiment(experiment_fixture()$path), "virchow")
  train <- c(1L, 2L, 5L, 6L)
  f$x[, 1L] <- seq_len(nrow(f$x)) * 1e4
  f$x[train, 1L] <- 7
  f$x[train[1], 2L] <- NA_real_
  labels <- stats::setNames(c("stroma", "stroma", "epithelium", "epithelium"), f$observations$id[train])
  a <- wsi_experiment_train_classifier(f, "virchow", labels, train_ids = names(labels), max_features = 8L)
  changed <- f
  changed$x[-train, ] <- changed$x[-train, ] * 1e6
  b <- wsi_experiment_train_classifier(changed, "virchow", labels, train_ids = names(labels), max_features = 8L)
  expect_identical(a$preprocessing, b$preprocessing)
  expect_false("feat_1" %in% a$preprocessing$feature_columns)
  expect_equal(a$preprocessing$fitted_on, "training_rows_only")
  path <- tempfile(fileext = ".rds")
  wsi_write_experiment_classifier(a, path)
  restored <- wsi_read_experiment_classifier(path)
  expect_identical(wsi_experiment_predict_classifier(a, f)$predicted,
                   wsi_experiment_predict_classifier(restored, f)$predicted)
  expect_true(all(wsi_experiment_predict_classifier(restored, f)$unit == "tile"))
  incompatible <- f
  incompatible$signature$encoder$revision <- "another-checkpoint"
  expect_error(wsi_experiment_predict_classifier(a, incompatible), "incompatible")
  expect_error(wsi_experiment_train_classifier(f, "virchow", labels), "Declare train_ids")
})

test_that("feature and local appearance scopes are bounded before execution", {
  skip_if_not_installed("openssl")
  exp <- wsi_read_experiment(experiment_fixture(2L, "virchow2-spatial", rows = 16L)$path)
  old <- options(wsiTools.experiment_max_rows = 5L)
  on.exit(options(old), add = TRUE)
  expect_error(wsi_experiment_features(exp, "virchow"), "exceed")
  options(wsiTools.experiment_max_rows = 10000L)
  expect_error(wsi_experiment_appearance(exp, "virchow", mode = "local", active_tissue_id = "t1",
    tissue_ids = c("t1", "t2")), "another tissue")
  expect_error(wsi_experiment_appearance(exp, "virchow", mode = "local"), "active_tissue_id")
})

experiment_roi_fixture <- function(root, hole = FALSE, conflict = FALSE) {
  feature <- function(id, class, x0, x1, hole = FALSE) {
    rings <- list(list(c(x0, 0), c(x1, 0), c(x1, 8), c(x0, 8), c(x0, 0)))
    if (hole) rings[[2L]] <- list(c(1, 1), c(2, 1), c(2, 2), c(1, 2), c(1, 1))
    list(type = "Feature", id = id, properties = list(name = id,
      classification = list(name = class), tissue_id = "t1", project_image_index = 0L),
      geometry = list(type = "Polygon", coordinates = rings))
  }
  features <- list(feature("a", "A", 0, 30, hole), feature("b", "B", 32, 64))
  if (conflict) features[[3L]] <- feature("conflict", "B", 0, 16)
  path <- file.path(root, "synthetic-annotations.geojson")
  jsonlite::write_json(list(type = "FeatureCollection", features = features), path, auto_unbox = TRUE)
  path
}

test_that("annotation training excludes mixed footprints, holes and cross-tissue leakage", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture()
  f <- wsi_experiment_features(fixture$path, "virchow")
  rois <- read_geojson(experiment_roi_fixture(fixture$root, hole = TRUE))
  training <- wsi_experiment_roi_training(f$observations, rois, c("a", "b"))
  expect_equal(training$rows, c(2L, 3L, 5L, 6L, 7L, 8L))
  expect_equal(training$coverage$excluded_boundary_or_mixed, 2L)
  expect_false(any(training$rows > 8L))
  expect_equal(training$coverage$policy, "complete_rectangular_footprint_inside_one_annotation")
  conflict <- read_geojson(experiment_roi_fixture(fixture$root, conflict = TRUE))
  expect_error(wsi_experiment_roi_training(f$observations, conflict, c("a", "b", "conflict")), "Conflicting")
  rois$properties <- I(lapply(seq_len(nrow(rois)), function(i) list()))
  expect_error(wsi_experiment_roi_training(f$observations, rois, c("a", "b")), "Multi-tissue")
})

test_that("experiment annotation artifacts retain hashed tissue identity on project records", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture()
  annotations <- experiment_roi_fixture(fixture$root)
  fixture$manifest$tissues[[1L]]$artifacts <- list(list(role = "tissue_annotations",
    path = basename(annotations), sha256 = wsi_experiment_sha256(annotations), frame_id = "frame1"))
  jsonlite::write_json(fixture$manifest, fixture$path, auto_unbox = TRUE)
  images <- wsi_experiment_images(wsi_read_experiment(fixture$path))
  expect_length(images[[1L]]$rois, 2L)
  expect_length(images[[2L]]$rois, 0L)
  expect_match(images[[1L]]$rois[[1L]]$id, "synthetic")
  expect_equal(images[[1L]]$artifact_references[[1L]]$sha256, wsi_experiment_sha256(annotations))
})

test_that("zero contained training footprints give actionable annotation guidance", {
  skip_if_not_installed("openssl")
  fixture <- experiment_fixture(1L)
  f <- wsi_experiment_features(fixture$path, "virchow")
  rois <- read_geojson(experiment_roi_fixture(fixture$root))
  rois$coordinates[[1L]] <- list(list(c(3, 3), c(5, 3), c(5, 5), c(3, 5), c(3, 3)))
  expect_error(wsi_experiment_roi_training(f$observations, rois, "a"),
    "No complete tile/token footprints.*Enlarge the annotations or choose a spatial-token source")
  expect_error(wsi_experiment_roi_training(f$observations, rois, "a"),
    "Accepted: 0; centre-labelled: 1; boundary/mixed excluded: 1")
})

test_that("prediction request annotations never replace synchronized active viewer IDs", {
  feature <- function(id) list(type = "Feature", id = id,
    properties = list(name = id, class = "A"), geometry = list(type = "Polygon",
      coordinates = list(list(c(0, 0), c(8, 0), c(8, 8), c(0, 8), c(0, 0)))))
  header <- function(revision, full = FALSE) list(version = 1L, client = "experiment-test-browser",
    revision = revision, base_revision = revision - 1L, project_key = "active-image",
    full = full, selected_ids = list(), selected_id = NULL)
  state <- wsiTools:::wsi_new_viewer_state()
  wsiTools:::wsi_viewer_state_apply(state, list(event = "viewer_loaded",
    sync = header(1L, TRUE), rois = list(type = "FeatureCollection",
      features = list(feature("active-a"), feature("active-b")))))
  analytical_ids <- c("image-one::active-a", "image-two::active-a")
  supplied <- list(type = "FeatureCollection", features = lapply(analytical_ids, feature))
  seen <- list()
  result <- data.frame(id = "tile-one", label = "tile-one", x = 4, y = 4, set = "test",
    train_annotation_id = NA_character_, test_annotation_id = NA_character_, observed = NA_character_,
    predicted = "A", predicted_pls_lda = "A", svm_refined = FALSE, feature_source = "experiment:test")
  class(result) <- c("wsi_prediction_result", class(result))
  testthat::local_mocked_bindings(
    wsi_prediction_run = function(context, rois, ...) {
      seen$pls <<- rois$roi_id
      result
    },
    wsi_experiment_appearance_response = function(context, rois, payload) {
      seen$appearance <<- rois$roi_id
      list(points = list(), classes = list("A"), scope = "shared")
    }, .package = "wsiTools")
  wsiTools:::wsi_prediction_response(list(), state, list(feature_source = "experiment:test",
    rois = supplied, train_annotations = analytical_ids))
  expect_identical(seen$pls, analytical_ids)
  expect_identical(state$rois$roi_id, c("active-a", "active-b"))
  wsiTools:::wsi_prediction_response(list(), state, list(action = "experiment_appearance",
    feature_source = "experiment:test", rois = supplied, train_annotations = analytical_ids))
  expect_identical(seen$appearance, analytical_ids)
  expect_identical(state$rois$roi_id, c("active-a", "active-b"))
  patch <- header(2L)
  patch$rois_patch <- list(upsert = list(), remove = "active-a", order = "active-b")
  expect_no_error(wsiTools:::wsi_viewer_state_apply(state, list(event = "roi_deleted", sync = patch)))
  expect_identical(state$rois$roi_id, "active-b")
  expect_equal(state$browser_sync_ack$revision, 2L)
})

test_that("optional Bora CPU bridge fits and replays frozen appearance without refitting", {
  skip_if_not_installed("openssl")
  python <- Sys.getenv("WSITOOLS_TEST_BORA_PYTHON", "")
  skip_if(!nzchar(python), "Set WSITOOLS_TEST_BORA_PYTHON for the optional installed Bora CPU bridge")
  path <- Sys.getenv("WSITOOLS_TEST_BORA_PYTHONPATH", "")
  old <- options(wsiTools.bora_python = python, wsiTools.bora_pythonpath = if (nzchar(path)) path else NULL)
  on.exit(options(old), add = TRUE)
  exp <- wsi_read_experiment(experiment_fixture(2L, "virchow2-spatial", rows = 16L)$path)
  f <- wsi_experiment_features(exp, "virchow", "t1")
  labels <- stats::setNames(rep(c("A", "B"), each = 8), f$observations$id)
  fitted <- wsi_experiment_appearance(exp, "virchow", labels, names(labels), mode = "local", active_tissue_id = "t1")
  replayed <- wsi_experiment_appearance(exp, "virchow", mode = "local", active_tissue_id = "t1", recipe_path = fitted$recipe_path)
  expect_equal(fitted$points, replayed$points)
  expect_true(file.exists(fitted$recipe_path))
  expect_length(fitted$points, 16L)
  expect_equal(fitted$scope, "local")
  expect_true(all(vapply(fitted$points, function(p) p$tissue_id == "t1", logical(1))))
  expect_error(wsi_experiment_appearance(exp, "virchow", mode = "local", active_tissue_id = "t2",
    recipe_path = fitted$recipe_path), "another tissue/experiment")
  shared <- wsi_experiment_appearance(exp, "virchow", labels, names(labels),
    mode = "shared", tissue_ids = c("t1", "t2"))
  target <- wsi_experiment_appearance(exp, "virchow", mode = "shared", tissue_ids = "t2",
    recipe_path = shared$recipe_path)
  expect_equal(target$points, shared$points[17:32])
  expect_equal(unlist(shared$classes), c("A", "B"))
})
