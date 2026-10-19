experiment_open_guard_fixture <- function() {
  root <- tempfile("open-guard-"); dir.create(root)
  image <- file.path(root, "image.ppm")
  writeBin(c(charToRaw("P6\n8 8\n255\n"), as.raw(rep(128L, 8L * 8L * 3L))), image)
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "open-guard", name = "Open guard", tissues = list(list(
      tissue_id = "t1", name = "Tissue", image = list(path = "image.ppm", sha256 = wsi_experiment_sha256(image),
        width = 8L, height = 8L, mpp_x = .5, mpp_y = .5, frame_id = "t1:image",
        coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"), artifacts = list(), feature_sets = list())))
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE)
  list(root = root, image = image, path = path, manifest = manifest)
}

test_that("explicit viewer opening rechecks cached experiment source identities", {
  skip_if_not_installed("openssl")
  fixture <- experiment_open_guard_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  cached <- wsi_read_experiment(fixture$path)
  changed <- fixture$manifest; changed$name <- "Revised"
  jsonlite::write_json(changed, fixture$path, auto_unbox = TRUE)
  expect_error(wsi_viewer_experiment(cached, open = FALSE), "index changed")
  current <- wsi_read_experiment(fixture$path)
  con <- file(fixture$image, "ab"); writeBin(as.raw(0), con); close(con)
  expect_error(wsi_viewer_experiment(current, open = FALSE), "SHA-256 mismatch")
})

test_that("viewer preparation closes its owned slide on failure before live handoff", {
  skip_if_not_installed("openssl")
  fixture <- experiment_open_guard_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  closed <- 0L
  testthat::local_mocked_bindings(
    wsi_open = function(...) list(path = fixture$image),
    wsi_close = function(...) { closed <<- closed + 1L; invisible(NULL) },
    wsi_experiment_images = function(...) stop("Preparation fixture failure"))
  expect_error(wsi_viewer_experiment(fixture$path, open = FALSE), "Preparation fixture failure")
  expect_equal(closed, 1L)
})
