registered_tissue_fixture <- function() {
  root <- tempfile("registered-view-"); dir.create(root)
  image <- file.path(root, "cell.ppm")
  tissue <- file.path(root, "tissue.ppm")
  writeBin(c(charToRaw("P6\n64 64\n255\n"), as.raw(rep(128, 64 * 64 * 3))), image)
  writeBin(c(charToRaw("P6\n32 32\n255\n"), as.raw(rep(96, 32 * 32 * 3))), tissue)
  cell_frame <- list(path = "cell.ppm", sha256 = wsi_experiment_sha256(image),
    width = 64L, height = 64L, mpp_x = .25, mpp_y = .25,
    frame_id = "cell-frame", origin_x = 0, origin_y = 0,
    coordinate_space = "crop-native-pixels",
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels")
  tissue_frame <- modifyList(cell_frame, list(path = "tissue.ppm", sha256 = wsi_experiment_sha256(tissue),
    width = 32L, height = 32L, mpp_x = .5, mpp_y = .5, frame_id = "tissue-frame"))
  sidecar <- list(format = "cellphenotyper-dual-mpp-registration-v1", verified = TRUE,
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels",
    source_scene_sha256 = strrep("c", 64), image = tissue_frame,
    cell_full_image = cell_frame, cell_project_image = cell_frame,
    cell_crop_bbox_in_cell_full_xyxy = list(0L, 0L, 64L, 64L),
    tissue_to_cell_full_affine = list(list(2,0,0), list(0,2,0), list(0,0,1)),
    tissue_to_cell_project_affine = list(list(2,0,0), list(0,2,0), list(0,0,1)),
    cell_project_to_tissue_affine = list(list(.5,0,0), list(0,.5,0), list(0,0,1)))
  registration <- file.path(root, "registration.json")
  jsonlite::write_json(sidecar, registration, auto_unbox = TRUE, digits = 17)
  kodama <- file.path(root, "kodama.csv")
  virchow <- file.path(root, "virchow.csv")
  utils::write.csv(data.frame(tile_id = c("a", "b"), native_x = c(8,20), native_y = c(10,20),
    kodama_1 = c(.2,.9), kodama_2 = c(.4,.1)), kodama, row.names = FALSE)
  utils::write.csv(data.frame(tile_id = c("a", "b"), native_x = c(8,20), native_y = c(10,20)),
    virchow, row.names = FALSE)
  bora <- file.path(root, "bora.geojson")
  features <- list(list(type = "Feature", id = "tumor-1",
    properties = list(classification = list(name = "tumor")),
    geometry = list(type = "Polygon", coordinates = list(list(
      list(4,4), list(12,4), list(12,12), list(4,12), list(4,4))))))
  jsonlite::write_json(list(type = "FeatureCollection", features = features), bora,
    auto_unbox = TRUE, digits = 17)
  asset <- function(path, role, id, format) list(asset_id = id, role = role, format = format,
    path = basename(path), sha256 = wsi_experiment_sha256(path),
    image_sha256 = tissue_frame$sha256, frame_id = tissue_frame$frame_id)
  k <- c(asset(kodama, "kodama_map", "kodama", "csv"),
    list(id_column = "tile_id", x_column = "native_x", y_column = "native_y",
      dimension_columns = list("kodama_1", "kodama_2"), row_count = 2L))
  v <- c(asset(virchow, "virchow2_tiles", "virchow", "csv"),
    list(id_column = "tile_id", x_column = "native_x", y_column = "native_y", row_count = 2L))
  b <- asset(bora, "bora_geometry", "bora", "geojson")
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "registered-fixture", name = "Registered tissue fixture",
    tissues = list(list(tissue_id = "t1", name = "Tissue 1", image = cell_frame,
      feature_sets = list(), artifacts = list(), registered_tissue = list(
        registration = list(path = basename(registration), sha256 = wsi_experiment_sha256(registration)),
        image = tissue_frame, assets = list(k,v,b)))))
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, digits = 17)
  list(root = root, path = path)
}

test_that("registered tissue catalog is metadata-only and views use the exact affine", {
  fixture <- registered_tissue_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  experiment <- wsi_read_experiment(fixture$path)
  catalog <- wsi_experiment_registered_tissue_catalog(experiment)[[1L]]
  expect_equal(catalog$tissue_to_cell_full_affine[[1L]][[1L]], 2)
  expect_null(catalog$assets[[1L]]$resolved_path)
  box <- list(xmin = 0, ymin = 0, xmax = 35, ymax = 35)
  k <- wsi_experiment_registered_tissue_query(experiment, "t1", "kodama", box, limit = 1L)
  expect_equal(k$total, 1L)
  expect_equal(k$items[[1L]]$x, 16)
  expect_equal(k$coordinate_frame, "cell-frame")
  v <- wsi_experiment_registered_tissue_query(experiment, "t1", "virchow", box, limit = 1L)
  expect_equal(v$total, 1L)
  expect_equal(v$items[[1L]]$y, 20)
  b <- wsi_experiment_registered_tissue_query(experiment, "t1", "bora", box)
  expect_equal(b$total, 1L)
  expect_true(b$items[[1L]]$locked)
  expect_equal(b$items[[1L]]$bbox$xmin, 8)
  expect_equal(b$items[[1L]]$bbox$xmax, 24)
  expect_equal(b$items[[1L]]$source_type, "bora_predicted")
  response <- wsi_prediction_response(wsi_experiment_prediction_context(experiment),
    new.env(parent = emptyenv()), list(action = "experiment_registered_tissue",
      tissue_id = "t1", asset_id = "kodama", bbox = box, limit = 1L))
  expect_true(response$ok)
  expect_equal(response$registered_tissue$items[[1L]]$x, 16)
})

test_that("registered tissue response rejects stale assets and invalid requests", {
  fixture <- registered_tissue_fixture()
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  experiment <- wsi_read_experiment(fixture$path)
  box <- list(xmin = 0, ymin = 0, xmax = 64, ymax = 64)
  expect_error(wsi_experiment_registered_tissue_query(experiment, "t1", "missing", box),
    "Select one registered tissue asset")
  expect_error(wsi_experiment_registered_tissue_query(experiment, "t1", "kodama",
    list(xmin = 4, ymin = 4, xmax = 3, ymax = 5)), "finite ordered")
  write("changed", file.path(fixture$root, "virchow.csv"), append = TRUE)
  expect_error(wsi_experiment_registered_tissue_query(experiment, "t1", "virchow", box),
    "changed since Open Project")
})
