partitioned_cell_fixture <- function(rows = 12L) {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")
  root <- tempfile("partition-cell-"); dir.create(root)
  image_path <- file.path(root, "image.ppm")
  writeBin(c(charToRaw("P6\n64 64\n255\n"), as.raw(rep(128, 64 * 64 * 3))), image_path)
  image <- list(path = "image.ppm", sha256 = wsi_experiment_sha256(image_path), width = 64L, height = 64L,
    mpp_x = .25, mpp_y = .25, origin_x = 0, origin_y = 0,
    coordinate_space = "crop-native-pixels", frame_id = "synthetic-frame",
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels")
  feature <- list(kind = "cellvit-cell", observation_unit = "cell", feature_set_id = "synthetic-cells",
    frame_id = image$frame_id, path = "features.json", format = "partitioned-sqlite", row_count = rows,
    id_column = "cell_id", x_column = "x", y_column = "y", feature_columns = as.list(c("f1", "f2")),
    coordinate_frame = "image", descriptor = list(backbone = "HIPT", checkpoint_sha256 = strrep("a", 64),
      runtime_version = "SYNTHETIC-test", feature_definition = "synthetic", feature_count = 2L,
      coordinate_frame = "image", sampling = list(model_input_mpp = .25, patch_size_px = 1024L,
        patch_overlap_px = 64L, token_size_px = 16L),
      preprocessing = list(runtime_wheel_sha256 = strrep("f", 64), normalization = "synthetic",
        resampling = "synthetic", token_provenance = "synthetic")))
  model <- list(model_id = "synthetic", backbone = "HIPT", taxonomy = "SYNTHETIC", status = "available",
    class_names = list("A", "B"), checkpoint_sha256 = strrep("b", 64),
    score_semantics = "uncalibrated_model_probability")
  source_path <- file.path(root, "partitions.json")
  jsonlite::write_json(list(schema = "cellphenotyper-cellvit-shards-v1", image = image,
    cell_count = rows, partition_count = 1L, partitions = list(list(id = "synthetic"))),
    source_path, auto_unbox = TRUE, null = "null", digits = 17)
  source_hash <- wsi_experiment_sha256(source_path)
  pointer_path <- file.path(root, "features.json")
  jsonlite::write_json(list(schema = "cellphenotyper-partitioned-cell-features-v1",
    image = image, partition_manifest_sha256 = source_hash, row_count = rows,
    feature_set_id = feature$feature_set_id, feature_columns = feature$feature_columns,
    descriptor = feature$descriptor), pointer_path, auto_unbox = TRUE, null = "null", digits = 17)
  feature$sha256 <- wsi_experiment_sha256(pointer_path)
  db <- file.path(root, "cells.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), db)
  DBI::dbExecute(connection, "CREATE TABLE metadata(key TEXT PRIMARY KEY,value TEXT NOT NULL)")
  DBI::dbExecute(connection, "CREATE TABLE cells(row_id INTEGER PRIMARY KEY,cell_id TEXT UNIQUE,source_id TEXT UNIQUE,x REAL,y REAL,xmin REAL,ymin REAL,xmax REAL,ymax REAL,raw_json TEXT)")
  DBI::dbExecute(connection, "CREATE TABLE predictions(row_id INTEGER,model_id TEXT,status TEXT,class_id INTEGER,label TEXT,probability REAL,PRIMARY KEY(row_id,model_id))")
  DBI::dbExecute(connection, "CREATE TABLE features(row_id INTEGER PRIMARY KEY,values_json TEXT NOT NULL)")
  DBI::dbExecute(connection, "CREATE VIRTUAL TABLE bbox_rtree USING rtree(row_id,xmin,xmax,ymin,ymax)")
  cells <- lapply(seq_len(rows), function(i) list(cell_id = sprintf("cell-%04d", i), source_id = as.character(i),
    x = i * 2, y = 10, predictions = list(synthetic = list(status = "predicted", class_id = 1L,
      label = "B", probability = .75, probabilities = list(.25, .75)))))
  for (i in seq_len(rows)) {
    cell <- cells[[i]]
    DBI::dbExecute(connection, "INSERT INTO cells VALUES(?,?,?,?,?,?,?,?,?,?)",
      params = list(i, cell$cell_id, cell$source_id, cell$x, cell$y, cell$x - .5,
        cell$y - .5, cell$x + .5, cell$y + .5,
        as.character(jsonlite::toJSON(cell, auto_unbox = TRUE, null = "null", digits = 17))))
    DBI::dbExecute(connection, "INSERT INTO predictions VALUES(?,?,?,?,?,?)",
      params = list(i, "synthetic", "predicted", 1L, "B", .75))
    DBI::dbExecute(connection, "INSERT INTO features VALUES(?,?)", params = list(i, "[0.1,0.2]"))
    DBI::dbExecute(connection, "INSERT INTO bbox_rtree VALUES(?,?,?,?,?)",
      params = list(i, cell$x - .5, cell$x + .5, cell$y - .5, cell$y + .5))
  }
  header <- list(schema = "cellphenotyper-cell-classification", schema_version = 1L,
    image = image, feature_set = feature, models = list(model), cells = list())
  metadata <- list(schema = "cellphenotyper-cell-index", schema_version = 2L,
    source_format = "cellphenotyper-cellvit-shards-v1",
    classification = list(path = "partitions.json", sha256 = source_hash),
    classification_header = header, image = image, feature_set = feature,
    feature_csv_sha256 = feature$sha256, models = list(model), cell_count = rows, sidecars = list())
  for (key in names(metadata)) DBI::dbExecute(connection,
    "INSERT INTO metadata(key,value) VALUES(?,?)", params = list(key,
      as.character(jsonlite::toJSON(metadata[[key]], auto_unbox = TRUE, null = "null", digits = 17))))
  DBI::dbDisconnect(connection)
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "synthetic-partition", name = "Synthetic partition review",
    tissues = list(list(tissue_id = "t1", name = "Synthetic tissue", image = image,
      feature_sets = list(feature), artifacts = list(),
      cell_classification = list(path = "partitions.json", sha256 = source_hash,
        feature_manifest = list(path = "partitions.json", sha256 = source_hash),
        index = list(path = "cells.sqlite", sha256 = wsi_experiment_sha256(db), format = "sqlite")))))
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, null = "null", digits = 17)
  list(root = root, path = path)
}

test_that("partitioned CellViT projects reopen with bounded indexed review", {
  fixture <- partitioned_cell_fixture(rows = 12L)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  experiment <- wsi_read_experiment(fixture$path)
  catalog <- wsi_experiment_cell_catalog(experiment)[[1L]]
  expect_true(catalog$indexed)
  expect_equal(catalog$cell_count, 12L)
  expect_equal(catalog$models[[1L]]$taxonomy, "SYNTHETIC")
  expect_error(wsi_experiment_features(experiment, "synthetic-cells"), "bounded indexed-cell selection")
  context <- wsi_experiment_prediction_context(experiment)
  result <- wsi_prediction_response(context, new.env(), list(action = "experiment_cell_index",
    mode = "viewport", tissue_id = "t1", limit = 3L,
    bbox = list(xmin = 0, ymin = 0, xmax = 64, ymax = 64)))$cell_index
  expect_equal(result$total, 12L)
  expect_length(result$cells, 3L)
})

test_that("version 3 partition-row feature pointers reopen without loading vectors", {
  fixture <- partitioned_cell_fixture(rows = 12L)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  pointer_path <- file.path(fixture$root, "features.json")
  pointer <- jsonlite::fromJSON(pointer_path, simplifyVector = FALSE)
  pointer$schema <- "cellphenotyper-partitioned-cell-features-v2"
  pointer$feature_storage <- "partition-row-reference-v1"
  jsonlite::write_json(pointer, pointer_path, auto_unbox = TRUE, null = "null", digits = 17)
  pointer_sha <- wsi_experiment_sha256(pointer_path)
  index_path <- file.path(fixture$root, "cells.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), index_path)
  DBI::dbExecute(connection, "UPDATE metadata SET value='3' WHERE key='schema_version'")
  DBI::dbExecute(connection, "INSERT INTO metadata(key,value) VALUES('feature_storage','\"partition-row-reference-v1\"')")
  DBI::dbExecute(connection, "DELETE FROM metadata WHERE key='feature_csv_sha256'")
  feature <- jsonlite::fromJSON(DBI::dbGetQuery(connection,
    "SELECT value FROM metadata WHERE key='feature_set'")$value[[1L]], simplifyVector = FALSE)
  feature$sha256 <- pointer_sha
  for (key in c("feature_set", "classification_header")) {
    value <- jsonlite::fromJSON(DBI::dbGetQuery(connection,
      "SELECT value FROM metadata WHERE key=?", params = list(key))$value[[1L]], simplifyVector = FALSE)
    if (key == "feature_set") value <- feature else value$feature_set <- feature
    DBI::dbExecute(connection, "UPDATE metadata SET value=? WHERE key=?",
      params = list(as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = 17)), key))
  }
  DBI::dbExecute(connection, "UPDATE features SET values_json=json_object('partition_id','synthetic','row',row_id-1)")
  DBI::dbDisconnect(connection)
  project <- jsonlite::fromJSON(fixture$path, simplifyVector = FALSE)
  project$tissues[[1L]]$feature_sets[[1L]]$sha256 <- pointer_sha
  project$tissues[[1L]]$cell_classification$index$sha256 <- wsi_experiment_sha256(index_path)
  jsonlite::write_json(project, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  opened <- wsi_read_experiment(fixture$path)
  catalog <- wsi_experiment_cell_catalog(opened)[[1L]]
  expect_equal(catalog$cell_count, 12L)
  expect_equal(catalog$index_metadata$schema_version, 3L)
  result <- wsi_prediction_response(wsi_experiment_prediction_context(opened), new.env(),
    list(action = "experiment_cell_index", mode = "display_points", tissue_id = "t1",
      model_id = "synthetic", bbox = list(xmin = 0, ymin = 0, xmax = 8, ymax = 64)))$cell_index
  expect_equal(result$total, 4)
  expect_length(result$points, 4L)
  expect_true(all(vapply(result$points, function(point) point$x <= 8, logical(1))))
})

test_that("a 100k-cell SQLite index returns every visible cell in bounded pages", {
  fixture <- partitioned_cell_fixture(rows = 1L)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  source_path <- file.path(fixture$root, "partitions.json")
  source <- jsonlite::fromJSON(source_path, simplifyVector = FALSE)
  source$cell_count <- 100000L
  jsonlite::write_json(source, source_path, auto_unbox = TRUE, null = "null", digits = 17)
  source_sha <- wsi_experiment_sha256(source_path)
  pointer_path <- file.path(fixture$root, "features.json")
  pointer <- jsonlite::fromJSON(pointer_path, simplifyVector = FALSE)
  pointer$row_count <- 100000L
  pointer$partition_manifest_sha256 <- source_sha
  jsonlite::write_json(pointer, pointer_path, auto_unbox = TRUE, null = "null", digits = 17)
  pointer_sha <- wsi_experiment_sha256(pointer_path)
  index_path <- file.path(fixture$root, "cells.sqlite")
  connection <- DBI::dbConnect(RSQLite::SQLite(), index_path)
  DBI::dbExecute(connection, "CREATE TEMP TABLE added AS WITH RECURSIVE seq(i) AS (SELECT 2 UNION ALL SELECT i+1 FROM seq WHERE i<100000) SELECT i, CAST((i-1)%64 AS REAL)+0.25 AS x, CAST(((i-1)/64)%64 AS REAL)+0.25 AS y FROM seq")
  DBI::dbExecute(connection, paste0("INSERT INTO cells SELECT i,printf('cell-%06d',i),CAST(i AS TEXT),x,y,x-0.1,y-0.1,x+0.1,y+0.1,",
    "json_object('cell_id',printf('cell-%06d',i),'source_id',CAST(i AS TEXT),'x',x,'y',y,",
    "'predictions',json_object('synthetic',json_object('status','predicted','class_id',1,'label','B',",
    "'probability',0.75,'probabilities',json_array(0.25,0.75)))) FROM added"))
  DBI::dbExecute(connection, "INSERT INTO predictions SELECT i,'synthetic','predicted',1,'B',0.75 FROM added")
  DBI::dbExecute(connection, "INSERT INTO features SELECT i,'[0.1,0.2]' FROM added")
  DBI::dbExecute(connection, "INSERT INTO bbox_rtree SELECT i,x-0.1,x+0.1,y-0.1,y+0.1 FROM added")
  for (key in c("classification", "classification_header", "feature_set", "feature_csv_sha256", "cell_count")) {
    value <- jsonlite::fromJSON(DBI::dbGetQuery(connection,
      "SELECT value FROM metadata WHERE key=?", params = list(key))$value[[1L]], simplifyVector = FALSE)
    if (key == "classification") value$sha256 <- source_sha
    if (key == "classification_header") {
      value$feature_set$row_count <- 100000L
      value$feature_set$sha256 <- pointer_sha
    }
    if (key == "feature_set") {
      value$row_count <- 100000L
      value$sha256 <- pointer_sha
    }
    if (key == "feature_csv_sha256") value <- pointer_sha
    if (key == "cell_count") value <- 100000L
    DBI::dbExecute(connection, "UPDATE metadata SET value=? WHERE key=?", params = list(
      as.character(jsonlite::toJSON(value, auto_unbox = TRUE, null = "null", digits = 17)), key))
  }
  DBI::dbDisconnect(connection)
  project <- jsonlite::fromJSON(fixture$path, simplifyVector = FALSE)
  project$tissues[[1L]]$feature_sets[[1L]]$row_count <- 100000L
  project$tissues[[1L]]$feature_sets[[1L]]$sha256 <- pointer_sha
  project$tissues[[1L]]$cell_classification$sha256 <- source_sha
  project$tissues[[1L]]$cell_classification$feature_manifest$sha256 <- source_sha
  project$tissues[[1L]]$cell_classification$index$sha256 <- wsi_experiment_sha256(index_path)
  jsonlite::write_json(project, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  experiment <- wsi_read_experiment(fixture$path)
  catalog <- wsi_experiment_cell_catalog(experiment)[[1L]]
  expect_equal(catalog$cell_count, 100000L)
  context <- wsi_experiment_prediction_context(experiment)
  bounds <- list(xmin = 0, ymin = 0, xmax = 1, ymax = 64)
  cursor <- NULL
  ids <- character()
  repeat {
    page <- wsi_prediction_response(context, new.env(), c(list(action = "experiment_cell_index",
      mode = "display_points", tissue_id = "t1", model_id = "synthetic",
      bbox = bounds, display_limit = 500L), if (is.null(cursor)) list() else list(cursor = cursor)))$cell_index
    expect_lte(length(page$points), 500L)
    expect_equal(page$cell_count, 100000L)
    expect_true(all(vapply(page$points, function(point) point$x <= 1, logical(1))))
    ids <- c(ids, vapply(page$points, `[[`, character(1), "cell_id"))
    cursor <- page$next_cursor
    if (is.null(cursor)) break
  }
  expect_equal(length(ids), 1562L)
  expect_equal(length(unique(ids)), length(ids))
  expect_equal(page$total, length(ids))
})

test_that("actual CellPhenotyper v3 matrix project reopens and decodes a reviewed row", {
  path <- Sys.getenv("WSITOOLS_CELLVIT_V3_EXPERIMENT")
  skip_if(!nzchar(path) || !file.exists(path), "No exported CellPhenotyper v3 fixture was supplied")
  experiment <- wsi_read_experiment(path)
  catalog <- wsi_experiment_cell_catalog(experiment)[[1L]]
  expect_equal(catalog$index_metadata$schema_version, 3L)
  result <- wsi_prediction_response(wsi_experiment_prediction_context(experiment), new.env(),
    list(action = "experiment_cell_index", mode = "display_points",
      tissue_id = experiment$tissues[[1L]]$tissue_id, model_id = catalog$models[[1L]]$model_id,
      bbox = list(xmin = 0, ymin = 0, xmax = experiment$tissues[[1L]]$image$width,
        ymax = experiment$tissues[[1L]]$image$height)))$cell_index
  expect_equal(length(result$points), 1L)
  features <- wsi_experiment_index_features(experiment, catalog$feature_set_id,
    result$points[[1L]]$id)
  expect_identical(unname(features$x[1L, 1L]), 0.5)
})

test_that("verified 0.5-um KODAMA attaches to a 0.25-um cell project by exact affine", {
  fixture <- partitioned_cell_fixture(rows = 12L)
  on.exit(unlink(fixture$root, recursive = TRUE), add = TRUE)
  tissue_path <- file.path(fixture$root, "tissue.ppm")
  writeBin(c(charToRaw("P6\n32 32\n255\n"), as.raw(rep(96, 32 * 32 * 3))), tissue_path)
  project <- jsonlite::fromJSON(fixture$path, simplifyVector = FALSE)
  cell_frame <- project$tissues[[1L]]$image
  tissue_frame <- list(path = "tissue.ppm", sha256 = wsi_experiment_sha256(tissue_path),
    width = 32L, height = 32L, mpp_x = .5, mpp_y = .5,
    origin_x = 0, origin_y = 0, coordinate_space = "crop-native-pixels",
    frame_id = "synthetic-tissue-frame",
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels")
  sidecar <- list(format = "cellphenotyper-dual-mpp-registration-v1", verified = TRUE,
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels",
    source_scene_sha256 = strrep("c", 64), image = tissue_frame,
    cell_full_image = cell_frame, cell_project_image = cell_frame,
    cell_crop_bbox_in_cell_full_xyxy = list(0L, 0L, 64L, 64L),
    tissue_to_cell_full_affine = list(list(2, 0, 0), list(0, 2, 0), list(0, 0, 1)),
    tissue_to_cell_project_affine = list(list(2, 0, 0), list(0, 2, 0), list(0, 0, 1)),
    cell_project_to_tissue_affine = list(list(.5, 0, 0), list(0, .5, 0), list(0, 0, 1)))
  registration_path <- file.path(fixture$root, "tissue_registration.json")
  jsonlite::write_json(sidecar, registration_path, auto_unbox = TRUE, null = "null", digits = 17)
  kodama_path <- file.path(fixture$root, "kodama.csv")
  utils::write.csv(data.frame(tile_id = c("a", "b"), native_x = c(8, 20),
    native_y = c(10, 20), kodama_1 = c(.2, .9), kodama_2 = c(.4, .1)),
    kodama_path, row.names = FALSE)
  project$tissues[[1L]]$registered_tissue <- list(
    registration = list(path = basename(registration_path), sha256 = wsi_experiment_sha256(registration_path)),
    image = tissue_frame,
    assets = list(list(asset_id = "kodama-v2", role = "kodama_map", format = "csv",
      path = basename(kodama_path), sha256 = wsi_experiment_sha256(kodama_path),
      image_sha256 = tissue_frame$sha256, frame_id = tissue_frame$frame_id,
      id_column = "tile_id", x_column = "native_x", y_column = "native_y",
      dimension_columns = list("kodama_1", "kodama_2"), row_count = 2L)))
  jsonlite::write_json(project, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  experiment <- wsi_read_experiment(fixture$path)
  expect_equal(experiment$tissues[[1L]]$image$sha256, cell_frame$sha256)
  opened_image <- wsi_experiment_images(experiment)[[1L]]
  expect_equal(opened_image$frame_id, cell_frame$frame_id)
  expect_equal(opened_image$path, normalizePath(file.path(fixture$root, "image.ppm")))
  view <- wsi_experiment_registered_tissue_viewport(experiment, "t1", "kodama-v2",
    list(xmin = 0, ymin = 0, xmax = 35, ymax = 35), limit = 1L)
  expect_equal(view$total, 1L)
  expect_equal(view$points[[1L]]$x, 16)
  expect_equal(view$points[[1L]]$y, 20)
  expect_equal(view$source_frame_id, tissue_frame$frame_id)
  expect_equal(view$coordinate_frame, cell_frame$frame_id)
  whole <- wsi_experiment_registered_tissue_viewport(experiment, "t1", "kodama-v2",
    list(xmin = 0, ymin = 0, xmax = 64, ymax = 64), limit = 1L)
  expect_equal(whole$total, 2L)
  expect_true(whole$truncated)
  project$tissues[[1L]]$registered_tissue$registration$sha256 <- strrep("0", 64)
  jsonlite::write_json(project, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  expect_error(wsi_read_experiment(fixture$path), "SHA-256 mismatch")
  sidecar$tissue_to_cell_full_affine[[1L]][[1L]] <- 2.1
  jsonlite::write_json(sidecar, registration_path, auto_unbox = TRUE, null = "null", digits = 17)
  project$tissues[[1L]]$registered_tissue$registration$sha256 <- wsi_experiment_sha256(registration_path)
  jsonlite::write_json(project, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  expect_error(wsi_read_experiment(fixture$path), "tissue_to_cell_full_affine")
  project$tissues[[1L]]$registered_tissue$registration <- NULL
  jsonlite::write_json(project, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  expect_error(wsi_read_experiment(fixture$path), "requires an exact sidecar")
})
