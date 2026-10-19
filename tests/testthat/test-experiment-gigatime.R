# Tiny unsigned label/marker TIFFs written with base R; no inference or GPU.
gigatime_test_tiff <- function(path, markers = c("CD3", "CD8"), width = 64L, height = 48L, values = c(1000L, 2000L), floating = FALSE) {
  pages <- length(markers); entries <- 12L; stride <- 2L + 12L * entries + 4L
  xml <- paste0('<?xml version="1.0"?><OME xmlns="http://www.openmicroscopy.org/Schemas/OME/2016-06"><Image ID="Image:0"><Pixels ID="Pixels:0" DimensionOrder="XYCZT" Type="uint16" SizeX="', width,
    '" SizeY="', height, '" SizeC="', pages, '" SizeZ="1" SizeT="1">',
    paste0('<Channel ID="Channel:', seq_len(pages) - 1L, '" Name="', markers, '" SamplesPerPixel="1"/>', collapse = ""),
    '<TiffData IFD="0" PlaneCount="', pages, '"/></Pixels></Image></OME>')
  text <- c(charToRaw(xml), as.raw(0)); text_offset <- 8L + pages * stride
  if (floating) text <- c(charToRaw(sub('Type="uint16"', 'Type="float"', xml, fixed = TRUE)), as.raw(0))
  pixel_offset <- text_offset + length(text) + length(text) %% 2L
  page_bytes <- width * height * if (floating) 4L else 2L
  con <- file(path, "w+b"); on.exit(close(con), add = TRUE)
  writeBin(raw(pixel_offset + pages * page_bytes), con)
  seek(con, 0); writeBin(charToRaw("II"), con); writeBin(42L, con, size = 2L, endian = "little"); writeBin(8L, con, size = 4L, endian = "little")
  tag <- function(id, type, count, value) {
    writeBin(as.integer(c(id, type)), con, size = 2L, endian = "little")
    writeBin(as.integer(count), con, size = 4L, endian = "little")
    if (type == 3L && count == 1L) writeBin(as.integer(c(value, 0L)), con, size = 2L, endian = "little")
    else writeBin(as.integer(value), con, size = 4L, endian = "little")
  }
  for (page in seq_len(pages)) {
    seek(con, 8L + (page - 1L) * stride)
    writeBin(entries, con, size = 2L, endian = "little")
    tag(256, 4, 1, width); tag(257, 4, 1, height); tag(258, 3, 1, if (floating) 32 else 16)
    tag(259, 3, 1, 1); tag(262, 3, 1, 1); tag(270, 2, length(text), text_offset)
    tag(273, 4, 1, pixel_offset + (page - 1L) * page_bytes); tag(277, 3, 1, 1)
    tag(278, 4, 1, height); tag(279, 4, 1, page_bytes); tag(284, 3, 1, 1); tag(339, 3, 1, if (floating) 3 else 1)
    writeBin(if (page < pages) 8L + page * stride else 0L, con, size = 4L, endian = "little")
    seek(con, pixel_offset + (page - 1L) * page_bytes)
    pixels <- rep(values[[page]], length.out = width * height)
    writeBin(if (floating) as.numeric(pixels) else as.integer(pixels), con, size = if (floating) 4L else 2L, endian = "little")
  }
  seek(con, text_offset); writeBin(text, con)
  invisible(path)
}

gigatime_experiment_fixture <- function(tissues = 2L) {
  root <- tempfile("gigatime-experiment-"); dir.create(root); root <- normalizePath(root, winslash = "/")
  image <- file.path(root, "image.ppm")
  writeBin(c(charToRaw("P6\n64 48\n255\n"), as.raw(rep(128L, 64L * 48L * 3L))), image)
  marker <- gigatime_test_tiff(file.path(root, "markers.ome.tif"))
  mask <- gigatime_test_tiff(file.path(root, "cells.tif"), markers = "labels", values = 1L)
  csv <- file.path(root, "scores.csv")
  utils::write.csv(data.frame(label_id = c(3L, 1L, 2L), x = c(50, 10, 30), y = 20,
    mask_name = "cells", CD3_mean = c(30, 10, 20), CD8_sum = c(3000, 1000, 2000), ignored_numeric_id = 100:102), csv, row.names = FALSE)
  manifest <- list(schema = "cellphenotyper-experiment", schema_version = 1L,
    experiment_id = "gigatime-fixture", name = "Synthetic GigaTIME contracts", settings = structure(list(), names = character()),
    provenance = structure(list(), names = character()), tissues = list())
  for (i in seq_len(tissues)) {
    identity <- list(frame_id = paste0("frame", i), image_sha256 = wsi_experiment_sha256(image))
    artifacts <- list(
      c(list(artifact_id = "markers", role = "gigatime_marker_image", path = basename(marker), sha256 = wsi_experiment_sha256(marker),
        format = "ome-tiff", score_semantics = "uncalibrated_virtual_marker_score",
        channels = list(list(marker = "CD3", page = 0L), list(marker = "CD8", page = 1L)),
        extent = list(x = 0, y = 0, width = 64, height = 48)), identity),
      c(list(artifact_id = "mask", role = "cell_segmentation_mask", path = basename(mask), sha256 = wsi_experiment_sha256(mask),
        format = "tiff", mask_name = "cells", segmentation_unit = "cell", width = 64L, height = 48L), identity),
      c(list(artifact_id = "scores", role = "gigatime_cell_scores", path = basename(csv), sha256 = wsi_experiment_sha256(csv),
        format = "csv", row_count = 3L, id_column = "label_id", x_column = "x", y_column = "y",
        marker_columns = list(list(marker = "CD3", column = "CD3_mean"), list(marker = "CD8", column = "CD8_sum")),
        mask_name = "cells", mask_artifact_id = "mask", source_artifact_id = "markers",
        source_coordinate_convention = "native-image-pixel-centres", source_to_image_affine = list(list(1, 0, 0), list(0, 1, 0), list(0, 0, 1)),
        score_semantics = "uncalibrated_virtual_marker_score"), identity))
    manifest$tissues[[i]] <- list(tissue_id = paste0("t", i), name = paste("Tissue", i),
      image = list(path = basename(image), sha256 = wsi_experiment_sha256(image), width = 64L, height = 48L,
        mpp_x = .5, mpp_y = .5, frame_id = identity$frame_id, coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"),
      feature_sets = list(), artifacts = artifacts)
  }
  path <- file.path(root, "experiment.cellphenotyper.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, null = "null", digits = 17)
  list(root = root, path = path, manifest = manifest, csv = csv)
}

test_that("typed GigaTIME creates independent tissue-scoped image and cell marker layers", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture()
  experiment <- wsi_read_experiment(fixture$path)
  view <- wsi_experiment_gigatime_viewer(experiment)
  expect_length(view$channel_sources, 4L); expect_length(view$layers, 4L)
  expect_length(unique(vapply(view$channel_sources, `[[`, character(1), "id")), 4L)
  expect_length(unique(vapply(view$layers, `[[`, character(1), "id")), 4L)
  expect_true(all(!vapply(view$channel_sources, `[[`, logical(1), "visible")))
  expect_true(all(!vapply(view$layers, `[[`, logical(1), "visible")))
  expect_equal(view$channel_sources[[1]]$gain, 1)
  expect_equal(view$channel_sources[[1]]$metadata$gigatime$display_max, 65535)
  expect_equal(view$channel_sources[[2]]$page, 1)
  expect_identical(view$channel_sources[[1]]$metadata$target_project_item_id, "t1")
  expect_identical(view$layers[[1]]$metadata$gigatime$source_artifact_id, "markers")
  expect_identical(view$layers[[1]]$metadata$gigatime$source_column, "CD3_mean")
  expect_identical(view$layers[[2]]$metadata$gigatime$source_column, "CD8_sum")
  expect_identical(view$layers[[1]]$items[[1]]$original_id, "3")
  expect_equal(vapply(view$layers[[2]]$items, `[[`, numeric(1), "value"), c(3000, 1000, 2000))
  expect_identical(view$layers[[1]]$items[[1]]$image_id, "t1")
  expect_identical(view$layers[[3]]$items[[1]]$image_id, "t2")
  expect_false(identical(view$layers[[1]]$items[[1]]$id, view$layers[[3]]$items[[1]]$id))
  expect_equal(view$layers[[1]]$items[[1]]$project_image_index, 0)
  expect_identical(names(view$layers[[1]]$items[[1]]$metadata$scores), "CD3")
  expect_length(wsi_experiment_review_context(experiment)$observations, 0L)
  expect_length(wsi_experiment_catalog(experiment), 0L)
})

test_that("GigaTIME float and uint16 tiles retain normalized contrast without changing user gain", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  for (floating in c(FALSE, TRUE)) {
    root <- tempfile("gigatime-scalar-"); dir.create(root)
    values <- if (floating) c(0, .25, .5, .75, 1) else c(0, 16384, 32768, 49151, 65535)
    input <- gigatime_test_tiff(file.path(root, "scalar.ome.tif"), markers = "CD3", width = 5L, height = 1L,
      values = list(values), floating = floating)
    source <- wsi_dynamic_image_tile_source(input, visible = FALSE, colour = "#ffffff", gain = 1,
      metadata = list(gigatime = list(marker = "CD3", mode = "image", display_max = if (floating) 1 else 65535)))
    region <- wsi_dynamic_tile_region(source, source$max_level, 0L, 0L)
    alpha <- function(lo = 0, hi = 1) {
      output <- file.path(root, "tile.png"); channel <- file.path(root, "alpha.v"); raw <- file.path(root, "alpha.raw")
      wsi_dynamic_image_region_to_file(source, region, output, settings = list(contrast_min = lo, contrast_max = hi))
      wsi_run_command("vips", c("extract_band", output, channel, "3"))
      wsi_run_command("vips", c("rawsave", channel, raw))
      as.integer(readBin(raw, "raw", n = 5L))
    }
    expect_lte(max(abs(alpha() - c(0, 64, 128, 191, 255))), 1)
    expect_lte(max(abs(alpha(.25, .75) - c(0, 0, 128, 255, 255))), 1)
    expect_equal(source$gain, 1)
  }
})

test_that("experiment viewer keeps manual sources and installs cell layers per tissue", {
  skip_if_not_installed("openssl"); skip_if_not_installed("httpuv"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture()
  captured <- NULL
  result <- with_mocked_bindings(wsi_viewer_experiment(fixture$path, open = FALSE, wait = FALSE,
    layers = list(list(id = "manual-layer", type = "vector", items = list())),
    channel_sources = list(id = "manual-channel", type = "dynamic")),
    wsi_viewer_live = function(...) { captured <<- list(...); list() }, .package = "wsiTools")
  on.exit(wsi_close(captured$slide), add = TRUE)
  expect_length(captured$layers, 1L)
  expect_identical(captured$layers[[1]]$id, "manual-layer")
  expect_length(captured$channel_sources, 5L)
  expect_identical(captured$channel_sources[[1]]$id, "manual-channel")
  expect_length(captured$project_images[[1]]$layers, 2L)
  expect_length(captured$project_images[[2]]$layers, 2L)
  expect_identical(captured$project_images[[1]]$layers[[1]]$metadata$gigatime$tissue_id, "t1")
  expect_identical(captured$project_images[[2]]$layers[[1]]$metadata$gigatime$tissue_id, "t2")
  expect_s3_class(result$experiment, "wsi_experiment")
})

test_that("GigaTIME native values and explicit raw-pixel centre affine are preserved", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture(tissues = 1L)
  manifest <- fixture$manifest
  manifest$tissues[[1]]$artifacts[[3]]$source_coordinate_convention <- "zero-based-pixel-index"
  manifest$tissues[[1]]$artifacts[[3]]$source_to_image_affine <- list(list(1, 0, .5), list(0, 1, .5), list(0, 0, 1))
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  view <- wsi_experiment_gigatime_viewer(wsi_read_experiment(fixture$path))
  expect_equal(view$layers[[1]]$items[[1]]$x, 50.5)
  expect_equal(view$layers[[1]]$items[[1]]$y, 20.5)
  expect_equal(view$layers[[2]]$metadata$gigatime$min, 1000)
  expect_equal(view$layers[[2]]$metadata$gigatime$max, 3000)
  manifest$tissues[[1]]$artifacts[[3]]$source_to_image_affine <- list(list(1, 0, 0), list(0, 1, 0), list(0, 0, 1))
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  expect_error(wsi_read_experiment(fixture$path), "pixel-centre shift")
})

test_that("typed GigaTIME rejects altered identities, channels, masks and links", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture(tissues = 1L)
  for (failure in c("hash", "frame", "page", "marker", "unit", "mask_size", "source", "mask", "semantics", "columns", "affine")) {
    manifest <- fixture$manifest; artifacts <- manifest$tissues[[1]]$artifacts
    if (failure == "hash") artifacts[[1]]$sha256 <- paste(rep("f", 64), collapse = "")
    if (failure == "frame") artifacts[[3]]$frame_id <- "foreign"
    if (failure == "page") artifacts[[1]]$channels[[2]]$page <- 2L
    if (failure == "marker") artifacts[[1]]$channels[[2]]$marker <- "Different"
    if (failure == "unit") artifacts[[2]]$segmentation_unit <- "grid"
    if (failure == "mask_size") artifacts[[2]]$width <- 32L
    if (failure == "source") artifacts[[3]]$source_artifact_id <- "missing"
    if (failure == "mask") artifacts[[3]]$mask_name <- "nuclei"
    if (failure == "semantics") artifacts[[3]]$score_semantics <- "measured_abundance"
    if (failure == "columns") artifacts[[3]]$marker_columns[[1]]$column <- "label_id"
    if (failure == "affine") artifacts[[3]]$source_to_image_affine[[3]] <- list(0, 0, 0)
    manifest$tissues[[1]]$artifacts <- artifacts
    jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
    expect_error(wsi_read_experiment(fixture$path), info = failure)
  }
})

test_that("bad per-cell rows fail explicitly without silent dropping or score normalization", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture(tissues = 1L)
  original <- utils::read.csv(fixture$csv)
  for (failure in c("duplicate", "zero", "overflow", "outside", "nonfinite", "negative", "mask_name", "rows")) {
    manifest <- fixture$manifest; table <- original
    if (failure == "duplicate") table$label_id[[2]] <- table$label_id[[1]]
    if (failure == "zero") table$label_id[[2]] <- 0
    if (failure == "overflow") table$label_id[[2]] <- 65536
    if (failure == "outside") table$x[[2]] <- 65
    if (failure == "nonfinite") table$CD3_mean[[2]] <- Inf
    if (failure == "negative") table$CD3_mean[[2]] <- -1
    if (failure == "mask_name") table$mask_name[[2]] <- "nuclei"
    if (failure == "rows") table <- table[-1, ]
    utils::write.csv(table, fixture$csv, row.names = FALSE)
    manifest$tissues[[1]]$artifacts[[3]]$sha256 <- wsi_experiment_sha256(fixture$csv)
    jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
    expect_error(wsi_experiment_gigatime_viewer(wsi_read_experiment(fixture$path)), info = failure)
  }
})

test_that("GigaTIME display limits are explicit and image-only experiments work", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture(tissues = 1L)
  experiment <- wsi_read_experiment(fixture$path)
  withr::local_options(wsiTools.experiment_max_marker_rows = 2L)
  expect_error(wsi_experiment_gigatime_viewer(experiment), "No rows were sampled")
  withr::local_options(wsiTools.experiment_max_marker_rows = 10000L, wsiTools.experiment_max_marker_items = 5L)
  expect_error(wsi_experiment_gigatime_viewer(experiment), "No cells were sampled")
  fixture$manifest$tissues[[1]]$artifacts <- fixture$manifest$tissues[[1]]$artifacts[1]
  jsonlite::write_json(fixture$manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  view <- wsi_experiment_gigatime_viewer(wsi_read_experiment(fixture$path))
  expect_length(view$channel_sources, 2L); expect_length(view$layers, 0L)
})

test_that("float display ranges must be explicit and cropped image scope is validated", {
  skip_if_not_installed("openssl"); skip_if_not(wsi_has_vips())
  fixture <- gigatime_experiment_fixture(tissues = 1L)
  marker <- file.path(fixture$root, "markers.ome.tif")
  gigatime_test_tiff(marker, values = c(.25, .75), floating = TRUE)
  manifest <- fixture$manifest
  manifest$tissues[[1]]$artifacts[[1]]$sha256 <- wsi_experiment_sha256(marker)
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  expect_error(wsi_read_experiment(fixture$path), "display_max")
  for (i in 1:2) manifest$tissues[[1]]$artifacts[[1]]$channels[[i]]$display_max <- 1
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  view <- wsi_experiment_gigatime_viewer(wsi_read_experiment(fixture$path))
  expect_equal(view$channel_sources[[1]]$metadata$gigatime$display_max, 1)
  expect_equal(view$channel_sources[[1]]$gain, 1)
  manifest$tissues[[1]]$artifacts[[1]]$extent$width <- 32
  jsonlite::write_json(manifest, fixture$path, auto_unbox = TRUE, null = "null", digits = 17)
  expect_error(wsi_experiment_gigatime_viewer(wsi_read_experiment(fixture$path)), "linked marker-image extent")
})
