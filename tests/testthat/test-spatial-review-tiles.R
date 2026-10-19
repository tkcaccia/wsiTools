review_geometry_fixture <- function(coordinates, type = "Polygon") {
  path <- tempfile(fileext = ".geojson")
  on.exit(unlink(path), add = TRUE)
  jsonlite::write_json(list(type = "FeatureCollection", features = list(list(type = "Feature", id = "geometry",
    properties = list(name = "Synthetic geometry"), geometry = list(type = type, coordinates = coordinates)))),
    path, auto_unbox = TRUE)
  read_geojson(path)
}

spatial_review_context_fixture <- function() {
  images <- lapply(c("t1", "t2", "t3"), function(id) list(tissue_id = id,
    frame_id = paste0(id, ":image"), sha256 = paste(rep(substr(id, 2L, 2L), 64L), collapse = ""),
    width = 300L, height = 240L, mpp_x = .5, mpp_y = .5,
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"))
  observations <- unlist(lapply(images, function(image) lapply(seq_len(4L), function(i) list(
    id = paste("study", image$tissue_id, "rna", paste0("BC", i), sep = "::"),
    original_id = paste0("BC", i), dataset_id = "rna", tissue_id = image$tissue_id,
    frame_id = image$frame_id, observation_unit = "spot", x = c(10, 20, 130, 290)[[i]],
    y = 20, cluster_values = list(rna_cluster = if (i <= 2L) "alpha" else "beta")))), recursive = FALSE)
  wsi_review_context(images, observations, context_id = "spatial-review-test", provenance = list(synthetic = TRUE))
}

test_that("rectangular ROI coverage excludes holes and concavities missed by corners", {
  outer <- list(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0))
  hole <- list(c(20, 20), c(24, 20), c(24, 24), c(20, 24), c(20, 20))
  roi <- review_geometry_fixture(list(outer, hole))
  boxes <- data.frame(x0 = c(0, 10, 30, -1), y0 = c(0, 10, 30, 0),
    x1 = c(10, 30, 40, 9), y1 = c(10, 30, 40, 10))
  expect_identical(wsi_rectangular_footprints_in_roi(boxes, roi), c(TRUE, FALSE, TRUE, FALSE))
  # All corners and the centre of 10..40 remain inside this U-shaped notch.
  notch <- list(c(0, 0), c(100, 0), c(100, 100), c(30, 100), c(30, 20),
    c(28, 20), c(28, 100), c(0, 100), c(0, 0))
  roi <- review_geometry_fixture(list(notch))
  expect_false(wsi_rectangular_footprints_in_roi(data.frame(x0 = 10, y0 = 10, x1 = 40, y1 = 40), roi))
  expect_true(wsi_rectangular_footprints_in_roi(data.frame(x0 = 0, y0 = 0, x1 = 10, y1 = 10), roi))
  multi <- review_geometry_fixture(list(list(outer), list(lapply(outer, function(point) point + c(200, 0)))), "MultiPolygon")
  expect_identical(wsi_rectangular_footprints_in_roi(data.frame(x0 = c(1, 201), y0 = c(1, 1), x1 = c(9, 209), y1 = c(9, 9)), multi), c(TRUE, TRUE))
})

test_that("selected spot grids never fall back to all spots", {
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 100, levels = 1)
  linked <- structure(list(slide = slide, spots = data.frame(barcode = c("BC1", "BC2"), x = c(20, 60), y = c(20, 60))), class = "wsi_seurat_spatial")
  all <- wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 10)
  expect_equal(nrow(all), 2L)
  selected <- wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 10, selected_ids = "BC2")
  expect_identical(selected$spot_id, "BC2")
  expect_equal(nrow(wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 10, selection_scope = "selected")), 0L)
  expect_equal(nrow(wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 10, selected_ids = character())), 0L)
  expect_error(wsiTools:::wsi_spatial_tile_grid(linked, selected_ids = "unknown"), "unknown")
  outer <- list(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0))
  hole <- list(c(18, 18), c(19, 18), c(19, 19), c(18, 19), c(18, 18))
  roi <- review_geometry_fixture(list(outer, hole))
  clipped <- wsiTools:::wsi_spatial_tile_grid(linked, tile_size = 10, roi = roi)
  expect_identical(clipped$spot_id, "BC2")
})

test_that("canonical work tiles deduplicate centres across repeated barcodes and datasets", {
  skip_if_not_installed("openssl")
  context <- spatial_review_context_fixture()
  ids <- wsi_review_select(context, cluster_field = "rna_cluster", cluster_values = "alpha")
  plan <- wsi_review_tile_plan(context, ids)
  expect_equal(plan$row_count, 6L)
  expect_equal(plan$work_count, 3L)
  expect_true(all(vapply(plan$tiles, function(tile) length(tile$observation_ids) == 2L, logical(1))))
  expect_identical(plan$membership_rule, "observation_centres")
  expect_equal(wsi_review_tile_plan(context, character())$work_count, 0L)
  expect_error(wsi_review_tile_plan(context, "BC1"), "unknown")
  expect_error(wsi_review_tile_plan(context, ids, max_tiles = 2), "max_tiles")
  same <- wsi_review_tile_plan(context, rev(ids))
  expect_identical(plan, same)
  physical <- wsi_review_tile_plan(context, ids, tile_size = 112, units = "um")
  expect_equal(physical$tiles[[1L]]$width, 224)
  expect_equal(physical$tiles[[1L]]$height, 224)
})

test_that("work tile edge policies and anisotropic calibration stay explicit", {
  skip_if_not_installed("openssl")
  context <- spatial_review_context_fixture()
  id <- context$observations[[4L]]$id
  expect_equal(wsi_review_tile_plan(context, id, bounds = "drop")$excluded_ids, as.list(id))
  trimmed <- wsi_review_tile_plan(context, id, bounds = "trim", context_px = 5)
  expect_equal(trimmed$tiles[[1L]]$x, 224)
  expect_equal(trimmed$tiles[[1L]]$width, 76)
  expect_equal(trimmed$tiles[[1L]]$read_x, 219)
  expect_equal(trimmed$tiles[[1L]]$read_width, 81)
  expect_error(wsi_review_tile_plan(context, id, bounds = "error"), "incomplete")
  image <- context$images[[1L]]
  image$mpp_y <- 1
  changed <- wsi_review_context(list(image), context$observations[1:4], context_id = "anisotropic")
  physical <- wsi_review_tile_plan(changed, context$observations[[1L]]$id, tile_size = 112, units = "um")
  expect_equal(physical$tiles[[1L]]$width, 224)
  expect_equal(physical$tiles[[1L]]$height, 112)
  image$mpp_x <- image$mpp_y <- NULL
  image$calibration_status <- "unavailable"
  changed <- wsi_review_context(list(image), context$observations[1:4], context_id = "unavailable")
  expect_error(wsi_review_tile_plan(changed, context$observations[[1L]]$id, units = "um"), "calibration")
})

test_that("review export validates selected IDs, image identity and preview coordinates", {
  skip_if_not_installed("openssl")
  context <- spatial_review_context_fixture()
  id <- context$observations[[1L]]$id
  grid <- data.frame(spot_id = c("BC1", "BC2"), x = c(5, 15), y = 15,
    width = 10, height = 10, downsample = 1, level = 0)
  filter <- function(rows = grid, ids = id, signature = context$source_signature, ...)
    wsiTools:::wsi_spatial_tile_review_filter(rows, context, ids, tissue_id = "t1", source_signature = signature, ...)
  selected <- filter()
  expect_equal(nrow(selected), 1L)
  expect_identical(selected$observation_id, id)
  expect_identical(selected$tissue_id, "t1")
  expect_error(filter(ids = character()), "No observations")
  expect_error(filter(signature = "stale"), "signature")
  expect_error(filter(frame_id = "different"), "hash/frame")
  expect_error(filter(image_sha256 = paste(rep("f", 64), collapse = "")), "hash/frame")
  moved <- grid
  moved$x <- moved$x + 5
  expect_error(filter(moved), "coordinates")
  expect_error(filter(rbind(grid[1L, ], grid[1L, ])), "duplicate")
  pyramid <- grid
  pyramid$level <- 1L
  expect_error(filter(pyramid), "level-0")
})

test_that("tile export rejects empty selections and hole-crossing footprints before writing", {
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 100, levels = 1)
  output <- tempfile("review-export-not-created-")
  on.exit(unlink(output, recursive = TRUE), add = TRUE)
  tile <- list(tile_id = "BC1", spot_id = "BC1", x = 10, y = 10, width = 20, height = 20)
  payload <- list(output_dir = output, tiles = list(tile), selection_scope = "selected", selected_ids = list())
  expect_error(wsiTools:::wsi_spatial_tile_export_response(slide, payload), "No observations are selected")
  expect_false(dir.exists(output))
  payload$selected_ids <- "unknown"
  expect_error(wsiTools:::wsi_spatial_tile_export_response(slide, payload), "do not match")
  expect_false(dir.exists(output))
  outer <- list(c(0, 0), c(100, 0), c(100, 100), c(0, 100), c(0, 0))
  hole <- list(c(20, 20), c(24, 20), c(24, 24), c(20, 24), c(20, 20))
  payload$selected_ids <- "BC1"
  payload$selected_roi <- review_geometry_fixture(list(outer, hole))
  expect_error(wsiTools:::wsi_spatial_tile_export_response(slide, payload), "No complete selected tile footprints")
  expect_false(dir.exists(output))
})
