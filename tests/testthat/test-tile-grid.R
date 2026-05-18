test_that("tile grid returns expected level-0 coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1024, height = 1024, levels = c(1, 4))
  grid <- wsi_tile_grid(slide, tile_size = 512, overlap = 0, level = 0)

  expect_s3_class(grid, "data.frame")
  expect_equal(nrow(grid), 4)
  expect_equal(grid$x, c(0, 512, 0, 512))
  expect_equal(grid$y, c(0, 0, 512, 512))
  expect_equal(grid$width, rep(512L, 4))
  expect_equal(grid$height, rep(512L, 4))
})

test_that("tile grid handles downsampled levels", {
  slide <- wsiTools:::wsi_mock_slide(width = 2048, height = 2048, levels = c(1, 4))
  grid <- wsi_tile_grid(slide, tile_size = 256, overlap = 0, level = 1)

  expect_equal(nrow(grid), 4)
  expect_equal(unique(grid$downsample), 4)
  expect_equal(grid$x, c(0, 1024, 0, 1024))
  expect_equal(grid$width, rep(256L, 4))
})

test_that("tile grid can include partial tiles", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1))
  grid <- wsi_tile_grid(slide, tile_size = 512, include_partial = TRUE)

  expect_equal(nrow(grid), 4)
  expect_true(any(grid$width < 512 | grid$height < 512))
})

test_that("tile grid can be generated from explicit top-left coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1200, height = 900, levels = c(1, 4))
  coords <- data.frame(
    tile_id = c("a", "b"),
    x = c(0, 512),
    y = c(0, 256),
    roi_id = c("roi1", "roi2")
  )

  grid <- wsi_tile_grid_from_coords(slide, coords, tile_size = 256)

  expect_equal(nrow(grid), 2)
  expect_equal(grid$tile_id, c("a", "b"))
  expect_equal(grid$x, c(0, 512))
  expect_equal(grid$y, c(0, 256))
  expect_equal(grid$width, rep(256L, 2))
  expect_equal(grid$height, rep(256L, 2))
  expect_equal(grid$roi_id, c("roi1", "roi2"))
})

test_that("coordinate tile grid supports center anchoring and downsampled levels", {
  slide <- wsiTools:::wsi_mock_slide(width = 2048, height = 2048, levels = c(1, 4))
  coords <- matrix(c(512, 512), ncol = 2)

  grid <- wsi_tile_grid_from_coords(
    slide,
    coords,
    tile_size = 128,
    level = 1,
    anchor = "center"
  )

  expect_equal(grid$x, 256)
  expect_equal(grid$y, 256)
  expect_equal(grid$width, 128L)
  expect_equal(grid$height, 128L)
  expect_equal(grid$downsample, 4)
})

test_that("coordinate tile grid handles boundary policies", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1))
  coords <- data.frame(x = c(900, 1200), y = c(700, 900))

  expect_error(
    wsi_tile_grid_from_coords(slide, coords[1, , drop = FALSE], tile_size = 256),
    "outside slide bounds"
  )

  trimmed <- wsi_tile_grid_from_coords(
    slide,
    coords[1, , drop = FALSE],
    tile_size = 256,
    bounds = "trim"
  )
  expect_equal(trimmed$x, 900)
  expect_equal(trimmed$y, 700)
  expect_true(trimmed$width < 256)
  expect_true(trimmed$height < 256)

  expect_warning(
    dropped <- wsi_tile_grid_from_coords(slide, coords, tile_size = 256, bounds = "drop"),
    "Dropped"
  )
  expect_equal(nrow(dropped), 0)
})

test_that("coordinate tile grids can be read as region arrays", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1))
  coords <- list(c(0, 0), c(100, 100, 32, 48))
  grid <- wsi_tile_grid_from_coords(slide, coords, tile_size = 64)

  patches <- wsi_read_region_grid(slide, grid, output_dir = NULL, format = "array")

  expect_equal(length(patches), 2)
  expect_equal(dim(patches[[1]]), c(64, 64, 4))
  expect_equal(dim(patches[[2]]), c(48, 32, 4))
})

test_that("extract_tiles returns stride-based coordinates without reading pixels", {
  slide <- wsiTools:::wsi_mock_slide(width = 1024, height = 1024, levels = c(1))

  grid <- extract_tiles(slide, tile_size = 256, stride = 128, save_images = FALSE)

  expect_s3_class(grid, "data.frame")
  expect_equal(grid$x[1:3], c(0, 128, 256))
  expect_equal(grid$y[1:3], c(0, 0, 0))
  expect_equal(unique(grid$width), 256L)
  expect_error(
    extract_tiles(slide, tile_size = 128, stride = 256, save_images = FALSE),
    "stride"
  )
})
