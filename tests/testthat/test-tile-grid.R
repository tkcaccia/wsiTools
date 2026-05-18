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
