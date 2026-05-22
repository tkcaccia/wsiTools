test_that("whitespace detector labels bright low-saturation tiles", {
  white <- array(1, dim = c(32, 32, 3))
  pale <- array(0.92, dim = c(32, 32, 3))
  tissue_like <- array(0, dim = c(32, 32, 3))
  tissue_like[, , 1] <- 0.68
  tissue_like[, , 2] <- 0.34
  tissue_like[, , 3] <- 0.58

  white_metrics <- wsi_detect_whitespace(white)
  pale_metrics <- wsi_detect_whitespace(pale)
  tissue_metrics <- wsi_detect_whitespace(tissue_like)

  expect_true(white_metrics$whitespace_flag)
  expect_true(pale_metrics$background_flag)
  expect_equal(white_metrics$whitespace_fraction, 1)
  expect_false(tissue_metrics$whitespace_flag)
  expect_lt(tissue_metrics$whitespace_fraction, 0.1)
})

test_that("whitespace filtering appends columns and can drop mock background tiles", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 512, levels = c(1))
  grid <- wsi_tile_grid(slide, tile_size = 256)

  flagged <- wsi_flag_whitespace(slide, grid, action = "flag")
  expect_equal(nrow(flagged), nrow(grid))
  expect_true(all(c("whitespace_flag", "whitespace_fraction", "background_flag") %in% names(flagged)))
  expect_true(all(flagged$whitespace_flag))

  dropped <- wsi_flag_whitespace(slide, grid, action = "drop")
  expect_equal(nrow(dropped), 0)
  expect_true(all(c("whitespace_flag", "whitespace_fraction", "background_flag") %in% names(dropped)))
})

test_that("extract_tiles can add whitespace labels to manifests", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 512, levels = c(1))

  manifest <- extract_tiles(
    slide,
    tile_size = 256,
    stride = 256,
    save_images = FALSE,
    whitespace_filter = TRUE,
    whitespace_action = "flag"
  )

  expect_s3_class(manifest, "wsi_tile_manifest")
  expect_equal(nrow(manifest), 4)
  expect_true(all(c("whitespace_flag", "whitespace_fraction", "background_fraction") %in% names(manifest)))
  expect_true(all(manifest$whitespace_flag))
})

test_that("whitespace thresholds can be tightened", {
  slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256, levels = c(1))
  grid <- wsi_tile_grid(slide, tile_size = 256)
  options <- wsi_whitespace_options(
    brightness_threshold = 1,
    saturation_threshold = 0,
    whitespace_fraction_threshold = 1,
    brightness_mean_threshold = 1,
    saturation_mean_threshold = 0
  )

  flagged <- wsi_flag_whitespace(slide, grid, options = options, action = "flag")

  expect_false(flagged$whitespace_flag)
  expect_equal(flagged$whitespace_fraction, 0)
  expect_error(wsi_whitespace_options(brightness_threshold = 1.1), "less than or equal to 1")
})
