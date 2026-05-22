test_that("artifact detection flags bright, dark, pen, fold, and bubble-like tiles", {
  white <- array(1, dim = c(32, 32, 3))
  dark <- array(0.02, dim = c(32, 32, 3))
  pen <- array(1, dim = c(32, 32, 3))
  pen[, 10:18, 1] <- 0.05
  pen[, 10:18, 2] <- 0.10
  pen[, 10:18, 3] <- 0.90
  fold <- array(0.05, dim = c(32, 32, 3))
  fold[, , 1] <- 0.25
  fold[, , 2] <- 0.04
  fold[, , 3] <- 0.02

  expect_true(wsi_detect_artifacts(white)$artifact_too_bright)
  expect_true(wsi_detect_artifacts(white)$artifact_bubble)
  expect_true(wsi_detect_artifacts(dark)$artifact_too_dark)
  expect_true(wsi_detect_artifacts(pen)$artifact_pen)
  expect_true(wsi_detect_artifacts(fold)$artifact_fold)
})

test_that("artifact detection distinguishes textured tiles from blur", {
  checker <- matrix(rep(c(0, 1), 32 * 32 / 2), nrow = 32)
  checker <- array(rep(checker, 3), dim = c(32, 32, 3))
  flat <- array(0.5, dim = c(32, 32, 3))

  textured <- wsi_detect_artifacts(checker)
  blurry <- wsi_detect_artifacts(flat)

  expect_false(textured$artifact_blur)
  expect_true(blurry$artifact_blur)
  expect_gt(textured$artifact_blur_score, blurry$artifact_blur_score)
})

test_that("artifact filtering appends columns and can drop flagged mock tiles", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 512, levels = c(1))
  grid <- extract_tiles(slide, tile_size = 256, stride = 256, save_images = FALSE)

  flagged <- wsi_flag_artifacts(slide, grid, action = "flag")
  expect_equal(nrow(flagged), nrow(grid))
  expect_true(all(c("artifact_flag", "artifact_blur", "artifact_too_bright") %in% names(flagged)))
  expect_true(any(flagged$artifact_flag))

  dropped <- wsi_flag_artifacts(slide, grid, action = "drop")
  expect_equal(nrow(dropped), 0)
  expect_true(all(c("artifact_flag", "artifact_blur", "artifact_too_bright") %in% names(dropped)))
})

test_that("extract_tiles can return artifact quality columns", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 512, levels = c(1))

  manifest <- extract_tiles(
    slide,
    tile_size = 256,
    stride = 256,
    save_images = FALSE,
    artifact_filter = TRUE,
    artifact_action = "flag"
  )

  expect_s3_class(manifest, "wsi_tile_manifest")
  expect_equal(nrow(manifest), 4)
  expect_true(all(c("artifact_score", "artifact_flag", "artifact_read_error") %in% names(manifest)))
  expect_true(any(manifest$artifact_flag))
})
