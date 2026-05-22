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

test_that("pen mark detector returns colour masks and tissue affected percentage", {
  img <- array(1, dim = c(40, 40, 3))
  img[11:30, 11:30, ] <- 0.70
  img[11:30, 18:22, 1] <- 0.05
  img[11:30, 18:22, 2] <- 0.10
  img[11:30, 18:22, 3] <- 0.90
  tissue <- matrix(FALSE, 40, 40)
  tissue[11:30, 11:30] <- TRUE

  marks <- wsi_detect_pen_marks(img, tissue_mask = tissue, min_area = 1)

  expect_s3_class(marks, "wsi_pen_mark_mask")
  expect_equal(dim(marks$mask), c(40, 40))
  expect_equal(marks$pen_pixel_count, 100)
  expect_equal(marks$blue_pixel_count, 100)
  expect_equal(marks$green_pixel_count, 0)
  expect_equal(marks$red_pixel_count, 0)
  expect_equal(marks$tissue_pixel_count, 400)
  expect_equal(marks$tissue_pen_pixel_count, 100)
  expect_equal(marks$tissue_affected_percentage, 25)
  expect_equal(nrow(marks$component_bboxes), 1)
})

test_that("pen mark detector separates red, green, and black ink-like regions", {
  img <- array(1, dim = c(64, 64, 3))
  img[8:18, 8:12, 1] <- 0.90
  img[8:18, 8:12, 2] <- 0.08
  img[8:18, 8:12, 3] <- 0.08
  img[24:35, 28:33, 1] <- 0.08
  img[24:35, 28:33, 2] <- 0.85
  img[24:35, 28:33, 3] <- 0.08
  img[5:60, 50:53, ] <- 0.02

  marks <- wsi_detect_pen_marks(
    img,
    estimate_tissue = FALSE,
    min_area = 1,
    black_edge_fraction_threshold = 0.01
  )

  expect_gt(marks$red_pixel_count, 0)
  expect_gt(marks$green_pixel_count, 0)
  expect_gt(marks$black_ink_pixel_count, 0)
  expect_equal(marks$tissue_affected_percentage, NA_real_)
  expect_true(all(marks$mask[8:18, 8:12]))
  expect_true(all(marks$mask[24:35, 28:33]))
  expect_true(all(marks$mask[5:60, 50:53]))
})

test_that("blur detector separates sharp from flat images", {
  edge <- matrix(0, nrow = 64, ncol = 64)
  edge[, 33:64] <- 1
  sharp <- array(rep(edge, 3), dim = c(64, 64, 3))
  flat <- array(0.5, dim = c(64, 64, 3))

  sharp_focus <- wsi_detect_blur(sharp, threshold = 0.001)
  flat_focus <- wsi_detect_blur(flat, threshold = 0.001)

  expect_false(sharp_focus$focus_blurry)
  expect_true(flat_focus$focus_blurry)
  expect_gt(sharp_focus$focus_score, flat_focus$focus_score)
  expect_gt(sharp_focus$tenengrad_score, flat_focus$tenengrad_score)
})

test_that("blur detector can skip tiles with too little tissue", {
  checker <- matrix(rep(c(0, 1), 32 * 32 / 2), nrow = 32)
  sharp <- array(rep(checker, 3), dim = c(32, 32, 3))
  tissue <- matrix(FALSE, 32, 32)
  tissue[1, 1] <- TRUE

  focus <- wsi_detect_blur(sharp, tissue_mask = tissue, min_tissue_fraction = 0.1)

  expect_false(focus$focus_evaluable)
  expect_equal(focus$focus_tissue_fraction, 1 / (32 * 32))
  expect_true(is.na(focus$focus_blurry))
})

test_that("focus heatmap reads tiles and returns heatmap matrices", {
  slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256, levels = c(1))

  focus <- wsi_focus_heatmap(slide, tile_size = 128, threshold = 0.001)

  expect_s3_class(focus, "wsi_focus_heatmap")
  expect_equal(nrow(focus$tiles), 4)
  expect_equal(dim(focus$heatmap), c(2, 2))
  expect_equal(dim(focus$blurry_tile_mask), c(2, 2))
  expect_equal(focus$slide_focus_score, 0)
  expect_equal(focus$blurry_tile_fraction, 1)
  expect_true(all(focus$blurry_tile_mask))
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
