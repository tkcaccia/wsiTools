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

test_that("stain quality detector flags low and excessive staining", {
  tissue <- matrix(TRUE, 32, 32)
  low <- array(0.95, dim = c(32, 32, 3))
  over <- array(0.04, dim = c(32, 32, 3))
  good <- array(1, dim = c(32, 32, 3))
  good[, , 1] <- 0.78
  good[, , 2] <- 0.55
  good[, , 3] <- 0.74

  low_qc <- wsi_detect_stain_quality(low, tissue_mask = tissue, min_area = 1)
  over_qc <- wsi_detect_stain_quality(over, tissue_mask = tissue, min_area = 1)
  good_qc <- wsi_detect_stain_quality(good, tissue_mask = tissue, min_area = 1)

  expect_s3_class(low_qc, "wsi_stain_quality")
  expect_true(low_qc$low_stain)
  expect_false(low_qc$over_stained)
  expect_equal(low_qc$low_stain_percentage, 100)
  expect_gt(nrow(low_qc$low_stain_regions), 0)
  expect_true(over_qc$over_stained)
  expect_false(over_qc$low_stain)
  expect_equal(over_qc$over_stain_percentage, 100)
  expect_gt(nrow(over_qc$over_stain_regions), 0)
  expect_false(good_qc$low_stain)
  expect_false(good_qc$over_stained)
  expect_gt(good_qc$staining_score, low_qc$staining_score)
  expect_gt(good_qc$staining_score, over_qc$staining_score)
})

test_that("stain quality heatmap reads tiles and returns score matrices", {
  slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256, levels = c(1))

  stain <- wsi_stain_quality_heatmap(slide, tile_size = 128, min_area = 1)

  expect_s3_class(stain, "wsi_stain_quality_heatmap")
  expect_equal(nrow(stain$tiles), 4)
  expect_equal(dim(stain$stain_score_heatmap), c(2, 2))
  expect_equal(dim(stain$low_stain_tile_mask), c(2, 2))
  expect_equal(dim(stain$over_stain_tile_mask), c(2, 2))
  expect_equal(stain$slide_staining_score, 0)
  expect_equal(stain$low_stain_tile_fraction, 1)
  expect_equal(stain$over_stain_tile_fraction, 0)
  expect_true(all(stain$low_stain_tile_mask))
})

test_that("fold candidate detector returns a filtered candidate mask", {
  img <- array(0.80, dim = c(32, 32, 3))
  img[10:24, 14:18, 1] <- 0.18
  img[10:24, 14:18, 2] <- 0.03
  img[10:24, 14:18, 3] <- 0.10
  tissue <- matrix(TRUE, 32, 32)

  folds <- wsi_detect_fold_candidates(
    img,
    tissue_mask = tissue,
    min_area = 1,
    min_edge_fraction = 0.01
  )

  expect_s3_class(folds, "wsi_fold_candidate_mask")
  expect_equal(dim(folds$mask), c(32, 32))
  expect_true(folds$fold_candidate)
  expect_equal(folds$fold_pixel_count, 15 * 5)
  expect_equal(folds$tissue_fold_pixel_count, 15 * 5)
  expect_equal(nrow(folds$component_bboxes), 1)
  expect_gt(folds$component_bboxes$edge_fraction, 0)
  expect_gte(folds$component_bboxes$aspect_ratio, 1)
  expect_true(all(folds$mask[10:24, 14:18]))
})

test_that("fold candidate detector rejects dark smooth components without edge content", {
  img <- array(0.08, dim = c(32, 32, 3))
  img[, , 1] <- 0.12
  img[, , 2] <- 0.02
  img[, , 3] <- 0.07

  folds <- wsi_detect_fold_candidates(
    img,
    estimate_tissue = FALSE,
    min_area = 1
  )

  expect_s3_class(folds, "wsi_fold_candidate_mask")
  expect_equal(folds$raw_candidate_pixel_count, 32 * 32)
  expect_equal(folds$fold_pixel_count, 0)
  expect_false(folds$fold_candidate)
})

test_that("fold candidate heatmap reads tiles and returns fold matrices", {
  slide <- wsiTools:::wsi_mock_slide(width = 256, height = 256, levels = c(1))

  folds <- wsi_fold_candidate_heatmap(slide, tile_size = 128, min_area = 1)

  expect_s3_class(folds, "wsi_fold_candidate_heatmap")
  expect_equal(nrow(folds$tiles), 4)
  expect_equal(dim(folds$fold_fraction_heatmap), c(2, 2))
  expect_equal(dim(folds$tissue_fold_fraction_heatmap), c(2, 2))
  expect_equal(dim(folds$fold_candidate_tile_mask), c(2, 2))
  expect_equal(folds$slide_fold_candidate_fraction, 0)
  expect_equal(folds$fold_candidate_tile_fraction, 0)
  expect_false(any(folds$fold_candidate_tile_mask, na.rm = TRUE))
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
