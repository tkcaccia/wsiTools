test_that("mock thumbnails and tissue masks work without WSI backends", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  thumb <- wsi_thumbnail(slide, width = 100, format = "array")
  expect_equal(dim(thumb), c(50, 100, 4))

  mask <- wsi_tissue_mask(slide, thumbnail_width = 100)
  expect_s3_class(mask, "wsi_tissue_mask")
  expect_equal(dim(mask$mask), c(50, 100))
  expect_equal(mask$tissue_percentage, 0)
  expect_equal(mask$tissue_area, 0)
  expect_true(all(c("x", "y", "width", "height", "xmin", "ymin", "xmax", "ymax") %in% names(mask$tissue_bounding_box)))
  expect_equal(nrow(mask$component_bboxes), 0)
  expect_equal(mask$background_percentage, 100)
})

test_that("tissue detector separates coloured tissue from bright background", {
  img <- array(1, dim = c(80, 100, 3))
  img[21:60, 31:70, 1] <- 0.68
  img[21:60, 31:70, 2] <- 0.34
  img[21:60, 31:70, 3] <- 0.58

  mask <- wsi_detect_tissue(img, min_area = 1, scale = c(10, 20))

  expect_s3_class(mask, "wsi_tissue_mask")
  expect_equal(dim(mask$mask), c(80, 100))
  expect_equal(mask$tissue_pixel_count, 1600)
  expect_equal(mask$tissue_fraction, 1600 / (80 * 100))
  expect_equal(mask$tissue_percentage, 20)
  expect_equal(mask$tissue_area, 1600 * 10 * 20)
  expect_equal(nrow(mask$component_bboxes), 1)
  expect_equal(mask$component_bboxes$x, 300)
  expect_equal(mask$component_bboxes$y, 400)
  expect_equal(mask$component_bboxes$width, 400)
  expect_equal(mask$component_bboxes$height, 800)
  expect_equal(unname(mask$tissue_bounding_box[c("x", "y", "width", "height")]), c(300, 400, 400, 800))
})

test_that("tissue detector removes small components before summaries", {
  img <- array(1, dim = c(16, 16, 3))
  img[2, 2, 1] <- 0.65
  img[2, 2, 2] <- 0.25
  img[2, 2, 3] <- 0.55
  img[6:10, 6:10, 1] <- 0.65
  img[6:10, 6:10, 2] <- 0.25
  img[6:10, 6:10, 3] <- 0.55

  mask <- wsi_detect_tissue(img, min_area = 4)

  expect_equal(mask$tissue_pixel_count, 25)
  expect_equal(nrow(mask$component_bboxes), 1)
  expect_false(mask$mask[2, 2])
  expect_true(all(mask$mask[6:10, 6:10]))
})
