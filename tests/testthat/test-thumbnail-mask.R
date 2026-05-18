test_that("mock thumbnails and tissue masks work without WSI backends", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 500, levels = c(1, 4))
  thumb <- wsi_thumbnail(slide, width = 100, format = "array")
  expect_equal(dim(thumb), c(50, 100, 4))

  mask <- wsi_tissue_mask(slide, thumbnail_width = 100)
  expect_s3_class(mask, "wsi_tissue_mask")
  expect_equal(dim(mask$mask), c(50, 100))
})
