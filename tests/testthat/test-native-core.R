test_that("native edge magnitude matches the R fallback", {
  gray <- matrix(c(
    0.1, 0.2, 0.3,
    0.5, 0.4, 0.2,
    0.7, 0.1, 0.9
  ), nrow = 3, byrow = TRUE)

  expect_equal(
    wsiTools:::wsi_pen_edge_magnitude(gray),
    wsiTools:::wsi_pen_edge_magnitude_r(gray)
  )
})

test_that("native binary dilation matches the R fallback", {
  mask <- matrix(FALSE, 9, 9)
  mask[3, 3] <- TRUE
  mask[7, 5] <- TRUE
  mask[2, 8] <- TRUE

  expect_equal(
    wsiTools:::wsi_binary_dilate(mask, radius = 2),
    wsiTools:::wsi_binary_dilate_r(mask, radius = 2)
  )
})

test_that("native connected components match the R fallback", {
  mask <- matrix(FALSE, 8, 8)
  mask[1:2, 1:2] <- TRUE
  mask[5, 5] <- TRUE
  mask[6, 6] <- TRUE
  mask[7:8, 2] <- TRUE

  expect_equal(
    wsiTools:::wsi_mask_component_list(mask, connectivity = "4", min_area = 1),
    wsiTools:::wsi_mask_component_list_r(mask, connectivity = "4", min_area = 1)
  )
  expect_equal(
    wsiTools:::wsi_mask_component_list(mask, connectivity = "8", min_area = 2),
    wsiTools:::wsi_mask_component_list_r(mask, connectivity = "8", min_area = 2)
  )
})

test_that("mask helper fallbacks do not require compiled code", {
  expect_type(wsiTools:::wsi_native_available("wsi_cpp_edge_magnitude"), "logical")
  expect_type(wsiTools:::wsi_native_available("wsi_cpp_binary_dilate"), "logical")
  expect_type(wsiTools:::wsi_native_available("wsi_cpp_mask_components"), "logical")
})
