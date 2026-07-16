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

test_that("native CZI persistent tile handle symbols are registered", {
  expect_type(wsiTools:::wsi_native_available("wsi_native_czi_open_handle"), "logical")
  expect_type(wsiTools:::wsi_native_available("wsi_native_czi_close_handle"), "logical")
  expect_type(wsiTools:::wsi_native_available("wsi_native_czi_handle_read_region"), "logical")
})

test_that("native bounding-box index matches an exact viewport scan", {
  set.seed(11)
  xmin <- runif(5000, 0, 100000)
  ymin <- runif(5000, 0, 80000)
  width <- runif(5000, 2, 900)
  height <- runif(5000, 2, 700)
  bbox <- cbind(
    xmin = xmin,
    ymin = ymin,
    xmax = xmin + width,
    ymax = ymin + height
  )
  index <- wsiTools:::wsi_bbox_index_create(bbox)
  skip_if(is.null(index), "Native bounding-box index is unavailable")

  query <- c(xmin = 22000, ymin = 17000, xmax = 44000, ymax = 36000)
  expected <- which(
    bbox[, "xmin"] <= query[["xmax"]] &
      bbox[, "xmax"] >= query[["xmin"]] &
      bbox[, "ymin"] <= query[["ymax"]] &
      bbox[, "ymax"] >= query[["ymin"]]
  )
  observed <- wsiTools:::wsi_bbox_index_query(
    index,
    query[["xmin"]],
    query[["ymin"]],
    query[["xmax"]],
    query[["ymax"]]
  )

  expect_s3_class(index, "wsi_bbox_index")
  expect_identical(observed, as.integer(expected))
})
