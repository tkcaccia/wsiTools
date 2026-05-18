test_that("IHC deconvolution returns separate hematoxylin and HRP channels", {
  image <- array(1, dim = c(4, 5, 4))
  image[, , 1] <- 0.55
  image[, , 2] <- 0.42
  image[, , 3] <- 0.35

  channels <- wsi_deconvolve_ihc(image)

  expect_s3_class(channels, "wsi_ihc_channels")
  expect_equal(dim(channels$hematoxylin), c(4, 5))
  expect_equal(dim(channels$hrp), c(4, 5))
  expect_true(all(channels$hematoxylin >= 0))
  expect_true(all(channels$hrp >= 0))
})

test_that("IHC deconvolution can return a recolored image", {
  image <- array(0.8, dim = c(3, 4, 3))

  recolored <- wsi_deconvolve_ihc(
    image,
    format = "array",
    hematoxylin_colour = "purple",
    hrp_colour = "brown"
  )

  expect_equal(dim(recolored), c(3, 4, 4))
  expect_true(all(recolored >= 0))
  expect_true(all(recolored <= 1))
})

test_that("region-based IHC deconvolution works on mock slides", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))

  channels <- wsi_deconvolve_region(
    slide,
    x = 10,
    y = 20,
    width = 32,
    height = 24,
    level = 0
  )

  expect_s3_class(channels, "wsi_ihc_channels")
  expect_equal(dim(channels$hematoxylin), c(24, 32))
})
