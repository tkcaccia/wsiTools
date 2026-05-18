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

test_that("magick images convert to height x width x channel arrays", {
  skip_if_not_installed("magick")

  image <- magick::image_blank(width = 7, height = 5, color = "red")
  array <- wsiTools:::wsi_magick_to_array(image)

  expect_equal(dim(array), c(5, 7, 4))
  expect_true(all(array[, , 1] > 0.9))
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

test_that("region-based IHC deconvolution accepts image paths with spaces", {
  skip_if_not(wsi_has_vips())
  skip_if_not_installed("magick")

  input <- file.path(tempdir(), "SAPC 0052.ppm")
  pixels <- paste(rep("180 130 100", 32 * 24), collapse = " ")
  writeLines(c("P3", "32 24", "255", pixels), input)

  channels <- wsi_deconvolve_region(
    input,
    x = 0,
    y = 0,
    width = 16,
    height = 12,
    level = 0
  )

  expect_s3_class(channels, "wsi_ihc_channels")
  expect_equal(dim(channels$hematoxylin), c(12, 16))
  expect_equal(dim(channels$hrp), c(12, 16))
})

test_that("multi-IHC deconvolution supports selectable stain channel definitions", {
  image <- array(0.75, dim = c(5, 6, 3))
  channels <- wsi_stain_channels(
    name = c("Hematoxylin", "HRP/DAB", "Fast Red"),
    vector = list(
      c(0.650, 0.704, 0.286),
      c(0.268, 0.570, 0.776),
      c(0.213, 0.851, 0.477)
    ),
    colour = c("purple", "brown", "red"),
    visible = c(TRUE, FALSE, TRUE)
  )

  result <- wsi_deconvolve_multi_ihc(image, channels = channels)

  expect_s3_class(result, "wsi_ihc_channels")
  expect_equal(dim(result$hematoxylin), c(5, 6))
  expect_equal(dim(result$hrp_dab), c(5, 6))
  expect_equal(dim(result$fast_red), c(5, 6))
  expect_equal(length(result$channel_metadata), 3)
  expect_false(result$channel_metadata[[2]]$visible)
})

test_that("multi-IHC deconvolution rejects more than three RGB stain channels", {
  expect_error(
    wsi_stain_channels(
      name = paste("marker", 1:4),
      vector = rep(list(c(0.2, 0.6, 0.7)), 4)
    ),
    "at most three"
  )
})
