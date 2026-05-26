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

make_he_patch <- function(width = 16, height = 12, stain_matrix = wsi_he_stain_matrix()) {
  h <- matrix(seq(0.05, 1.1, length.out = width * height), nrow = height, ncol = width)
  e <- matrix(seq(0.9, 0.05, length.out = width * height), nrow = height, ncol = width)
  od <- cbind(as.vector(h), as.vector(e)) %*% t(stain_matrix[, 1:2, drop = FALSE])
  array(exp(-od), dim = c(height, width, 3))
}

test_that("H&E deconvolution returns hematoxylin, eosin, and residual channels", {
  image <- make_he_patch(width = 10, height = 8)

  channels <- wsi_deconvolve_he(image)

  expect_s3_class(channels, "wsi_ihc_channels")
  expect_equal(dim(channels$hematoxylin), c(8, 10))
  expect_equal(dim(channels$eosin), c(8, 10))
  expect_equal(dim(channels$residual), c(8, 10))
  expect_equal(wsiTools:::wsi_channel_ids_from_output(channels), c("hematoxylin", "eosin", "residual"))
  expect_true(all(channels$hematoxylin >= 0))
  expect_true(all(channels$eosin >= 0))
  expect_true(all(channels$residual >= 0))
  expect_false(channels$channel_metadata[[3]]$visible)
})

test_that("H&E deconvolution recovers hematoxylin and eosin without forced residual", {
  image <- make_he_patch(width = 9, height = 7)
  expected_h <- matrix(seq(0.05, 1.1, length.out = 9 * 7), nrow = 7, ncol = 9)
  expected_e <- matrix(seq(0.9, 0.05, length.out = 9 * 7), nrow = 7, ncol = 9)

  channels <- wsi_deconvolve_he(image, include_residual = FALSE)

  expect_equal(wsiTools:::wsi_channel_ids_from_output(channels), c("hematoxylin", "eosin"))
  expect_true(max(abs(channels$hematoxylin - expected_h)) < 1e-6)
  expect_true(max(abs(channels$eosin - expected_e)) < 1e-6)
  expect_null(channels$residual)
})

test_that("H&E residual channel can be visualized in recoloured output", {
  image <- make_he_patch(width = 6, height = 5)

  recoloured <- wsi_deconvolve_he(
    image,
    format = "array",
    residual_colour = "gray40",
    residual_visible = TRUE
  )

  expect_equal(dim(recoloured), c(5, 6, 4))
  expect_true(all(recoloured >= 0))
  expect_true(all(recoloured <= 1))
})

test_that("H&E stain matrices can be estimated and reconstructed", {
  image <- make_he_patch(width = 24, height = 20)

  fixed <- wsi_estimate_stain_matrix(image, method = "fixed")
  macenko <- wsi_estimate_stain_matrix(image, method = "macenko", max_pixels = 200)
  vahadane <- wsi_estimate_stain_matrix(
    image,
    method = "vahadane",
    max_pixels = 200,
    nmf_iterations = 5
  )

  expect_equal(dim(fixed), c(3, 3))
  expect_equal(dim(macenko), c(3, 3))
  expect_equal(dim(vahadane), c(3, 3))
  expect_equal(colnames(fixed), c("hematoxylin", "eosin", "residual"))
  expect_true(all(abs(sqrt(colSums(macenko^2)) - 1) < 1e-8))

  channels <- wsi_deconvolve_he(image, stain_matrix = fixed)
  reconstructed <- wsi_reconstruct_stains(channels, stain_matrix = fixed)

  expect_equal(dim(reconstructed), dim(image))
  expect_true(max(abs(reconstructed - image)) < 1e-6)
})

test_that("H&E stain normalization works on patches and regions", {
  image <- make_he_patch(width = 16, height = 12)
  target_matrix <- wsi_he_stain_matrix(
    hematoxylin = c(0.70, 0.68, 0.22),
    eosin = c(0.12, 0.92, 0.37)
  )

  normalized <- wsi_normalize_stains(
    image,
    method = "fixed",
    target_matrix = target_matrix,
    format = "result"
  )

  expect_s3_class(normalized, "wsi_stain_normalization")
  expect_equal(dim(normalized$image), dim(image))
  expect_equal(dim(normalized$source_matrix), c(3, 3))
  expect_equal(dim(normalized$target_matrix), c(3, 3))
  expect_true(all(normalized$image >= 0))
  expect_true(all(normalized$image <= 1))

  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1))
  region <- wsi_stain_normalize_region(
    slide,
    x = 10,
    y = 20,
    width = 32,
    height = 24,
    method = "fixed"
  )
  expect_equal(dim(region), c(24, 32, 4))
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

test_that("region-based H&E deconvolution returns residual channel on mock slides", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))

  channels <- wsi_deconvolve_he_region(
    slide,
    x = 10,
    y = 20,
    width = 32,
    height = 24,
    level = 0
  )

  expect_s3_class(channels, "wsi_ihc_channels")
  expect_equal(dim(channels$hematoxylin), c(24, 32))
  expect_equal(dim(channels$eosin), c(24, 32))
  expect_equal(dim(channels$residual), c(24, 32))
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
