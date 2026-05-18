test_that("SAPC0052.svs can be deconvolved into hematoxylin and HRP/DAB channels", {
  path <- Sys.getenv("WSITOOLS_TEST_SAPC0052_SVS", unset = "")
  skip_if(!nzchar(path), "Set WSITOOLS_TEST_SAPC0052_SVS to test SAPC0052.svs.")
  skip_if_not(file.exists(path), "SAPC0052.svs test file is not available.")
  skip_if_not(wsi_has_openslide() || wsi_has_vips(), "SAPC0052.svs needs OpenSlide or libvips.")
  skip_if_not_installed("magick")

  slide <- wsi_open(path, backend = "auto")
  on.exit(wsi_close(slide), add = TRUE)
  info <- wsi_info(slide)

  width <- min(256L, as.integer(info$dimensions[["width"]]))
  height <- min(256L, as.integer(info$dimensions[["height"]]))
  x <- as.integer(max(0, floor((info$dimensions[["width"]] - width) / 2)))
  y <- as.integer(max(0, floor((info$dimensions[["height"]] - height) / 2)))

  channels <- wsi_deconvolve_region(
    slide,
    x = x,
    y = y,
    width = width,
    height = height,
    level = 0,
    format = "channels"
  )

  expect_s3_class(channels, "wsi_ihc_channels")
  expect_equal(dim(channels$hematoxylin), c(height, width))
  hrp_channel <- if (!is.null(channels$hrp_dab)) channels$hrp_dab else channels$hrp
  expect_equal(dim(hrp_channel), c(height, width))
  expect_true(all(is.finite(channels$hematoxylin)))
  expect_true(all(is.finite(hrp_channel)))
})
