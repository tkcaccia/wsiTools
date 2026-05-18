test_that("backend checks return stable structures", {
  backends <- wsi_backends()
  expect_s3_class(backends, "data.frame")
  expect_named(backends, c("backend", "installed", "version", "capabilities", "notes"))
  expect_true(all(c("openslide", "libvips", "bioformats", "imagemagick") %in% backends$backend))
  expect_type(wsi_has_openslide(), "logical")
  expect_type(wsi_has_vips(), "logical")
  expect_type(wsi_has_bioformats(), "logical")
})
