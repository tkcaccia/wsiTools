test_that("backend checks return stable structures", {
  backends <- wsi_backends()
  expect_s3_class(backends, "data.frame")
  expect_named(backends, c("backend", "installed", "version", "capabilities", "notes"))
  expect_true(all(c(
    "openslide", "libvips", "bioformats", "imagemagick",
    "aicspylibczi", "httpuv", "callr", "stardist", "cellpose"
  ) %in% backends$backend))
  expect_type(wsi_has_openslide(), "logical")
  expect_type(wsi_has_vips(), "logical")
  expect_type(wsi_has_bioformats(), "logical")
  expect_type(wsi_has_czi_python(), "logical")
  expect_type(wsi_has_cellpose(), "logical")
})
