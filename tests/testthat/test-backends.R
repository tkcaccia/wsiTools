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

test_that("invalid UTF-8 from command-line tools is cleaned before regex parsing", {
  invalid <- rawToChar(c(charToRaw("openslide.level-count: 1"), as.raw(0xff)))
  Encoding(invalid) <- "UTF-8"

  expect_false(validUTF8(invalid))
  expect_warning(parsed <- wsiTools:::wsi_parse_key_value(invalid), NA)
  expect_true(startsWith(parsed[["openslide.level-count"]], "1"))
  expect_true(validUTF8(names(parsed)))
  expect_true(validUTF8(unlist(parsed, use.names = FALSE)))
})
