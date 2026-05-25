test_that("wsi_open errors on missing files", {
  expect_error(wsi_open("definitely-not-a-slide.svs"), "does not exist")
})

test_that("wsi_open gives CZI-specific guidance instead of falling through to ImageMagick", {
  path <- tempfile("sample_", fileext = ".czi")
  writeBin(as.raw(1:8), path)
  expect_error(
    wsi_open(path, backend = "auto"),
    "CZI files are not readable through the ImageMagick fallback backend",
    fixed = TRUE
  )
})

test_that("Bio-Formats OME-XML metadata parser extracts series dimensions", {
  xml <- c(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<OME>",
    "<Image ID=\"Image:0\">",
    "<Pixels ID=\"Pixels:0\" DimensionOrder=\"XYCZT\" Type=\"uint16\" SizeX=\"2048\" SizeY=\"1024\" SizeZ=\"1\" SizeC=\"3\" SizeT=\"1\">",
    "</Pixels>",
    "</Image>",
    "</OME>"
  )
  series <- wsiTools:::wsi_bioformats_parse_omexml(xml)
  expect_s3_class(series, "data.frame")
  expect_equal(series$width[[1]], 2048)
  expect_equal(series$height[[1]], 1024)
  expect_equal(series$size_c[[1]], 3L)
})

test_that("mock slides support metadata helpers", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  expect_s3_class(slide, "wsi_slide")
  expect_equal(wsi_levels(slide)$level, c(0L, 1L))
  expect_equal(wsi_info(slide)$dimensions[["width"]], 1000)
  expect_equal(wsi_mpp(slide), c(x = 0.25, y = 0.25))
  expect_equal(wsi_objective_power(slide), 40)
})
