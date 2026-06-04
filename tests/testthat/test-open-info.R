test_that("wsi_open errors on missing files", {
  expect_error(wsi_open("definitely-not-a-slide.svs"), "does not exist")
})

test_that("wsi_open gives CZI-specific guidance instead of falling through to ImageMagick", {
  path <- tempfile("sample_", fileext = ".czi")
  writeBin(as.raw(1:8), path)
  err <- expect_error(wsi_open(path, backend = "auto"))
  message <- conditionMessage(err)

  expect_match(message, "CZI could not be opened", fixed = TRUE)
  expect_match(message, "Requested backend: auto", fixed = TRUE)
  expect_match(message, "native_czi:", fixed = TRUE)
  expect_match(message, "bioformats_java:", fixed = TRUE)
  expect_match(message, "imagemagick:", fixed = TRUE)
  expect_match(message, "wsi_backends()", fixed = TRUE)
  expect_match(message, "wsi_diagnose(live_test = FALSE)", fixed = TRUE)
  expect_match(message, "wsi_install_backends(\"bioformats\")", fixed = TRUE)
  expect_match(message, "CZI files are not readable through the ImageMagick fallback backend", fixed = TRUE)
})

test_that("explicit backend open failures include check and fix commands", {
  path <- tempfile("not_really_tiff_", fileext = ".tif")
  writeBin(as.raw(1:8), path)
  err <- expect_error(wsi_open(path, backend = "vips"))
  message <- conditionMessage(err)

  expect_match(message, "Image could not be opened", fixed = TRUE)
  expect_match(message, "Requested backend: vips", fixed = TRUE)
  expect_match(message, "vips:", fixed = TRUE)
  expect_match(message, "wsi_backends()", fixed = TRUE)
  expect_match(message, "wsi_install_backends(\"libvips\")", fixed = TRUE)
})

test_that("backend action diagnostics include check and install guidance", {
  message <- wsiTools:::wsi_backend_action_message(
    "libvips conversion could not start.",
    backend = "vips",
    details = "vipsheader was not found"
  )

  expect_match(message, "libvips conversion could not start.", fixed = TRUE)
  expect_match(message, "Backend tried: vips", fixed = TRUE)
  expect_match(message, "How to check:", fixed = TRUE)
  expect_match(message, "wsi_has_vips()", fixed = TRUE)
  expect_match(message, "wsi_diagnose(live_test = FALSE)", fixed = TRUE)
  expect_match(message, "wsi_install_backends(\"libvips\")", fixed = TRUE)
  expect_match(message, "Details:", fixed = TRUE)
})

test_that("CZI auto backend order gives OpenSlide and libvips first refusal", {
  candidates <- wsiTools:::wsi_auto_backend_candidates(
    is_czi = TRUE,
    has_openslide = TRUE,
    has_vips = TRUE,
    has_native_czi = TRUE,
    has_bioformats = TRUE,
    has_imagemagick = TRUE
  )

  expect_equal(candidates, c("openslide", "vips", "native_czi", "bioformats"))
  expect_false("imagemagick" %in% candidates)
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

test_that("Bio-Formats parser tolerates invalid UTF-8 bytes in tool output", {
  line <- rawToChar(c(
    as.raw(0xff),
    charToRaw("<Pixels ID=\"Pixels:0\" DimensionOrder=\"XYCZT\" Type=\"uint16\" SizeX=\"64\" SizeY=\"32\" SizeZ=\"1\" SizeC=\"1\" SizeT=\"1\">")
  ))
  Encoding(line) <- "UTF-8"

  expect_false(validUTF8(line))
  expect_warning(series <- wsiTools:::wsi_bioformats_parse_omexml(line), NA)
  expect_equal(series$width[[1]], 64)
  expect_equal(series$height[[1]], 32)
})

test_that("Bio-Formats preview selection prefers browser-sized series", {
  series <- data.frame(
    series = 0:2,
    width = c(40000, 4096, 1200),
    height = c(30000, 3000, 900),
    stringsAsFactors = FALSE
  )
  rows <- wsiTools:::wsi_bioformats_preview_rows(series, width = 1000, max_series = 2, max_input_pixels = 5e7)
  expect_equal(rows$series[[1]], 2L)
  expect_true(all(rows$width * rows$height <= 5e7))
})

test_that("mock slides support metadata helpers", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  expect_s3_class(slide, "wsi_slide")
  expect_equal(wsi_levels(slide)$level, c(0L, 1L))
  expect_equal(wsi_info(slide)$dimensions[["width"]], 1000)
  expect_equal(wsi_mpp(slide), c(x = 0.25, y = 0.25))
  expect_equal(wsi_objective_power(slide), 40)
})
