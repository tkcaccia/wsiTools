test_that("backend checks return stable structures", {
  backends <- wsi_backends()
  expect_s3_class(backends, "data.frame")
  expect_named(backends, c("backend", "installed", "version", "capabilities", "notes"))
  expect_true(all(c(
    "openslide", "libvips", "bioformats", "imagemagick",
    "bioformats_java", "native_czi", "aicspylibczi", "httpuv", "callr", "stardist", "cellpose"
  ) %in% backends$backend))
  expect_type(wsi_has_openslide(), "logical")
  expect_type(wsi_has_vips(), "logical")
  expect_type(wsi_has_bioformats(), "logical")
  expect_type(wsi_has_native_czi(), "logical")
  expect_type(wsi_has_czi_python(), "logical")
  expect_type(wsi_has_cellpose(), "logical")
})

test_that("Bio-Formats Java helper discovers configured jar paths", {
  jar <- tempfile(fileext = ".jar")
  file.create(jar)
  old <- Sys.getenv("WSITOOLS_BIOFORMATS_JAR", unset = NA_character_)
  on.exit({
    if (is.na(old)) {
      Sys.unsetenv("WSITOOLS_BIOFORMATS_JAR")
    } else {
      Sys.setenv(WSITOOLS_BIOFORMATS_JAR = old)
    }
  }, add = TRUE)
  Sys.setenv(WSITOOLS_BIOFORMATS_JAR = jar)

  expect_identical(wsiTools:::wsi_bioformats_java_jar(), jar)
  expect_type(wsiTools:::wsi_has_bioformats_java(), "logical")
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

test_that("native CZI preview plan prefers a low-resolution pyramid layer first", {
  pyramid_json <- paste0(
    '{"scenePyramidStatistics":{"0":[',
    '{"layerInfo":{"minificationFactor":2,"pyramidLayerNo":1},"count":1},',
    '{"layerInfo":{"minificationFactor":16,"pyramidLayerNo":4},"count":1},',
    '{"layerInfo":{"minificationFactor":64,"pyramidLayerNo":6},"count":1},',
    '{"layerInfo":{"minificationFactor":128,"pyramidLayerNo":7},"count":1}',
    ']}}'
  )
  plan <- wsiTools:::wsi_native_czi_preview_plan(
    list(width = 50000, height = 30000, pyramid_json = pyramid_json),
    width = 4096
  )

  expect_equal(plan$target_width, 1024L)
  expect_equal(plan$downsample, 64)
  expect_equal(plan$zoom, 1 / 64)
  expect_equal(plan$source, "native pyramid")
})

test_that("native CZI preview plan falls back to a scaled low-resolution overview", {
  plan <- wsiTools:::wsi_native_czi_preview_plan(
    list(width = 50000, height = 30000, pyramid_json = NA_character_),
    width = 4096
  )

  expect_equal(plan$target_width, 1024L)
  expect_equal(plan$source, "scaled native accessor")
  expect_equal(plan$zoom, 1024 / 50000, tolerance = 1e-8)
})

test_that("native CZI scene metadata parser preserves section names", {
  xml <- paste0(
    "<ImageDocument><Metadata><Information><Image><Dimensions><S><Scenes>",
    '<Scene Index="0" Name="ScanRegion0"><RegionId>1</RegionId></Scene>',
    '<Scene Index="3" Name="Tumour area"><RegionId>2</RegionId></Scene>',
    "</Scenes></S></Dimensions></Image></Information></Metadata></ImageDocument>"
  )

  scenes <- wsiTools:::wsi_native_czi_scene_metadata(xml)

  expect_s3_class(scenes, "data.frame")
  expect_equal(scenes$scene, c(0L, 3L))
  expect_equal(scenes$name, c("ScanRegion0", "Tumour area"))
})
