test_that("OpenSlide CLI properties with quoted values retain dimensions", {
  testthat::local_mocked_bindings(
    wsi_run_command = function(...) c(
      "openslide.level-count: '1'",
      "openslide.level[0].width: '30966'",
      "openslide.level[0].height: '39421'",
      "openslide.level[0].downsample: '1'",
      "openslide.vendor: 'generic-tiff'"
    ),
    .package = "wsiTools"
  )
  properties <- wsiTools:::wsi_openslide_properties("slide.ome.tif")
  levels <- wsiTools:::wsi_properties_to_levels(properties)
  expect_equal(levels$width, 30966)
  expect_equal(levels$height, 39421)
  expect_equal(properties[["openslide.vendor"]], "generic-tiff")
})

test_that("OpenSlide region reader uses the documented CLI argument order", {
  seen <- NULL
  testthat::local_mocked_bindings(
    wsi_command_exists = function(...) TRUE,
    wsi_run_command = function(command, args, ...) {
      seen <<- args
      character()
    },
    .package = "wsiTools"
  )
  slide <- list(path = "slide.ome.tif")
  region <- list(x = 100, y = 200, level = 0, width = 512, height = 513)
  wsiTools:::wsi_openslide_read_region_file(slide, region, "tile.png")
  expect_equal(seen, c("slide.ome.tif", "100", "200", "0", "512", "513", "tile.png"))
})

test_that("region reads respect an explicitly opened OpenSlide backend", {
  slide <- list(backend = "openslide")
  testthat::local_mocked_bindings(
    wsi_has_vips = function() TRUE,
    wsi_command_exists = function(...) TRUE,
    .package = "wsiTools"
  )
  expect_equal(wsiTools:::wsi_choose_region_backend(slide), "openslide")
})

test_that("single-level OpenSlide overviews are cached only at bounded levels", {
  slide <- list(
    backend = "openslide", path = tempfile(fileext = ".tif"),
    levels = data.frame(level = 0L, width = 30966, height = 39421, downsample = 1)
  )
  file.create(slide$path)
  on.exit(unlink(slide$path), add = TRUE)
  source <- list(kind = "slide", slide = slide, width = 30966, height = 39421)
  class(source) <- c("wsi_dynamic_tile_source", "list")
  testthat::local_mocked_bindings(
    wsi_has_vips = function() TRUE,
    wsi_openslide_pyvips_available = function() TRUE,
    .package = "wsiTools"
  )
  region <- list(deepzoom_downsample = 16)
  expect_true(wsiTools:::wsi_dynamic_can_cache_vips_level(source, 13L, region))
  region$deepzoom_downsample <- 4
  expect_false(wsiTools:::wsi_dynamic_can_cache_vips_level(source, 15L, region))
})
