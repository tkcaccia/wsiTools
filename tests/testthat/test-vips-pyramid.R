test_that("libvips exposes and reads TIFF SubIFD pyramid levels", {
  skip_if_not(wsi_has_vips())
  input <- tempfile(fileext = ".tif")
  pyramid <- tempfile(fileext = ".tif")
  output <- tempfile(fileext = ".jpg")
  on.exit(unlink(c(input, pyramid, output)), add = TRUE)

  wsi_run_command("vips", c("black", input, "2048", "2048", "--bands", "3"))
  wsi_run_command("vips", c(
    "tiffsave", input, pyramid, "--tile", "--pyramid", "--subifd",
    "--tile-width", "512", "--tile-height", "512"
  ))

  slide <- wsi_open(pyramid, backend = "vips")
  levels <- wsi_levels(slide)
  expect_gt(nrow(levels), 1L)
  expect_equal(levels$subifd[[2L]], 0L)
  expect_equal(levels$width[[2L]], 1024)
  expect_equal(wsi_dynamic_preferred_tile_size(slide), 1024L)
  expect_match(wsi_vips_thumbnail_input(slide, 512), "\\[subifd=1\\]$")
  expect_equal(wsi_vips_thumbnail_input(slide, 4096), slide$path)

  source <- wsi_dynamic_tile_source(slide, format = "jpg")
  on.exit(wsi_dynamic_tile_cleanup(source), add = TRUE)
  region <- wsi_dynamic_tile_region(source, source$max_level - 1L, 0L, 0L)
  expect_equal(region$level, 1L)
  expect_false(wsi_dynamic_can_cache_vips_level(source, source$max_level - 2L, region))
  wsi_region_to_file(slide, region, output, backend = "vips")
  expect_true(file.exists(output))
  expect_equal(as.numeric(wsi_vips_field(output, "width")), region$width)
})

test_that("live viewer uses larger tiles for embedded TIFF pyramids", {
  skip_if_not(wsi_has_vips())
  skip_if_not_installed("httpuv")
  input <- tempfile(fileext = ".tif")
  pyramid <- tempfile(fileext = ".tif")
  on.exit(unlink(c(input, pyramid)), add = TRUE)
  wsi_run_command("vips", c("black", input, "2048", "2048", "--bands", "3"))
  wsi_run_command("vips", c("tiffsave", input, pyramid, "--tile", "--pyramid", "--subifd"))

  slide <- wsi_open(pyramid, backend = "vips")
  session <- wsi_viewer_live(slide, dynamic_tiles = TRUE, mode = "tiles",
                             open = FALSE, wait = FALSE)
  on.exit(wsi_viewer_stop(session), add = TRUE)
  expect_equal(session$dynamic_tile_source$tile_size, 1024L)
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  expect_match(html, '"tile_cache_count":192', fixed = TRUE)
})

test_that("dynamic tiles never upsample a coarser native pyramid level", {
  slide <- structure(list(levels = data.frame(
    level = 0:2, downsample = c(1, 4, 16)
  )), class = "wsi_slide")
  expect_equal(wsi_dynamic_preferred_tile_size(slide), 512L)
  expect_equal(wsi_dynamic_native_level(slide, 2)$level, 0L)
  expect_equal(wsi_dynamic_native_level(slide, 4)$level, 1L)
})
