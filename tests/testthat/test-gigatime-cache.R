test_that("GigaTIME display encoding participates in rendered tile cache identity", {
  path <- tempfile(); writeLines("unchanged scalar source", path)
  on.exit(unlink(path), add = TRUE)
  source <- list(path = path, kind = "image", width = 20, height = 20, page = 0,
    tile_size = 512, tile_format = "png", metadata = list(gigatime = list(display_max = 1)))
  original <- wsi_dynamic_source_cache_key(source)
  source$metadata$gigatime$display_max <- .5
  expect_false(identical(wsi_dynamic_source_cache_key(source), original))
  source$metadata$gigatime <- NULL
  ordinary <- wsi_dynamic_source_cache_key(source)
  source$metadata$unrelated_note <- "does not change ordinary rendered pixels"
  expect_identical(wsi_dynamic_source_cache_key(source), ordinary)
})
