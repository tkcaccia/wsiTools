test_that("demo project creates lightweight onboarding assets", {
  skip_if_not(capabilities("png"), "PNG graphics device is not available")

  path <- tempfile("wsi-demo-test-")
  demo <- wsi_demo_project(path = path, open = FALSE, overwrite = TRUE)

  expect_s3_class(demo, "wsi_demo_project")
  expect_s3_class(demo$slide, "wsi_slide")
  expect_true(dir.exists(demo$path))
  expect_true(file.exists(demo$viewer))
  expect_true(file.exists(demo$files$tiny_png))
  expect_true(file.exists(demo$files$tiny_tiff))
  expect_true(file.exists(demo$files$fake_rois))
  expect_true(file.exists(demo$files$cells_csv))
  expect_true(file.exists(demo$files$fake_tile_grid))
  expect_true(file.exists(demo$files$spatial_spots))
  expect_true(file.exists(demo$files$spatial_expression))
  expect_true(file.exists(demo$files$spatial_reduction))
  expect_true(file.exists(demo$files$demo_readme))

  expect_gt(nrow(demo$rois), 0)
  expect_gt(nrow(demo$centroids), 0)
  expect_gt(nrow(demo$tile_grid), 0)
  expect_gt(nrow(demo$spatial_spots), 0)
  expect_true(all(c("DemoGeneA", "DemoGeneB", "DemoGeneC") %in% rownames(demo$spatial_expression)))
  expect_equal(demo$project$metadata$viewer_file, "demo-viewer.html")

  html <- paste(readLines(demo$viewer, warn = FALSE), collapse = "\n")
  expect_match(html, "wsiTools demo project", fixed = TRUE)
  expect_match(html, "Spatial spots", fixed = TRUE)
  expect_match(html, "DemoGeneA", fixed = TRUE)
})

test_that("demo viewer is the short onboarding command", {
  skip_if_not(capabilities("png"), "PNG graphics device is not available")

  demo <- wsi_demo_viewer(path = tempfile("wsi-demo-viewer-"), open = FALSE, overwrite = TRUE)

  expect_s3_class(demo, "wsi_demo_project")
  expect_true(file.exists(demo$viewer))
  printed <- capture.output(print(demo))
  expect_true(any(grepl("<wsi_demo_project>", printed, fixed = TRUE)))
})
