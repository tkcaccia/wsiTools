test_that("wsi_first_run_example creates an install-friendly mock project", {
  dir <- tempfile("first-run-example-")

  example <- wsi_first_run_example(dir, open = FALSE)

  expect_s3_class(example, "wsi_first_run_example")
  expect_s3_class(example$slide, "wsi_slide")
  expect_s3_class(example$manual_rois, "wsi_roi")
  expect_s3_class(example$mask_rois, "wsi_roi")
  expect_s3_class(example$rois, "wsi_roi")
  expect_s3_class(example$segmentation, "wsi_roi")
  expect_s3_class(example$project, "wsi_project")
  expect_true(file.exists(example$viewer))
  expect_true(file.exists(file.path(example$path, "project.json")))
  expect_true(file.exists(file.path(example$path, "README.md")))
  expect_true(file.exists(file.path(example$path, "mask.csv")))
  expect_true(file.exists(file.path(example$path, "rois.geojson")))
  expect_true(file.exists(file.path(example$path, "segmentation.geojson")))
  expect_true(file.exists(file.path(example$path, "tile_manifests", "tile_manifest.csv")))
  expect_gt(nrow(example$manual_rois), 0)
  expect_gt(nrow(example$mask_rois), 0)
  expect_gt(nrow(example$segmentation), 0)
  expect_gt(nrow(example$tile_grid), 0)
  expect_false(isTRUE(example$project$processing_provenance$pixel_data_loaded))

  reopened <- wsi_read_project(example$path)
  expect_s3_class(reopened, "wsi_project")
  expect_s3_class(reopened$rois, "wsi_roi")
  expect_s3_class(reopened$segmentation, "wsi_segmentation")
  expect_equal(nrow(reopened$tile_manifest), nrow(example$tile_grid))

  expect_error(
    wsi_first_run_example(example$path, open = FALSE),
    "overwrite = FALSE"
  )

  updated <- wsi_first_run_example(example$path, open = FALSE, overwrite = TRUE)
  expect_s3_class(updated, "wsi_first_run_example")
  expect_true(file.exists(updated$viewer))
})
