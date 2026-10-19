test_that("multi-image spatial inspection offers choices before extracting coordinates", {
  object <- list(images = list(FOV_A = list(), FOV_B = list()),
    meta.data = data.frame(seurat_clusters = c("0", "1"), row.names = c("AA", "BB")),
    reductions = list(pca = list()))
  inspected <- wsi_review_inspect_spatial(object)
  expect_true(inspected$needs_image_selection)
  expect_identical(inspected$image_names, list("FOV_A", "FOV_B"))
  expect_null(inspected$observation_count)
  expect_null(inspected$coordinate_metadata)
  expect_identical(inspected$metadata_fields, list("seurat_clusters"))
  expect_identical(inspected$reduction_names, list("pca"))
  expect_false(inspected$mapping_confirmed)
  expect_error(wsi_review_spatial_coordinates(object), "select image_name")
})

test_that("selected spatial FOV inspection validates only its explicit coordinate view", {
  coordinates <- data.frame(barcode = c("AA", "BB"), x = c(10, 20), y = c(15, 25))
  object <- list(images = list(FOV_A = list(coordinates = coordinates), FOV_B = list()),
    meta.data = data.frame(cluster = c("0", "1"), row.names = c("AA", "BB")))
  inspected <- wsi_review_inspect_spatial(object, image_name = "FOV_A")
  expect_false(inspected$needs_image_selection)
  expect_equal(inspected$observation_count, 2)
  expect_identical(inspected$image_names, list("FOV_A", "FOV_B"))
  expect_identical(inspected$coordinate_metadata$image_name, "FOV_A")
  expect_false(inspected$mapping_confirmed)
  expect_error(wsi_review_inspect_spatial(object, image_name = "missing"), "was not found")
  expect_error(wsi_review_inspect_spatial(object, image_name = "FOV_B"), "no identifiable coordinate table")
})
