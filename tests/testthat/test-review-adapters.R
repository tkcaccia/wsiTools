review_adapter_fixture <- function() {
  image <- tempfile(fileext = ".ppm")
  writeBin(c(charToRaw("P6\n100 80\n255\n"), as.raw(rep(128L, 100L * 80L * 3L))), image)
  image <- list(tissue_id = "t1", frame_id = "frame1", sha256 = wsi_experiment_sha256(image),
    path = image, width = 100L, height = 80L, mpp_x = .5, mpp_y = .5,
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels")
  context <- wsi_review_context(list(image), list(), context_id = "adapter-fixture")
  object <- list(coordinates = data.frame(barcode = c("AA", "BB", "CC"), x = c(10, 20, 30), y = c(15, 25, 35)),
    meta.data = data.frame(seurat_clusters = c("2", "0", "1"), row.names = c("CC", "AA", "BB")),
    assays = list(expression = "DO_NOT_SERIALIZE_EXPRESSION"),
    reductions = list(pca = list(cell.embeddings = matrix(c(30, 10, 20, -3, -1, -2), ncol = 2,
      dimnames = list(c("CC", "AA", "BB"), c("PC1", "PC2"))))))
  mapping <- list(confirmed = TRUE, image_sha256 = image$sha256, frame_id = image$frame_id,
    coordinate_space = "native_pixels", same_section = TRUE)
  list(context = context, object = object, mapping = mapping, image = image)
}

test_that("same-section metadata and reductions use explicit IDs without promoting labels", {
  skip_if_not_installed("openssl")
  fixture <- review_adapter_fixture()
  context <- wsi_review_attach_seurat(fixture$context, fixture$object, tissue_id = "t1",
    dataset_id = "rna", coordinate_mapping = fixture$mapping, reduction_names = "pca")
  expect_length(context$observations, 3L)
  expect_equal(vapply(context$observations, function(row) row$cluster_values$seurat_clusters, character(1)), c("0", "1", "2"))
  expect_equal(vapply(context$reductions[[1]]$points, `[[`, numeric(1), "x"), c(10, 20, 30))
  expect_true(all(vapply(context$observations, function(row) identical(row$observation_unit, "spot"), logical(1))))
  expect_null(context$review_records)
  expect_false(grepl("DO_NOT_SERIALIZE_EXPRESSION", jsonlite::toJSON(context, auto_unbox = TRUE)))
  expect_length(fixture$context$observations, 0L)
  same_ids <- wsi_review_attach_spatial(context, fixture$object, "t1", dataset_id = "rna-second",
    coordinate_mapping = fixture$mapping)
  expect_length(unique(vapply(same_ids$observations, `[[`, character(1), "id")), 6L)
  expect_error(wsi_review_attach_spatial(context, fixture$object, "t1", dataset_id = "rna",
    coordinate_mapping = fixture$mapping), "already has")
})

test_that("mapping needs exact H&E identity, confirmation and a finite invertible affine", {
  skip_if_not_installed("openssl")
  fixture <- review_adapter_fixture()
  expect_error(wsi_review_attach_spatial(fixture$context, fixture$object, "t1"), "Confirm.*mapping")
  bad <- fixture$mapping
  bad$confirmed <- FALSE
  expect_error(wsi_review_attach_spatial(fixture$context, fixture$object, "t1", coordinate_mapping = bad), "Confirm")
  for (key in c("image_sha256", "frame_id")) {
    bad <- fixture$mapping; bad[[key]] <- "wrong"
    expect_error(wsi_review_attach_spatial(fixture$context, fixture$object, "t1", coordinate_mapping = bad), "SHA-256/frame")
  }
  bad <- fixture$mapping; bad$coordinate_space <- "unknown"
  expect_error(wsi_review_attach_spatial(fixture$context, fixture$object, "t1", coordinate_mapping = bad), "Unknown coordinate space")
  affine <- fixture$mapping
  affine$coordinate_space <- "source_pixels"
  affine$affine <- list(list(2, 0, 1), list(0, 1, 3), list(0, 0, 1))
  result <- wsi_review_attach_spatial(fixture$context, fixture$object, "t1", coordinate_mapping = affine)
  expect_equal(vapply(result$observations, `[[`, numeric(1), "x"), c(21, 41, 61))
  expect_equal(vapply(result$observations, `[[`, numeric(1), "y"), c(18, 28, 38))
  affine$affine[[3]] <- list(0, 0, 0)
  expect_error(wsi_review_attach_spatial(fixture$context, fixture$object, "t1", coordinate_mapping = affine), "invertible")
})

test_that("trusted RDS attachment persists bounded reconstruction metadata", {
  skip_if_not_installed("openssl")
  fixture <- review_adapter_fixture()
  path <- tempfile(fileext = ".rds")
  saveRDS(fixture$object, path)
  expect_error(wsi_review_attach_spatial(fixture$context, path, "t1", coordinate_mapping = fixture$mapping), "trusted=TRUE")
  context <- wsi_review_attach_spatial(fixture$context, path, "t1", trusted = TRUE,
    coordinate_mapping = fixture$mapping, reduction_names = "pca")
  reference <- context$provenance$spatial_attachments[[1]]$source_reference
  expect_equal(reference$sha256, wsi_experiment_sha256(path))
  expect_equal(reference$type, "trusted-local-rds")
  unlink(path)
  restored <- jsonlite::fromJSON(jsonlite::toJSON(context, auto_unbox = TRUE, null = "null", digits = 17), simplifyVector = FALSE)
  expect_identical(wsi_validate_review_context(restored)$source_signature, context$source_signature)
  expect_length(restored$observations, 3L)
  expect_length(restored$reductions, 1L)
  inspected <- wsi_review_inspect_spatial(fixture$object)
  expect_equal(inspected$observation_count, 3L)
  expect_equal(unlist(inspected$metadata_fields), "seurat_clusters")
  expect_equal(unlist(inspected$reduction_names), "pca")
  expect_false(inspected$mapping_confirmed)
  fractional <- fixture$object
  fractional$coordinates$x[[1]] <- 10.1234567890123
  fractional <- wsi_review_attach_spatial(fixture$context, fractional, "t1", coordinate_mapping = fixture$mapping)
  fractional_saved <- jsonlite::fromJSON(jsonlite::toJSON(fractional, auto_unbox = TRUE, null = "null", digits = 17), simplifyVector = FALSE)
  expect_identical(wsi_validate_review_context(fractional_saved)$source_signature, fractional$source_signature)
})

test_that("invalid or oversized spatial input is rejected without guessing IDs or units", {
  skip_if_not_installed("openssl")
  fixture <- review_adapter_fixture()
  attach <- function(object, ...) wsi_review_attach_spatial(fixture$context, object, "t1", coordinate_mapping = fixture$mapping, ...)
  bad <- fixture$object; bad$coordinates$barcode[[2]] <- "AA"
  expect_error(attach(bad), "unique")
  bad <- fixture$object; bad$coordinates$x[[2]] <- NA_real_
  expect_error(attach(bad), "finite")
  bad <- fixture$object; bad$coordinates$x[[2]] <- 1000
  expect_error(attach(bad), "outside")
  bad <- fixture$object; rownames(bad$reductions$pca$cell.embeddings) <- c("AA", "AA", "CC")
  expect_error(attach(bad, reduction_names = "pca"), "uniquely")
  expect_error(attach(fixture$object, max_observations = 2L), "No rows were sampled")
  expect_error(attach(fixture$object, observation_unit = "tile"), "never tile/token")
})

test_that("Seurat metadata coordinates prefill fields but still require native mapping confirmation", {
  skip_if_not_installed("openssl")
  fixture <- review_adapter_fixture()
  object <- list(meta.data = data.frame(slide_x = c(10, 20, 30), slide_y = c(15, 25, 35),
    seurat_clusters = c("0", "1", "2"), row.names = c("AA", "BB", "CC")))
  inspect <- wsi_review_inspect_spatial(object)
  expect_equal(inspect$coordinate_metadata$declared_coordinate_space, "fullres")
  expect_false(inspect$mapping_confirmed)
  expect_error(wsi_review_attach_spatial(fixture$context, object, "t1"), "Confirm")
  attached <- wsi_review_attach_spatial(fixture$context, object, "t1", coordinate_mapping = fixture$mapping)
  expect_equal(vapply(attached$observations, `[[`, numeric(1), "x"), c(10, 20, 30))
  object$meta.data$slide_x[[2]] <- NA_real_
  expect_error(wsi_review_attach_spatial(fixture$context, object, "t1", coordinate_mapping = fixture$mapping), "finite")
  multiple <- fixture$object
  multiple$images <- list(one = list(), two = list())
  expect_true(wsi_review_inspect_spatial(multiple)$needs_image_selection)
})

test_that("linked-image review uses image calibration and never confirms an automatic display mapping", {
  skip_if_not_installed("openssl")
  fixture <- review_adapter_fixture()
  slide <- wsi_mock_slide(width = 100L, height = 80L)
  slide$path <- fixture$image$path
  linked <- list(slide = slide, image_path = slide$path, image_name = "section",
    coordinate_mapping = list(method = "auto_hires", scale_x = 2, scale_y = 2),
    mpp = list(x = 99, y = 99), scale_metadata = list(inferred = TRUE),
    spots = data.frame(id = c("AA", "BB"), x = c(10, 20), y = c(15, 25)), spot_count = 2L)
  pending <- wsi_review_context_from_linked(linked)
  expect_length(pending$observations, 0L)
  expect_identical(pending$provenance$availability$status, "unavailable")
  expect_equal(pending$images[[1]]$mpp_x, .25)
  approved <- wsi_review_context_from_linked(linked, confirmed = TRUE)
  expect_length(approved$observations, 2L)
  expect_true(approved$provenance$spatial_attachments[[1]]$mapping$confirmed)
})
