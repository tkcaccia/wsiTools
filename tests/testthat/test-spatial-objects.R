test_that("Giotto-like objects can be linked with explicit coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
  ids <- paste0("spot", 1:4)
  coords <- data.frame(
    cell_ID = ids,
    sdimx = c(100, 200, 300, 400),
    sdimy = c(150, 250, 350, 450)
  )
  embeddings <- data.frame(
    cell_ID = ids,
    PC_1 = c(-1, -0.5, 0.5, 1),
    PC_2 = c(0.2, 0.4, 0.8, 1.2)
  )
  expression <- matrix(
    c(1, 2, 3, 4, 4, 3, 2, 1),
    nrow = 2,
    dimnames = list(c("GeneA", "GeneB"), ids)
  )

  linked <- wsi_link_giotto_image(
    list(expression = expression),
    slide,
    coordinates = coords,
    embeddings = embeddings,
    spot_genes = "GeneA",
    default_gene = "GeneA"
  )

  expect_s3_class(linked, "wsi_spatial_object")
  expect_s3_class(linked, "wsi_seurat_spatial")
  expect_equal(linked$source_name, "Giotto")
  expect_equal(nrow(linked$spots), 4)
  expect_equal(linked$component_names, c("PC_1", "PC_2"))
  expect_true(linked$gene_expression$enabled)
})

test_that("SpatialExperiment-like objects can be linked with explicit coordinates", {
  slide <- wsiTools:::wsi_mock_slide(width = 1000, height = 800)
  ids <- paste0("spot", 1:3)
  coords <- data.frame(
    barcode = ids,
    x = c(10, 20, 30),
    y = c(40, 50, 60)
  )
  embeddings <- matrix(
    c(1, 2, 3, 3, 2, 1),
    ncol = 2,
    dimnames = list(ids, c("UMAP_1", "UMAP_2"))
  )

  linked <- wsi_link_spatialexperiment_image(
    list(),
    slide,
    coordinates = coords,
    embeddings = embeddings,
    reduction = "UMAP"
  )

  expect_s3_class(linked, "wsi_spatial_object")
  expect_equal(linked$source_name, "SpatialExperiment")
  expect_equal(nrow(linked$spots), 3)
  expect_equal(linked$component_names, c("UMAP_1", "UMAP_2"))
})
