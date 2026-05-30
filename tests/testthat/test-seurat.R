test_that("Seurat spatial objects can be linked to high-resolution slide coordinates", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5, 2, 1.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b", "spot_c"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b", "spot_c"),
          imagecol = c(10, 20, 30),
          imagerow = c(5, 15, 25)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(spot = 12)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide)

  expect_s3_class(linked, "wsi_seurat_spatial")
  expect_equal(linked$image_name, "anterior1")
  expect_equal(linked$coordinate_mapping$method, "auto_seurat_image")
  expect_equal(linked$coordinate_mapping$scale_x, 10)
  expect_equal(linked$coordinate_mapping$scale_y, 10)
  expect_equal(linked$spots$x, c(100, 200, 300))
  expect_equal(linked$spots$y, c(50, 150, 250))
  expect_equal(linked$pca$x_label, "PC_1")
  expect_equal(linked$pca$y_label, "PC_2")
  expect_equal(nrow(linked$pca$points), 3)

  layer <- wsiTools:::wsi_seurat_spots_layer(linked)
  expect_equal(layer$id, "seurat_spots")
  expect_equal(layer$source_type, "seurat_spots")
  expect_equal(length(layer$items), 3)
  expect_true(length(unique(vapply(layer$items, `[[`, character(1), "colour"))) > 1)
})

test_that("Seurat viewer exposes spot layer and PCA controls", {
  embeddings <- matrix(
    c(-1, 0, 1, 1),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("a", "b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(slice = list(
      coordinates = data.frame(barcode = c("a", "b"), imagecol = c(20, 80), imagerow = c(30, 70)),
      image = array(0, dim = c(100, 100, 3))
    ))
  )
  slide <- wsi_mock_slide(width = 100, height = 100, levels = c(1, 2))
  linked <- wsi_link_seurat_image(seurat_like, slide, coordinate_scale = "none", spot_radius = 4)
  html <- tempfile(fileext = ".html")

  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    layers = list(wsiTools:::wsi_seurat_spots_layer(linked)),
    seurat = linked
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")

  expect_match(text, "Seurat", fixed = TRUE)
  expect_match(text, "seurat_spots", fixed = TRUE)
  expect_match(text, "seuratPlotWindow", fixed = TRUE)
  expect_match(text, "bindSeuratControls", fixed = TRUE)
  expect_match(text, "seuratSelectionPayload", fixed = TRUE)
  expect_match(text, "Draw a lasso around PCA points", fixed = TRUE)
})

test_that("Seurat selection events are accepted by the live bridge validator", {
  payload <- wsiTools:::wsi_viewer_validate_state_payload(list(
    event = "seurat_spots_selected",
    sequence = 1,
    seurat_selection = list(labels = list("a", "b"), count = 2, matched_count = 2)
  ))

  expect_equal(payload$event, "seurat_spots_selected")
})
