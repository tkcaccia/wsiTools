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

test_that("10x tissue positions and scalefactors align Seurat spots to full-resolution images", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(10, 20),
          imagerow = c(5, 15)
        ),
        image = array(0, dim = c(50, 100, 3)),
        scale.factors = list(lowres = 0.1, spot = 20)
      )
    )
  )
  spatial_dir <- tempfile("spatial")
  dir.create(spatial_dir)
  writeLines(
    '{"spot_diameter_fullres": 80, "tissue_lowres_scalef": 0.1}',
    file.path(spatial_dir, "scalefactors_json.json")
  )
  utils::write.table(
    data.frame(
      barcode = c("spot_a", "spot_b", "other"),
      in_tissue = c(1, 1, 0),
      array_row = c(0, 0, 0),
      array_col = c(0, 1, 2),
      pxl_row_in_fullres = c(120, 360, 999),
      pxl_col_in_fullres = c(240, 480, 999)
    ),
    file.path(spatial_dir, "tissue_positions.csv"),
    sep = ",",
    row.names = FALSE,
    quote = FALSE
  )
  slide <- wsi_mock_slide(width = 1000, height = 500, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide, spatial_dir = spatial_dir)

  expect_equal(linked$coordinate_mapping$method, "auto_fullres")
  expect_equal(linked$coordinate_mapping$scale_x, 1)
  expect_equal(linked$coordinate_mapping$scale_y, 1)
  expect_equal(linked$spots$x, c(240, 480))
  expect_equal(linked$spots$y, c(120, 360))
  expect_equal(linked$spot_radius, 40)
})

test_that("Seurat object scale factors are used before external spatial files", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(240, 480),
          imagerow = c(120, 360)
        ),
        image = array(0, dim = c(100, 100, 3)),
        scale.factors = list(lowres = 0.1, spot = 80)
      )
    )
  )
  slide <- wsi_mock_slide(width = 1000, height = 1000, levels = c(1, 2))

  linked <- wsi_link_seurat_image(seurat_like, slide)

  expect_equal(linked$coordinate_mapping$method, "auto_fullres")
  expect_equal(linked$spots$x, c(240, 480))
  expect_equal(linked$spots$y, c(120, 360))
  expect_equal(linked$spot_radius, 40)
})

test_that("Seurat spot coordinates can be transformed with x1 equals y and y1 equals negative x", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(100, 250),
          imagerow = c(20, 80)
        ),
        image = array(0, dim = c(100, 300, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 300, height = 100, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "x_y_y_neg_x"
  )

  expect_equal(linked$coordinate_mapping$coordinate_transform, "x_y_y_neg_x")
  expect_equal(linked$spots$x, c(20, 80))
  expect_equal(linked$spots$y, c(200, 50))

  alias <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "rotate_90_cw"
  )
  expect_equal(alias$spots$x, linked$spots$x)
  expect_equal(alias$spots$y, linked$spots$y)
})

test_that("Seurat spot coordinates can be flipped vertically", {
  embeddings <- matrix(
    c(-2, 0.5, 1, -0.5),
    ncol = 2,
    byrow = TRUE,
    dimnames = list(c("spot_a", "spot_b"), c("PC_1", "PC_2"))
  )
  seurat_like <- list(
    reductions = list(pca = list(cell.embeddings = embeddings)),
    images = list(
      anterior1 = list(
        coordinates = data.frame(
          barcode = c("spot_a", "spot_b"),
          imagecol = c(100, 250),
          imagerow = c(20, 80)
        ),
        image = array(0, dim = c(100, 300, 3))
      )
    )
  )
  slide <- wsi_mock_slide(width = 300, height = 100, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "flip_y"
  )

  expect_equal(linked$coordinate_mapping$coordinate_transform, "flip_y")
  expect_equal(linked$spots$x, c(100, 250))
  expect_equal(linked$spots$y, c(80, 20))

  alias <- wsi_link_seurat_image(
    seurat_like,
    slide,
    coordinate_scale = "none",
    coordinate_transform = "y_neg_y"
  )
  expect_equal(alias$spots$x, linked$spots$x)
  expect_equal(alias$spots$y, linked$spots$y)
})

test_that("missing scalefactors path can be recovered from a spatial directory", {
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
  spatial_dir <- tempfile("spatial")
  dir.create(spatial_dir)
  writeLines(
    '{"tissue_lowres_scalef": 0.1, "spot_diameter_fullres": 60}',
    file.path(spatial_dir, "prefix_scalefactors_json.json")
  )
  slide <- wsi_mock_slide(width = 1000, height = 1000, levels = c(1, 2))

  linked <- wsi_link_seurat_image(
    seurat_like,
    slide,
    scalefactors_json = file.path(spatial_dir, "scalefactors_json.json")
  )

  expect_equal(linked$coordinate_mapping$method, "auto_lowres")
  expect_equal(linked$spots$x, c(200, 800))
  expect_equal(linked$spots$y, c(300, 700))
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
