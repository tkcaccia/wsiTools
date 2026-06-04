test_that("prediction config is enabled for spatial viewers", {
  slide <- wsiTools:::wsi_mock_slide()
  linked <- list(
    source_name = "Seurat",
    image_name = "mock",
    reduction = "pca",
    dims = c(1L, 2L),
    component_names = c("PC1", "PC2"),
    spot_count = 2L,
    displayed_spot_count = 2L,
    spot_radius = 10,
    plots = list(list(
      id = "seurat_pca",
      label = "Seurat PCA plot",
      reduction = "pca",
      x_label = "PC1",
      y_label = "PC2",
      points = data.frame(
        label = c("s1", "s2"),
        spot_id = c("s1", "s2"),
        x = c(0, 1),
        y = c(1, 0),
        slide_x = c(100, 200),
        slide_y = c(100, 200),
        colour = c("#111111", "#222222"),
        stringsAsFactors = FALSE
      )
    )),
    gene_expression = list(enabled = FALSE, genes = character(), default_gene = NULL),
    clusters = list(enabled = FALSE, fields = list()),
    spots = data.frame(
      id = c("s1", "s2"),
      label = c("s1", "s2"),
      x = c(100, 200),
      y = c(100, 200),
      stringsAsFactors = FALSE
    ),
    slide = slide
  )
  class(linked) <- c("wsi_seurat_spatial", "list")
  config <- wsiTools:::wsi_prediction_config(
    wsiTools:::wsi_viewer_seurat_config(linked),
    list(enabled = FALSE)
  )
  expect_true(config$enabled)
  expect_true("spatial:raw" %in% vapply(config$sources, `[[`, character(1), "id"))
  expect_true(any(grepl("^spatial:reduction:", vapply(config$sources, `[[`, character(1), "id"))))
})

test_that("prediction menu is only rendered for managed analysis sources", {
  slide <- wsiTools:::wsi_mock_slide()
  linked <- list(
    source_name = "Seurat",
    image_name = "mock",
    reduction = "pca",
    dims = c(1L, 2L),
    component_names = c("PC1", "PC2"),
    spot_count = 1L,
    displayed_spot_count = 1L,
    spot_radius = 10,
    plots = list(),
    gene_expression = list(enabled = FALSE, genes = character(), default_gene = NULL),
    clusters = list(enabled = FALSE, fields = list()),
    spots = data.frame(id = "s1", label = "s1", x = 100, y = 100),
    slide = slide
  )
  class(linked) <- c("wsi_seurat_spatial", "list")

  out <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = out, open = FALSE, overwrite = TRUE, seurat = linked)
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_true(grepl("Prediction", html, fixed = TRUE))
  expect_true(grepl("predictionWindow", html, fixed = TRUE))

  out2 <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = out2, open = FALSE, overwrite = TRUE)
  html2 <- paste(readLines(out2, warn = FALSE), collapse = "\n")
  expect_false(grepl("PLS-LDA annotation prediction", html2, fixed = TRUE))
})

test_that("prediction request validation rejects arbitrary fields", {
  expect_error(
    wsiTools:::wsi_prediction_validate_payload(list(feature_source = "spatial:raw", code = "system('date')")),
    "unsupported field"
  )
})

test_that("prediction assignment maps points to selected ROI classes", {
  geojson <- list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "roi_tumour",
        properties = list(name = "Tumour train", classification = list(name = "tumour")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(20, 0), c(20, 20), c(0, 20), c(0, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "roi_stroma",
        properties = list(name = "Stroma train", classification = list(name = "stroma")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(30, 0), c(50, 0), c(50, 20), c(30, 20), c(30, 0)
        )))
      )
    )
  )
  rois <- wsiTools:::wsi_roi_from_geojson(geojson)
  points <- data.frame(
    id = c("a", "b", "c"),
    label = c("a", "b", "c"),
    x = c(10, 40, 80),
    y = c(10, 10, 10),
    stringsAsFactors = FALSE
  )
  mapped <- wsiTools:::wsi_prediction_assign_points(
    points,
    rois,
    ids = c("roi_tumour", "roi_stroma")
  )
  expect_equal(mapped$label, c("tumour", "stroma", NA))
})

test_that("prediction feature filtering removes zero variance and limits features", {
  x <- cbind(
    constant = c(1, 1, 1, 1),
    useful1 = c(1, 2, 3, 4),
    useful2 = c(4, 3, 2, 1),
    useful3 = c(1, 3, 1, 3)
  )
  out <- wsiTools:::wsi_prediction_feature_filter(x, max_features = 2)
  expect_equal(ncol(out), 2)
  expect_false("constant" %in% colnames(out))
})
