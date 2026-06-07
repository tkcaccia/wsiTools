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
      dimension_count = 4L,
      component_names = paste0("PC", 1:4),
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
  expect_true(is.logical(config$svm_refinement_installed))
  expect_match(config$svm_refinement_install, "e1071", fixed = TRUE)
  expect_true("spatial:raw" %in% vapply(config$sources, `[[`, character(1), "id"))
  expect_true(any(grepl("^spatial:reduction:", vapply(config$sources, `[[`, character(1), "id"))))
  reduction_source <- config$sources[[which(grepl(
    "^spatial:reduction:",
    vapply(config$sources, `[[`, character(1), "id")
  ))[[1L]]]]
  expect_equal(reduction_source$dimension_count, 4L)
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
  expect_true(grepl("predictionReductionDims", html, fixed = TRUE))
  expect_true(grepl("syncPredictionReductionDimsControl", html, fixed = TRUE))
  expect_true(grepl("reduction_dims", html, fixed = TRUE))
  expect_true(grepl("predictionRefineSvm", html, fixed = TRUE))
  expect_true(grepl("refine_svm", html, fixed = TRUE))
  expect_true(grepl("predictionProjectAnnotationEntries", html, fixed = TRUE))
  expect_true(grepl("predictionRoiGeojsonObject", html, fixed = TRUE))
  expect_true(grepl("wsiToolsProject", html, fixed = TRUE))

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
  expect_silent(wsiTools:::wsi_prediction_validate_payload(list(
    feature_source = "spatial:reduction:0",
    reduction_dims = 3L,
    refine_svm = TRUE
  )))
})

test_that("internal SVM refinement preserves label structure", {
  skip_if_not_installed("e1071")
  x <- rbind(
    a1 = c(0, 0),
    a2 = c(0.1, 0.2),
    b1 = c(5, 5),
    b2 = c(5.2, 4.9)
  )
  labels <- c("tumour", "tumour", "stroma", "stroma")
  out <- wsiTools:::wsi_refine_SVM(x, labels, kernel = "linear", scale = FALSE)
  expect_s3_class(out, "factor")
  expect_named(out, rownames(x))
  expect_equal(levels(out), sort(unique(labels)))
  expect_equal(length(out), nrow(x))
})

test_that("SVM prediction refinement uses spatial coordinates and section groups", {
  skip_if_not_installed("e1071")
  points <- data.frame(
    id = paste0("p", 1:8),
    x = c(0, 0.1, 4.9, 5.1, 0, 0.1, 4.9, 5.1),
    y = c(0, 0.2, 0.1, 0.2, 10, 10.2, 10.1, 10.2),
    project_key = rep(c("section_a", "section_b"), each = 4),
    stringsAsFactors = FALSE
  )
  xy <- wsiTools:::wsi_prediction_spatial_refinement_matrix(points, c(4L, 1L))
  expect_equal(unname(xy[, "x"]), c(5.1, 0))
  expect_equal(rownames(xy), c("p4", "p1"))
  samples <- wsiTools:::wsi_prediction_refinement_samples(points, seq_len(nrow(points)))
  expect_equal(samples, rep(c("section_a", "section_b"), each = 4))

  refined <- wsiTools:::wsi_prediction_apply_svm_refinement(
    points = points,
    train_rows = c(1L, 4L, 5L, 8L),
    test_rows = c(2L, 3L, 6L, 7L),
    y_train = c("left", "right", "left", "right"),
    predicted = c("left", "right", "left", "right")
  )
  expect_equal(length(refined), 4L)
  expect_true(all(refined %in% c("left", "right")))
})

test_that("prediction reduction sources can use requested dimensions", {
  ids <- paste0("s", 1:5)
  embeddings <- matrix(
    seq_len(20),
    nrow = 5,
    dimnames = list(ids, paste0("PC_", 1:4))
  )
  linked <- list(
    source_name = "Seurat",
    image_name = "mock",
    reduction = "pca",
    reduction_embeddings = embeddings,
    reduction_embedding_name = "pca",
    spots = data.frame(
      id = ids,
      label = ids,
      x = seq(10, 50, 10),
      y = seq(10, 50, 10),
      stringsAsFactors = FALSE
    ),
    plots = list(list(
      id = "seurat_pca",
      label = "Seurat PCA plot",
      reduction = "pca",
      dimension_count = 4L,
      points = data.frame(
        spot_id = ids,
        label = ids,
        x = embeddings[, 1L],
        y = embeddings[, 2L],
        stringsAsFactors = FALSE
      )
    ))
  )
  class(linked) <- c("wsi_seurat_spatial", "list")
  x <- wsiTools:::wsi_prediction_feature_matrix(
    wsi_prediction_context(spatial = linked),
    "spatial:reduction:0",
    ids,
    reduction_dims = 3L
  )
  expect_equal(dim(x), c(5L, 3L))
  expect_equal(colnames(x), paste0("PC_", 1:3))
  expect_equal(attr(x, "reduction_dimensions"), 3L)
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

test_that("prediction assignment respects project slide scopes", {
  geojson <- list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "slide_a::roi_tumour",
        properties = list(
          name = "Tumour on slide A",
          classification = list(name = "tumour"),
          project_key = "slide_a::image",
          project_image = "slide_a"
        ),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(20, 0), c(20, 20), c(0, 20), c(0, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "slide_b::roi_stroma",
        properties = list(
          name = "Stroma on slide B",
          classification = list(name = "stroma"),
          project_key = "slide_b::image",
          project_image = "slide_b"
        ),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(20, 0), c(20, 20), c(0, 20), c(0, 0)
        )))
      )
    )
  )
  rois <- wsiTools:::wsi_roi_from_geojson(geojson)
  points <- data.frame(
    id = c("a1", "b1", "c1"),
    label = c("a1", "b1", "c1"),
    x = c(10, 10, 10),
    y = c(10, 10, 10),
    project_key = c("slide_a::image", "slide_b::image", "slide_c::image"),
    project_image = c("slide_a", "slide_b", "slide_c"),
    stringsAsFactors = FALSE
  )
  mapped <- wsiTools:::wsi_prediction_assign_points(
    points,
    rois,
    ids = c("slide_b::roi_stroma")
  )
  expect_equal(mapped$label, c(NA, "stroma", NA))
  expect_equal(mapped$roi_id, c(NA, "slide_b::roi_stroma", NA))
})

test_that("prediction project context keeps ROI scopes tied to each tissue", {
  slide <- wsiTools:::wsi_mock_slide(width = 120, height = 100, levels = c(1, 2))
  make_linked <- function(name, x_offset = 0) {
    linked <- list(
      slide = slide,
      image_path = paste0(name, ".tif"),
      image_name = name,
      source_name = "SpatialExperiment",
      reduction = "PCA",
      dims = c(1L, 2L),
      component_names = c("PC_1", "PC_2"),
      coordinate_mapping = list(method = "none"),
      spot_radius = 5,
      spot_count = 2L,
      displayed_spot_count = 2L,
      gene_expression = list(enabled = FALSE, genes = character(), values = NULL),
      spots = data.frame(
        id = c("spot1", "spot2"),
        label = c("spot1", "spot2"),
        barcode = c("spot1", "spot2"),
        x = c(10 + x_offset, 80 + x_offset),
        y = c(10, 80),
        PC_1 = c(-1, 1),
        PC_2 = c(0, 2),
        colour = c("#2B6CB0", "#2B6CB0"),
        base_colour = c("#2B6CB0", "#2B6CB0"),
        stringsAsFactors = FALSE
      ),
      reduction_embeddings = matrix(
        c(-1, 0, 1, 2),
        nrow = 2,
        dimnames = list(c("spot1", "spot2"), c("PC_1", "PC_2"))
      ),
      reduction_embedding_name = "PCA",
      plots = list(list(
        id = "spe_pca",
        label = "SpatialExperiment PCA plot",
        reduction = "PCA",
        x_label = "PC_1",
        y_label = "PC_2",
        points = data.frame(
          label = c("spot1", "spot2"),
          spot_id = c("spot1", "spot2"),
          x = c(-1, 1),
          y = c(0, 2),
          slide_x = c(10 + x_offset, 80 + x_offset),
          slide_y = c(10, 80),
          stringsAsFactors = FALSE
        )
      ))
    )
    class(linked) <- c("wsi_seurat_spatial", "wsi_spatial_object", "list")
    linked
  }
  records <- list(
    list(id = "seurat_project_section1", label = "section1", path = "section1.tif"),
    list(id = "seurat_project_section2", label = "section2", path = "section2.tif")
  )
  project_linked <- wsiTools:::wsi_seurat_project_prediction_context(
    list(make_linked("section1"), make_linked("section2")),
    records
  )
  points <- wsiTools:::wsi_prediction_points(
    wsi_prediction_context(spatial = project_linked),
    "spatial:reduction:0"
  )
  expect_s3_class(project_linked, "wsi_spatial_project")
  expect_equal(sort(unique(points$project_key)), c("seurat_project_section1::image", "seurat_project_section2::image"))
  expect_true(any(grepl("^seurat_project_section2::image::", points$id)))
  expect_equal(points$feature_id[points$project_key == "seurat_project_section2::image"], c("spot1", "spot2"))

  geojson <- list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "seurat_project_section2::image::roi_tumour",
        properties = list(
          name = "Tumour on section 2",
          classification = list(name = "tumour"),
          project_key = "seurat_project_section2::image",
          project_image = "section2"
        ),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(30, 0), c(30, 30), c(0, 30), c(0, 0)
        )))
      )
    )
  )
  rois <- wsiTools:::wsi_roi_from_geojson(geojson)
  mapped <- wsiTools:::wsi_prediction_assign_points(
    points,
    rois,
    ids = "seurat_project_section2::image::roi_tumour"
  )
  expect_equal(mapped$label, c(NA, NA, "tumour", NA))

  x <- wsiTools:::wsi_prediction_feature_matrix(
    wsi_prediction_context(spatial = project_linked),
    "spatial:reduction:0",
    points$id,
    reduction_dims = 2L,
    points = points
  )
  expect_equal(rownames(x), points$id)
  expect_equal(nrow(x), nrow(points))
  expect_equal(ncol(x), 2L)
})

test_that("prediction layer items retain project scope metadata", {
  result <- data.frame(
    id = "section2::spot1",
    label = "spot1",
    x = 10,
    y = 10,
    set = "test",
    train_annotation_id = NA_character_,
    test_annotation_id = "roi",
    observed = NA_character_,
    predicted = "tumour",
    predicted_pls_lda = "tumour",
    svm_refined = FALSE,
    feature_source = "spatial:reduction:0",
    project_key = "seurat_project_section2::image",
    project_image = "section2",
    project_image_index = 1L,
    project_section_index = -1L,
    feature_id = "spot1",
    stringsAsFactors = FALSE
  )
  layer <- wsiTools:::wsi_prediction_layer(result)
  expect_true(layer$items[[1]]$project_scoped)
  expect_equal(layer$items[[1]]$project_key, "seurat_project_section2::image")
  expect_equal(layer$items[[1]]$project_image_index, 1L)
  expect_equal(layer$items[[1]]$feature_id, "spot1")
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
