test_that("proximity config is enabled for spatial and CellPhenotyper viewers", {
  spatial_config <- list(enabled = TRUE, source_name = "Seurat", spot_count = 4L)
  cell_config <- list(enabled = TRUE, cell_count = 3L)

  spatial <- wsiTools:::wsi_proximity_config(spatial_config, list(enabled = FALSE))
  expect_true(spatial$enabled)
  expect_equal(spatial$sources[[1]]$id, "spatial:points")

  cells <- wsiTools:::wsi_proximity_config(list(enabled = FALSE), cell_config)
  expect_true(cells$enabled)
  expect_equal(cells$sources[[1]]$id, "cellphenotyper:cells")
})

test_that("proximity request validation rejects arbitrary fields", {
  expect_error(
    wsiTools:::wsi_proximity_validate_payload(list(point_source = "spatial:points", code = "system('date')")),
    "unsupported field"
  )
})

test_that("proximity analysis computes nearest target spot distances", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "query_roi",
        properties = list(name = "Desmoplastic submucosa", classification = list(name = "desmoplastic")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "target_roi",
        properties = list(name = "Invasive carcinoma", classification = list(name = "carcinoma")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(9, 0), c(14, 0), c(14, 4), c(9, 4), c(9, 0)
        )))
      )
    )
  ))
  linked <- list(
    source_name = "Seurat",
    spots = data.frame(
      id = c("q1", "q2", "t1", "t2", "outside"),
      label = c("q1", "q2", "t1", "t2", "outside"),
      x = c(1, 2, 10, 12, 50),
      y = c(1, 2, 1, 1, 50),
      stringsAsFactors = FALSE
    )
  )
  class(linked) <- c("wsi_spatial_object", "list")

  result <- wsiTools:::wsi_proximity_run(
    context = list(spatial = linked),
    rois = rois,
    point_source = "spatial:points",
    query_ids = "query_roi",
    target_ids = "target_roi",
    pixel_size = c(x = 0.5, y = 0.5)
  )

  expect_s3_class(result, "wsi_proximity_result")
  expect_equal(nrow(result), 2L)
  expect_equal(result$id, c("q1", "q2"))
  expect_equal(result$nearest_target_id[[1]], "t1")
  expect_equal(result$distance_px[[1]], 9)
  expect_equal(result$distance_um[[1]], 4.5)
})

test_that("proximity response stores result and queues viewer layer", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "query_roi",
        properties = list(name = "Query", classification = list(name = "query")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "target_roi",
        properties = list(name = "Target", classification = list(name = "target")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(10, 0), c(15, 0), c(15, 5), c(10, 5), c(10, 0)
        )))
      )
    )
  ))
  linked <- list(
    spots = data.frame(
      id = c("q1", "t1"),
      label = c("q1", "t1"),
      x = c(1, 10),
      y = c(1, 1),
      stringsAsFactors = FALSE
    )
  )
  class(linked) <- c("wsi_spatial_object", "list")
  state <- wsiTools:::wsi_new_viewer_state(envir = new.env(parent = emptyenv()))

  response <- wsiTools:::wsi_proximity_response(
    context = list(spatial = linked),
    state = state,
    payload = list(
      point_source = "spatial:points",
      query_annotations = "query_roi",
      target_annotations = "target_roi",
      rois = wsiTools:::wsi_viewer_rois_to_geojson(rois)
    )
  )

  expect_equal(nrow(state$proximity), 1L)
  expect_equal(response$proximity$count, 1L)
  expect_true(any(vapply(response$commands, function(x) identical(x$type, "add_layer"), logical(1))))
})

test_that("proximity controls are rendered only for managed point sources", {
  slide <- wsiTools:::wsi_mock_slide()
  linked <- list(
    source_name = "Seurat",
    image_name = "mock",
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
  expect_match(html, "Proximity analysis", fixed = TRUE)
  expect_match(html, "runProximityAnalysis", fixed = TRUE)

  out2 <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = out2, open = FALSE, overwrite = TRUE)
  html2 <- paste(readLines(out2, warn = FALSE), collapse = "\n")
  expect_false(grepl("id=\"runProximityAnalysis\"", html2, fixed = TRUE))
})
