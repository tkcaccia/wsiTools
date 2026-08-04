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

test_that("proximity colours use empirical quantile positions", {
  position <- wsiTools:::wsi_proximity_quantile_position(c(0, 1, 2, 100, NA))
  expect_equal(position[1:4], c(0, 1 / 3, 2 / 3, 1))
  expect_true(is.na(position[[5]]))
  expect_equal(
    wsiTools:::wsi_proximity_quantile_position(c(4, 4, 4)),
    rep(0.5, 3)
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

test_that("proximity analysis supports annotation category selectors", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "tumour_a",
        properties = list(name = "Tumour A", classification = list(name = "tumour")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "tumour_b",
        properties = list(name = "Tumour B", classification = list(name = "tumour")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 5), c(3, 5), c(3, 8), c(0, 8), c(0, 5)
        )))
      ),
      list(
        type = "Feature",
        id = "stroma_a",
        properties = list(name = "Stroma A", classification = list(name = "stroma")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(10, 0), c(13, 0), c(13, 8), c(10, 8), c(10, 0)
        )))
      )
    )
  ))
  linked <- list(
    source_name = "Seurat",
    spots = data.frame(
      id = c("t1", "t2", "s1", "outside"),
      label = c("t1", "t2", "s1", "outside"),
      x = c(1, 1, 11, 30),
      y = c(1, 6, 1, 30),
      stringsAsFactors = FALSE
    )
  )
  class(linked) <- c("wsi_spatial_object", "list")

  result <- wsiTools:::wsi_proximity_run(
    context = list(spatial = linked),
    rois = rois,
    point_source = "spatial:points",
    query_ids = "class:tumour",
    target_ids = "class:stroma"
  )

  expect_equal(nrow(result), 2L)
  expect_setequal(result$query_annotation_id, c("tumour_a", "tumour_b"))
  expect_true(all(result$query_class == "tumour"))
  expect_true(all(result$target_class == "stroma"))
})

test_that("proximity analysis supports stable viewer ROI index selectors", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "duplicate",
        properties = list(name = "Region", classification = list(name = "query")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(3, 0), c(3, 3), c(0, 3), c(0, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "duplicate",
        properties = list(name = "Region", classification = list(name = "target")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(10, 0), c(13, 0), c(13, 3), c(10, 3), c(10, 0)
        )))
      )
    )
  ))
  linked <- list(
    spots = data.frame(
      id = c("q", "t"),
      label = c("q", "t"),
      x = c(1, 11),
      y = c(1, 1),
      stringsAsFactors = FALSE
    )
  )
  class(linked) <- c("wsi_spatial_object", "list")

  result <- wsiTools:::wsi_proximity_run(
    context = list(spatial = linked),
    rois = rois,
    point_source = "spatial:points",
    query_ids = "roi_index:0",
    target_ids = "roi_index:1"
  )

  expect_equal(result$id, "q")
  expect_equal(result$nearest_target_id, "t")
  expect_equal(result$query_class, "query")
  expect_equal(result$target_class, "target")
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
  layer <- response$commands[[which(vapply(response$commands, function(x) identical(x$type, "add_layer"), logical(1)))[[1]]]]$payload$layer
  expect_equal(layer$legend$title, "Distance to reference (quantile)")
  expect_equal(layer$legend$unit, "px")
  expect_equal(layer$legend$scale, "quantile")
  expect_length(layer$legend$stops, 3L)
  expect_named(layer$legend$stops[[1]], c("name", "value", "distance_px", "colour"))
  expect_true(layer$metadata$vector_rendering)
  expect_true(layer$metadata$coordinate_overlay)
  expect_true(layer$metadata$lod$full_coordinates)
})

test_that("proximity uses full analysis polygons instead of compact display ROIs", {
  full <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature", id = "query_roi",
        properties = list(name = "Query", classification = list(name = "query")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)
        )))
      ),
      list(
        type = "Feature", id = "target_roi",
        properties = list(name = "Target", classification = list(name = "target")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(9, 0), c(13, 0), c(13, 4), c(9, 4), c(9, 0)
        )))
      )
    )
  ))
  compact <- full
  compact$geometry_type <- "Point"
  compact$coordinates <- I(list(c(0, 0), c(9, 0)))
  linked <- list(spots = data.frame(
    id = c("q", "t"), label = c("q", "t"),
    x = c(1, 10), y = c(1, 1), stringsAsFactors = FALSE
  ))
  class(linked) <- c("wsi_spatial_object", "list")
  state <- wsiTools:::wsi_new_viewer_state(envir = new.env(parent = emptyenv()))
  state$rois <- compact
  state$analysis_rois <- full

  response <- wsiTools:::wsi_proximity_result_from_payload(
    context = list(spatial = linked),
    state = state,
    payload = list(
      point_source = "spatial:points",
      query_annotations = "query_roi",
      target_annotations = "target_roi"
    )
  )

  expect_equal(response$result$id, "q")
  expect_equal(response$result$nearest_target_id, "t")
  expect_equal(state$rois$geometry_type, c("Point", "Point"))
})

test_that("proximity reuses automatic annotation membership for imported ROIs", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature", id = "query_roi",
        properties = list(name = "Query", classification = list(name = "query")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0, 0), c(4, 0), c(4, 4), c(0, 4), c(0, 0)
        )))
      ),
      list(
        type = "Feature", id = "target_roi",
        properties = list(name = "Target", classification = list(name = "target")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(9, 0), c(13, 0), c(13, 4), c(9, 4), c(9, 0)
        )))
      )
    )
  ))
  linked <- list(spots = data.frame(
    id = c("q", "t"), label = c("q", "t"),
    x = c(1000, 2000), y = c(1000, 1000), stringsAsFactors = FALSE
  ))
  class(linked) <- c("wsi_spatial_object", "list")
  association <- data.frame(
    annotation_index = c(1L, 2L),
    annotation_id = c("query_roi", "target_roi"),
    spot_id = c("q", "t"),
    stringsAsFactors = FALSE
  )

  result <- wsiTools:::wsi_proximity_run(
    context = list(spatial = linked), rois = rois,
    point_source = "spatial:points",
    query_ids = "class:query", target_ids = "class:target",
    association = association
  )

  expect_equal(result$id, "q")
  expect_equal(result$nearest_target_id, "t")
  expect_equal(result$distance_px, 1000)
})

test_that("proximity resolves overlapping selected classes absent from first-match membership", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(type = "Feature", id = "first", properties = list(class = "first"),
           geometry = list(type = "Polygon", coordinates = list(list(
             c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0)
           )))),
      list(type = "Feature", id = "second", properties = list(class = "second"),
           geometry = list(type = "Polygon", coordinates = list(list(
             c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0)
           ))))
    )
  ))
  points <- data.frame(id = "p", label = "p", x = 2, y = 2)
  association <- data.frame(
    annotation_index = 1L, annotation_id = "first", spot_id = "p"
  )

  selected <- wsiTools:::wsi_proximity_assignment_from_table(
    points, rois, "class:second", association
  )

  expect_equal(selected$roi_id, "second")
  expect_equal(selected$label, "second")
})

test_that("explicit annotation association stores spatial membership in live state", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature", id = "tumour",
      properties = list(name = "Tumour", classification = list(name = "tumour")),
      geometry = list(type = "Polygon", coordinates = list(list(
        c(0, 0), c(5, 0), c(5, 5), c(0, 5), c(0, 0)
      )))
    ))
  ))
  linked <- list(spots = data.frame(
    id = c("inside", "outside"), label = c("inside", "outside"),
    x = c(2, 8), y = c(2, 8), stringsAsFactors = FALSE
  ))
  class(linked) <- c("wsi_spatial_object", "list")
  state <- wsiTools:::wsi_new_viewer_state(envir = new.env(parent = emptyenv()))
  state$analysis_rois <- rois

  response <- wsiTools:::wsi_proximity_response(
    context = list(spatial = linked), state = state,
    payload = list(action = "associate", point_source = "spatial:points")
  )

  expect_equal(response$annotation_association$assigned_count, 1L)
  expect_equal(response$annotation_association$count, 2L)
  expect_equal(nrow(state$annotation_spots), 1L)
  expect_equal(state$annotation_spots$annotation_id, "tumour")
  expect_equal(state$annotation_spots$spot_id, "inside")
})

test_that("proximity statistics rank live feature trends and sync state", {
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "query_roi",
        properties = list(name = "Query", classification = list(name = "query")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(0.5, 0), c(4.5, 0), c(4.5, 2), c(0.5, 2), c(0.5, 0)
        )))
      ),
      list(
        type = "Feature",
        id = "target_roi",
        properties = list(name = "Target", classification = list(name = "target")),
        geometry = list(type = "Polygon", coordinates = list(list(
          c(-1, 0), c(0.1, 0), c(0.1, 2), c(-1, 2), c(-1, 0)
        )))
      )
    )
  ))
  project <- list(
    cells = data.frame(
      id = c("q1", "q2", "q3", "q4", "t1"),
      x = c(1, 2, 3, 4, 0),
      y = c(1, 1, 1, 1, 1),
      DAB = c(1, 2, 3, 4, 0),
      hematoxylin = c(4, 3, 2, 1, 2),
      stringsAsFactors = FALSE
    )
  )
  class(project) <- c("wsi_cellphenotyper_project", "list")
  state <- wsiTools:::wsi_new_viewer_state(envir = new.env(parent = emptyenv()))

  response <- wsiTools:::wsi_proximity_response(
    context = list(cellphenotyper_project = project),
    state = state,
    payload = list(
      action = "stats",
      point_source = "cellphenotyper:cells",
      feature_source = "cellphenotyper:numeric",
      method = "spearman",
      quantile_step = 0.25,
      query_annotations = "query_roi",
      target_annotations = "target_roi",
      rois = wsiTools:::wsi_viewer_rois_to_geojson(rois)
    )
  )

  expect_s3_class(state$proximity_stats, "wsi_proximity_stats_result")
  expect_equal(response$proximity_stats$count, nrow(state$proximity_stats))
  expect_true("DAB" %in% state$proximity_stats$feature)
  dab <- state$proximity_stats[state$proximity_stats$feature == "DAB", , drop = FALSE]
  expect_equal(dab$correlation[[1]], 1)
  expect_equal(state$proximity_stats$feature_source[[1]], "cellphenotyper:numeric")
})

test_that("proximity statistics features can be fetched for viewer colouring", {
  project <- list(
    cells = data.frame(
      id = c("cell_1", "cell_2", "cell_3"),
      x = c(10, 20, 30),
      y = c(15, 25, 35),
      DAB = c(0.1, 0.5, 0.9),
      stringsAsFactors = FALSE
    )
  )
  class(project) <- c("wsi_cellphenotyper_project", "list")

  payload <- wsiTools:::wsi_prediction_feature_payload(
    context = list(cellphenotyper_project = project),
    feature = "dab",
    source_id = "cellphenotyper:numeric",
    point_source = "cellphenotyper:cells"
  )

  expect_true(payload$ok)
  expect_equal(payload$gene, "DAB")
  expect_equal(payload$feature_type, "cell")
  expect_equal(payload$feature_source, "cellphenotyper:numeric")
  expect_equal(payload$count, 3)
  expect_equal(vapply(payload$points, `[[`, numeric(1), "value"), c(0.1, 0.5, 0.9))
  expect_true(all(grepl("^#", vapply(payload$points, `[[`, character(1), "colour"))))
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
  expect_match(html, "id=\"proximityColourVisible\"", fixed = TRUE)
  expect_match(html, "function setProximityColourVisible", fixed = TRUE)
  expect_match(html, "proximityCurrentSource", fixed = TRUE)
  expect_match(html, "syncProximityAnnotations", fixed = TRUE)
  expect_match(html, "currentProximityRoiSignature", fixed = TRUE)
  expect_match(html, "id=\"proximityLegend\"", fixed = TRUE)
  expect_match(html, "renderProximityLegend", fixed = TRUE)
  expect_match(html, "runProximityStatistics", fixed = TRUE)
  expect_match(html, "id=\"proximityStatsWindow\"", fixed = TRUE)
  expect_match(html, "proximityControlGrid", fixed = TRUE)
  expect_match(html, "<span>Points</span><select id=\"proximityPointSource\"", fixed = TRUE)
  expect_match(html, "<span>Measure inside</span><select id=\"proximityQueryAnnotations\"", fixed = TRUE)
  expect_match(html, "<span>Distance from</span><select id=\"proximityTargetAnnotations\"", fixed = TRUE)
  expect_match(html, "function proximityCategoryOptions", fixed = TRUE)
  expect_match(html, "classGroup.label='Categories'", fixed = TRUE)
  expect_match(html, "roiGroup.label='Geometries'", fixed = TRUE)
  expect_match(html, "const value='class:'+cat.className", fixed = TRUE)
  expect_match(html, "value='roi_index:'+entry.index", fixed = TRUE)
  expect_match(html, "if(typeof syncProximityAnnotations==='function')syncProximityAnnotations(false)", fixed = TRUE)
  expect_match(html, "applyProximityStatsFeature", fixed = TRUE)
  expect_match(html, "feature_source:source", fixed = TRUE)
  expect_match(html, "proximityStatsFeature", fixed = TRUE)
  expect_false(grepl("id=\"proximityRefreshAnnotations\"", html, fixed = TRUE))
  expect_false(grepl("Refresh proximity annotation choices", html, fixed = TRUE))

  out2 <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = out2, open = FALSE, overwrite = TRUE)
  html2 <- paste(readLines(out2, warn = FALSE), collapse = "\n")
  expect_false(grepl("id=\"runProximityAnalysis\"", html2, fixed = TRUE))
})
