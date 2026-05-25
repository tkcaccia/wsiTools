test_that("wsi_project saves and reopens reproducible analysis state", {
  slide <- wsiTools:::wsi_mock_slide(width = 1024, height = 768, levels = c(1, 4))
  geojson <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "tumour-1",
          "properties": {"name": "Tumour", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [100, 0], [100, 100], [0, 100], [0, 0]]]
          }
        }
      ]
    }',
    geojson
  )
  rois <- read_geojson(geojson)
  cells <- data.frame(x = c(10, 20, 200), y = c(10, 20, 200))
  report <- measurement_report(rois, cells = cells, pixel_size = 1)
  tiles <- extract_tiles(slide, roi = rois, tile_size = 50, stride = 50, save_images = FALSE)
  stains <- wsi_deconvolve_ihc(array(0.75, dim = c(120, 120, 3)))
  viewer <- list(
    view = list(center = list(x = 50, y = 60), zoom = 2),
    stain = list(enabled = TRUE, type = "H-DAB"),
    rois = rois,
    measurements = data.frame(
      id = "measure_1",
      start_x = 0,
      start_y = 0,
      end_x = 3,
      end_y = 4,
      distance_px = 5,
      distance_um = 5
    ),
    segmentation = rois,
    events = list(list(event = "roi_created")),
    last_event = "roi_created"
  )

  dir <- tempfile("case-001.wsiproject")
  project <- wsi_project(
    dir,
    slide = slide,
    viewer_state = viewer,
    measurements = report,
    stains = stains,
    tile_manifest = tiles,
    metadata = list(case_id = "case-001")
  )

  expect_s3_class(project, "wsi_project")
  expect_true(file.exists(file.path(dir, "project.json")))
  expect_true(file.exists(file.path(dir, "rois.geojson")))
  expect_true(file.exists(file.path(dir, "segmentation.geojson")))
  expect_true(file.exists(file.path(dir, "measurements", "class_summary.csv")))
  expect_true(file.exists(file.path(dir, "tile_manifests", "tile_manifest.csv")))
  expect_error(wsi_project(dir, slide = slide), "overwrite = FALSE")

  reopened <- wsi_read_project(dir)
  expect_s3_class(reopened, "wsi_project")
  expect_s3_class(reopened$rois, "wsi_roi")
  expect_s3_class(reopened$measurements, "wsi_measurement_report")
  expect_s3_class(reopened$segmentation, "wsi_segmentation")
  expect_s3_class(reopened$tile_manifest, "wsi_tile_manifest")
  expect_equal(reopened$metadata$case_id, "case-001")
  expect_equal(reopened$viewer_state$view$zoom, 2)
  expect_equal(nrow(reopened$rois), 1)
  expect_equal(nrow(reopened$tile_manifest), nrow(tiles))

  updated <- wsi_project(dir, slide = slide, rois = rois, overwrite = TRUE)
  expect_s3_class(updated, "wsi_project")
})

test_that("wsi_project can infer state from a live viewer-state object", {
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "state", envir = env)
  payload <- list(
    event = "viewer_state",
    time = "2026-05-18T00:00:00+0200",
    rois = list(type = "FeatureCollection", features = list()),
    segmentation = list(type = "FeatureCollection", features = list()),
    selected_roi = NULL,
    measurements = list(
      list(
        id = "measure_1",
        start = list(x = 0, y = 0),
        end = list(x = 3, y = 4),
        distance_px = 5,
        distance_um = 2.5
      )
    ),
    view = list(zoom = 4),
    stain = list(enabled = FALSE)
  )
  wsiTools:::wsi_viewer_state_apply(state, payload)

  dir <- tempfile("viewer-state.wsiproject")
  project <- wsi_project(dir, viewer_state = state)
  reopened <- wsi_read_project(dir)

  expect_s3_class(project, "wsi_project")
  expect_equal(nrow(reopened$measurements), 1)
  expect_equal(reopened$measurements$distance_px, 5)
  expect_false(reopened$stain_settings$enabled)

  slide <- wsiTools:::wsi_mock_slide(width = 256, height = 128, levels = c(1, 2))
  session <- list(state = state, slide = slide)
  class(session) <- "wsi_viewer_session"
  in_memory <- wsi_project(session)
  expect_s3_class(in_memory, "wsi_project")
  expect_equal(in_memory$slide_info$backend, "mock")
  expect_equal(nrow(in_memory$measurements), 1)
})

test_that("project state round-trips channel and tile source metadata", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 384, levels = c(1, 4))
  env <- new.env(parent = emptyenv())
  state <- wsiTools:::wsi_new_viewer_state(name = "channel_project", envir = env)
  state$channel_sources <- list(wsi_channel_source(
    "DAB",
    type = "stain",
    vector = c(0.268, 0.570, 0.776),
    opacity = 0.6,
    contrast_min = 0.1,
    contrast_max = 1.4
  ))
  state$channel_settings <- wsiTools:::wsi_channel_settings_from_sources(state$channel_sources)
  state$tile_sources <- list(dynamic = list(
    id = "slide",
    type = "dynamic",
    tile_url_template = "http://127.0.0.1:8788/tiles/slide/{level}/{x}/{y}.{format}",
    tile_format = "png"
  ))
  session <- structure(
    list(state = state, slide = slide, url = NULL, ws_url = NULL, jobs = list()),
    class = "wsi_viewer_session"
  )
  session <- wsiTools:::wsi_attach_viewer_session_methods(session)
  dir <- tempfile("channel-project.wsiproject")
  project <- wsi_project(dir, viewer_state = session, slide = slide)
  reopened <- wsi_read_project(dir)

  expect_equal(reopened$viewer_state$channel_settings[[1]]$id, "DAB")
  expect_equal(reopened$viewer_state$tile_sources$dynamic$type, "dynamic")

  target_state <- wsiTools:::wsi_new_viewer_state(name = "channel_project_restore", envir = env)
  target <- structure(
    list(state = target_state, slide = slide, url = NULL, ws_url = NULL, jobs = list()),
    class = "wsi_viewer_session"
  )
  target <- wsiTools:::wsi_attach_viewer_session_methods(target)
  restore_project_state(target, reopened, service = FALSE)

  expect_equal(target$get_channel_settings(service = FALSE)$id, "DAB")
  expect_equal(target$get_state(service = FALSE)$tile_sources$dynamic$type, "dynamic")
  expect_true(any(vapply(target_state$commands, function(x) identical(x$type, "restore_project_state"), logical(1))))
})

test_that("wsi_project can be built in memory and saved later", {
  slide <- wsiTools:::wsi_mock_slide(width = 512, height = 384, levels = c(1, 4))
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature",
      id = "roi-1",
      properties = list(name = "Tumour", classification = list(name = "tumour")),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(c(0, 0), c(64, 0), c(64, 64), c(0, 64), c(0, 0)))
      )
    ))
  ))
  tiles <- data.frame(
    tile_id = c("tile_1", "tile_2"),
    x = c(0, 32),
    y = c(0, 0),
    width = c(32L, 32L),
    height = c(32L, 32L),
    level = c(0L, 0L),
    row = c(1L, 1L),
    col = c(1L, 2L),
    downsample = c(1, 1),
    tissue_fraction = c(1, 1),
    stringsAsFactors = FALSE
  )
  class(tiles) <- c("wsi_tile_manifest", class(tiles))

  project <- wsi_project(slide)
  expect_s3_class(project, "wsi_project")
  expect_null(project$path)
  expect_equal(project$slide_info$backend, "mock")

  project$rois <- rois
  project$measurements <- data.frame(
    id = "measure_1",
    start_x = 0,
    start_y = 0,
    end_x = 3,
    end_y = 4,
    distance_px = 5,
    distance_um = NA_real_
  )
  project$segmentation <- rois
  project$stain_settings <- list(enabled = TRUE, channels = list("DAB", "hematoxylin"))
  project$tile_manifest <- tiles
  project$viewer_state <- list(
    view = list(zoom = 8, center = list(x = 100, y = 80)),
    stain = project$stain_settings,
    layers = list(list(id = "dab", name = "DAB intensity", visible = TRUE, opacity = 0.4)),
    events = list(list(event = "roi_created")),
    last_event = "roi_created"
  )
  project$processing_provenance <- list(
    steps = list(list(name = "manual annotation", tool = "wsiTools viewer")),
    parameters = list(tile_size = 32)
  )

  dir <- tempfile("case-01.wsiproject")
  saved <- wsi_save_project(project, dir)
  manifest <- jsonlite::fromJSON(file.path(dir, "project.json"), simplifyVector = FALSE)

  expect_s3_class(saved, "wsi_project")
  expect_true(file.exists(file.path(dir, "rois.geojson")))
  expect_true(file.exists(file.path(dir, "measurements.csv")))
  expect_true(file.exists(file.path(dir, "segmentation.geojson")))
  expect_true(file.exists(file.path(dir, "tile_manifests", "tile_manifest.csv")))
  expect_equal(manifest$slide$backend, "mock")
  expect_equal(manifest$slide$dimensions$width, 512)
  expect_equal(manifest$viewer_state$view$zoom, 8)
  expect_true(manifest$stain_settings$enabled)
  expect_equal(manifest$processing_provenance$parameters$tile_size, 32)
  expect_false(is.null(manifest$processing_provenance$r_version))
  expect_false(is.null(manifest$processing_provenance$package_versions$wsiTools))

  reopened <- wsi_read_project(dir)
  expect_s3_class(reopened$rois, "wsi_roi")
  expect_s3_class(reopened$tile_manifest, "wsi_tile_manifest")
  expect_equal(reopened$viewer_state$view$zoom, 8)
  expect_equal(reopened$processing_provenance$steps[[1]]$name, "manual annotation")
})

test_that("case report export writes HTML and CSV summaries from a project", {
  slide <- wsiTools:::wsi_mock_slide(width = 256, height = 128, levels = c(1, 4))
  rois <- wsiTools:::wsi_roi_from_geojson(list(
    type = "FeatureCollection",
    features = list(
      list(
        type = "Feature",
        id = "tumour-1",
        properties = list(name = "Tumour", classification = list(name = "tumour")),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))
        )
      ),
      list(
        type = "Feature",
        id = "stroma-1",
        properties = list(name = "Stroma", classification = list(name = "stroma")),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(c(20, 0), c(30, 0), c(30, 10), c(20, 10), c(20, 0)))
        )
      )
    )
  ))
  cells <- data.frame(x = c(2, 5, 25), y = c(2, 5, 5))
  channels <- structure(
    list(
      hematoxylin = matrix(0.25, nrow = 12, ncol = 32),
      hrp_dab = matrix(0.5, nrow = 12, ncol = 32),
      channel_metadata = list(
        list(id = "hematoxylin", name = "Hematoxylin"),
        list(id = "hrp_dab", name = "HRP/DAB")
      )
    ),
    class = "wsi_ihc_channels"
  )
  tiles <- data.frame(
    tile_id = c("tile_1", "tile_2", "tile_3"),
    x = c(0, 50, 100),
    y = c(0, 0, 0),
    width = c(50L, 50L, 50L),
    height = c(50L, 50L, 50L),
    level = c(0L, 0L, 0L),
    split = c("train", "train", "valid"),
    roi_id = c("tumour-1", "tumour-1", "stroma-1"),
    roi_class = c("tumour", "tumour", "stroma"),
    stringsAsFactors = FALSE
  )
  class(tiles) <- c("wsi_tile_manifest", class(tiles))
  measurements <- measurement_report(
    rois,
    cells = cells,
    stains = channels,
    pixel_size = 1,
    dab_threshold = 0.3
  )
  project_dir <- tempfile("case-report-project.wsiproject")
  project <- wsi_project(
    project_dir,
    slide = slide,
    rois = rois,
    measurements = measurements,
    tile_manifest = tiles,
    metadata = list(case_id = "case-report-001"),
    processing_provenance = list(
      steps = list(list(name = "annotation", tool = "wsiTools")),
      parameters = list(tile_size = 50, stride = 50)
    )
  )
  reopened <- wsi_read_project(project_dir)
  report_dir <- tempfile("case-report")

  report <- wsi_case_report(reopened, output_dir = report_dir)

  expect_s3_class(project, "wsi_project")
  expect_s3_class(report, "wsi_case_report")
  expect_true(file.exists(report$html))
  expect_true(file.exists(file.path(report_dir, "tables", "overview.csv")))
  expect_true(file.exists(file.path(report_dir, "tables", "class_summary.csv")))
  expect_true(file.exists(file.path(report_dir, "tables", "ihc_summary.csv")))
  expect_true(file.exists(file.path(report_dir, "tables", "tile_summary.csv")))
  expect_true(file.exists(file.path(report_dir, "tables", "tile_counts.csv")))
  expect_true(file.exists(file.path(report_dir, "tables", "provenance.csv")))

  overview <- utils::read.csv(file.path(report_dir, "tables", "overview.csv"), stringsAsFactors = FALSE)
  tile_summary <- utils::read.csv(file.path(report_dir, "tables", "tile_summary.csv"), stringsAsFactors = FALSE)
  ihc_summary <- utils::read.csv(file.path(report_dir, "tables", "ihc_summary.csv"), stringsAsFactors = FALSE)
  html <- paste(readLines(report$html, warn = FALSE), collapse = "\n")

  expect_equal(overview$case_id, "case-report-001")
  expect_equal(overview$tile_count, 3)
  expect_equal(tile_summary$tile_count, 3)
  expect_true("ihc_dab_mean" %in% names(ihc_summary))
  expect_match(html, "wsiTools Case Report", fixed = TRUE)
  expect_match(html, "ROI Areas", fixed = TRUE)
  expect_match(html, "Class Percentages And Cell Density", fixed = TRUE)
  expect_match(html, "IHC ROI Intensity", fixed = TRUE)
  expect_match(html, "Tile Counts", fixed = TRUE)
  expect_match(html, "Provenance", fixed = TRUE)
  expect_match(html, "tables/overview.csv", fixed = TRUE)
})
