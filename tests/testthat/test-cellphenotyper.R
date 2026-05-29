write_cellphenotyper_test_png <- function(path) {
  grDevices::png(path, width = 80, height = 80)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "#f5f5f5", border = NA)
}

write_cellphenotyper_test_tiff <- function(path) {
  grDevices::tiff(path, width = 80, height = 60, units = "px")
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::rect(0, 0, 1, 1, col = "#dcc4cf", border = NA)
}

write_cellphenotyper_test_geojson <- function(path, class = "tumour") {
  geojson <- list(
    type = "FeatureCollection",
    features = list(list(
      type = "Feature",
      id = paste0("kodama_", class, "_1"),
      properties = list(
        name = paste("KODAMA", class),
        classification = list(name = class, color = "#F59E0B"),
        measurements = list(area = 361)
      ),
      geometry = list(
        type = "Polygon",
        coordinates = list(list(
          c(10, 10),
          c(30, 10),
          c(30, 30),
          c(10, 30),
          c(10, 10)
        ))
      )
    ))
  )
  writeLines(jsonlite::toJSON(geojson, auto_unbox = TRUE), path, useBytes = TRUE)
}

test_that("CellPhenotyper project reader resolves input and cells", {
  root <- file.path(tempdir(), "cellphenotyper_project")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "05_cell_assignments", "sample"), recursive = TRUE)
  dir.create(file.path(root, "02_stardist", "sample", "stardist_out"), recursive = TRUE)
  dir.create(file.path(root, "03_gigatime", "sample", "gigatime_sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  file.create(input)
  panel <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_5marker_channel_panels.png")
  probs <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_probs.ome.tif")
  channels <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_channels.json")
  metadata <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_metadata.json")
  write_cellphenotyper_test_png(panel)
  file.create(probs)
  writeLines(jsonlite::toJSON(c("DAPI", "CK"), auto_unbox = TRUE), channels, useBytes = TRUE)
  writeLines(
    jsonlite::toJSON(list(original_shape_yx = c(60, 80), store_channels = c("DAPI", "CK")), auto_unbox = TRUE),
    metadata,
    useBytes = TRUE
  )
  shift_file <- file.path(root, "02_stardist", "sample", "stardist_out", "shift.json")
  writeLines(
    jsonlite::toJSON(
      list(
        crop_bbox_xyxy = list(x0 = 11, y0 = 7, x1 = 91, y1 = 67),
        offset_crop_to_original = list(dx = 11, dy = 7),
        crop_size = list(width = 80, height = 60)
      ),
      auto_unbox = TRUE
    ),
    shift_file,
    useBytes = TRUE
  )
  manifest <- data.frame(
    output_id = c("input_1", "gigatime_1", "gigatime_2", "gigatime_3", "gigatime_4"),
    stage_id = c("input", rep("gigatime", 4)),
    stage_title = c("Input Conversion", rep("GigaTIME Virtual mIHC", 4)),
    stage_folder = c("01_input", rep("03_gigatime", 4)),
    relative_path = c(
      "sample/sample.ome.tif",
      "sample/gigatime_sample/gigatime_5marker_channel_panels.png",
      "sample/gigatime_sample/gigatime_probs.ome.tif",
      "sample/gigatime_sample/gigatime_channels.json",
      "sample/gigatime_sample/gigatime_metadata.json"
    ),
    absolute_path = c(
      "/remote/sample.ome.tif",
      "/remote/gigatime_5marker_channel_panels.png",
      "/remote/gigatime_probs.ome.tif",
      "/remote/gigatime_channels.json",
      "/remote/gigatime_metadata.json"
    ),
    size_bytes = rep(1, 5),
    check.names = FALSE
  )
  utils::write.table(
    manifest,
    file.path(root, "00_execution", "project_outputs.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  cells <- data.frame(
    label = c(1, 2),
    x = c(10, 20),
    y = c(30, 40),
    x_orig = c(110, 120),
    y_orig = c(130, 140),
    polygon_label = c("tumour", "stroma")
  )
  utils::write.csv(
    cells,
    file.path(root, "05_cell_assignments", "sample", "sample_objects_assigned.csv"),
    row.names = FALSE
  )

  project <- wsi_read_cellphenotyper_project(root)
  expect_s3_class(project, "wsi_cellphenotyper_project")
  expect_equal(project$input_image, normalizePath(input, mustWork = TRUE))
  expect_equal(project$cell_count, 2L)
  expect_equal(project$cells$x, c(110, 120))
  expect_equal(project$cells$y, c(130, 140))
  expect_equal(project$cells$class, c("tumour", "stroma"))
  expect_equal(project$files$gigatime_panel, normalizePath(panel, mustWork = TRUE))
  expect_equal(project$files$gigatime_probs, normalizePath(probs, mustWork = TRUE))
  expect_equal(project$files$gigatime_channels, normalizePath(channels, mustWork = TRUE))
  expect_equal(wsiTools:::wsi_cellphenotyper_gigatime_channel_names(project), c("DAPI", "CK"))
  expect_equal(wsiTools:::wsi_cellphenotyper_gigatime_extent(project), c(x = 11, y = 7, width = 80, height = 60))
})

test_that("CellPhenotyper KODAMA MedSAM GeoJSON is exposed in the viewer", {
  root <- file.path(tempdir(), "cellphenotyper_project_kodama")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "02_stardist", "sample", "stardist_out"), recursive = TRUE)
  dir.create(file.path(root, "18_cluster_geojson", "sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  file.create(input)
  shift <- file.path(root, "02_stardist", "sample", "stardist_out", "shift.json")
  writeLines(
    paste(
      "{",
      '  "crop_bbox_xyxy": {"x0": 100, "y0": 200, "x1": 900, "y1": 800},',
      '  "offset_crop_to_original": {"dx": 100, "dy": 200},',
      '  "crop_size": {"width": 800, "height": 600}',
      "}",
      sep = "\n"
    ),
    shift,
    useBytes = TRUE
  )
  fine <- file.path(root, "18_cluster_geojson", "sample", "sample_fine_grown_mask_smooth_class.geojson")
  standard <- file.path(root, "18_cluster_geojson", "sample", "sample_standard_grown_mask_smooth_class.geojson")
  write_cellphenotyper_test_geojson(fine, class = "tumour")
  write_cellphenotyper_test_geojson(standard, class = "stroma")

  manifest <- data.frame(
    output_id = c("input_1", "cluster_geojson_fine", "cluster_geojson_standard"),
    stage_id = c("input", "medsam_refinement", "medsam_refinement"),
    stage_title = c("Input Conversion", "KODAMA MedSAM refinement", "KODAMA MedSAM refinement"),
    stage_folder = c("01_input", "18_cluster_geojson", "18_cluster_geojson"),
    relative_path = c(
      "sample/sample.ome.tif",
      "sample/sample_fine_grown_mask_smooth_class.geojson",
      "sample/sample_standard_grown_mask_smooth_class.geojson"
    ),
    absolute_path = c("", "", ""),
    size_bytes = c(1, file.info(fine)$size, file.info(standard)$size),
    check.names = FALSE
  )
  utils::write.table(
    manifest,
    file.path(root, "00_execution", "project_outputs.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  project <- wsi_read_cellphenotyper_project(root, load_cells = FALSE)
  expect_equal(nrow(project$files$kodama_geojson), 2L)

  config <- wsiTools:::wsi_cellphenotyper_viewer_config(project)
  expect_true(config$kodama$enabled)
  expect_length(config$kodama$geojsons, 2L)
  expect_equal(config$kodama$geojsons[[1L]]$feature_count, 1L)
  expect_equal(config$kodama$geojsons[[1L]]$shift_dx, 100)
  expect_equal(config$kodama$geojsons[[1L]]$shift_dy, 200)
  shifted_point <- config$kodama$geojsons[[1L]]$geojson$features[[1L]]$geometry$coordinates[[1L]][[1L]]
  expect_equal(unlist(shifted_point, use.names = FALSE), c(110, 210))
  expect_equal(
    config$kodama$geojsons[[1L]]$geojson$features[[1L]]$properties$wsiTools$source_coordinate_space,
    "cellphenotyper_crop"
  )

  slide <- wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    cellphenotyper = config
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "KODAMA", fixed = TRUE)
  expect_match(text, "kodamaLoadAll", fixed = TRUE)
  expect_match(text, "bindKodamaControls", fixed = TRUE)
  expect_match(text, "KODAMA Fine MedSAM", fixed = TRUE)
  expect_match(text, "sample_fine_grown_mask_smooth_class.geojson", fixed = TRUE)
})

test_that("CellPhenotyper layer and menu are included in viewer HTML", {
  slide <- wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  cells <- data.frame(
    id = c("cell_1", "cell_2"),
    x = c(100, 250),
    y = c(120, 260),
    class = c("cell", "cell")
  )
  layer <- wsi_cellphenotyper_cell_layer(cells, radius = 7)
  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    layers = list(layer),
    cellphenotyper = list(
      enabled = TRUE,
      stardist_layer_id = "cellphenotyper_stardist_cells",
      cell_count = 2L
    )
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "CellPhenotyper StarDist", fixed = TRUE)
  expect_match(text, "cellToggle", fixed = TRUE)
  expect_match(text, "cellphenotyper_stardist_cells", fixed = TRUE)
  expect_match(text, "bindCellControls", fixed = TRUE)
})

test_that("CellPhenotyper GigaTIME panel can be added as a project fallback image", {
  root <- file.path(tempdir(), "cellphenotyper_project_panel")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "03_gigatime", "sample", "gigatime_sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  file.create(input)
  panel <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_5marker_channel_panels.png")
  write_cellphenotyper_test_png(panel)

  manifest <- data.frame(
    output_id = c("input_1", "gigatime_1"),
    stage_id = c("input", "gigatime"),
    stage_title = c("Input Conversion", "GigaTIME Virtual mIHC"),
    stage_folder = c("01_input", "03_gigatime"),
    relative_path = c(
      "sample/sample.ome.tif",
      "sample/gigatime_sample/gigatime_5marker_channel_panels.png"
    ),
    absolute_path = c("", ""),
    size_bytes = c(1, 1),
    check.names = FALSE
  )
  utils::write.table(
    manifest,
    file.path(root, "00_execution", "project_outputs.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  project <- wsi_read_cellphenotyper_project(root, load_cells = FALSE)
  slide <- wsi_mock_slide(width = 1000, height = 800, levels = c(1, 4))
  slide$path <- project$input_image
  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    project_images = wsi_cellphenotyper_project_images(project)
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_false(grepl("GigaTIME channel panels", text, fixed = TRUE))

  html <- tempfile(fileext = ".html")
  wsi_viewer(
    slide,
    output = html,
    open = FALSE,
    overwrite = TRUE,
    mode = "thumbnail",
    project_images = wsi_cellphenotyper_project_images(project, include_gigatime_panel = TRUE)
  )
  text <- paste(readLines(html, warn = FALSE), collapse = "\n")
  expect_match(text, "GigaTIME channel panels", fixed = TRUE)
  expect_match(text, "gigatime_5marker_channel_panels.png", fixed = TRUE)
})

test_that("CellPhenotyper viewer overlays GigaTIME OME-TIFF channels in live mode", {
  skip_if_not_installed("httpuv")
  skip_if_not(wsi_has_vips())

  root <- file.path(tempdir(), "cellphenotyper_project_gigatime")
  unlink(root, recursive = TRUE, force = TRUE)
  dir.create(file.path(root, "00_execution"), recursive = TRUE)
  dir.create(file.path(root, "01_input", "sample"), recursive = TRUE)
  dir.create(file.path(root, "03_gigatime", "sample", "gigatime_sample"), recursive = TRUE)

  input <- file.path(root, "01_input", "sample", "sample.ome.tif")
  probs <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_probs.ome.tif")
  channels <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_channels.json")
  metadata <- file.path(root, "03_gigatime", "sample", "gigatime_sample", "gigatime_metadata.json")
  write_cellphenotyper_test_tiff(input)
  wsiTools:::wsi_run_command(
    "vips",
    c("black", probs, "64", "32", "--bands", "1"),
    error_message = "libvips failed to create a test GigaTIME image."
  )
  writeLines(jsonlite::toJSON(c("DAPI"), auto_unbox = TRUE), channels, useBytes = TRUE)
  writeLines(
    jsonlite::toJSON(list(original_shape_yx = c(60, 80), store_channels = c("DAPI")), auto_unbox = TRUE),
    metadata,
    useBytes = TRUE
  )

  manifest <- data.frame(
    output_id = c("input_1", "gigatime_1", "gigatime_2", "gigatime_3"),
    stage_id = c("input", "gigatime", "gigatime", "gigatime"),
    stage_title = c("Input Conversion", rep("GigaTIME Virtual mIHC", 3)),
    stage_folder = c("01_input", rep("03_gigatime", 3)),
    relative_path = c(
      "sample/sample.ome.tif",
      "sample/gigatime_sample/gigatime_probs.ome.tif",
      "sample/gigatime_sample/gigatime_channels.json",
      "sample/gigatime_sample/gigatime_metadata.json"
    ),
    absolute_path = c("", "", "", ""),
    size_bytes = c(1, 1, 1, 1),
    check.names = FALSE
  )
  utils::write.table(
    manifest,
    file.path(root, "00_execution", "project_outputs.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE
  )

  project <- wsi_read_cellphenotyper_project(root, load_cells = FALSE)
  session <- wsi_viewer_cellphenotyper(
    project,
    output = tempfile(fileext = ".html"),
    mode = "thumbnail",
    open = FALSE,
    wait = FALSE,
    transport = "polling",
    live = TRUE
  )
  on.exit(if (inherits(session, "wsi_viewer_session")) wsi_viewer_stop(session), add = TRUE)

  sources <- session$state$channel_sources
  expect_s3_class(session, "wsi_viewer_session")
  expect_length(sources, 1L)
  expect_equal(sources[[1L]]$name, "DAPI")
  expect_equal(sources[[1L]]$metadata$source_type, "cellphenotyper_gigatime")
  expect_equal(sources[[1L]]$metadata$extent$width, 80)
  html <- paste(readLines(session$html, warn = FALSE), collapse = "\n")
  expect_match(html, "channelMenuList", fixed = TRUE)
  expect_match(html, "channelPendingItems", fixed = TRUE)
  expect_match(html, "function installInitialChannelSources(){clearChannelItems();syncChannelSourcesForActiveImage();}", fixed = TRUE)
})
