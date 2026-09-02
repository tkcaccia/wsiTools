test_that("wsi_viewer adds a left-side project section", {
  slide <- wsiTools:::wsi_mock_slide(width = 1200, height = 800, levels = c(1, 4))
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer(
    slide,
    width = 256,
    output = output,
    open = FALSE,
    project_images = list(wsiTools:::wsi_mock_slide(width = 600, height = 400, levels = c(1, 2)))
  )

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, "Project", fixed = TRUE)
  expect_match(html, "projectPanel", fixed = TRUE)
  expect_match(html, "projectImageList", fixed = TRUE)
  expect_match(html, "projectSectionList", fixed = TRUE)
  expect_match(html, "bindProjectPanel", fixed = TRUE)
  expect_match(html, "switchProjectItem", fixed = TRUE)
  expect_match(html, "projectStatePayload", fixed = TRUE)
  expect_match(html, "projectAnnotationStore", fixed = TRUE)
  expect_match(html, "projectAnnotationKey", fixed = TRUE)
  expect_match(html, "projectItemCanTile", fixed = TRUE)
  expect_match(html, "projectItemClose", fixed = TRUE)
  expect_match(html, "removeProjectItem", fixed = TRUE)
  expect_match(html, "initialProjectSource", fixed = TRUE)
  expect_match(html, "projectInitialSource", fixed = TRUE)
  expect_match(html, "projectDisplaySource", fixed = TRUE)
  expect_match(html, "applyProjectOsd", fixed = TRUE)
  expect_match(html, "const display=projectDisplaySource(item,section)", fixed = TRUE)
  expect_match(html, "const dims=display||section||item||{}", fixed = TRUE)
  expect_match(html, "cfg.tile_url_template=String(src.tile_url_template||'')", fixed = TRUE)
  expect_match(html, "src.cache_key||(src.metadata||{}).cache_key||cfg.tile_cache_buster", fixed = TRUE)
  expect_match(html, "let projectOsdGeneration=0", fixed = TRUE)
  expect_match(html, "generation!==projectOsdGeneration", fixed = TRUE)
  expect_match(html, "osdViewer.tileCache.clear()", fixed = TRUE)
  expect_match(html, "resetWebgpuBaseCompositor()", fixed = TRUE)
  expect_match(html, "cfg.navigator_image_data_uri=navSource||''", fixed = TRUE)
  expect_false(grepl("Active viewer image selected; section-specific annotations restored", html, fixed = TRUE))
  expect_match(html, "Annotations are stored separately for each image/section", fixed = TRUE)
  expect_match(html, "project_section_selected", fixed = TRUE)
  expect_match(html, '"project"', fixed = TRUE)
})

test_that("wsi_viewer_project lists CZI paths without loading them into R", {
  czi1 <- tempfile("section_1_", fileext = ".czi")
  czi2 <- tempfile("section_2_", fileext = ".czi")
  writeBin(as.raw(1:8), czi1)
  writeBin(as.raw(9:16), czi2)
  output <- tempfile(fileext = ".html")

  result <- wsi_viewer_project(c(czi1, czi2), output = output, open = FALSE)

  expect_identical(result, output)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, basename(czi1), fixed = TRUE)
  expect_match(html, basename(czi2), fixed = TRUE)
  expect_match(html, "CZI detected", fixed = TRUE)
  expect_match(html, "Bio-Formats", fixed = TRUE)
  expect_match(html, "WSITOOLS_LIBCZIAPI", fixed = TRUE)
  expect_match(html, "will not use Python unless", fixed = TRUE)
  expect_false(grepl("CZI preview generated with Bio-Formats bfconvert", html, fixed = TRUE))
  expect_match(html, "Project section selected", fixed = TRUE)
  expect_match(html, '"viewer_mode":"project"', fixed = TRUE)
  expect_match(html, "openseadragon.min.js", fixed = TRUE)
  expect_match(html, "OpenSeadragon preview mode", fixed = TRUE)
  expect_match(html, "multiViewProjectEntries", fixed = TRUE)
  expect_match(html, "multiViewTileSource", fixed = TRUE)
  expect_match(html, "project images/sections side by side", fixed = TRUE)
})

test_that("project viewers retain the shared renderer configuration", {
  czi <- tempfile("renderer_", fileext = ".czi")
  writeBin(as.raw(1:8), czi)
  output <- tempfile(fileext = ".html")

  wsi_viewer_project(
    czi,
    output = output,
    open = FALSE,
    renderer = "gpu",
    tile_max_per_frame = 29
  )

  html <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(html, '"renderer":"gpu"', fixed = TRUE)
  expect_match(html, '"tile_max_per_frame":29', fixed = TRUE)
  expect_match(html, "drawer:baseDrawerCandidates()", fixed = TRUE)
})

test_that("project summaries preserve native WGPU state for each source", {
  state <- wsiTools:::wsi_new_viewer_state(name = "native_project_save_test")
  expect_true(wsiTools:::wsi_native_project_state_activate(state, "slide-a"))
  state$layers <- list(list(id = "layer-a", visible = TRUE))
  expect_true(wsiTools:::wsi_native_project_state_activate(state, "slide-b"))
  state$layers <- list(list(id = "layer-b", visible = TRUE))

  saved <- wsiTools:::wsi_project_viewer_summary(state)
  expect_identical(saved$native_active_source_id, "slide-b")
  expect_identical(saved$native_project_states[["slide-a"]]$layers[[1L]]$id, "layer-a")
  expect_identical(saved$native_project_states[["slide-b"]]$layers[[1L]]$id, "layer-b")

  path <- tempfile("native_project_", fileext = ".wsiproject")
  wsi_save_project(wsi_project(viewer_state = state), path)
  reopened <- wsi_read_project(path)
  expect_identical(
    reopened$viewer_state$native_project_states[["slide-a"]]$layers[[1L]]$id,
    "layer-a"
  )
  expect_identical(
    reopened$viewer_state$native_project_states[["slide-b"]]$layers[[1L]]$id,
    "layer-b"
  )
})
