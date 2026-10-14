test_that("annotation registration is available in the viewer", {
  slide <- wsiTools:::wsi_mock_slide(width = 800, height = 400, levels = c(1, 4))
  output <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = output, open = FALSE)
  html <- paste(readLines(output, warn = FALSE), collapse = "\n")

  expect_match(html, 'id="annotationRegistrationOpen"', fixed = TRUE)
  expect_match(html, 'id="annotationRegistrationWindow"', fixed = TRUE)
  expect_match(html, 'id="annotationRegistrationScope"', fixed = TRUE)
  expect_match(html, 'id="annotationRegistrationDrag"', fixed = TRUE)
  expect_match(html, 'id="annotationRegistrationSave"', fixed = TRUE)
  expect_match(html, 'function annotationRegistrationCommit(', fixed = TRUE)
  expect_match(html, "scheduleViewerStateSync('roi_updated'", fixed = TRUE)
})

test_that("annotation registration transforms and resets coordinates", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is unavailable")
  script <- testthat::test_path("..", "viewer", "annotation-registration.test.js")
  viewer_assets <- system.file("viewer", package = "wsiTools")
  expect_true(nzchar(viewer_assets))
  result <- suppressWarnings(system2(node, c(shQuote(script), shQuote(viewer_assets)),
                                     stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  expect_identical(if (is.null(status)) 0L else status, 0L, info = paste(result, collapse = "\n"))
  expect_true(any(grepl("registration transforms", result, fixed = TRUE)))
})
