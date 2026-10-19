desktop_experiment_test_scope <- function() {
  launcher <- test_path("../../tools/wsiToolsDesktop/src-tauri/resources/launch-viewer.R")
  skip_if_not(file.exists(launcher), "Desktop launcher sources are not available.")
  scope <- new.env(parent = baseenv())
  scope$desktop_stage <- function(...) invisible(NULL)
  scope$desktop_log <- function(...) invisible(NULL)
  wanted <- c("%||%", "desktop_project_input", "desktop_experiment_reference",
              "desktop_open_experiment", "desktop_open_target")
  for (definition in as.list(parse(launcher))) {
    if (is.call(definition) && identical(definition[[1L]], as.name("<-")) &&
        as.character(definition[[2L]]) %in% wanted) eval(definition, scope)
  }
  scope
}

test_that("Desktop recognizes CellPhenotyper experiments without relying on file extension", {
  scope <- desktop_experiment_test_scope()
  folder <- tempfile("desktop-experiment-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  file <- file.path(folder, "experiment.cellphenotyper.json")
  jsonlite::write_json(list(schema = "cellphenotyper-experiment", schema_version = 1L),
                       file, auto_unbox = TRUE)
  expect_identical(scope$desktop_project_input(file),
                   list(kind = "experiment", path = normalizePath(file, winslash = "/")))
  jsonlite::write_json(list(schema = "cellphenotyper-experiment", schema_version = 2L),
                       file, auto_unbox = TRUE)
  expect_error(scope$desktop_project_input(file), "unsupported schema_version")
  jsonlite::write_json(list(schema = "cellphenotyper-experiment", schema_version = "1"),
                       file, auto_unbox = TRUE)
  expect_error(scope$desktop_project_input(file), "unsupported schema_version")
  writeLines("{", file)
  expect_error(scope$desktop_project_input(file), "Could not read project JSON")
  expect_error(scope$desktop_project_input(folder), "no project.json")
  expect_error(scope$desktop_project_input(file.path(folder, "missing.json")), "Project not found")
})

test_that("Desktop resolves experiment links beside saved sessions", {
  scope <- desktop_experiment_test_scope()
  folder <- tempfile("desktop-experiment-link-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  experiment <- file.path(folder, "experiment.cellphenotyper.json")
  writeLines("{}", experiment)
  session <- file.path(folder, "saved.wsiproject.json")
  expect_identical(scope$desktop_experiment_reference(list(path = basename(experiment)), session),
                   normalizePath(experiment, winslash = "/"))
  expect_identical(scope$desktop_experiment_reference(list(manifest_path = basename(experiment)), session),
                   normalizePath(experiment, winslash = "/"))
  expect_identical(scope$desktop_experiment_reference(NULL, session), NULL)
  expect_error(scope$desktop_experiment_reference(list(path = character()), session), "invalid experiment reference")
  expect_error(scope$desktop_experiment_reference(list(), session), "invalid experiment reference")
  expect_error(scope$desktop_experiment_reference(list(path = "missing.json"), session), "linked CellPhenotyper experiment is missing")
  expect_error(scope$desktop_experiment_reference(list(path = basename(experiment), manifest_sha256 = "bad"), session),
               "invalid experiment digest")
  if (requireNamespace("openssl", quietly = TRUE)) {
    digest <- paste0(openssl::sha256(readBin(experiment, "raw", file.info(experiment)$size)))
    expect_identical(scope$desktop_experiment_reference(list(path = basename(experiment), manifest_sha256 = digest), session),
                     normalizePath(experiment, winslash = "/"))
    expect_error(scope$desktop_experiment_reference(list(path = basename(experiment), manifest_sha256 = paste(rep("0", 64), collapse = "")), session),
                 "experiment index changed")
  }
})

test_that("Desktop experiment opener uses the existing-results viewer and does not request inference", {
  scope <- desktop_experiment_test_scope()
  folder <- tempfile("desktop-experiment-open-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  experiment <- file.path(folder, "experiment.cellphenotyper.json")
  jsonlite::write_json(list(schema = "cellphenotyper-experiment", schema_version = 1L,
                           name = "Two tissues", tissues = list(list(id = "a"), list(id = "b"))),
                       experiment, auto_unbox = TRUE)
  received <- NULL
  opener <- function(path, output, open, wait) {
    received <<- list(path = path, output = output, open = open, wait = wait)
    structure(list(html = output), class = "wsi_viewer_session")
  }
  viewer <- scope$desktop_open_experiment(experiment, "viewer.html", NULL, open_fun = opener)
  expect_identical(received, list(path = experiment, output = "viewer.html", open = FALSE, wait = FALSE))
  expect_identical(attr(viewer, "desktop_experiment_path"), experiment)
})

test_that("saved experiment sessions rebuild the experiment backend before restoring annotations", {
  scope <- desktop_experiment_test_scope()
  folder <- tempfile("desktop-experiment-reopen-")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  experiment <- file.path(folder, "experiment.cellphenotyper.json")
  writeLines("{}", experiment)
  session <- file.path(folder, "saved.wsiproject.json")
  jsonlite::write_json(list(schema = "wsiTools-viewer-project", version = 1L,
                           experiment = list(path = basename(experiment)),
                           project = list(items = list(list(path = "stale-original-slide.tif")))),
                       session, auto_unbox = TRUE)
  scope$desktop_open_experiment <- function(path, output, log_file) {
    structure(list(experiment = path), class = "wsi_viewer_session")
  }
  scope$desktop_viewer_project_items <- function(...) stop("Experiment must supply the current image paths.")
  viewer <- scope$desktop_open_target(session, "project", "viewer.html", NULL)
  expect_identical(viewer$experiment, normalizePath(experiment, winslash = "/"))
  expect_identical(attr(viewer, "desktop_restore_viewer_project"), normalizePath(session, winslash = "/"))
})
