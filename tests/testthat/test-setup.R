test_that("dependency plans are CRAN-safe and copyable", {
  plan <- wsi_dependency_plan(
    tools = c("openslide", "libvips", "imagemagick"),
    method = "homebrew"
  )

  expect_s3_class(plan, "data.frame")
  expect_named(plan, c("tool", "installed", "method", "command", "command_line", "notes", "args"))
  expect_equal(plan$method, rep("homebrew", 3))
  expect_true(all(grepl("^brew 'install'", plan$command_line)))
  expect_true(all(vapply(plan$args, length, integer(1)) >= 2L))
})

test_that("Windows winget backend plan installs supported tools and reports gaps", {
  plan <- wsi_dependency_plan(
    tools = c("openslide", "libvips", "imagemagick"),
    method = "winget"
  )

  expect_equal(plan$method, rep("winget", 3))
  expect_true(is.na(plan$command[plan$tool == "openslide"]))
  expect_match(plan$notes[plan$tool == "openslide"], "OpenSlide", fixed = TRUE)
  expect_equal(plan$command[plan$tool == "libvips"], "winget")
  expect_true(any(grepl("libvips.libvips", plan$command_line, fixed = TRUE)))
  expect_true(any(grepl("ImageMagick.Q16-HDRI", plan$command_line, fixed = TRUE)))
})

test_that("Bio-Formats setup points to OME bftools", {
  conda_plan <- wsi_dependency_plan(tools = "bioformats", method = "conda")
  expect_equal(conda_plan$tool, "bioformats")
  expect_equal(conda_plan$command, "conda")
  expect_true(all(c("--override-channels", "-c", "ome", "conda-forge", "bftools") %in% conda_plan$args[[1L]]))
  expect_match(conda_plan$notes, "showinf")
  expect_match(conda_plan$notes, "bfconvert")

  brew_plan <- wsi_dependency_plan(tools = "bioformats", method = "homebrew")
  expect_true(is.na(brew_plan$command))
  expect_true(is.na(brew_plan$command_line))
  expect_match(brew_plan$notes, "bftools.zip", fixed = TRUE)
})

test_that("auto setup can resolve Bio-Formats separately from winget-only tools", {
  resolver_code <- paste(deparse(wsiTools:::wsi_setup_tool_method), collapse = "\n")

  expect_match(resolver_code, 'identical(tool, "bioformats")', fixed = TRUE)
  expect_match(resolver_code, 'return("conda")', fixed = TRUE)
  expect_match(resolver_code, 'return("manual")', fixed = TRUE)
})

test_that("native CZI setup is explicit and CRAN-safe", {
  backend_plan <- wsi_dependency_plan(tools = "native_czi", method = "manual")

  expect_equal(backend_plan$tool, "native_czi")
  expect_true(is.na(backend_plan$command))
  expect_true(is.na(backend_plan$command_line))
  expect_match(backend_plan$notes, "wsi_install_native_czi", fixed = TRUE)

  plan <- wsi_install_native_czi(
    install = FALSE,
    install_dir = tempfile("native-czi-plan-")
  )

  expect_s3_class(plan, "wsi_native_czi_installation")
  expect_s3_class(plan$plan, "data.frame")
  expect_named(plan$plan, c("step", "command", "command_line", "notes", "args"))
  expect_equal(plan$plan$command, c("git", "cmake", "cmake"))
  expect_true(any(grepl("libCZIAPI", plan$plan$command_line, fixed = TRUE)))
  expect_match(plan$license_notice, "LGPL")
  expect_length(plan$command_output, 0)
})

test_that("native CZI installer finds built shared libraries", {
  root <- tempfile("native-czi-lib-")
  build_dir <- file.path(root, "libczi-build", "Src", "libCZIAPI")
  dir.create(build_dir, recursive = TRUE)
  ext <- if (.Platform$OS.type == "windows") {
    ".dll"
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    ".dylib"
  } else {
    ".so"
  }
  lib <- file.path(build_dir, paste0("liblibCZIAPI", ext))
  file.create(lib)

  found <- wsiTools:::wsi_native_czi_find_library(root)
  expect_equal(normalizePath(found, winslash = "/", mustWork = TRUE), normalizePath(lib, winslash = "/", mustWork = TRUE))
})

test_that("conda backend plans avoid default Anaconda channels", {
  plan <- wsi_dependency_plan(
    tools = c("openslide", "libvips", "imagemagick"),
    method = "conda"
  )

  for (args in plan$args) {
    expect_true("--override-channels" %in% args)
    expect_true("conda-forge" %in% args)
    expect_false("defaults" %in% args)
  }
  expect_true(all(grepl("--override-channels", plan$command_line, fixed = TRUE)))
})

test_that("setup reports missing dependencies without installing by default", {
  plan <- wsi_setup(
    tools = c("openslide", "libvips"),
    r_packages = "wsiToolsDefinitelyMissingPackage",
    method = "manual",
    install = FALSE
  )

  expect_s3_class(plan, "wsi_setup_plan")
  expect_s3_class(plan$backends, "data.frame")
  expect_equal(nrow(plan$r_packages), 1)
  expect_false(plan$r_packages$installed)
  expect_equal(plan$system_tools$method, c("manual", "manual"))
  expect_true(all(is.na(plan$system_tools$command_line)))
  expect_length(plan$command_output, 0)

  printed <- capture.output(print(plan))
  expect_true(any(grepl("Nothing is installed", printed, fixed = TRUE)))
})

test_that("wsi_start prints a concise first-run checklist", {
  output <- capture.output(
    start <- wsi_start(live_test = FALSE)
  )

  expect_s3_class(start, "wsi_start")
  expect_named(
    start,
    c("package_version", "backends", "installed_backends", "missing_core_backends", "live_viewer", "suggested_next_command")
  )
  expect_s3_class(start$backends, "data.frame")
  expect_s3_class(start$live_viewer, "data.frame")
  expect_true(any(grepl("1. Checking package", output, fixed = TRUE)))
  expect_true(any(grepl("2. Checking backends", output, fixed = TRUE)))
  expect_true(any(grepl("3. Checking live viewer", output, fixed = TRUE)))
  expect_true(any(grepl("4. Suggested next command", output, fixed = TRUE)))
  expect_true(any(grepl("wsi_diagnose(live_test = FALSE)", output, fixed = TRUE)))
})

test_that("setup report is read-only and includes diagnostic sections", {
  setup_output <- capture.output(
    report <- wsi_setup_report(
      tools = c("openslide", "libvips"),
      r_packages = "wsiToolsDefinitelyMissingPackage",
      method = "manual",
      include_optional = FALSE
    )
  )

  expect_s3_class(report, "wsi_setup_report")
  expect_named(
    report,
    c("version", "session", "backends", "helpers", "r_packages", "system_tools", "environment")
  )
  expect_s3_class(report$backends, "data.frame")
  expect_equal(report$system_tools$tool, c("openslide", "libvips"))
  expect_true(all(c("wsi_has_vips()", "wsi_has_openslide()", "wsi_has_native_czi()", "wsi_has_stardist()", "wsi_has_mesmer()") %in% report$helpers$check))

  printed <- capture.output(print(report))
  expect_true(any(grepl("<wsi_setup_report>", setup_output, fixed = TRUE)))
  expect_true(any(grepl("This report is read-only", printed, fixed = TRUE)))
})

test_that("diagnose report includes remote-debugging sections", {
  diagnose_output <- capture.output(
    report <- wsi_diagnose(
      method = "manual",
      include_optional = FALSE,
      live_test = FALSE
    )
  )

  expect_s3_class(report, "wsi_diagnose")
  expect_named(
    report,
    c(
      "os", "r", "package_version", "backends", "helpers", "executables",
      "environment", "live_viewer", "setup_plan", "suggested_fixes"
    )
  )
  expect_s3_class(report$backends, "data.frame")
  expect_s3_class(report$executables, "data.frame")
  expect_s3_class(report$environment, "data.frame")
  expect_s3_class(report$live_viewer, "data.frame")
  expect_s3_class(report$suggested_fixes, "data.frame")
  expect_true("vips" %in% report$executables$command)
  expect_true("PATH" %in% report$environment$variable)
  expect_true("browser_sync_self_test" %in% report$live_viewer$check)
  expect_true(any(grepl("<wsi_diagnose>", diagnose_output, fixed = TRUE)))
})

test_that("diagnose can self-test the local httpuv bridge", {
  skip_on_cran()
  skip_if_not_installed("httpuv")
  skip_if_not_installed("callr")

  report <- wsi_diagnose(
    method = "manual",
    include_optional = FALSE,
    live_test = TRUE,
    sync_test = TRUE,
    port = sample(20000:45000, 1),
    max_tries = 5,
    timeout = 5
  )

  expect_true(isTRUE(report$live_viewer$value[report$live_viewer$check == "live_viewer_can_start"]))
  expect_true(isTRUE(report$live_viewer$value[report$live_viewer$check == "browser_sync_self_test"]))
})

test_that("backend installer can return a setup plan without running commands", {
  plan <- wsi_install_backends(
    tools = "libvips",
    r_packages = character(),
    method = "manual",
    install = FALSE
  )

  expect_s3_class(plan, "wsi_setup_plan")
  expect_equal(plan$system_tools$tool, "libvips")
  expect_equal(plan$system_tools$method, "manual")
  expect_length(plan$command_output, 0)
})

test_that("setup rejects unknown tool names", {
  expect_error(
    wsi_dependency_plan(tools = "unknown-tool", method = "manual"),
    "unsupported value"
  )
})

test_that("setup no longer accepts model-execution backends", {
  expect_error(
    wsi_dependency_plan(tools = "stardist", method = "manual"),
    "unsupported value"
  )
  expect_error(
    wsi_dependency_plan(tools = "cellpose", method = "manual"),
    "unsupported value"
  )
})
