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

test_that("StarDist installer can return a CRAN-safe plan", {
  plan <- wsi_install_stardist(method = "manual", install = FALSE)

  expect_s3_class(plan, "wsi_stardist_installation")
  expect_equal(plan$method, "manual")
  expect_s3_class(plan$plan, "data.frame")
  expect_named(plan$plan, c("step", "command", "command_line", "notes"))
  expect_length(plan$command_output, 0)
})

test_that("StarDist conda setup plan uses a dedicated environment", {
  plan <- wsi_install_stardist(
    method = "conda",
    envname = "wsitools-test-stardist",
    install = FALSE
  )

  expect_s3_class(plan, "wsi_stardist_installation")
  expect_equal(plan$method, "conda")
  expect_true(any(grepl("wsitools-test-stardist", plan$plan$command_line, fixed = TRUE)))
  expect_true(any(grepl("stardist", plan$plan$command_line, fixed = TRUE)))
  expect_true(any(grepl("tensorflow", plan$plan$command_line, fixed = TRUE)))
})
