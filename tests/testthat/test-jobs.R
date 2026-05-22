test_that("background job availability check is logical", {
  expect_type(wsi_has_callr(), "logical")
  expect_length(wsi_has_callr(), 1)
})

test_that("background job helpers expose UI-friendly progress metadata", {
  expect_equal(wsiTools:::wsi_job_display_status("finished"), "completed")
  expect_equal(wsiTools:::wsi_job_display_status("running"), "running")
  expect_equal(wsiTools:::wsi_job_parse_progress("progress: 37%"), 37)
  expect_equal(wsiTools:::wsi_job_parse_progress("WSITOOLS_PROGRESS=12.5"), 12.5)
  expect_true(is.na(wsiTools:::wsi_job_parse_progress("no percent here")))
})

test_that("background jobs return results without blocking construction", {
  skip_if_not_installed("callr")

  job <- wsi_job(function(x) x + 1, args = list(x = 41), name = "add one")

  expect_s3_class(job, "wsi_job")
  expect_true(job$status() %in% c("running", "finished"))
  expect_equal(job$result(), 42)
  expect_equal(job$status(), "finished")
  meta <- job$metadata()
  expect_equal(meta$display_status, "completed")
  expect_equal(meta$progress, 100)
  expect_true(is.function(job$progress))
  expect_true(is.function(job$log))
})

test_that("background jobs report child errors", {
  skip_if_not_installed("callr")

  job <- wsi_job(function() stop("boom", call. = FALSE), name = "failure")

  expect_error(job$result(), "boom")
  expect_equal(job$status(), "failed")
})

test_that("background jobs run result callbacks when collected", {
  skip_if_not_installed("callr")

  value <- NULL
  job <- wsi_job(function() "done", name = "callback")
  job$then(function(result) value <<- result)

  expect_equal(job$result(), "done")
  expect_equal(value, "done")
})
