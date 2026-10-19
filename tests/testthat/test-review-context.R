review_contract_fixture <- function() {
  images <- lapply(c("t1", "t2", "t3"), function(id) list(tissue_id = id,
    frame_id = paste0(id, ":image"), sha256 = paste(rep(substr(id, 2L, 2L), 64L), collapse = ""),
    width = 300L, height = 240L, mpp_x = .5, mpp_y = .5,
    coordinate_convention = "pixel-edge-origin_centres-in-native-pixels"))
  observations <- unlist(lapply(images, function(image) lapply(seq_len(4L), function(i) list(
    id = paste("study", image$tissue_id, "rna", paste0("BC", i), sep = "::"),
    original_id = paste0("BC", i), dataset_id = "rna", tissue_id = image$tissue_id,
    frame_id = image$frame_id, observation_unit = "spot", x = c(10, 20, 130, 290)[[i]],
    y = 20, footprint_width_px = 8, footprint_height_px = 8,
    cluster_values = list(rna_cluster = if (i <= 2L) "alpha" else "beta")))), recursive = FALSE)
  points <- lapply(rev(observations), function(observation) list(id = observation$id, x = -2, y = 4))
  wsi_review_context(images, observations,
    list(list(id = "rna:kodama", label = "KODAMA", observation_unit = "spot", points = points)),
    context_id = "study", experiment_id = "study", provenance = list(synthetic = TRUE))
}

test_that("review identity survives repeated local IDs and shuffled reductions", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  expect_equal(length(context$observations), 12L)
  expect_match(context$source_signature, "^[0-9a-f]{64}$")
  ids <- vapply(context$observations, `[[`, character(1), "id")
  expect_false(anyDuplicated(ids) > 0L)
  expect_equal(length(unique(vapply(context$observations, `[[`, character(1), "original_id"))), 4L)
  reordered <- wsi_review_context(rev(context$images), rev(context$observations),
    context$reductions, context_id = context$context_id, experiment_id = context$experiment_id,
    provenance = context$provenance)
  expect_identical(context$source_signature, reordered$source_signature)
  expect_identical(wsi_review_select(context, cluster_field = "rna_cluster", cluster_values = "alpha", tissue_ids = "t2"), ids[5:6])
  expect_identical(wsi_review_select(context), character())
  expect_identical(wsi_review_select(context, ids = character()), character())
  expect_error(wsi_review_select(context, ids = "missing"), "unknown")
  expect_error(wsi_review_select(context, ids = rep(ids[[1L]], 2L)), "unique")
  expect_error(wsi_review_select(context, cluster_field = "not_a_field", cluster_values = "alpha"), "Unknown")
})

test_that("review contexts reject frame, unit, coordinate and identity drift", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  build <- function(observations = context$observations, reductions = context$reductions)
    wsi_review_context(context$images, observations, reductions, context_id = "study")
  observations <- context$observations
  observations[[2L]]$id <- observations[[1L]]$id
  expect_error(build(observations), "unique")
  observations <- context$observations
  observations[[1L]]$frame_id <- "another-frame"
  expect_error(build(observations), "frame_id")
  observations <- context$observations
  observations[[1L]]$x <- 300
  expect_error(build(observations), "outside")
  reductions <- context$reductions
  reductions[[1L]]$observation_unit <- "cell"
  expect_error(build(reductions = reductions), "units")
  reductions <- context$reductions
  reductions[[1L]]$points[[1L]]$id <- "unknown"
  expect_error(build(reductions = reductions), "unknown")
  changed <- context
  changed$images[[1L]]$sha256 <- paste(rep("f", 64), collapse = "")
  expect_error(wsi_validate_review_context(changed), "signature")
  changed <- context
  changed$images[[1L]]$coordinate_convention <- "unknown"
  expect_error(wsi_validate_review_context(changed), "pixel")
  changed <- context
  changed$schema_version <- NA_real_
  expect_error(wsi_validate_review_context(changed), "schema/version")
  expect_error(wsi_review_context(context$images, list(), provenance = "invalid"), "provenance")
})

test_that("unknown calibration is explicit and never converted to physical units", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  image <- context$images[[1L]]
  image$mpp_x <- image$mpp_y <- NULL
  expect_error(wsi_review_context(list(image), list()), "mpp_x")
  image$calibration_status <- "unavailable"
  unavailable <- wsi_review_context(list(image), list(), context_id = "unavailable")
  expect_length(unavailable$observations, 0L)
  expect_null(unavailable$images[[1L]]$mpp_x)
  expect_no_error(wsi_validate_review_context(unavailable))
  expect_error(wsi_validate_review_context(unavailable, require_calibration = TRUE), "calibration")
})

test_that("image file hashes are verified before explicit processing", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  path <- tempfile("review-hash-")
  on.exit(unlink(path), add = TRUE)
  writeBin(charToRaw("synthetic fixture bytes"), path)
  image <- context$images[[1L]]
  image$path <- path
  image$sha256 <- paste0(openssl::sha256(readBin(path, "raw", file.info(path)$size)))
  context <- wsi_review_context(list(image), context$observations[1:4], context_id = "image-hash")
  expect_no_error(wsi_validate_review_context(context, verify_hashes = TRUE))
  writeBin(charToRaw("changed fixture bytes"), path)
  expect_error(wsi_validate_review_context(context, verify_hashes = TRUE), "SHA-256 mismatch")
})

test_that("human checkpoint labels remain distinct and stale sources fail on reopen", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  id <- context$observations[[1L]]$id
  raw <- context$observations[[1L]]$cluster_values
  record <- list(id = id, label = "expert tissue label", status = "reviewed",
    reviewer = "synthetic reviewer", updated_at = "2026-10-19T12:00:00.000Z", action = "correct")
  checkpoint <- wsi_review_checkpoint(context, id, list(record), revision = 1,
    history = list(past = list(list(selected_ids = list(), review_records = list())), future = list()))
  expect_identical(context$observations[[1L]]$cluster_values, raw)
  expect_identical(checkpoint$review_records[[1L]]$label, "expert tissue label")
  path <- tempfile(fileext = ".json")
  on.exit(unlink(path), add = TRUE)
  wsi_write_review_checkpoint(checkpoint, context, path)
  restored <- wsi_read_review_checkpoint(path, context)
  expect_identical(restored, checkpoint)
  replacement <- checkpoint
  replacement$revision <- 2
  wsi_write_review_checkpoint(replacement, context, path, overwrite = TRUE)
  expect_identical(wsi_read_review_checkpoint(path, context), replacement)
  changed <- context$images
  changed[[1L]]$mpp_x <- .25
  revised <- wsi_review_context(changed, context$observations, context$reductions,
    context_id = context$context_id, experiment_id = context$experiment_id, provenance = context$provenance)
  expect_error(wsi_read_review_checkpoint(path, revised), "different or changed")
  expect_error(wsi_review_checkpoint(context, review_records = list(record, record)), "duplicate")
  bad <- record
  bad$cluster_values <- list(rna_cluster = "changed")
  expect_error(wsi_review_checkpoint(context, review_records = list(bad)), "raw clusters")
  bad <- record
  bad$updated_at <- "yesterday"
  expect_error(wsi_review_checkpoint(context, review_records = list(bad)), "ISO-8601")
  bad <- record
  bad$action <- "exclude"
  expect_error(wsi_review_checkpoint(context, review_records = list(bad)), "action and status")
  old <- options(wsiTools.review_max_checkpoint_bytes = 10L)
  expect_error(wsi_write_review_checkpoint(checkpoint, context, path, overwrite = TRUE), "bounded size")
  options(old)
  expect_identical(wsi_read_review_checkpoint(path, context), replacement)
  checkpoint$history$past[[1L]]$selected_ids <- list("unknown")
  expect_error(wsi_validate_review_checkpoint(checkpoint, context), "unknown")
})

test_that("source attachments and bounded histories are part of review integrity", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  attached <- wsi_review_context(context$images, context$observations, context$reductions,
    context_id = "study", experiment_id = "study", provenance = list(source_reference = list(sha256 = "source-A")))
  changed <- wsi_review_context(context$images, context$observations, context$reductions,
    context_id = "study", experiment_id = "study", provenance = list(source_reference = list(sha256 = "source-B")))
  expect_false(identical(attached$source_signature, changed$source_signature))
  old <- options(wsiTools.review_max_history = 1L)
  on.exit(options(old), add = TRUE)
  expect_error(wsi_review_checkpoint(context, history = list(past = list(list(), list()), future = list())), "bounded")
  expect_error(wsi_review_checkpoint(context, history = list(past = list(), future = list(), past = list())), "past and future")
})

test_that("full-precision JSON preserves decimal review source signatures", {
  skip_if_not_installed("openssl")
  context <- review_contract_fixture()
  images <- context$images
  images[[1L]]$mpp_x <- 0.252345678912345
  observations <- context$observations
  observations[[1L]]$x <- 10.12345678912345
  reductions <- context$reductions
  reductions[[1L]]$points[[1L]]$x <- -0.12345678912345
  context <- wsi_review_context(images, observations, reductions, context_id = "decimal-source")
  roundtrip <- function(digits) jsonlite::fromJSON(jsonlite::toJSON(context,
    auto_unbox = TRUE, null = "null", digits = digits), simplifyVector = FALSE)
  expect_identical(wsi_validate_review_context(roundtrip(17))$source_signature, context$source_signature)
  expect_error(wsi_validate_review_context(roundtrip(4)), "signature")
  context$observations[[1L]]$footprint <- list(x0 = -2, y0 = 16, x1 = 18, y1 = 24)
  context$observations[[1L]]$footprint_width_px <- 20
  rebuilt <- wsi_review_context(context$images, context$observations, context$reductions)
  expect_equal(rebuilt$observations[[1L]]$footprint$x0, -2)
})
