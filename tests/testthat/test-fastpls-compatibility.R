test_that("CPU adapter supports old and current fastPLS without dropping solver settings", {
  old <- options(cores = 7L, n.cores = 9L)
  on.exit(options(old), add = TRUE)
  args <- list(Xtrain = matrix(1:12, 4), Ytrain = factor(c("a", "a", "b", "b")),
    ncomp = 2L, method = "simpls", classifier = "lda", scaling = "none",
    seed = 4L, fit = FALSE, proj = FALSE, return_variance = FALSE)
  legacy <- function(Xtrain, Ytrain, backend, n.cores, ...) {
    expect_identical(backend, "cpu")
    expect_identical(n.cores, 1L)
    expect_identical(getOption("n.cores"), 1L)
    list(...)
  }
  modern <- function(Xtrain, Ytrain, backend, ...) {
    dots <- list(...)
    expect_identical(backend, "cpu")
    expect_identical(getOption("cores"), 1L)
    expect_false("n.cores" %in% names(dots))
    expect_false("cores" %in% names(dots))
    dots
  }
  expected <- args[setdiff(names(args), c("Xtrain", "Ytrain"))]
  expect_identical(wsiTools:::wsi_fastpls_cpu_call(legacy, args), expected)
  expect_identical(wsiTools:::wsi_fastpls_cpu_call(modern, args), expected)
  expect_identical(getOption("cores"), 7L)
  expect_identical(getOption("n.cores"), 9L)
  failing <- function(backend, ...) stop("CPU fit failed")
  expect_error(wsiTools:::wsi_fastpls_cpu_call(failing, args), "CPU fit failed")
  expect_identical(getOption("cores"), 7L)
  expect_identical(getOption("n.cores"), 9L)
  expect_error(wsiTools:::wsi_fastpls_cpu_call(function(...) NULL, args), "explicitly select")
  expect_error(wsiTools:::wsi_fastpls_cpu_call(modern, c(args, list(backend = "cuda"))), "backend='cpu'", fixed = TRUE)
})

test_that("CPU prediction adapter preserves raw score requests across API generations", {
  object <- structure(list(id = "model"), class = "fastPLS")
  data <- matrix(1:6, 3)
  for (legacy in c(TRUE, FALSE)) {
    fun <- if (legacy) function(object, newdata, backend, n.cores, raw_scores, ...) {
      expect_identical(n.cores, 1L)
      list(object = object, newdata = newdata, backend = backend, raw_scores = raw_scores)
    } else function(object, newdata, backend, raw_scores, ...) {
      expect_length(list(...), 0L)
      expect_identical(getOption("cores"), 1L)
      list(object = object, newdata = newdata, backend = backend, raw_scores = raw_scores)
    }
    result <- wsiTools:::wsi_fastpls_cpu_call(fun, list(object = object, newdata = data, raw_scores = TRUE))
    expect_identical(result, list(object = object, newdata = data, backend = "cpu", raw_scores = TRUE))
  }
})

test_that("installed fastPLS CPU adapter fits and predicts a bounded twelve-row fixture", {
  skip_if_not(wsiTools::wsi_has_fastpls())
  x <- matrix(sin(seq_len(12 * 8)), 12, 8)
  y <- factor(rep(c("a", "b"), each = 6))
  x[, 1:3] <- x[, 1:3] + as.numeric(y) * 3
  fit <- wsiTools:::wsi_fastpls_cpu_call(get("pls", asNamespace("fastPLS")),
    list(Xtrain = x, Ytrain = y, ncomp = 2L, method = "simpls", classifier = "lda",
      scaling = "none", fit = FALSE, proj = FALSE, return_variance = FALSE, seed = 4L))
  prediction <- wsiTools:::wsi_fastpls_cpu_predict(fit, x, raw_scores = TRUE)
  labels <- prediction$Ypred
  if (is.data.frame(labels) || is.matrix(labels)) labels <- labels[, 1L]
  expect_length(labels, 12L)
  expect_identical(as.character(labels), as.character(y))
  expect_true(length(if (is.null(prediction$LDA_scores)) prediction$lda_scores else prediction$LDA_scores) > 0L)
  expect_identical(wsiTools:::wsi_prediction_fit(x, y, x, ncomp = 2L, scaling = "none"), as.character(y))
})
