test_that("CRAN core keeps power backends optional", {
  description <- read.dcf(system.file("DESCRIPTION", package = "wsiTools"))
  fields <- colnames(description)

  imports <- character()
  if ("Imports" %in% fields) {
    imports <- trimws(unlist(strsplit(description[1L, "Imports"], ",")))
    imports <- sub("\\s*\\(.*\\)$", "", imports)
  }

  suggests <- character()
  if ("Suggests" %in% fields) {
    suggests <- trimws(unlist(strsplit(description[1L, "Suggests"], ",")))
    suggests <- sub("\\s*\\(.*\\)$", "", suggests)
  }

  mandatory_heavy <- c(
    "Rcpp", "reticulate", "sf", "magick", "httpuv", "processx",
    "terra", "stars", "EBImage", "rhdf5", "zellkonverter"
  )

  expect_false("LinkingTo" %in% fields)
  expect_false(any(mandatory_heavy %in% imports))
  expect_true(all(c("callr", "httpuv", "magick", "sf") %in% suggests))
  expect_match(
    description[1L, "SystemRequirements"],
    "None required for installation",
    fixed = TRUE
  )
  expect_match(description[1L, "SystemRequirements"], "Optional runtime", fixed = TRUE)
})
