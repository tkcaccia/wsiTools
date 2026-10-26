test_that("checked PM016 display derivatives reject broken bindings and inflated coverage", {
  skip_if_not_installed("openssl")
  root <- withr::local_tempdir()
  write <- function(name, value) {
    path <- file.path(root, name)
    jsonlite::write_json(value, path, auto_unbox = TRUE, pretty = TRUE)
    path
  }
  hash <- function(path) wsi_experiment_sha256(path)
  png <- file.path(root, "map.png")
  writeBin(openssl::base64_decode(paste0(
    "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJ",
    "AAAADUlEQVQIHWP4z8DwHwAFgAI/ScL6NwAAAABJRU5ErkJggg==")), png)
  clusters <- file.path(root, "clusters.csv")
  utils::write.csv(data.frame(label = c("a", "b"), cluster = c(1L, 2L),
                              native_x = c(10, 20), native_y = c(10, 20)),
                   clusters, row.names = FALSE)
  fit <- write("fit.json", list(schema = "cellphenotyper-kodama-coordinate-kmeans-two-group",
    native_fit_reused = TRUE, requested_groups = 2L, obtained_groups = 2L,
    posthoc_merge = FALSE, original_image_sha256 = paste(rep("a", 64), collapse = ""),
    observation_count = 2L, csv_sha256 = hash(clusters)))
  binding <- write("binding.json", list(schema = "pm016-kodama-two-group-map-binding-v1",
    plot_sha256 = hash(png), cluster_csv_sha256 = hash(clusters),
    cluster_receipt_sha256 = hash(fit), working_image_sha256 = paste(rep("a", 64), collapse = ""),
    observation_count = 2L, groups = c(1L, 2L)))
  heads <- c("pannuke", "consep", "lizard", "nucls_main", "nucls_super",
             "ocelot", "midog", "panoptils")
  cells <- data.frame(id = c("cell-a", "cell-b"), x = c(11, 22), y = c(12, 23),
                      shard_id = c("s1", "s2"))
  for (head in heads) {
    cells[[paste0(head, "_label")]] <- "other"
    cells[[paste0(head, "_status")]] <- "predicted"
    cells[[paste0(head, "_probability")]] <- 0.7
  }
  csv <- file.path(root, "cells.csv")
  utils::write.csv(cells, csv, row.names = FALSE)
  source_archive <- function(id) list(shard_id = id, cell_count = 1L,
    archive_sha256 = paste(rep("b", 64), collapse = ""),
    plan_sha256 = paste(rep("c", 64), collapse = ""),
    classification_member_sha256 = paste(rep("d", 64), collapse = ""))
  cell_receipt <- write("cells.json", list(schema = "pm016-partial-cell-review-layer-v1",
    coverage = "partial", planned_shards = 104L, completed_shards = c("s1", "s2"),
    cell_count = 2L, classifier_taxonomies = heads,
    source_archives = list(source_archive("s1"), source_archive("s2")),
    csv_sha256 = hash(csv)))
  artifact <- function(role, path, ...) c(list(role = role, path = basename(path),
    sha256 = hash(path)), list(...))
  experiment <- list(root = root, provenance = list(), tissues = list(list(
    tissue_id = "t1", name = "tissue", image = list(width = 100L, height = 100L),
    artifacts = list(
      artifact("kodama_two_group_map", png),
      artifact("kodama_map_binding_receipt", binding),
      artifact("kodama_two_group_clusters", clusters),
      artifact("kodama_two_group_receipt", fit),
      artifact("cellvit_partial_cells", csv, planned_shards = 104L,
               completed_shards = c("s1", "s2")),
      artifact("cellvit_partial_receipt", cell_receipt)))))
  check <- function(value) wsi_experiment_viewer_artifacts(value)
  expect_equal(check(experiment)$cell_coverage[[1L]]$cell_count, 2L)
  expect_equal(check(experiment)$cell_coverage[[1L]]$cell_position_bounds,
               list(xmin = 11, ymin = 12, xmax = 22, ymax = 23))
  expect_equal(check(experiment)$cell_coverage[[1L]]$focus_cell_position,
               list(x = 11, y = 12))
  expect_equal(length(check(experiment)$kodama$plots), 1L)

  wrong_binding <- jsonlite::fromJSON(binding, simplifyVector = FALSE)
  wrong_binding$cluster_csv_sha256 <- paste(rep("0", 64), collapse = "")
  jsonlite::write_json(wrong_binding, binding, auto_unbox = TRUE)
  experiment$tissues[[1L]]$artifacts[[2L]]$sha256 <- hash(binding)
  expect_error(check(experiment), "two-group binding disagree")

  wrong_binding$cluster_csv_sha256 <- hash(clusters)
  jsonlite::write_json(wrong_binding, binding, auto_unbox = TRUE)
  experiment$tissues[[1L]]$artifacts[[2L]]$sha256 <- hash(binding)
  wrong_cells <- jsonlite::fromJSON(cell_receipt, simplifyVector = FALSE)
  wrong_cells$coverage <- "complete"
  jsonlite::write_json(wrong_cells, cell_receipt, auto_unbox = TRUE)
  experiment$tissues[[1L]]$artifacts[[6L]]$sha256 <- hash(cell_receipt)
  expect_error(check(experiment), "Partial CellViT receipt disagrees")

  wrong_cells$coverage <- "partial"
  wrong_cells$source_archives[[1L]]$cell_count <- 2L
  jsonlite::write_json(wrong_cells, cell_receipt, auto_unbox = TRUE)
  experiment$tissues[[1L]]$artifacts[[6L]]$sha256 <- hash(cell_receipt)
  expect_error(check(experiment), "source archives or eight-head catalog")
})
