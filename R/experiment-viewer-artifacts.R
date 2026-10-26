# Small, checked display derivatives from an experiment manifest. The source
# image, cell vectors, and model outputs are never embedded in viewer HTML.
wsi_experiment_viewer_artifacts <- function(experiment) {
  plots <- list()
  coverage <- list()
  for (tissue in experiment$tissues) {
    artifacts <- tissue$artifacts %||% list()
    role <- vapply(artifacts, function(item) as.character(item$role %||% ""), character(1))
    three_roles <- c("kodama_three_group_embedding_plot",
      "kodama_three_group_spatial_map", "kodama_three_group_clusters",
      "kodama_three_group_tiles", "kodama_three_group_binding_receipt")
    has_three <- any(role %in% three_roles)
    if (has_three) {
      indices <- lapply(three_roles, function(r) which(role == r))
      names(indices) <- three_roles
      if (any(lengths(indices) != 1L))
        wsi_abort("Three-group KODAMA display requires one embedding, spatial map, cluster CSV, tile CSV and binding receipt.")
      checked <- lapply(three_roles, function(r) wsi_experiment_file(
        artifacts[[indices[[r]]]], experiment$root, r, TRUE))
      names(checked) <- three_roles
      sizes <- vapply(checked, function(a) file.info(a$resolved_path)$size, numeric(1))
      if (any(is.na(sizes)) || any(sizes > 8 * 1024^2))
        wsi_abort("Three-group KODAMA display artifact exceeds 8 MiB.")
      receipt <- jsonlite::fromJSON(
        checked$kodama_three_group_binding_receipt$resolved_path,
        simplifyVector = FALSE)
      bound <- vapply(three_roles[-5L], function(r)
        identical(receipt$files[[r]]$sha256, checked[[r]]$sha256), logical(1))
      counts <- as.integer(unlist(receipt$cluster_sizes, use.names = FALSE))
      if (!identical(receipt$schema,
                     "cellphenotyper-pm016-kodama-three-group-binding") ||
          !identical(as.integer(receipt$schema_version), 1L) ||
          !identical(receipt$status, "exploratory_unlabeled_three_group_review") ||
          !identical(receipt$observation_unit,
                     "nonoverlapping_224px_virchow2_tissue_tile") ||
          !identical(receipt$label_semantics,
                     "anonymous_KODAMA_domains_not_tissue_classes") ||
          !identical(as.integer(receipt$observation_count), 1103L) ||
          !identical(names(receipt$cluster_sizes), c("1", "2", "3")) ||
          length(counts) != 3L || any(counts < 1L) || sum(counts) != 1103L ||
          !all(bound))
        wsi_abort("Three-group KODAMA artifacts disagree with their source-bound receipt.")
      cluster <- utils::read.csv(
        checked$kodama_three_group_clusters$resolved_path,
        stringsAsFactors = FALSE, nrows = 1104L)
      tile <- utils::read.csv(
        checked$kodama_three_group_tiles$resolved_path,
        stringsAsFactors = FALSE, nrows = 1104L)
      required <- c("label", "cluster")
      if (nrow(cluster) != 1103L || nrow(tile) != 1103L ||
          !all(required %in% names(cluster)) ||
          !all(c(required, "native_x", "native_y", "kodama_1", "kodama_2") %in% names(tile)) ||
          anyDuplicated(cluster$label) || anyDuplicated(tile$label) ||
          !identical(sort(unique(as.integer(cluster$cluster))), 1:3) ||
          !identical(sort(unique(as.integer(tile$cluster))), 1:3) ||
          !identical(as.integer(table(factor(cluster$cluster, levels = 1:3))), counts) ||
          !identical(as.integer(table(factor(tile$cluster, levels = 1:3))), counts) ||
          !identical(as.character(cluster$label), as.character(tile$label)) ||
          !identical(as.integer(cluster$cluster), as.integer(tile$cluster)) ||
          any(!is.finite(tile$kodama_1) | !is.finite(tile$kodama_2)))
        wsi_abort("Three-group KODAMA cluster and embedding rows disagree.")
      png <- checked$kodama_three_group_embedding_plot
      data <- readBin(png$resolved_path, "raw", n = as.integer(sizes[[three_roles[[1L]]]]))
      if (!identical(data[seq_len(min(8L, length(data)))],
                     as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))))
        wsi_abort("Three-group KODAMA embedding is not a PNG.")
      plots[[length(plots) + 1L]] <- list(
        id = paste0("experiment_kodama_three_", tissue$tissue_id),
        label = paste(tissue$name, "· KODAMA Virchow2 embedding · 3 groups"),
        tissue_id = tissue$tissue_id, profile = "virchow2", plot_type = "image",
        path = png$resolved_path,
        image_data = paste0("data:image/png;base64,", openssl::base64_encode(data)),
        point_count = 0L)
    }
    map <- which(role == "kodama_two_group_map")
    receipt <- which(role == "kodama_map_binding_receipt")
    cluster <- which(role == "kodama_two_group_clusters")
    cluster_receipt <- which(role == "kodama_two_group_receipt")
    if (!has_three && (length(map) || length(receipt) || length(cluster) || length(cluster_receipt))) {
      if (any(c(length(map), length(receipt), length(cluster), length(cluster_receipt)) != 1L))
        wsi_abort("KODAMA display map requires one map, binding, cluster CSV and cluster receipt per tissue.")
      png <- wsi_experiment_file(artifacts[[map]], experiment$root, "KODAMA map", TRUE)
      bound <- wsi_experiment_file(artifacts[[receipt]], experiment$root, "KODAMA map binding", TRUE)
      table <- wsi_experiment_file(artifacts[[cluster]], experiment$root, "KODAMA two-group clusters", TRUE)
      fit <- wsi_experiment_file(artifacts[[cluster_receipt]], experiment$root, "KODAMA two-group receipt", TRUE)
      if (file.info(png$resolved_path)$size > 8 * 1024^2 ||
          file.info(bound$resolved_path)$size > 2 * 1024^2 ||
          file.info(fit$resolved_path)$size > 2 * 1024^2 ||
          file.info(table$resolved_path)$size > 8 * 1024^2)
        wsi_abort("KODAMA display artifact exceeds its bounded size.")
      data <- readBin(png$resolved_path, "raw", n = as.integer(file.info(png$resolved_path)$size))
      if (!identical(data[seq_len(min(8L, length(data)))],
                     as.raw(c(137, 80, 78, 71, 13, 10, 26, 10))))
        wsi_abort("KODAMA display map is not a PNG.")
      binding <- jsonlite::fromJSON(bound$resolved_path, simplifyVector = FALSE)
      clustering <- jsonlite::fromJSON(fit$resolved_path, simplifyVector = FALSE)
      groups <- as.integer(unlist(binding$groups %||% list(), use.names = FALSE))
      if (!identical(binding$plot_sha256, png$sha256) ||
          !identical(binding$cluster_csv_sha256, table$sha256) ||
          !identical(binding$cluster_receipt_sha256, fit$sha256) ||
          !identical(binding$working_image_sha256, clustering$original_image_sha256) ||
          !identical(binding$observation_count, clustering$observation_count) ||
          !identical(clustering$csv_sha256, table$sha256) ||
          !identical(clustering$schema, "cellphenotyper-kodama-coordinate-kmeans-two-group") ||
          !isTRUE(clustering$native_fit_reused) ||
          !identical(as.integer(clustering$requested_groups), 2L) ||
          !identical(as.integer(clustering$obtained_groups), 2L) ||
          !identical(clustering$posthoc_merge, FALSE) ||
          !identical(groups, c(1L, 2L)) ||
          !identical(binding$schema, "pm016-kodama-two-group-map-binding-v1"))
        wsi_abort("KODAMA display map and two-group binding disagree.")
      assignments <- utils::read.csv(table$resolved_path, stringsAsFactors = FALSE, nrows = 100001L)
      if (nrow(assignments) != as.integer(binding$observation_count) ||
          !all(c("label", "cluster", "native_x", "native_y") %in% names(assignments)) ||
          anyDuplicated(assignments$label) ||
          !identical(sort(unique(as.integer(assignments$cluster))), c(1L, 2L)))
        wsi_abort("KODAMA cluster rows or groups disagree with the checked map binding.")
      plots[[length(plots) + 1L]] <- list(
        id = paste0("experiment_kodama_", tissue$tissue_id),
        label = paste(tissue$name, "· KODAMA Virchow2 · 2 groups"),
        tissue_id = tissue$tissue_id, profile = "virchow2", plot_type = "image",
        path = png$resolved_path, image_data = paste0("data:image/png;base64,", openssl::base64_encode(data)),
        point_count = 0L)
    }
    cells <- which(role == "cellvit_partial_cells")
    cell_receipt <- which(role == "cellvit_partial_receipt")
    if (length(cells) || length(cell_receipt)) {
      if (length(cells) != 1L || length(cell_receipt) != 1L)
        wsi_abort("Partial CellViT display requires one cell artifact and one receipt per tissue.")
      csv <- wsi_experiment_file(artifacts[[cells]], experiment$root, "partial CellViT cells", TRUE)
      checked <- wsi_experiment_file(artifacts[[cell_receipt]], experiment$root, "partial CellViT receipt", TRUE)
      if (file.info(checked$resolved_path)$size > 2 * 1024^2)
        wsi_abort("Partial CellViT receipt exceeds its bounded size.")
      record <- jsonlite::fromJSON(checked$resolved_path, simplifyVector = FALSE)
      completed <- unlist(record$completed_shards %||% list(), use.names = FALSE)
      planned <- as.integer(record$planned_shards)
      count <- as.integer(record$cell_count)
      if (!identical(record$schema, "pm016-partial-cell-review-layer-v1") ||
          !identical(record$coverage, "partial") ||
          !identical(record$csv_sha256, csv$sha256) ||
          length(planned) != 1L || is.na(planned) || planned < 1L ||
          !length(completed) || length(completed) > planned || anyDuplicated(completed) ||
          length(count) != 1L || is.na(count) || count < 0L ||
          !identical(as.integer(artifacts[[cells]]$planned_shards), planned) ||
          !identical(unlist(artifacts[[cells]]$completed_shards, use.names = FALSE), completed))
        wsi_abort("Partial CellViT receipt disagrees with its checked cell artifact.")
      heads <- c("pannuke", "consep", "lizard", "nucls_main", "nucls_super",
                 "ocelot", "midog", "panoptils")
      archives <- record$source_archives %||% list()
      valid_hash <- function(value) is.character(value) && length(value) == 1L &&
        grepl("^[0-9a-f]{64}$", value)
      taxonomies <- unlist(record$classifier_taxonomies %||% list(), use.names = FALSE)
      if (length(taxonomies) != length(heads) || anyDuplicated(taxonomies) ||
          !setequal(taxonomies, heads) ||
          length(archives) != length(completed) ||
          !identical(vapply(archives, `[[`, character(1), "shard_id"), completed) ||
          sum(vapply(archives, function(item) as.integer(item$cell_count), integer(1))) != count ||
          any(!vapply(archives, function(item) all(vapply(
            c("archive_sha256", "plan_sha256", "classification_member_sha256"),
            function(key) valid_hash(item[[key]]), logical(1))), logical(1))))
        wsi_abort("Partial CellViT source archives or eight-head catalog disagree with the receipt.")
      if (file.info(csv$resolved_path)$size > 64 * 1024^2)
        wsi_abort("Partial CellViT display CSV exceeds the bounded source size.")
      cells_table <- utils::read.csv(csv$resolved_path, stringsAsFactors = FALSE,
                                     nrows = 200001L, check.names = FALSE)
      required <- c("id", "x", "y", "shard_id", unlist(lapply(heads, function(head)
        paste0(head, c("_label", "_status", "_probability"))), use.names = FALSE))
      if (nrow(cells_table) != count || !all(required %in% names(cells_table)) ||
          anyDuplicated(cells_table$id) ||
          !setequal(unique(cells_table$shard_id), completed) ||
          any(!is.finite(cells_table$x) | !is.finite(cells_table$y)) ||
          any(cells_table$x < 0 | cells_table$x >= tissue$image$width |
              cells_table$y < 0 | cells_table$y >= tissue$image$height))
        wsi_abort("Partial CellViT rows, heads, shard IDs or image coordinates disagree with the receipt.")
      # These are the native coordinates of the indexed cell positions, not a
      # claim that the surrounding rectangular tissue has been processed.
      cell_bounds <- if (count) list(
        xmin = min(cells_table$x), ymin = min(cells_table$y),
        xmax = max(cells_table$x), ymax = max(cells_table$y)) else NULL
      coverage[[length(coverage) + 1L]] <- list(
        tissue_id = tissue$tissue_id, cell_count = count,
        completed_shards = length(completed), planned_shards = planned,
        source_sha256 = csv$sha256, coverage = "partial",
        cell_position_bounds = cell_bounds,
        focus_cell_position = if (count) list(x = cells_table$x[[1L]],
                                             y = cells_table$y[[1L]]) else NULL)
    }
  }
  list(kodama = list(enabled = length(plots) > 0L, geojsons = list(), plots = plots),
       cell_coverage = coverage,
       bora_interpretation = experiment$provenance$interpretation %||% NULL,
       bora_status = experiment$settings$bora_status %||%
         experiment$provenance$bora_status %||% NULL)
}
