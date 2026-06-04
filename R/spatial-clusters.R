#' Inspect spatial-object clustering metadata
#'
#' These helpers look for already-computed clustering, class, or annotation
#' fields stored inside Seurat, Giotto-like, and SpatialExperiment-like objects.
#' They inspect metadata tables only; expression matrices and whole-slide images
#' are not copied into the browser or loaded into memory.
#'
#' @param object A Seurat, Giotto, SpatialExperiment, linked wsiTools spatial
#'   object, or compatible list-like object.
#' @param spot_ids Optional spot/cell identifiers used to align metadata to the
#'   viewer spot order.
#' @param field Optional cluster field name. When `NULL`, all detected fields
#'   are returned.
#'
#' @return `wsi_spatial_cluster_fields()` returns a data frame describing
#'   available fields. `wsi_spatial_clusters()` returns one row per spot/cell
#'   with aligned cluster values.
#' @export
#'
#' @examples
#' obj <- list(meta.data = data.frame(
#'   seurat_clusters = c("0", "1"),
#'   row.names = c("spot_a", "spot_b")
#' ))
#' wsi_spatial_cluster_fields(obj, spot_ids = c("spot_a", "spot_b"))
wsi_spatial_cluster_fields <- function(object, spot_ids = NULL) {
  if (inherits(object, "wsi_seurat_spatial") || inherits(object, "wsi_spatial_object")) {
    fields <- object$cluster_fields %||% object$clusters$fields %||% NULL
    if (is.data.frame(fields)) {
      class(fields) <- unique(c("wsi_spatial_cluster_fields", class(fields)))
      return(fields)
    }
  }
  spot_ids <- wsi_spatial_cluster_spot_ids(spot_ids)
  tables <- wsi_spatial_cluster_tables(object)
  if (!length(tables)) {
    return(wsi_empty_spatial_cluster_fields())
  }

  rows <- list()
  for (entry in tables) {
    data <- entry$data
    fields <- wsi_spatial_cluster_candidate_columns(data)
    if (!length(fields)) {
      next
    }
    id_info <- wsi_spatial_cluster_ids(data, spot_ids = spot_ids)
    for (field in fields) {
      values <- wsi_spatial_cluster_align_values(data[[field]], id_info, spot_ids)
      matched <- if (is.null(spot_ids)) sum(!is.na(values)) else sum(!is.na(values))
      if (matched < 1L) {
        next
      }
      levels <- wsi_spatial_cluster_levels(values)
      if (!length(levels)) {
        next
      }
      rows[[length(rows) + 1L]] <- data.frame(
        field = field,
        label = wsi_spatial_cluster_label(field),
        n_clusters = length(levels),
        storage = entry$storage,
        matched_count = as.integer(matched),
        value_preview = paste(utils::head(levels, 6L), collapse = ", "),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) {
    return(wsi_empty_spatial_cluster_fields())
  }
  out <- do.call(rbind, rows)
  out <- out[order(out$field, -out$matched_count, out$storage), , drop = FALSE]
  out <- out[!duplicated(tolower(out$field)), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("wsi_spatial_cluster_fields", "data.frame")
  out
}

#' @rdname wsi_spatial_cluster_fields
#' @export
wsi_spatial_clusters <- function(object, spot_ids = NULL, field = NULL) {
  if (inherits(object, "wsi_seurat_spatial") || inherits(object, "wsi_spatial_object")) {
    values <- object$cluster_values %||% NULL
    if (is.data.frame(values)) {
      if (is.null(field)) {
        return(values)
      }
      fields <- wsi_spatial_cluster_match_fields(field, names(values))
      out <- values[, unique(c("id", fields)), drop = FALSE]
      attr(out, "fields") <- object$cluster_fields %||% attr(values, "fields", exact = TRUE)
      class(out) <- c("wsi_spatial_clusters", "data.frame")
      return(out)
    }
  }

  spot_ids <- wsi_spatial_cluster_spot_ids(spot_ids)
  fields <- wsi_spatial_cluster_fields(object, spot_ids = spot_ids)
  if (!nrow(fields)) {
    out <- data.frame(id = spot_ids %||% character(), stringsAsFactors = FALSE)
    attr(out, "fields") <- fields
    class(out) <- c("wsi_spatial_clusters", "data.frame")
    return(out)
  }
  requested <- if (is.null(field)) fields$field else wsi_spatial_cluster_match_fields(field, fields$field)
  fields <- fields[match(requested, fields$field), , drop = FALSE]

  ids <- spot_ids
  if (is.null(ids)) {
    ids <- wsi_spatial_cluster_first_ids(object, fields)
  }
  out <- data.frame(id = ids %||% character(), stringsAsFactors = FALSE)
  tables <- wsi_spatial_cluster_tables(object)
  for (i in seq_len(nrow(fields))) {
    field_name <- fields$field[[i]]
    storage <- fields$storage[[i]]
    entry <- wsi_spatial_cluster_find_table(tables, storage = storage, field = field_name)
    if (is.null(entry)) {
      next
    }
    id_info <- wsi_spatial_cluster_ids(entry$data, spot_ids = ids)
    out[[field_name]] <- wsi_spatial_cluster_align_values(entry$data[[field_name]], id_info, ids)
  }
  attr(out, "fields") <- fields
  class(out) <- c("wsi_spatial_clusters", "data.frame")
  out
}

wsi_empty_spatial_cluster_fields <- function() {
  out <- data.frame(
    field = character(),
    label = character(),
    n_clusters = integer(),
    storage = character(),
    matched_count = integer(),
    value_preview = character(),
    stringsAsFactors = FALSE
  )
  class(out) <- c("wsi_spatial_cluster_fields", "data.frame")
  out
}

wsi_spatial_cluster_spot_ids <- function(spot_ids) {
  if (is.null(spot_ids)) {
    return(NULL)
  }
  spot_ids <- as.character(spot_ids)
  spot_ids[nzchar(spot_ids) & !is.na(spot_ids)]
}

wsi_spatial_cluster_tables <- function(object) {
  tables <- list()
  add_table <- function(x, storage) {
    data <- wsi_spatial_cluster_table_data(x)
    if (is.null(data) || !nrow(data) || !length(wsi_spatial_cluster_candidate_columns(data))) {
      return(invisible(NULL))
    }
    key <- paste(storage, nrow(data), paste(names(data), collapse = "\r"), sep = "\n")
    if (!key %in% vapply(tables, `[[`, character(1), "key")) {
      tables[[length(tables) + 1L]] <<- list(storage = storage, data = data, key = key)
    }
    invisible(NULL)
  }

  add_idents <- function(x) {
    idents <- wsi_seurat_try_accessor("SeuratObject", "Idents", object = x) %||%
      wsi_seurat_try_accessor("Seurat", "Idents", object = x)
    if (is.null(idents)) {
      return(invisible(NULL))
    }
    values <- as.character(idents)
    ids <- names(idents) %||% rownames(as.data.frame(idents, stringsAsFactors = FALSE))
    if (is.null(ids) || length(ids) != length(values)) {
      ids <- as.character(seq_along(values))
    }
    add_table(
      data.frame(id = as.character(ids), active_ident = values, stringsAsFactors = FALSE),
      "Idents"
    )
    invisible(NULL)
  }

  for (slot_name in c(
    "meta.data", "meta_data", "metadata", "colData", "col_data",
    "cell_metadata", "spot_metadata", "cellMetaObj", "obs"
  )) {
    add_table(wsi_seurat_slot(object, slot_name), slot_name)
  }
  add_table(wsi_optional_accessor("SummarizedExperiment", "colData", x = object), "colData")
  add_idents(object)

  visit <- function(x, path = "object", depth = 0L, seen = character()) {
    if (depth > 4L || is.null(x)) {
      return(invisible(NULL))
    }
    key <- wsi_spatial_seen_key(x)
    if (key %in% seen) {
      return(invisible(NULL))
    }
    seen <- c(seen, key)
    add_table(x, path)
    children <- wsi_spatial_children(x)
    if (!length(children)) {
      return(invisible(NULL))
    }
    skip <- c(
      "assays", "images", "image", "reductions", "dimension_reduction",
      "dim_reduction", "dimReduction", "expression", "exprs", "counts",
      "data", "scale.data", "scale_data", "spatial_locs", "spatialCoords"
    )
    preferred <- intersect(c(
      "meta.data", "meta_data", "metadata", "colData", "col_data",
      "cell_metadata", "spot_metadata", "cellMetaObj", "obs"
    ), names(children))
    ordered <- c(preferred, setdiff(names(children), c(preferred, skip)))
    for (nm in ordered) {
      visit(children[[nm]], paste(path, nm, sep = "$"), depth = depth + 1L, seen = seen)
    }
    invisible(NULL)
  }
  visit(object)
  lapply(tables, function(x) x[c("storage", "data")])
}

wsi_spatial_cluster_table_data <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  data <- if (is.data.frame(x)) {
    as.data.frame(x, stringsAsFactors = FALSE)
  } else if (inherits(x, "DataFrame")) {
    tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(data) || !length(data)) {
    return(NULL)
  }
  data
}

wsi_spatial_cluster_candidate_columns <- function(data) {
  if (!is.data.frame(data) || !length(data)) {
    return(character())
  }
  id_cols <- c(
    "barcode", "barcodes", "cell_ID", "cell_id", "cell", "cells", "spot",
    "spot_id", "sample_id", "id", "ID", "image_id", "section"
  )
  candidates <- setdiff(names(data), id_cols)
  if (!length(candidates)) {
    return(character())
  }
  key <- tolower(gsub("[^a-z0-9]+", "_", candidates))
  name_hit <- grepl(
    paste(
      "seurat_clusters", "cluster", "clusters", "clustering", "leiden",
      "louvain", "community", "subcluster", "kmeans", "graphclust",
      "membership", "cell_type", "celltype", "annotation", "annot",
      "active_ident", "identity", "ident", "class", "group", "groups",
      "snn_res", "res",
      sep = "|"
    ),
    key
  )
  candidates <- candidates[name_hit]
  candidates[vapply(candidates, function(nm) wsi_spatial_cluster_value_column(data[[nm]]), logical(1))]
}

wsi_spatial_cluster_value_column <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  if (is.factor(x) || is.character(x) || is.logical(x)) {
    return(length(wsi_spatial_cluster_levels(x)) > 0L)
  }
  if (is.numeric(x) || is.integer(x)) {
    finite <- is.finite(x)
    if (!any(finite)) {
      return(FALSE)
    }
    unique_n <- length(unique(x[finite]))
    return(unique_n <= min(200L, max(50L, ceiling(sum(finite) * 0.5))))
  }
  FALSE
}

wsi_spatial_cluster_ids <- function(data, spot_ids = NULL) {
  id_col <- wsi_seurat_first_column(data, c(
    "barcode", "barcodes", "cell_ID", "cell_id", "cell", "cells",
    "spot", "spot_id", "sample_id", "id", "ID"
  ))
  ids <- if (!is.null(id_col)) as.character(data[[id_col]]) else rownames(data)
  if (is.null(ids) || length(ids) != nrow(data) || all(!nzchar(ids)) ||
      identical(ids, as.character(seq_len(nrow(data))))) {
    ids <- NULL
  }
  if (is.null(spot_ids)) {
    return(list(ids = ids %||% as.character(seq_len(nrow(data))), index = seq_len(nrow(data)), by_order = FALSE))
  }
  if (!is.null(ids)) {
    index <- match(spot_ids, ids)
    if (any(!is.na(index))) {
      return(list(ids = ids, index = index, by_order = FALSE))
    }
  }
  if (nrow(data) == length(spot_ids)) {
    return(list(ids = spot_ids, index = seq_along(spot_ids), by_order = TRUE))
  }
  list(ids = ids %||% as.character(seq_len(nrow(data))), index = rep(NA_integer_, length(spot_ids)), by_order = FALSE)
}

wsi_spatial_cluster_align_values <- function(values, id_info, spot_ids = NULL) {
  values <- as.character(values)
  values[is.na(values) | !nzchar(values)] <- NA_character_
  if (is.null(spot_ids)) {
    return(values)
  }
  out <- rep(NA_character_, length(spot_ids))
  idx <- id_info$index
  keep <- !is.na(idx) & idx >= 1L & idx <= length(values)
  out[keep] <- values[idx[keep]]
  out
}

wsi_spatial_cluster_levels <- function(values) {
  values <- as.character(values)
  values <- values[!is.na(values) & nzchar(values)]
  unique(values)
}

wsi_spatial_cluster_label <- function(field) {
  label <- gsub("[._-]+", " ", as.character(field))
  label <- trimws(label)
  if (!nzchar(label)) {
    return("Cluster")
  }
  paste0(toupper(substr(label, 1L, 1L)), substr(label, 2L, nchar(label)))
}

wsi_spatial_cluster_match_fields <- function(field, fields) {
  field <- as.character(field)
  if (!length(field) || anyNA(field) || any(!nzchar(field))) {
    wsi_abort("`field` must contain non-empty cluster field names.")
  }
  out <- character()
  for (query in field) {
    idx <- match(tolower(query), tolower(fields))
    if (is.na(idx)) {
      wsi_abort(sprintf("Cluster field `%s` was not found.", query))
    }
    out <- c(out, fields[[idx]])
  }
  out
}

wsi_spatial_cluster_find_table <- function(tables, storage, field) {
  for (entry in tables) {
    if (identical(entry$storage, storage) && field %in% names(entry$data)) {
      return(entry)
    }
  }
  NULL
}

wsi_spatial_cluster_first_ids <- function(object, fields) {
  tables <- wsi_spatial_cluster_tables(object)
  if (!length(tables) || !nrow(fields)) {
    return(character())
  }
  entry <- wsi_spatial_cluster_find_table(tables, fields$storage[[1L]], fields$field[[1L]])
  if (is.null(entry)) {
    return(character())
  }
  wsi_spatial_cluster_ids(entry$data)$ids
}

wsi_spatial_subset_clusters <- function(clusters, idx) {
  if (!is.data.frame(clusters) || !nrow(clusters)) {
    return(clusters)
  }
  out <- clusters[idx, , drop = FALSE]
  attr(out, "fields") <- attr(clusters, "fields", exact = TRUE)
  class(out) <- class(clusters)
  rownames(out) <- NULL
  out
}

wsi_spatial_cluster_value_items <- function(clusters) {
  if (!is.data.frame(clusters) || ncol(clusters) <= 1L || !nrow(clusters)) {
    return(list())
  }
  fields <- setdiff(names(clusters), "id")
  lapply(seq_len(nrow(clusters)), function(i) {
    row <- as.list(as.character(clusters[i, fields, drop = TRUE]))
    names(row) <- fields
    row
  })
}

wsi_spatial_add_cluster_columns <- function(spots, clusters) {
  if (!is.data.frame(spots) || !is.data.frame(clusters) || ncol(clusters) <= 1L) {
    return(spots)
  }
  fields <- setdiff(names(clusters), "id")
  for (field in fields) {
    col <- paste0("cluster_", wsi_safe_id(field, "field"))
    spots[[col]] <- clusters[[field]]
  }
  spots
}

wsi_spatial_cluster_config <- function(clusters) {
  fields <- attr(clusters, "fields", exact = TRUE)
  if (!is.data.frame(clusters) || ncol(clusters) <= 1L || !is.data.frame(fields) || !nrow(fields)) {
    return(list(enabled = FALSE, fields = list(), default_field = NULL))
  }
  field_payload <- lapply(seq_len(nrow(fields)), function(i) {
    field <- fields$field[[i]]
    values <- if (field %in% names(clusters)) clusters[[field]] else character()
    colours <- wsi_spatial_cluster_colour_map(values)
    list(
      field = field,
      label = fields$label[[i]],
      storage = fields$storage[[i]],
      n_clusters = as.integer(fields$n_clusters[[i]]),
      matched_count = as.integer(fields$matched_count[[i]]),
      value_preview = fields$value_preview[[i]],
      levels = lapply(names(colours), function(value) {
        list(value = value, colour = unname(colours[[value]]))
      })
    )
  })
  list(
    enabled = length(field_payload) > 0L,
    fields = field_payload,
    default_field = field_payload[[1L]]$field %||% NULL
  )
}

wsi_spatial_cluster_colour_map <- function(values) {
  levels <- wsi_spatial_cluster_levels(values)
  palette <- c(
    "#2563eb", "#dc2626", "#16a34a", "#9333ea", "#ea580c", "#0891b2",
    "#be123c", "#4f46e5", "#65a30d", "#c026d3", "#ca8a04", "#0f766e",
    "#7c3aed", "#db2777", "#0284c7", "#84cc16"
  )
  stats::setNames(palette[((seq_along(levels) - 1L) %% length(palette)) + 1L], levels)
}
