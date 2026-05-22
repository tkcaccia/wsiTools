wsi_case_project <- function(project) {
  if (inherits(project, "wsi_project")) {
    return(project)
  }
  if (is.character(project) && length(project) == 1L && !is.na(project) && nzchar(project)) {
    return(wsi_read_project(project))
  }
  if (inherits(project, "wsi_viewer_session") || inherits(project, "wsi_viewer_state") ||
      inherits(project, "wsi_slide")) {
    return(wsi_project(project))
  }
  if (is.list(project)) {
    class(project) <- c("wsi_project", class(project))
    return(project)
  }
  wsi_abort("`project` must be a `wsi_project`, project directory, viewer session/state, slide, or compatible list.")
}

wsi_case_table <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (is.data.frame(x)) {
    return(as.data.frame(x, stringsAsFactors = FALSE))
  }
  NULL
}

wsi_case_measurement_table <- function(project, name) {
  measurements <- project$measurements
  if (inherits(measurements, "wsi_measurement_report") || is.list(measurements)) {
    return(wsi_case_table(measurements[[name]]))
  }
  NULL
}

wsi_case_viewer_table <- function(project, name) {
  viewer <- project$viewer_state
  if (is.list(viewer)) {
    return(wsi_case_table(viewer[[name]]))
  }
  NULL
}

wsi_case_roi_summary <- function(project) {
  table <- wsi_case_measurement_table(project, "roi_summary") %||%
    wsi_case_viewer_table(project, "roi_summary")
  if (!is.null(table)) {
    return(table)
  }
  if (inherits(project$rois, "wsi_roi") && nrow(project$rois)) {
    return(wsi_roi_measurement_table(project$rois))
  }
  data.frame()
}

wsi_case_class_summary <- function(project, roi_summary = NULL) {
  table <- wsi_case_measurement_table(project, "class_summary") %||%
    wsi_case_viewer_table(project, "class_summary")
  if (!is.null(table)) {
    return(table)
  }
  if (inherits(project$rois, "wsi_roi") && nrow(project$rois)) {
    return(summarise_rois(project$rois))
  }
  if (is.data.frame(roi_summary) && nrow(roi_summary) && "roi_class" %in% names(roi_summary)) {
    area_col <- if ("area_px2" %in% names(roi_summary)) "area_px2" else NULL
    if (!is.null(area_col)) {
      out <- stats::aggregate(roi_summary[[area_col]], list(class = roi_summary$roi_class), sum)
      names(out)[[2L]] <- "area_px2"
      out$roi_count <- as.integer(tabulate(match(roi_summary$roi_class, out$class), nbins = nrow(out)))
      total <- sum(out$area_px2, na.rm = TRUE)
      out$percent_area <- if (total > 0) 100 * out$area_px2 / total else NA_real_
      return(out)
    }
  }
  data.frame()
}

wsi_case_ihc_summary <- function(project) {
  wsi_case_measurement_table(project, "ihc_summary") %||%
    wsi_case_viewer_table(project, "ihc_summary") %||%
    data.frame()
}

wsi_case_ihc_class_summary <- function(project, ihc_summary = NULL) {
  table <- wsi_case_measurement_table(project, "ihc_class_summary") %||%
    wsi_case_viewer_table(project, "ihc_class_summary")
  if (!is.null(table)) {
    return(table)
  }
  if (is.data.frame(ihc_summary) && nrow(ihc_summary)) {
    return(summarise_ihc_intensity(ihc_summary))
  }
  data.frame()
}

wsi_case_stain_summary <- function(project) {
  wsi_case_measurement_table(project, "stain_summary") %||%
    wsi_case_viewer_table(project, "stain_summary") %||%
    data.frame()
}

wsi_case_tile_manifests <- function(tile_manifest) {
  if (is.null(tile_manifest)) {
    return(list())
  }
  if (is.data.frame(tile_manifest)) {
    return(list(tile_manifest = tile_manifest))
  }
  if (is.list(tile_manifest)) {
    out <- tile_manifest[vapply(tile_manifest, is.data.frame, logical(1))]
    if (length(out)) {
      nms <- names(out)
      if (is.null(nms)) {
        nms <- rep("", length(out))
      }
      empty <- !nzchar(nms)
      nms[empty] <- sprintf("tile_manifest_%d", which(empty))
      names(out) <- nms
      return(out)
    }
  }
  list()
}

wsi_case_tile_tables <- function(tile_manifest) {
  manifests <- wsi_case_tile_manifests(tile_manifest)
  if (!length(manifests)) {
    return(list(summary = data.frame(), counts = data.frame()))
  }
  summary_rows <- lapply(names(manifests), function(name) {
    manifest <- manifests[[name]]
    data.frame(
      manifest = name,
      tile_count = nrow(manifest),
      saved_tile_count = if ("file" %in% names(manifest)) sum(!is.na(manifest$file) & nzchar(as.character(manifest$file))) else NA_integer_,
      roi_count = if ("roi_id" %in% names(manifest)) length(unique(stats::na.omit(manifest$roi_id))) else NA_integer_,
      class_count = if ("roi_class" %in% names(manifest)) length(unique(stats::na.omit(manifest$roi_class))) else if ("class" %in% names(manifest)) length(unique(stats::na.omit(manifest$class))) else NA_integer_,
      level_count = if ("level" %in% names(manifest)) length(unique(stats::na.omit(manifest$level))) else NA_integer_,
      stringsAsFactors = FALSE
    )
  })
  count_rows <- list()
  for (name in names(manifests)) {
    manifest <- manifests[[name]]
    variables <- intersect(c("split", "roi_class", "class", "roi_id", "level"), names(manifest))
    if (!length(variables)) {
      count_rows[[length(count_rows) + 1L]] <- data.frame(
        manifest = name,
        variable = "all",
        value = "all",
        tile_count = nrow(manifest),
        stringsAsFactors = FALSE
      )
      next
    }
    for (variable in variables) {
      value <- as.character(manifest[[variable]])
      value[is.na(value) | !nzchar(value)] <- "NA"
      counts <- as.data.frame(table(value), stringsAsFactors = FALSE)
      names(counts) <- c("value", "tile_count")
      count_rows[[length(count_rows) + 1L]] <- data.frame(
        manifest = name,
        variable = variable,
        value = counts$value,
        tile_count = as.integer(counts$tile_count),
        stringsAsFactors = FALSE
      )
    }
  }
  list(
    summary = do.call(rbind, summary_rows),
    counts = do.call(rbind, count_rows)
  )
}

wsi_case_json_value <- function(x) {
  if (is.null(x)) {
    return("null")
  }
  if (is.atomic(x) && length(x) <= 1L) {
    return(as.character(x %||% NA_character_))
  }
  if (is.atomic(x)) {
    return(paste(as.character(x), collapse = ", "))
  }
  jsonlite::toJSON(x, auto_unbox = TRUE, null = "null")
}

wsi_case_flatten_list <- function(x, prefix = "") {
  if (is.null(x) || !length(x)) {
    return(data.frame(key = character(), value = character(), stringsAsFactors = FALSE))
  }
  rows <- list()
  nms <- names(x)
  if (is.null(nms)) {
    nms <- as.character(seq_along(x))
  }
  for (i in seq_along(x)) {
    key <- if (nzchar(prefix)) paste(prefix, nms[[i]], sep = ".") else nms[[i]]
    value <- x[[i]]
    if (is.list(value) && !is.data.frame(value) && length(value) &&
        !all(vapply(value, function(item) is.atomic(item) && length(item) <= 1L, logical(1)))) {
      rows[[length(rows) + 1L]] <- wsi_case_flatten_list(value, key)
    } else {
      rows[[length(rows) + 1L]] <- data.frame(
        key = key,
        value = wsi_case_json_value(value),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

wsi_case_provenance_table <- function(project) {
  provenance <- project$processing_provenance %||%
    project$manifest$processing_provenance %||%
    project$provenance %||%
    list()
  wsi_case_flatten_list(provenance)
}

wsi_case_overview <- function(project, tables) {
  slide <- project$slide_info %||% project$manifest$slide %||% list()
  dims <- slide$dimensions %||% list()
  metadata <- project$metadata %||% project$manifest$metadata %||% list()
  tile_count <- if (is.data.frame(tables$tile_summary) && nrow(tables$tile_summary)) {
    sum(tables$tile_summary$tile_count, na.rm = TRUE)
  } else {
    0L
  }
  data.frame(
    case_id = as.character(metadata$case_id %||% metadata$id %||% NA_character_),
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS%z"),
    slide_path = as.character(project$slide_path %||% slide$path %||% NA_character_),
    backend = as.character(slide$backend %||% NA_character_),
    width = as.numeric(dims$width %||% NA_real_),
    height = as.numeric(dims$height %||% NA_real_),
    roi_count = if (inherits(project$rois, "wsi_roi")) nrow(project$rois) else if (is.data.frame(tables$roi_summary)) nrow(tables$roi_summary) else 0L,
    class_count = if (is.data.frame(tables$class_summary) && nrow(tables$class_summary)) nrow(tables$class_summary) else 0L,
    tile_count = tile_count,
    provenance_entries = if (is.data.frame(tables$provenance)) nrow(tables$provenance) else 0L,
    stringsAsFactors = FALSE
  )
}

wsi_case_collect_tables <- function(project) {
  roi_summary <- wsi_case_roi_summary(project)
  class_summary <- wsi_case_class_summary(project, roi_summary)
  ihc_summary <- wsi_case_ihc_summary(project)
  ihc_class_summary <- wsi_case_ihc_class_summary(project, ihc_summary)
  stain_summary <- wsi_case_stain_summary(project)
  tile_tables <- wsi_case_tile_tables(project$tile_manifest %||% project$tile_manifests)
  tables <- list(
    roi_summary = roi_summary,
    class_summary = class_summary,
    ihc_summary = ihc_summary,
    ihc_class_summary = ihc_class_summary,
    stain_summary = stain_summary,
    tile_summary = tile_tables$summary,
    tile_counts = tile_tables$counts,
    provenance = wsi_case_provenance_table(project)
  )
  tables$overview <- wsi_case_overview(project, tables)
  tables[c("overview", "roi_summary", "class_summary", "ihc_summary", "ihc_class_summary", "stain_summary", "tile_summary", "tile_counts", "provenance")]
}

wsi_case_report_dir <- function(output_dir = NULL, project = NULL) {
  if (is.null(output_dir)) {
    root <- project$path %||% NULL
    output_dir <- if (!is.null(root) && nzchar(root)) file.path(root, "case_report") else tempfile("wsi_case_report")
  }
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir)) {
    wsi_abort("`output_dir` must be a single non-empty directory path.")
  }
  if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create report directory: %s", output_dir))
  }
  normalizePath(output_dir, winslash = "/", mustWork = TRUE)
}

wsi_case_write_tables <- function(tables, output_dir, overwrite = FALSE) {
  table_dir <- file.path(output_dir, "tables")
  if (!dir.exists(table_dir) && !dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)) {
    wsi_abort(sprintf("Could not create report table directory: %s", table_dir))
  }
  files <- character()
  for (name in names(tables)) {
    table <- tables[[name]]
    if (!is.data.frame(table) || !ncol(table)) {
      next
    }
    file <- file.path(table_dir, sprintf("%s.csv", name))
    wsi_project_write_csv(table, file, overwrite = overwrite)
    files[[name]] <- file
  }
  files
}

wsi_case_html_table <- function(table, max_rows = 20L) {
  if (!is.data.frame(table) || !nrow(table) || !ncol(table)) {
    return("<p class=\"empty\">No data available.</p>")
  }
  table <- as.data.frame(table, stringsAsFactors = FALSE)
  more <- nrow(table) > max_rows
  table <- utils::head(table, max_rows)
  header <- paste0("<tr>", paste0("<th>", wsi_html_escape(names(table)), "</th>", collapse = ""), "</tr>")
  rows <- apply(table, 1L, function(row) {
    paste0("<tr>", paste0("<td>", wsi_html_escape(row), "</td>", collapse = ""), "</tr>")
  })
  footer <- if (more) sprintf("<p class=\"caption\">Showing first %d rows.</p>", max_rows) else ""
  paste0("<table>", header, paste(rows, collapse = "\n"), "</table>", footer)
}

wsi_case_metric <- function(label, value) {
  sprintf(
    "<div class=\"metric\"><span>%s</span><strong>%s</strong></div>",
    wsi_html_escape(label),
    wsi_html_escape(value)
  )
}

wsi_case_html <- function(project, tables, files, output_dir, title = NULL) {
  overview <- tables$overview
  title <- title %||% sprintf("wsiTools Case Report%s", if (!is.na(overview$case_id[[1L]])) paste0(": ", overview$case_id[[1L]]) else "")
  csv_links <- if (length(files)) {
    paste0(
      "<ul>",
      paste(sprintf(
        "<li><a href=\"%s\">%s.csv</a></li>",
        wsi_html_escape(wsi_project_rel(files, output_dir)),
        wsi_html_escape(names(files))
      ), collapse = ""),
      "</ul>"
    )
  } else {
    "<p class=\"empty\">No CSV tables were written.</p>"
  }
  sections <- list(
    "ROI Areas" = tables$roi_summary,
    "Class Percentages And Cell Density" = tables$class_summary,
    "IHC ROI Intensity" = tables$ihc_summary,
    "IHC Class Intensity" = tables$ihc_class_summary,
    "Stain Channel Summary" = tables$stain_summary,
    "Tile Counts" = tables$tile_summary,
    "Tile Count Breakdowns" = tables$tile_counts,
    "Provenance" = tables$provenance
  )
  section_html <- paste(vapply(names(sections), function(name) {
    paste0("<section><h2>", wsi_html_escape(name), "</h2>", wsi_case_html_table(sections[[name]]), "</section>")
  }, character(1)), collapse = "\n")
  paste0(
    "<!doctype html>\n<html lang=\"en\"><head><meta charset=\"utf-8\">\n",
    "<title>", wsi_html_escape(title), "</title>\n",
    "<style>",
    "body{margin:0;background:#f8fafc;color:#111827;font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;line-height:1.45;}",
    "header{background:#111827;color:white;padding:28px 34px;}main{max-width:1180px;margin:0 auto;padding:24px;}",
    "h1{margin:0 0 6px;font-size:28px;}h2{font-size:19px;margin:0 0 12px;}section{background:white;border:1px solid #e5e7eb;border-radius:6px;padding:16px;margin:0 0 18px;box-shadow:0 1px 2px rgba(15,23,42,.04);}",
    ".meta{color:#cbd5e1;margin:0}.metrics{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:10px;margin:18px 0;}",
    ".metric{background:#eef2ff;border:1px solid #c7d2fe;border-radius:6px;padding:10px}.metric span{display:block;color:#475569;font-size:12px}.metric strong{font-size:20px;}",
    "table{width:100%;border-collapse:collapse;font-size:13px;}th,td{border-bottom:1px solid #e5e7eb;text-align:left;padding:7px 8px;vertical-align:top;}th{background:#f1f5f9;color:#334155;position:sticky;top:0;}",
    ".tableWrap{overflow:auto}.empty,.caption{color:#64748b;font-size:13px;}a{color:#0f766e;}",
    "</style></head><body><header><h1>", wsi_html_escape(title), "</h1>",
    "<p class=\"meta\">Generated ", wsi_html_escape(overview$generated_at[[1L]]), " | slide: ", wsi_html_escape(overview$slide_path[[1L]]), "</p></header>",
    "<main><div class=\"metrics\">",
    wsi_case_metric("ROIs", overview$roi_count[[1L]]),
    wsi_case_metric("Classes", overview$class_count[[1L]]),
    wsi_case_metric("Tiles", overview$tile_count[[1L]]),
    wsi_case_metric("Backend", overview$backend[[1L]]),
    "</div><section><h2>CSV Exports</h2>", csv_links, "</section>",
    section_html,
    "</main></body></html>\n"
  )
}

#' Export a case report from a wsiTools project
#'
#' Generates CSV tables and a lightweight HTML report from a `wsi_project`.
#' Reports include ROI areas, class percentages, cell-density columns when
#' available, IHC/stain intensity summaries, tile manifest counts, and
#' processing provenance. Pixel data are not copied or loaded.
#'
#' @param project A `wsi_project`, project directory, viewer session/state, or
#'   compatible project-like list.
#' @param output_dir Report directory. Defaults to `case_report` inside a saved
#'   project directory, or a temporary directory for in-memory projects.
#' @param html_file Optional HTML output path. Defaults to
#'   `output_dir/case_report.html`.
#' @param title Optional report title.
#' @param overwrite Whether to overwrite existing report files.
#' @param open Whether to open the HTML report with [utils::browseURL()].
#'
#' @return A `wsi_case_report` object with `html`, `tables`, `files`, and
#'   `output_dir` entries.
#' @export
#'
#' @examples
#' \dontrun{
#' project <- wsi_read_project("case_001.wsiproject")
#' report <- wsi_case_report(project, output_dir = "case_001_report")
#' }
wsi_case_report <- function(project, output_dir = NULL, html_file = NULL,
                            title = NULL, overwrite = FALSE, open = FALSE) {
  project <- wsi_case_project(project)
  output_dir <- wsi_case_report_dir(output_dir, project = project)
  html_file <- html_file %||% file.path(output_dir, "case_report.html")
  html_file <- wsi_validate_output_path(html_file, overwrite = overwrite)
  tables <- wsi_case_collect_tables(project)
  files <- wsi_case_write_tables(tables, output_dir, overwrite = overwrite)
  html <- wsi_case_html(project, tables, files, output_dir = output_dir, title = title)
  writeLines(html, html_file, useBytes = TRUE)
  if (isTRUE(open)) {
    utils::browseURL(html_file)
  }
  out <- list(
    html = html_file,
    output_dir = output_dir,
    files = files,
    tables = tables,
    project = project
  )
  class(out) <- "wsi_case_report"
  out
}

#' @rdname wsi_case_report
#' @export
write_case_report <- wsi_case_report

#' @rdname wsi_case_report
#' @export
wsi_write_case_report <- wsi_case_report

#' @export
print.wsi_case_report <- function(x, ...) {
  cat("<wsi_case_report>\n")
  cat(sprintf("  html: %s\n", x$html %||% NA_character_))
  cat(sprintf("  csv tables: %d\n", length(x$files %||% character())))
  invisible(x)
}
