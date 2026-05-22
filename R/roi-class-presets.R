wsi_class_preset_hex <- function(value, fallback = NA_character_) {
  if (is.null(value) || length(value) != 1L || is.na(value) || !nzchar(value)) {
    return(fallback)
  }
  out <- tryCatch({
    rgb <- grDevices::col2rgb(value)[, 1L]
    sprintf("#%02X%02X%02X", rgb[[1L]], rgb[[2L]], rgb[[3L]])
  }, error = function(err) fallback)
  out
}

wsi_default_roi_class_presets_data <- function() {
  data.frame(
    class = c("tumour", "stroma", "necrosis", "normal", "artefact", "exclusion", "invasive front"),
    label = c("tumour", "stroma", "necrosis", "normal", "artefact", "exclusion", "invasive front"),
    color = c("#D73027", "#4575B4", "#7F7F7F", "#1A9850", "#984EA3", "#000000", "#F46D43"),
    export = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
    export_rule = c("include", "include", "include", "include", "exclude", "exclude", "include"),
    stringsAsFactors = FALSE
  )
}

wsi_normalize_roi_class_presets <- function(presets = NULL, include_defaults = TRUE) {
  defaults <- wsi_default_roi_class_presets_data()
  if (is.null(presets)) {
    out <- defaults
  } else {
    if (is.list(presets) && !is.data.frame(presets)) {
      presets <- as.data.frame(presets, stringsAsFactors = FALSE)
    }
    if (!is.data.frame(presets)) {
      wsi_abort("`presets` must be `NULL`, a data frame, or a list coercible to a data frame.")
    }
    presets <- as.data.frame(presets, stringsAsFactors = FALSE)
    if ("colour" %in% names(presets) && !"color" %in% names(presets)) {
      presets$color <- presets$colour
    }
    if (!"class" %in% names(presets)) {
      wsi_abort("ROI class presets must contain a `class` column.")
    }
    missing_cols <- setdiff(names(defaults), names(presets))
    for (column in missing_cols) {
      presets[[column]] <- NA
    }
    presets <- presets[names(defaults)]
    out <- if (isTRUE(include_defaults)) defaults else defaults[0, , drop = FALSE]
    for (i in seq_len(nrow(presets))) {
      cls <- trimws(as.character(presets$class[[i]] %||% ""))
      if (!nzchar(cls)) {
        next
      }
      row <- presets[i, , drop = FALSE]
      row$class <- cls
      idx <- match(tolower(cls), tolower(out$class))
      if (is.na(idx)) {
        out <- rbind(out, row)
      } else {
        for (column in names(defaults)) {
          value <- row[[column]][[1L]]
          if (!is.na(value) && nzchar(as.character(value))) {
            out[[column]][[idx]] <- value
          }
        }
      }
    }
  }

  out$class <- trimws(as.character(out$class))
  out$label <- trimws(as.character(out$label))
  out$label[is.na(out$label) | !nzchar(out$label)] <- out$class[is.na(out$label) | !nzchar(out$label)]
  out$color <- vapply(out$color, wsi_class_preset_hex, character(1), fallback = "#00BFC4")
  out$export <- as.logical(out$export)
  out$export[is.na(out$export)] <- TRUE
  out$export_rule <- tolower(trimws(as.character(out$export_rule)))
  out$export_rule[is.na(out$export_rule) | !nzchar(out$export_rule)] <- ifelse(out$export, "include", "exclude")
  valid_rules <- c("include", "exclude")
  bad <- !out$export_rule %in% valid_rules
  if (any(bad)) {
    wsi_abort("ROI class preset `export_rule` values must be `include` or `exclude`.")
  }
  out$export[out$export_rule == "exclude"] <- FALSE
  out <- out[!duplicated(tolower(out$class)), , drop = FALSE]
  rownames(out) <- NULL
  class(out) <- c("wsi_roi_class_presets", class(out))
  out
}

#' ROI Class Presets
#'
#' Return or validate editable pathology class presets used by the viewer and
#' GeoJSON export helpers. The defaults include fixed colours for `tumour`,
#' `stroma`, `necrosis`, `normal`, `artefact`, `exclusion`, and
#' `invasive front`. Presets also carry optional export rules; by default
#' `artefact` and `exclusion` are marked as non-export classes when export rules
#' are explicitly respected.
#'
#' @param presets Optional data frame or list with columns `class`, `label`,
#'   `color`, `export`, and `export_rule`. Missing default classes are filled
#'   when `include_defaults = TRUE`.
#' @param include_defaults Whether to merge user presets into the default class
#'   table.
#'
#' @return A `wsi_roi_class_presets` data frame.
#' @export
#'
#' @examples
#' presets <- wsi_roi_class_presets()
#' presets <- wsi_update_roi_class_preset(presets, "tumour", color = "#FF0000")
wsi_roi_class_presets <- function(presets = NULL, include_defaults = TRUE) {
  wsi_normalize_roi_class_presets(presets, include_defaults = include_defaults)
}

#' @rdname wsi_roi_class_presets
#' @export
wsi_default_roi_class_presets <- function() {
  wsi_normalize_roi_class_presets(NULL)
}

#' @rdname wsi_roi_class_presets
#' @param class Class name to update.
#' @param label,color,export,export_rule Optional replacement values.
#' @export
wsi_update_roi_class_preset <- function(presets = NULL, class, label = NULL,
                                        color = NULL, export = NULL,
                                        export_rule = NULL) {
  presets <- wsi_normalize_roi_class_presets(presets)
  if (!is.character(class) || length(class) != 1L || is.na(class) || !nzchar(class)) {
    wsi_abort("`class` must be a single non-empty class name.")
  }
  idx <- match(tolower(class), tolower(presets$class))
  if (is.na(idx)) {
    presets <- rbind(
      presets,
      data.frame(
        class = class,
        label = class,
        color = "#00BFC4",
        export = TRUE,
        export_rule = "include",
        stringsAsFactors = FALSE
      )
    )
    idx <- nrow(presets)
  }
  if (!is.null(label)) {
    presets$label[[idx]] <- as.character(label)
  }
  if (!is.null(color)) {
    presets$color[[idx]] <- wsi_class_preset_hex(color, fallback = presets$color[[idx]])
  }
  if (!is.null(export)) {
    if (!is.logical(export) || length(export) != 1L || is.na(export)) {
      wsi_abort("`export` must be `TRUE` or `FALSE`.")
    }
    presets$export[[idx]] <- export
    presets$export_rule[[idx]] <- if (isTRUE(export)) "include" else "exclude"
  }
  if (!is.null(export_rule)) {
    export_rule <- tolower(as.character(export_rule))
    if (!export_rule %in% c("include", "exclude")) {
      wsi_abort("`export_rule` must be `include` or `exclude`.")
    }
    presets$export_rule[[idx]] <- export_rule
    presets$export[[idx]] <- identical(export_rule, "include")
  }
  wsi_normalize_roi_class_presets(presets, include_defaults = FALSE)
}

wsi_roi_class_preset_lookup <- function(presets) {
  presets <- wsi_normalize_roi_class_presets(presets)
  stats::setNames(seq_len(nrow(presets)), tolower(presets$class))
}

wsi_stable_roi_class_colour <- function(class, fallback = "#00BFC4") {
  class <- trimws(as.character(class %||% ""))
  if (!nzchar(class) || is.na(class)) {
    return(fallback)
  }
  palette <- c("#00BFC4", "#F8766D", "#7CAE00", "#C77CFF", "#E69F00", "#56B4E9", "#CC79A7")
  code <- utf8ToInt(tolower(class))
  if (!length(code)) {
    return(fallback)
  }
  palette[(sum(code * seq_along(code)) %% length(palette)) + 1L]
}

wsi_roi_class_colour <- function(class, presets = NULL, fallback = "#00BFC4") {
  class <- trimws(as.character(class %||% ""))
  if (!nzchar(class) || is.na(class)) {
    return(fallback)
  }
  presets <- wsi_normalize_roi_class_presets(presets)
  idx <- match(tolower(class), tolower(presets$class))
  if (!is.na(idx)) {
    return(presets$color[[idx]])
  }
  wsi_stable_roi_class_colour(class, fallback = fallback)
}

wsi_roi_class_export_allowed <- function(classes, presets = NULL) {
  presets <- wsi_normalize_roi_class_presets(presets)
  lookup <- wsi_roi_class_preset_lookup(presets)
  keys <- tolower(trimws(as.character(classes)))
  out <- rep(TRUE, length(keys))
  matched <- keys %in% names(lookup)
  out[matched] <- presets$export[unname(lookup[keys[matched]])]
  out
}

#' Apply ROI Class Presets
#'
#' Apply preset colours to an ROI table, and optionally filter ROIs according
#' to preset export rules.
#'
#' @param rois A `wsi_roi` object.
#' @param presets A preset table from [wsi_roi_class_presets()].
#' @param update_existing Whether preset colours should replace existing ROI
#'   colours. When `FALSE`, colours are only filled where missing.
#'
#' @return A modified `wsi_roi` object.
#' @export
wsi_apply_roi_class_presets <- function(rois, presets = NULL, update_existing = TRUE) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  presets <- wsi_normalize_roi_class_presets(presets)
  lookup <- wsi_roi_class_preset_lookup(presets)
  if (!"color" %in% names(rois)) {
    rois$color <- NA_character_
  }
  if (!"classification_color" %in% names(rois)) {
    rois$classification_color <- NA_character_
  }
  for (i in seq_len(nrow(rois))) {
    cls <- tolower(trimws(as.character(rois$class[[i]] %||% "")))
    idx <- if (is.na(cls) || !nzchar(cls)) NA_integer_ else unname(lookup[cls])
    if (is.na(idx)) {
      next
    }
    color <- presets$color[[idx]]
    missing_color <- is.na(rois$color[[i]]) || !nzchar(rois$color[[i]])
    if (isTRUE(update_existing) || isTRUE(missing_color)) {
      rois$color[[i]] <- color
      rois$classification_color[[i]] <- color
      if ("properties" %in% names(rois)) {
        properties <- wsi_geojson_list(rois$properties[[i]])
        classification <- wsi_qupath_classification(properties)
        classification$name <- rois$class[[i]]
        classification$color <- color
        classification$colorRGB <- NULL
        classification$color_rgb <- NULL
        classification$colour <- NULL
        properties$classification <- classification
        properties$class <- rois$class[[i]]
        rois$properties[[i]] <- properties
      }
    }
  }
  rois
}

#' @rdname wsi_apply_roi_class_presets
#' @param respect_export_rules Whether to drop classes whose preset export rule
#'   is `exclude`.
#' @export
wsi_filter_roi_export <- function(rois, presets = NULL, respect_export_rules = TRUE) {
  if (!inherits(rois, "wsi_roi")) {
    wsi_abort("`rois` must be a `wsi_roi` object.")
  }
  if (!isTRUE(respect_export_rules)) {
    return(rois)
  }
  keep <- wsi_roi_class_export_allowed(rois$class, presets = presets)
  rois[keep, , drop = FALSE]
}
