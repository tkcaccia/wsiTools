wsi_navigator_cache_dir <- function(create = TRUE) {
  configured <- Sys.getenv("WSITOOLS_NAVIGATOR_CACHE_DIR", unset = "")
  path <- if (nzchar(configured)) {
    path.expand(configured)
  } else {
    file.path(tools::R_user_dir("wsiTools", which = "cache"), "navigator")
  }
  if (isTRUE(create) && !dir.exists(path) &&
      !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    return(NULL)
  }
  normalizePath(path, winslash = "/", mustWork = isTRUE(create))
}

wsi_navigator_cache_key <- function(slide, width = 512L) {
  signature <- wsi_dynamic_path_signature(slide$path %||% "")
  wsi_dynamic_hash_text(paste(
    "navigator-v1",
    signature,
    slide$backend %||% "",
    slide$dimensions[["width"]] %||% "",
    slide$dimensions[["height"]] %||% "",
    as.integer(width),
    sep = "|"
  ))
}

wsi_navigator_cache_file <- function(slide, width = 512L, create = TRUE) {
  cache_dir <- wsi_navigator_cache_dir(create = create)
  if (is.null(cache_dir)) {
    return(NULL)
  }
  file.path(cache_dir, paste0(wsi_navigator_cache_key(slide, width = width), ".jpg"))
}

wsi_navigator_materialize <- function(cache_file, target) {
  if (!file.exists(cache_file) || file.info(cache_file)$size <= 0) {
    return(FALSE)
  }
  if (file.exists(target) && file.info(target)$size > 0) {
    return(TRUE)
  }
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  isTRUE(tryCatch(file.link(cache_file, target), error = function(err) FALSE)) ||
    isTRUE(file.copy(cache_file, target, overwrite = TRUE, copy.mode = TRUE))
}

wsi_navigator_preview_start <- function(slide, output, width = 512L) {
  source_path <- as.character(slide$path %||% "")
  if (length(source_path) != 1L || is.na(source_path) || !nzchar(source_path) ||
      !file.exists(source_path) || !wsi_has_vips() ||
      !requireNamespace("callr", quietly = TRUE)) {
    return(list(source = "", process = NULL, cached = FALSE, target = NULL))
  }
  cache_dir <- wsi_navigator_cache_dir()
  if (is.null(cache_dir)) {
    return(list(source = "", process = NULL, cached = FALSE, target = NULL))
  }
  key <- wsi_navigator_cache_key(slide, width = width)
  cache_file <- wsi_navigator_cache_file(slide, width = width)
  target <- file.path(dirname(output), paste0("wsi_navigator_", key, ".jpg"))
  source <- utils::URLencode(basename(target), reserved = TRUE)
  if (wsi_navigator_materialize(cache_file, target)) {
    return(list(source = source, process = NULL, cached = TRUE, target = target))
  }

  vips <- unname(Sys.which("vips"))
  process <- callr::r_bg(
    func = function(vips, input, cache_file, target, width) {
      quote_arg <- function(x) shQuote(x, type = if (.Platform$OS.type == "windows") "cmd" else "sh")
      dir.create(dirname(cache_file), recursive = TRUE, showWarnings = FALSE)
      dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
      tmp <- tempfile("navigator-", tmpdir = dirname(cache_file), fileext = ".jpg")
      on.exit(unlink(tmp, force = TRUE), add = TRUE)
      output <- suppressWarnings(system2(
        vips,
        c("thumbnail", quote_arg(input), quote_arg(tmp), as.character(as.integer(width))),
        stdout = TRUE,
        stderr = TRUE
      ))
      status <- attr(output, "status", exact = TRUE)
      if (!is.null(status) && as.integer(status) != 0L) {
        stop(paste(output, collapse = "\n"), call. = FALSE)
      }
      if (!file.exists(tmp) || file.info(tmp)$size <= 0) {
        stop("libvips did not create the navigator preview.", call. = FALSE)
      }
      if (!file.rename(tmp, cache_file)) {
        if (!file.exists(cache_file) && !file.copy(tmp, cache_file, overwrite = FALSE)) {
          stop("Could not publish the cached navigator preview.", call. = FALSE)
        }
      }
      if (!file.link(cache_file, target) && !file.copy(cache_file, target, overwrite = TRUE)) {
        stop("Could not place the navigator preview beside the viewer HTML.", call. = FALSE)
      }
      invisible(target)
    },
    args = list(
      vips = vips,
      input = normalizePath(source_path, winslash = "/", mustWork = TRUE),
      cache_file = cache_file,
      target = target,
      width = as.integer(width)
    ),
    supervise = FALSE
  )
  list(source = source, process = process, cached = FALSE, target = target)
}

wsi_navigator_preview_wait <- function(preview, timeout = 60) {
  if (is.null(preview)) {
    return(FALSE)
  }
  if (isTRUE(preview$cached) && file.exists(preview$target %||% "")) {
    return(TRUE)
  }
  process <- preview$process %||% NULL
  if (is.null(process)) {
    return(FALSE)
  }
  timeout <- suppressWarnings(as.numeric(timeout))
  if (!is.finite(timeout) || timeout <= 0) {
    timeout <- 60
  }
  try(process$wait(timeout = as.integer(timeout * 1000)), silent = TRUE)
  ready <- file.exists(preview$target %||% "") &&
    file.info(preview$target)$size > 0
  if (!isTRUE(ready) &&
      isTRUE(tryCatch(process$is_alive(), error = function(err) FALSE))) {
    try(process$kill(), silent = TRUE)
  }
  isTRUE(ready)
}
