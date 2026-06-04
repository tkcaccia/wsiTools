wsi_setup_supported_tools <- function() {
  c("openslide", "libvips", "imagemagick", "bioformats", "native_czi")
}

wsi_setup_default_tools <- function(include_optional = FALSE) {
  tools <- c("openslide", "libvips", "imagemagick")
  if (isTRUE(include_optional)) {
    tools <- c(tools, "bioformats", "native_czi")
  }
  tools
}

wsi_setup_method <- function(method = c("auto", "homebrew", "apt", "dnf", "winget", "conda", "manual")) {
  method <- match.arg(method)
  if (!identical(method, "auto")) {
    return(method)
  }
  if (.Platform$OS.type == "windows" && wsi_command_exists("winget")) {
    return("winget")
  }
  if (wsi_command_exists("brew")) {
    return("homebrew")
  }
  if (wsi_command_exists("apt-get")) {
    return("apt")
  }
  if (wsi_command_exists("dnf")) {
    return("dnf")
  }
  if (wsi_command_exists("conda")) {
    return("conda")
  }
  "manual"
}

wsi_setup_normalize_tools <- function(tools = NULL, include_optional = FALSE) {
  if (is.null(tools)) {
    tools <- wsi_setup_default_tools(include_optional = include_optional)
  }
  tools <- unique(tolower(as.character(tools)))
  supported <- wsi_setup_supported_tools()
  unknown <- setdiff(tools, supported)
  if (length(unknown)) {
    wsi_abort(sprintf(
      "`tools` contains unsupported value%s: %s",
      if (length(unknown) == 1L) "" else "s",
      paste(unknown, collapse = ", ")
    ))
  }
  tools
}

wsi_setup_tool_installed <- function(tool) {
  switch(
    tool,
    openslide = wsi_has_openslide(),
    libvips = wsi_has_vips(),
    imagemagick = wsi_has_imagemagick(),
    bioformats = wsi_has_bioformats(),
    native_czi = wsi_has_native_czi(),
    FALSE
  )
}

wsi_setup_tool_packages <- function(tool, method) {
  switch(
    method,
    homebrew = switch(
      tool,
      openslide = "openslide",
      libvips = "vips",
      imagemagick = "imagemagick",
      character()
    ),
    apt = switch(
      tool,
      openslide = c("openslide-tools", "libopenslide-dev"),
      libvips = c("libvips-tools", "libvips-dev"),
      imagemagick = "imagemagick",
      character()
    ),
    dnf = switch(
      tool,
      openslide = c("openslide-tools", "openslide-devel"),
      libvips = c("vips-tools", "vips-devel"),
      imagemagick = "ImageMagick",
      character()
    ),
    winget = switch(
      tool,
      libvips = "libvips.libvips",
      imagemagick = "ImageMagick.Q16-HDRI",
      character()
    ),
    conda = switch(
      tool,
      openslide = "openslide",
      libvips = "libvips",
      imagemagick = "imagemagick",
      bioformats = "bftools",
      character()
    ),
    manual = character(),
    character()
  )
}

wsi_setup_tool_command <- function(tool, method) {
  packages <- wsi_setup_tool_packages(tool, method)
  if (!length(packages)) {
    return(list(command = NA_character_, args = list(character()), notes = wsi_setup_manual_note(tool, method)))
  }
  switch(
    method,
    homebrew = list(command = "brew", args = list(c("install", packages)), notes = "Requires Homebrew."),
    apt = list(command = "sudo", args = list(c("apt-get", "install", "-y", packages)), notes = "Requires Debian/Ubuntu apt and sudo privileges."),
    dnf = list(command = "sudo", args = list(c("dnf", "install", "-y", packages)), notes = "Requires Fedora/RHEL dnf and sudo privileges."),
    winget = list(
      command = "winget",
      args = list(c(
        "install", "--id", packages, "--exact", "--silent",
        "--accept-package-agreements", "--accept-source-agreements"
      )),
      notes = "Requires Windows Package Manager. Restart R/RStudio after installation so PATH changes are visible."
    ),
    conda = {
      channels <- if (identical(tool, "bioformats")) c("ome", "conda-forge") else "conda-forge"
      channel_args <- as.vector(rbind("-c", channels))
      note <- if (identical(tool, "bioformats")) {
        "Installs OME bftools (`showinf` and `bfconvert`) into the active conda environment, using only explicit conda channels."
      } else {
        "Installs into the active conda environment, using only explicit conda-forge channels."
      }
      list(command = "conda", args = list(c("install", "-y", "--override-channels", channel_args, packages)), notes = note)
    },
    manual = list(command = NA_character_, args = list(character()), notes = wsi_setup_manual_note(tool, method))
  )
}

wsi_setup_manual_note <- function(tool, method = "manual") {
  if (tool == "bioformats" && method %in% c("homebrew", "apt", "dnf", "manual")) {
    return("Install OME Bio-Formats command-line tools (`bftools.zip`) manually, or run `conda install --override-channels -c ome -c conda-forge bftools` and ensure `showinf`/`bfconvert` are on PATH.")
  }
  if (tool == "native_czi") {
    return("Use `wsi_install_native_czi()` to build ZEISS libCZI/libCZIAPI into the user cache after reviewing the LGPL notice, or install libCZIAPI manually and set `WSITOOLS_LIBCZIAPI`.")
  }
  if (tool == "openslide" && identical(method, "winget")) {
    return("No reliable winget OpenSlide package is configured. Use conda-forge, MSYS2/vcpkg, or official OpenSlide binaries and add the tools to PATH.")
  }
  if (tool == "bioformats" && identical(method, "winget")) {
    return("No winget command is configured for this optional backend. Use conda/pip or install manually.")
  }
  if (identical(method, "manual")) {
    return("No supported package manager was detected; install this tool manually or use conda/homebrew/apt/dnf/winget.")
  }
  "No automatic command is known for this tool and package manager."
}

wsi_setup_command_line <- function(command, args) {
  if (is.na(command) || !nzchar(command)) {
    return(NA_character_)
  }
  quote_type <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  paste(c(command, shQuote(args, type = quote_type)), collapse = " ")
}

wsi_stardist_setup_method <- function(method = c("auto", "conda", "pip", "manual")) {
  method <- match.arg(method)
  if (!identical(method, "auto")) {
    return(method)
  }
  if (wsi_command_exists("mamba") || wsi_command_exists("conda")) {
    return("conda")
  }
  if (wsi_command_exists("python3") || wsi_command_exists("python")) {
    return("pip")
  }
  "manual"
}

wsi_stardist_config_dir <- function() {
  dir <- file.path(tools::R_user_dir("wsiTools", "config"), "stardist")
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  dir
}

wsi_stardist_wrapper_path <- function() {
  file.path(
    wsi_stardist_config_dir(),
    if (.Platform$OS.type == "windows") "stardist-predict2d.bat" else "stardist-predict2d"
  )
}

wsi_stardist_venv_path <- function(envname = "wsitools-stardist") {
  file.path(tools::R_user_dir("wsiTools", "data"), envname)
}

wsi_stardist_python <- function(method, envname = "wsitools-stardist", venv = NULL) {
  if (identical(method, "pip")) {
    root <- venv %||% wsi_stardist_venv_path(envname)
    return(if (.Platform$OS.type == "windows") {
      file.path(root, "Scripts", "python.exe")
    } else {
      file.path(root, "bin", "python")
    })
  }
  NA_character_
}

wsi_stardist_binary <- function(method, envname = "wsitools-stardist", venv = NULL) {
  if (identical(method, "pip")) {
    root <- venv %||% wsi_stardist_venv_path(envname)
    return(if (.Platform$OS.type == "windows") {
      file.path(root, "Scripts", "stardist-predict2d.exe")
    } else {
      file.path(root, "bin", "stardist-predict2d")
    })
  }
  "stardist-predict2d"
}

wsi_stardist_conda_command <- function() {
  if (wsi_command_exists("conda")) {
    return("conda")
  }
  if (wsi_command_exists("mamba")) {
    return("mamba")
  }
  ""
}

wsi_stardist_conda_env_exists <- function(envname, command = wsi_stardist_conda_command()) {
  if (!nzchar(command)) {
    return(FALSE)
  }
  out <- tryCatch(
    suppressWarnings(system2(command, args = c("env", "list"), stdout = TRUE, stderr = TRUE)),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L)) {
    return(FALSE)
  }
  any(grepl(sprintf("(^|[[:space:]])%s([[:space:]]|$)", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", envname)), out))
}

wsi_stardist_conda_base <- function(command = wsi_stardist_conda_command()) {
  if (!nzchar(command)) {
    return(NA_character_)
  }
  out <- tryCatch(
    suppressWarnings(system2(command, args = c("info", "--base"), stdout = TRUE, stderr = TRUE)),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L) || !length(out)) {
    return(NA_character_)
  }
  trimws(out[[1L]])
}

wsi_stardist_install_root <- function(method, envname = "wsitools-stardist", venv = NULL) {
  if (identical(method, "conda")) {
    base <- wsi_stardist_conda_base()
    if (nzchar(base %||% "")) {
      return(file.path(base, "envs"))
    }
  }
  if (identical(method, "pip")) {
    return(dirname(venv %||% wsi_stardist_venv_path(envname)))
  }
  getwd()
}

wsi_free_disk_gb <- function(path = ".") {
  if (.Platform$OS.type == "windows" || !wsi_command_exists("df")) {
    return(NA_real_)
  }
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  out <- tryCatch(
    suppressWarnings(system2("df", args = c("-Pk", path), stdout = TRUE, stderr = TRUE)),
    error = function(err) character()
  )
  status <- attr(out, "status", exact = TRUE) %||% 0L
  if (!identical(as.integer(status), 0L) || length(out) < 2L) {
    return(NA_real_)
  }
  fields <- strsplit(trimws(out[[length(out)]]), "[[:space:]]+")[[1L]]
  if (length(fields) < 4L) {
    return(NA_real_)
  }
  available_kb <- suppressWarnings(as.numeric(fields[[4L]]))
  if (!is.finite(available_kb)) {
    return(NA_real_)
  }
  available_kb / 1024^2
}

wsi_check_stardist_free_space <- function(method,
                                          envname = "wsitools-stardist",
                                          venv = NULL,
                                          min_free_gb = 8) {
  min_free_gb <- as.numeric(min_free_gb %||% 0)
  if (!is.finite(min_free_gb) || min_free_gb <= 0) {
    return(invisible(TRUE))
  }
  path <- wsi_stardist_install_root(method, envname = envname, venv = venv)
  free <- wsi_free_disk_gb(path)
  if (is.finite(free) && free < min_free_gb) {
    wsi_abort(sprintf(
      paste0(
        "StarDist setup needs roughly %.1f GB free because it installs a Python/TensorFlow stack. ",
        "Only %.1f GB is available at `%s`. Free disk space, use a different environment location, ",
        "or set `WSITOOLS_STARDIST_COMMAND` to an existing StarDist command."
      ),
      min_free_gb,
      free,
      normalizePath(path, winslash = "/", mustWork = FALSE)
    ))
  }
  invisible(TRUE)
}

wsi_native_czi_default_root <- function() {
  file.path(tools::R_user_dir("wsiTools", "data"), "native-czi")
}

wsi_native_czi_license_notice <- function() {
  paste(
    "ZEISS libCZI/libCZIAPI is not part of wsiTools.",
    "This helper downloads/builds it as an optional external runtime backend.",
    "ZEISS libCZI is distributed under a dual license: LGPL v3 or a commercial/proprietary license from ZEISS.",
    "If you use the LGPL option, you are responsible for complying with LGPL v3 and the third-party license notices distributed with libCZI.",
    "wsiTools dynamically loads the resulting shared library through `WSITOOLS_LIBCZIAPI`; it does not statically link or bundle libCZI inside the R package.",
    sep = "\n"
  )
}

wsi_native_czi_accept_license <- function(accept_license = FALSE, ask = interactive()) {
  if (isTRUE(accept_license)) {
    return(invisible(TRUE))
  }
  notice <- wsi_native_czi_license_notice()
  if (!isTRUE(ask)) {
    wsi_abort(paste(
      notice,
      "Re-run with `accept_license = TRUE` after reviewing the license notice.",
      sep = "\n\n"
    ))
  }
  cat(notice, "\n\n")
  if (!wsi_setup_confirm("Do you accept responsibility for installing and using ZEISS libCZI under its license terms?", ask = TRUE)) {
    wsi_abort("Native CZI installation cancelled because the libCZI license terms were not accepted.")
  }
  invisible(TRUE)
}

wsi_native_czi_jobs <- function(jobs = NULL) {
  if (!is.null(jobs)) {
    jobs <- suppressWarnings(as.integer(jobs))
    if (!is.na(jobs) && jobs > 0L) {
      return(jobs)
    }
  }
  cores <- tryCatch(parallel::detectCores(logical = FALSE), error = function(err) NA_integer_)
  if (is.na(cores) || cores < 2L) 1L else max(1L, min(4L, cores - 1L))
}

wsi_native_czi_paths <- function(install_dir = wsi_native_czi_default_root()) {
  install_dir <- normalizePath(install_dir, winslash = "/", mustWork = FALSE)
  list(
    root = install_dir,
    source = file.path(install_dir, "libczi-src"),
    build = file.path(install_dir, "libczi-build")
  )
}

wsi_native_czi_plan_rows <- function(repo, ref, install_dir, jobs) {
  paths <- wsi_native_czi_paths(install_dir)
  configure_args <- c(
    "-S", paths$source,
    "-B", paths$build,
    "-DCMAKE_BUILD_TYPE=Release",
    "-DCMAKE_CXX_STANDARD=17",
    "-DCMAKE_CXX_STANDARD_REQUIRED=ON",
    "-DLIBCZI_BUILD_UNITTESTS=OFF",
    "-DLIBCZI_BUILD_LIBCZIAPI=ON",
    "-DLIBCZI_BUILD_PREFER_EXTERNALPACKAGE_RAPIDJSON=OFF",
    "-DLIBCZI_ENABLE_INSTALL=OFF"
  )
  rows <- list(
    list(
      step = "download",
      command = "git",
      args = c("clone", "--depth", "1", "--branch", ref, repo, paths$source),
      notes = "Downloads ZEISS libCZI source into the user wsiTools cache when it is not already present."
    ),
    list(
      step = "configure",
      command = "cmake",
      args = configure_args,
      notes = "Configures libCZIAPI as a shared library. CMake may fetch libCZI third-party build dependencies."
    ),
    list(
      step = "build",
      command = "cmake",
      args = c("--build", paths$build, "--config", "Release", "--target", "libCZIAPI", "--parallel", as.character(jobs)),
      notes = "Builds the libCZIAPI shared library used by wsiTools for native CZI reads."
    )
  )
  out <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      step = row$step,
      command = row$command,
      command_line = wsi_setup_command_line(row$command, row$args),
      notes = row$notes,
      stringsAsFactors = FALSE
    )
  }))
  out$args <- I(lapply(rows, `[[`, "args"))
  out
}

wsi_native_czi_find_library <- function(install_dir = wsi_native_czi_default_root()) {
  paths <- wsi_native_czi_paths(install_dir)
  roots <- c(paths$build, paths$root)
  roots <- roots[dir.exists(roots)]
  if (!length(roots)) {
    return(NA_character_)
  }
  files <- unique(unlist(lapply(roots, function(root) {
    list.files(root, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  }), use.names = FALSE))
  if (!length(files)) {
    return(NA_character_)
  }
  file_info <- file.info(files)
  files <- files[is.na(file_info$isdir) | !file_info$isdir]
  if (!length(files)) {
    return(NA_character_)
  }
  ext <- if (.Platform$OS.type == "windows") {
    "\\.dll$"
  } else if (identical(Sys.info()[["sysname"]], "Darwin")) {
    "\\.dylib$"
  } else {
    "\\.so(\\.[0-9.]+)?$"
  }
  base <- basename(files)
  stripped <- gsub("[^[:alnum:]]", "", base)
  keep <- grepl(ext, base, ignore.case = TRUE) &
    grepl("cziapi|libcziapi|liblibcziapi", stripped, ignore.case = TRUE)
  candidates <- files[keep]
  if (!length(candidates)) {
    keep <- grepl(ext, base, ignore.case = TRUE) & grepl("czi", base, ignore.case = TRUE)
    candidates <- files[keep]
  }
  if (!length(candidates)) {
    return(NA_character_)
  }
  candidates <- candidates[order(nchar(candidates))]
  normalizePath(candidates[[1L]], winslash = "/", mustWork = TRUE)
}

wsi_native_czi_git_update <- function(repo, ref, source_dir, clean = FALSE) {
  if (isTRUE(clean) && dir.exists(source_dir)) {
    unlink(source_dir, recursive = TRUE, force = TRUE)
  }
  if (!dir.exists(file.path(source_dir, ".git"))) {
    dir.create(dirname(source_dir), recursive = TRUE, showWarnings = FALSE)
    return(wsi_run_command(
      "git",
      args = c("clone", "--depth", "1", "--branch", ref, repo, source_dir),
      error_message = "Could not download ZEISS libCZI source with git."
    ))
  }
  output <- wsi_run_command(
    "git",
    args = c("-C", source_dir, "fetch", "--depth", "1", "origin", ref),
    error_message = "Could not update the local ZEISS libCZI source checkout."
  )
  c(output, wsi_run_command(
    "git",
    args = c("-C", source_dir, "checkout", "--detach", "FETCH_HEAD"),
    error_message = "Could not switch the local ZEISS libCZI source checkout to the requested ref."
  ))
}

wsi_native_czi_write_renviron <- function(library_path, ask = interactive()) {
  library_path <- normalizePath(library_path, winslash = "/", mustWork = TRUE)
  renviron <- path.expand("~/.Renviron")
  lines <- if (file.exists(renviron)) readLines(renviron, warn = FALSE) else character()
  replacement <- sprintf("WSITOOLS_LIBCZIAPI=%s", encodeString(library_path, quote = "\""))
  exists <- grepl("^\\s*WSITOOLS_LIBCZIAPI\\s*=", lines)
  new_lines <- if (any(exists)) {
    lines[exists] <- replacement
    lines
  } else {
    c(lines, "", "# wsiTools optional native CZI backend", replacement)
  }
  if (!wsi_setup_confirm(sprintf("Write WSITOOLS_LIBCZIAPI to %s?", renviron), ask = ask)) {
    return(invisible(FALSE))
  }
  writeLines(new_lines, renviron, useBytes = TRUE)
  invisible(TRUE)
}

#' Build and configure the optional native CZI backend
#'
#' `wsi_install_native_czi()` is an explicit, opt-in helper that downloads and
#' builds ZEISS libCZI/libCZIAPI into the user's wsiTools cache. It keeps
#' wsiTools CRAN-safe by treating libCZI as an external runtime library and
#' dynamically loading it through `WSITOOLS_LIBCZIAPI`.
#'
#' ZEISS libCZI is not bundled with wsiTools. It is distributed by ZEISS under a
#' dual license: LGPL v3 or a commercial/proprietary license from ZEISS. This
#' helper prints a license notice and requires `accept_license = TRUE` before it
#' runs build commands non-interactively.
#'
#' @param install Whether to run the download/configure/build commands. Use
#'   `FALSE` to inspect the plan only.
#' @param ask Whether to ask before running commands or writing `~/.Renviron`.
#' @param accept_license Set to `TRUE` after reviewing the libCZI license notice.
#' @param repo Git repository URL for ZEISS libCZI.
#' @param ref Git branch or tag to build.
#' @param install_dir Directory used for the source and build tree. Defaults to
#'   the user wsiTools data cache.
#' @param jobs Number of parallel build jobs. Defaults to a conservative value.
#' @param set_env Whether to set `WSITOOLS_LIBCZIAPI` for the current R session.
#' @param persist Whether to write `WSITOOLS_LIBCZIAPI` to `~/.Renviron` for
#'   future R sessions.
#' @param clean Whether to remove any existing source/build tree first.
#'
#' @return A `wsi_native_czi_installation` object.
#' @export
#'
#' @examples
#' plan <- wsi_install_native_czi(install = FALSE)
#' plan
wsi_install_native_czi <- function(install = TRUE,
                                   ask = interactive(),
                                   accept_license = FALSE,
                                   repo = "https://github.com/ZEISS/libczi.git",
                                   ref = "main",
                                   install_dir = wsi_native_czi_default_root(),
                                   jobs = NULL,
                                   set_env = TRUE,
                                   persist = FALSE,
                                   clean = FALSE) {
  repo <- as.character(repo %||% "")
  ref <- as.character(ref %||% "")
  if (length(repo) != 1L || is.na(repo) || !nzchar(repo)) {
    wsi_abort("`repo` must be a single non-empty git repository URL.")
  }
  if (length(ref) != 1L || is.na(ref) || !nzchar(ref)) {
    wsi_abort("`ref` must be a single non-empty branch or tag.")
  }
  jobs <- wsi_native_czi_jobs(jobs)
  paths <- wsi_native_czi_paths(install_dir)
  plan <- wsi_native_czi_plan_rows(repo = repo, ref = ref, install_dir = paths$root, jobs = jobs)
  library_path <- wsi_native_czi_find_library(paths$root)

  out <- structure(
    list(
      installed = wsi_has_native_czi(),
      available_library = library_path,
      install_dir = paths$root,
      source_dir = paths$source,
      build_dir = paths$build,
      repo = repo,
      ref = ref,
      plan = plan,
      license_notice = wsi_native_czi_license_notice(),
      command_output = character()
    ),
    class = "wsi_native_czi_installation"
  )

  if (!isTRUE(install)) {
    return(invisible(out))
  }
  if (!isTRUE(clean) && !is.na(library_path) && nzchar(library_path)) {
    if (isTRUE(set_env)) {
      Sys.setenv(WSITOOLS_LIBCZIAPI = library_path)
    }
    if (isTRUE(persist)) {
      wsi_native_czi_write_renviron(library_path, ask = ask)
    }
    out$installed <- wsi_has_native_czi()
    if (isTRUE(out$installed)) {
      return(invisible(out))
    }
  }
  if (!wsi_command_exists("git")) {
    wsi_abort("Installing native CZI support from source requires `git` on PATH.")
  }
  if (!wsi_command_exists("cmake")) {
    wsi_abort("Installing native CZI support from source requires `cmake` on PATH.")
  }
  wsi_native_czi_accept_license(accept_license = accept_license, ask = ask)
  if (!wsi_setup_confirm("Download and build ZEISS libCZI/libCZIAPI now?", ask = ask)) {
    return(invisible(out))
  }

  if (isTRUE(clean) && dir.exists(paths$build)) {
    unlink(paths$build, recursive = TRUE, force = TRUE)
  }
  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  output <- wsi_native_czi_git_update(repo = repo, ref = ref, source_dir = paths$source, clean = clean)
  configure_step <- which(plan$step == "configure")[[1L]]
  build_step <- which(plan$step == "build")[[1L]]
  output <- c(output, wsi_run_command(
    "cmake",
    args = plan$args[[configure_step]],
    error_message = "Configuring ZEISS libCZI/libCZIAPI failed."
  ))
  output <- c(output, wsi_run_command(
    "cmake",
    args = plan$args[[build_step]],
    error_message = "Building ZEISS libCZI/libCZIAPI failed."
  ))

  library_path <- wsi_native_czi_find_library(paths$root)
  if (is.na(library_path) || !nzchar(library_path)) {
    wsi_abort("The libCZIAPI build completed, but wsiTools could not find the generated shared library.")
  }
  if (isTRUE(set_env)) {
    Sys.setenv(WSITOOLS_LIBCZIAPI = library_path)
  }
  if (isTRUE(persist)) {
    wsi_native_czi_write_renviron(library_path, ask = ask)
  }
  out$available_library <- library_path
  out$installed <- wsi_has_native_czi()
  out$command_output <- output
  invisible(out)
}

#' @export
print.wsi_native_czi_installation <- function(x, ...) {
  cat("<wsi_native_czi_installation>\n")
  cat(sprintf("  installed:   %s\n", if (isTRUE(x$installed)) "TRUE" else "FALSE"))
  cat(sprintf("  install_dir: %s\n", x$install_dir))
  if (!is.na(x$available_library) && nzchar(x$available_library)) {
    cat(sprintf("  library:     %s\n", x$available_library))
  }
  cat(sprintf("  repo:        %s\n", x$repo))
  cat(sprintf("  ref:         %s\n", x$ref))
  if (nrow(x$plan)) {
    cat("\nCommands:\n")
    print(x$plan[, c("step", "command_line", "notes"), drop = FALSE], row.names = FALSE)
  }
  cat("\nLicense notice:\n")
  cat(x$license_notice, "\n")
  if (!isTRUE(x$installed)) {
    cat("\nRun `wsi_install_native_czi(accept_license = TRUE)` to build and activate the native CZI backend.\n")
  }
  invisible(x)
}

wsi_stardist_write_wrapper <- function(method,
                                       envname = "wsitools-stardist",
                                       venv = NULL,
                                       command = NULL) {
  path <- wsi_stardist_wrapper_path()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (identical(method, "conda")) {
    runner <- command %||% {
      if (wsi_command_exists("conda")) "conda" else wsi_stardist_conda_command()
    }
    if (!nzchar(runner)) {
      wsi_abort("Could not create a StarDist wrapper because conda was not found.")
    }
    if (.Platform$OS.type == "windows") {
      lines <- c(
        "@echo off",
        sprintf('"%s" run -n "%s" stardist-predict2d %%*', runner, envname)
      )
    } else {
      lines <- c(
        "#!/bin/sh",
        sprintf("exec %s run -n %s stardist-predict2d \"$@\"", shQuote(runner), shQuote(envname))
      )
    }
  } else if (identical(method, "pip")) {
    binary <- wsi_stardist_binary("pip", envname = envname, venv = venv)
    if (.Platform$OS.type == "windows") {
      lines <- c("@echo off", sprintf('"%s" %%*', binary))
    } else {
      lines <- c("#!/bin/sh", sprintf("exec %s \"$@\"", shQuote(binary)))
    }
  } else {
    wsi_abort("`method` must be `conda` or `pip` when creating a StarDist wrapper.")
  }

  writeLines(lines, path, useBytes = TRUE)
  if (.Platform$OS.type != "windows") {
    Sys.chmod(path, mode = "0755")
  }
  path
}

wsi_stardist_plan_rows <- function(method,
                                   envname = "wsitools-stardist",
                                   python_version = "3.10",
                                   tensorflow = TRUE,
                                   venv = NULL) {
  if (identical(method, "manual")) {
    return(data.frame(
      step = "manual",
      command = NA_character_,
      command_line = NA_character_,
      notes = "Install TensorFlow and StarDist manually, then set WSITOOLS_STARDIST_COMMAND.",
      stringsAsFactors = FALSE
    ))
  }

  if (identical(method, "conda")) {
    conda <- wsi_stardist_conda_command()
    packages <- c(sprintf("python=%s", python_version), if (isTRUE(tensorflow)) "tensorflow", "stardist")
    args <- if (wsi_stardist_conda_env_exists(envname, conda)) {
      c("install", "-y", "-n", envname, "-c", "conda-forge", packages[-1L])
    } else {
      c("create", "-y", "-n", envname, "-c", "conda-forge", packages)
    }
    rows <- list(list(
      step = "install",
      command = conda,
      args = args,
      notes = "Creates or updates a dedicated conda environment for StarDist."
    ))
  } else {
    python <- if (wsi_command_exists("python3")) "python3" else if (wsi_command_exists("python")) "python" else ""
    root <- venv %||% wsi_stardist_venv_path(envname)
    py <- wsi_stardist_python("pip", envname = envname, venv = root)
    packages <- c(if (isTRUE(tensorflow)) "tensorflow", "stardist")
    rows <- list(
      list(step = "create_venv", command = python, args = c("-m", "venv", root), notes = "Creates a dedicated Python virtual environment."),
      list(step = "upgrade_pip", command = py, args = c("-m", "pip", "install", "--upgrade", "pip"), notes = "Upgrades pip inside the dedicated environment."),
      list(step = "install", command = py, args = c("-m", "pip", "install", packages), notes = "Installs TensorFlow and StarDist inside the dedicated environment.")
    )
  }

  out <- do.call(rbind, lapply(rows, function(row) {
    data.frame(
      step = row$step,
      command = row$command,
      command_line = wsi_setup_command_line(row$command, row$args),
      notes = row$notes,
      stringsAsFactors = FALSE
    )
  }))
  out$args <- I(lapply(rows, `[[`, "args"))
  out
}

#' Optional StarDist installer
#'
#' Installs or configures an optional StarDist command wrapper for selected-ROI
#' segmentation. This helper is never run during package installation; call it
#' explicitly when you want wsiTools live viewers to launch StarDist on ROI
#' crops.
#'
#' @param method Setup method. `"auto"` prefers conda/mamba when available,
#'   otherwise a Python virtual environment via pip.
#' @param envname Name of the conda environment or local virtual environment.
#' @param python_version Python version used when creating a conda environment.
#' @param tensorflow Whether to install TensorFlow with StarDist.
#' @param install Whether to run the installation commands. Defaults to `TRUE`
#'   because this function is only called explicitly.
#' @param ask Whether to ask before running commands.
#' @param set_env Whether to set `WSITOOLS_STARDIST_COMMAND` for this R session.
#' @param venv Optional virtual environment path for `method = "pip"`.
#' @param min_free_gb Minimum free disk space required before installation is
#'   attempted. StarDist commonly pulls in a large TensorFlow stack.
#'
#' @return A `wsi_stardist_installation` object with the command plan and
#'   wrapper path.
#' @export
#'
#' @examples
#' plan <- wsi_install_stardist(method = "manual", install = FALSE)
#' plan
wsi_install_stardist <- function(method = c("auto", "conda", "pip", "manual"),
                                 envname = "wsitools-stardist",
                                 python_version = "3.10",
                                 tensorflow = TRUE,
                                 install = TRUE,
                                 ask = interactive(),
                                 set_env = TRUE,
                                 venv = NULL,
                                 min_free_gb = 8) {
  if (isTRUE(install)) {
    wsi_abort(
      "wsiTools no longer installs or launches StarDist/Cellpose. Run cell segmentation separately with CellPhenotyper, then open the resulting project/cell overlays in wsiTools.",
      class = "wsi_deprecated_segmentation"
    )
  }
  method <- wsi_stardist_setup_method(method)
  envname <- as.character(envname %||% "wsitools-stardist")
  if (length(envname) != 1L || is.na(envname) || !nzchar(envname)) {
    wsi_abort("`envname` must be a single non-empty string.")
  }
  python_version <- as.character(python_version %||% "3.10")
  plan <- wsi_stardist_plan_rows(
    method = method,
    envname = envname,
    python_version = python_version,
    tensorflow = tensorflow,
    venv = venv
  )
  wrapper <- if (identical(method, "manual")) NA_character_ else wsi_stardist_wrapper_path()
  out <- structure(
    list(
      method = method,
      envname = envname,
      plan = plan,
      wrapper = wrapper,
      installed = wsi_has_stardist(),
      command_output = character()
    ),
    class = "wsi_stardist_installation"
  )

  if (!isTRUE(install) || identical(method, "manual")) {
    return(invisible(out))
  }
  if (any(!nzchar(plan$command) | is.na(plan$command))) {
    wsi_abort("No StarDist installation command is available on this system.")
  }
  wsi_check_stardist_free_space(method, envname = envname, venv = venv, min_free_gb = min_free_gb)
  if (!wsi_setup_confirm("Install StarDist into a dedicated environment now?", ask = ask)) {
    return(invisible(out))
  }
  output <- character()
  for (i in seq_len(nrow(plan))) {
    output <- c(output, wsi_run_command(
      plan$command[[i]],
      args = plan$args[[i]],
      error_message = sprintf("StarDist setup failed during step `%s`.", plan$step[[i]])
    ))
  }
  wrapper <- wsi_stardist_write_wrapper(
    method = method,
    envname = envname,
    venv = venv,
    command = if (identical(method, "conda")) {
      if (wsi_command_exists("conda")) "conda" else wsi_stardist_conda_command()
    } else {
      NULL
    }
  )
  if (isTRUE(set_env)) {
    Sys.setenv(WSITOOLS_STARDIST_COMMAND = wrapper)
  }
  out$wrapper <- wrapper
  out$installed <- wsi_has_stardist(wrapper)
  out$command_output <- output
  invisible(out)
}

#' @keywords internal
print.wsi_stardist_installation <- function(x, ...) {
  cat("<wsi_stardist_installation>\n")
  cat(sprintf("  method:    %s\n", x$method))
  cat(sprintf("  envname:   %s\n", x$envname))
  cat(sprintf("  installed: %s\n", if (isTRUE(x$installed)) "TRUE" else "FALSE"))
  if (!is.na(x$wrapper)) {
    cat(sprintf("  wrapper:   %s\n", x$wrapper))
  }
  if (nrow(x$plan)) {
    cat("\nCommands:\n")
    print(x$plan[, c("step", "command_line", "notes"), drop = FALSE], row.names = FALSE)
  }
  if (!isTRUE(x$installed)) {
    cat("\nStarDist/Cellpose model execution is no longer installed or launched by wsiTools.\n")
  }
  invisible(x)
}

#' Create an installation plan for optional wsiTools tools
#'
#' Builds a CRAN-safe setup plan for optional system tools. It does not install
#' anything by itself; use [wsi_setup()], [wsi_install_backends()], or
#' [wsi_install_deps()] with `install = TRUE` only when the user explicitly
#' wants to run commands.
#'
#' @param tools Optional system tools to check. Defaults to OpenSlide, libvips,
#'   and ImageMagick.
#' @param method Package manager to target. `"auto"` detects Homebrew, apt, dnf,
#'   winget, or conda when available.
#' @param include_optional Whether to include Bio-Formats and native CZI support
#'   in the default `tools` set.
#'
#' @return A data frame with installation status and copyable commands.
#' @export
#'
#' @examples
#' wsi_dependency_plan(method = "manual")
wsi_dependency_plan <- function(tools = NULL,
                                method = c("auto", "homebrew", "apt", "dnf", "winget", "conda", "manual"),
                                include_optional = FALSE) {
  method <- wsi_setup_method(method)
  tools <- wsi_setup_normalize_tools(tools, include_optional = include_optional)
  rows <- lapply(tools, function(tool) {
    command <- wsi_setup_tool_command(tool, method)
    args <- command$args[[1L]]
    data.frame(
      tool = tool,
      installed = wsi_setup_tool_installed(tool),
      method = method,
      command = command$command,
      command_line = wsi_setup_command_line(command$command, args),
      notes = command$notes,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$args <- I(lapply(tools, function(tool) wsi_setup_tool_command(tool, method)$args[[1L]]))
  out
}

wsi_setup_r_package_plan <- function(r_packages = c("magick", "httpuv", "callr")) {
  r_packages <- unique(as.character(r_packages))
  r_packages <- r_packages[nzchar(r_packages)]
  if (!length(r_packages)) {
    return(data.frame(
      package = character(),
      installed = logical(),
      command_line = character(),
      notes = character(),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    package = r_packages,
    installed = vapply(r_packages, requireNamespace, logical(1), quietly = TRUE),
    command_line = sprintf("install.packages(%s)", vapply(r_packages, encodeString, character(1), quote = "\"")),
    notes = "Optional R package; install only if you need this capability.",
    stringsAsFactors = FALSE
  )
}

wsi_setup_confirm <- function(message, ask = interactive()) {
  if (!isTRUE(ask)) {
    return(TRUE)
  }
  choice <- utils::menu(c("Yes", "No"), title = message)
  identical(choice, 1L)
}

wsi_setup_run_system_commands <- function(plan, allow_sudo = FALSE, ask = interactive()) {
  commands <- plan$system_tools
  commands <- commands[!commands$installed & !is.na(commands$command), , drop = FALSE]
  if (!nrow(commands)) {
    return(character())
  }
  if (any(commands$command == "sudo") && !isTRUE(allow_sudo)) {
    wsi_abort("System setup would require `sudo`. Re-run with `allow_sudo = TRUE` after reviewing the commands.")
  }
  if (!wsi_setup_confirm("Run the listed system installation commands now?", ask = ask)) {
    return(character())
  }
  output <- character()
  for (i in seq_len(nrow(commands))) {
    output <- c(output, wsi_run_command(
      commands$command[[i]],
      args = commands$args[[i]],
      error_message = sprintf("Failed to install optional tool `%s`.", commands$tool[[i]])
    ))
  }
  output
}

wsi_setup_install_r_packages <- function(plan, ask = interactive()) {
  missing <- plan$r_packages$package[!plan$r_packages$installed]
  if (!length(missing)) {
    return(invisible(character()))
  }
  if (!wsi_setup_confirm("Install missing optional R packages now?", ask = ask)) {
    return(invisible(character()))
  }
  utils::install.packages(missing)
  invisible(missing)
}

#' Check and optionally install optional wsiTools dependencies
#'
#' `wsi_setup()` helps new users configure optional runtime dependencies without
#' making them mandatory package-installation requirements. By default it only
#' reports missing R packages and system tools and prints copyable commands.
#' Nothing is installed unless `install = TRUE` is supplied.
#'
#' This keeps the package CRAN-safe: installation remains lightweight, while
#' users can opt in to helper commands for OpenSlide, libvips, ImageMagick, and
#' optional analysis tools. For Bio-Formats, wsiTools expects OME bftools
#' command-line programs (`showinf` and `bfconvert`) on `PATH`; conda installs
#' use the OME `bftools` package.
#'
#' @param tools Optional system tools to check.
#' @param r_packages Optional R packages to check.
#' @param method Package manager target. `"auto"` detects Homebrew, apt, dnf,
#'   winget, or conda when available.
#' @param include_optional Whether to include Bio-Formats and native CZI support
#'   in the default system-tool plan.
#' @param install Whether to run installation steps. Defaults to `FALSE` for
#'   `wsi_setup()` and `TRUE` for installer wrappers.
#' @param install_r_packages,install_system_tools Whether `install = TRUE`
#'   applies to optional R packages and/or system tools.
#' @param allow_sudo Whether commands using `sudo` may be executed.
#' @param ask Whether to ask before running install commands.
#'
#' @return A `wsi_setup_plan` object.
#' @export
#'
#' @examples
#' plan <- wsi_setup(method = "manual")
#' plan
wsi_setup <- function(tools = NULL,
                      r_packages = c("magick", "httpuv", "callr"),
                      method = c("auto", "homebrew", "apt", "dnf", "winget", "conda", "manual"),
                      include_optional = FALSE,
                      install = FALSE,
                      install_r_packages = install,
                      install_system_tools = install,
                      allow_sudo = FALSE,
                      ask = interactive()) {
  method <- wsi_setup_method(method)
  plan <- structure(
    list(
      method = method,
      backends = wsi_backends(),
      r_packages = wsi_setup_r_package_plan(r_packages),
      system_tools = wsi_dependency_plan(tools = tools, method = method, include_optional = include_optional),
      command_output = character()
    ),
    class = "wsi_setup_plan"
  )
  if (isTRUE(install_r_packages)) {
    wsi_setup_install_r_packages(plan, ask = ask)
  }
  if (isTRUE(install_system_tools)) {
    plan$command_output <- wsi_setup_run_system_commands(plan, allow_sudo = allow_sudo, ask = ask)
  }
  invisible(plan)
}

#' @rdname wsi_setup
#' @param ... Arguments passed from `wsi_install_deps()` to `wsi_setup()`.
#' @export
wsi_install_deps <- function(..., install = TRUE, install_system_tools = TRUE) {
  wsi_setup(..., install = install, install_system_tools = install_system_tools)
}

#' @rdname wsi_setup
#' @export
wsi_install_backends <- function(tools = NULL,
                                 r_packages = c("magick", "httpuv", "callr"),
                                 method = c("auto", "homebrew", "apt", "dnf", "winget", "conda", "manual"),
                                 include_optional = FALSE,
                                 install = TRUE,
                                 install_r_packages = TRUE,
                                 install_system_tools = TRUE,
                                 allow_sudo = FALSE,
                                 ask = interactive()) {
  wsi_setup(
    tools = tools,
    r_packages = r_packages,
    method = method,
    include_optional = include_optional,
    install = install,
    install_r_packages = isTRUE(install) && isTRUE(install_r_packages),
    install_system_tools = isTRUE(install) && isTRUE(install_system_tools),
    allow_sudo = allow_sudo,
    ask = ask
  )
}

#' @export
print.wsi_setup_plan <- function(x, ...) {
  cat("<wsi_setup_plan>\n")
  cat(sprintf("  method: %s\n", x$method))
  r_missing <- if (nrow(x$r_packages)) sum(!x$r_packages$installed) else 0L
  tool_missing <- if (nrow(x$system_tools)) sum(!x$system_tools$installed) else 0L
  cat(sprintf("  missing optional R packages: %d\n", r_missing))
  cat(sprintf("  missing system tools:        %d\n", tool_missing))

  if (nrow(x$r_packages)) {
    cat("\nOptional R packages:\n")
    print(x$r_packages, row.names = FALSE)
  }
  if (nrow(x$system_tools)) {
    cat("\nSystem tools:\n")
    display <- x$system_tools[, c("tool", "installed", "method", "command_line", "notes"), drop = FALSE]
    print(display, row.names = FALSE)
  }
  if (tool_missing || r_missing) {
    cat("\nNothing is installed unless you call `wsi_install_backends()`, `wsi_install_deps()`, or `wsi_setup(install = TRUE, ...)`.\n")
  }
  invisible(x)
}
