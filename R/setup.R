wsi_setup_supported_tools <- function() {
  c("openslide", "libvips", "imagemagick", "bioformats", "stardist", "cellpose")
}

wsi_setup_default_tools <- function(include_optional = FALSE) {
  tools <- c("openslide", "libvips", "imagemagick")
  if (isTRUE(include_optional)) {
    tools <- c(tools, "bioformats", "stardist", "cellpose")
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
    stardist = wsi_has_stardist(),
    cellpose = wsi_has_cellpose(),
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
      stardist = "stardist",
      cellpose = "cellpose",
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
  if (tool %in% c("stardist", "cellpose") && !identical(method, "conda")) {
    return("Python segmentation tools are best installed in a dedicated conda/pip environment; configure the command path afterwards.")
  }
  if (tool == "bioformats" && method %in% c("homebrew", "apt", "dnf", "manual")) {
    return("Install OME Bio-Formats command-line tools (`bftools.zip`) manually, or run `conda install --override-channels -c ome -c conda-forge bftools` and ensure `showinf`/`bfconvert` are on PATH.")
  }
  if (tool == "openslide" && identical(method, "winget")) {
    return("No reliable winget OpenSlide package is configured. Use conda-forge, MSYS2/vcpkg, or official OpenSlide binaries and add the tools to PATH.")
  }
  if (tool %in% c("bioformats", "stardist", "cellpose") && identical(method, "winget")) {
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

#' Plan or install the optional StarDist command
#'
#' StarDist is a Python/TensorFlow tool and is intentionally not installed
#' automatically during `install.packages()` or `R CMD INSTALL`. This helper is
#' the explicit first-run setup for users who want the viewer's
#' `Run segmentation` button to work. It installs StarDist into a dedicated
#' conda environment or Python virtual environment, writes a small wrapper
#' command in the user configuration directory, and makes wsiTools detect that
#' wrapper automatically.
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

#' @export
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
    cat("\nRun `wsi_install_stardist()` to install StarDist, or set `WSITOOLS_STARDIST_COMMAND` to an existing command.\n")
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
#' @param include_optional Whether to include Bio-Formats, StarDist, and
#'   Cellpose in the default `tools` set.
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
#' @param include_optional Whether to include Bio-Formats, StarDist, and
#'   Cellpose in the default system-tool plan.
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
