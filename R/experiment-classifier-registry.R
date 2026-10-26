# Saved classifier references are plain metadata. Browsing/restoring never reads RDS.
wsi_experiment_classifier_reference <- function(record) {
  if (!is.list(record) || is.null(names(record)) || anyDuplicated(names(record)))
    wsi_abort("Saved classifier reference must be one metadata object.")
  allowed <- c("schema_version", "path", "sha256", "feature_source", "feature_set_id",
    "signature_sha256", "experiment_id", "experiment_manifest_sha256", "label", "training_experiment_id")
  if (any(!names(record) %in% allowed))
    wsi_abort("Saved classifier references contain metadata only; fitted models and arbitrary fields are not stored in project JSON.")
  if (length(record$schema_version) != 1L || !is.numeric(record$schema_version) ||
      !is.finite(record$schema_version) || record$schema_version != 1)
    wsi_abort("Unsupported saved classifier reference version.")
  for (field in c("path", "feature_source", "feature_set_id", "experiment_id", "label"))
    record[[field]] <- wsi_experiment_text(record[[field]], paste0("classifier reference.", field))
  if (grepl("^[[:alpha:]][[:alnum:]+.-]*://", record$path) ||
      !grepl("^([A-Za-z]:)?[/\\\\]", record$path))
    wsi_abort("Saved classifier references require an absolute local file path.")
  record$path <- normalizePath(record$path, winslash = "/", mustWork = FALSE)
  if (!identical(record$feature_source, paste0("experiment:", record$feature_set_id)))
    wsi_abort("Saved classifier feature source differs from its declared feature set.")
  for (field in c("sha256", "signature_sha256", "experiment_manifest_sha256")) {
    value <- tolower(wsi_experiment_text(record[[field]], paste0("classifier reference.", field)))
    if (!grepl("^[a-f0-9]{64}$", value)) wsi_abort("Saved classifier reference needs exact file, feature-signature and experiment SHA-256 identities.")
    record[[field]] <- value
  }
  if (!is.null(record$training_experiment_id))
    wsi_experiment_text(record$training_experiment_id, "classifier reference.training_experiment_id")
  record
}

wsi_experiment_classifier_registry <- function(records = list(), experiment = NULL, verify_files = FALSE) {
  if (is.null(records)) records <- list()
  if (!is.list(records) || (length(records) && !is.null(names(records))) ||
      length(records) > getOption("wsiTools.experiment_max_saved_classifiers", 128L))
    wsi_abort("Saved classifier registry must be a bounded array of metadata references.")
  records <- lapply(records, wsi_experiment_classifier_reference)
  if (anyDuplicated(vapply(records, `[[`, character(1), "path")))
    wsi_abort("Saved classifier registry paths must be unique.")
  if (!is.null(experiment)) {
    experiment <- wsi_read_experiment(experiment)
    for (record in records) {
      if (!identical(record$experiment_id, experiment$experiment_id) ||
          !identical(record$experiment_manifest_sha256, experiment$manifest_sha256))
        wsi_abort("Saved classifier belongs to a different or revised experiment. Reopen its matching project or explicitly import the model with checked features.")
      definitions <- unlist(lapply(experiment$tissues, function(tissue)
        Filter(function(feature) identical(feature$feature_set_id, record$feature_set_id), tissue$feature_sets)), recursive = FALSE)
      signatures <- vapply(definitions, function(feature)
        wsi_review_digest(wsi_experiment_feature_signature(feature)), character(1))
      if (!record$signature_sha256 %in% signatures)
        wsi_abort("Saved classifier feature definition is incompatible with the current experiment.")
    }
  }
  if (isTRUE(verify_files)) for (record in records) {
    if (!file.exists(record$path) || dir.exists(record$path))
      wsi_abort(paste("Saved classifier file is missing:", record$path))
    if (!identical(wsi_experiment_sha256(record$path), record$sha256))
      wsi_abort(paste("Saved classifier SHA-256 mismatch; the file changed after registration:", record$path))
  }
  unname(records)
}

wsi_experiment_register_classifier <- function(state, experiment, model_path, model, label = NULL) {
  experiment <- wsi_read_experiment(experiment)
  if (!inherits(model, "wsi_experiment_classifier") ||
      !identical(model$schema, "wsitools-experiment-classifier") ||
      !identical(model$backend, "cpu") || !is.list(model$signature))
    wsi_abort("Only a validated CPU experiment classifier can be registered.")
  model_path <- normalizePath(model_path, winslash = "/", mustWork = TRUE)
  if (dir.exists(model_path)) wsi_abort("Saved classifier must be a local RDS file.")
  fid <- wsi_experiment_text(model$feature_set_id, "classifier feature_set_id")
  record <- list(schema_version = 1L, path = model_path, sha256 = wsi_experiment_sha256(model_path),
    feature_source = paste0("experiment:", fid), feature_set_id = fid,
    signature_sha256 = wsi_review_digest(model$signature), experiment_id = experiment$experiment_id,
    experiment_manifest_sha256 = experiment$manifest_sha256,
    training_experiment_id = model$experiment_id,
    label = label %||% paste0("Saved ", model$signature$observation_unit, " classifier - ", basename(dirname(model_path))))
  checked <- wsi_experiment_classifier_registry(list(record), experiment)
  current <- wsi_experiment_classifier_registry(state$experiment_classifier_registry %||% list())
  previous <- Filter(function(item) identical(item$path, model_path), current)
  if (length(previous) && !identical(previous[[1L]]$sha256, record$sha256))
    wsi_abort("A registered classifier file changed; preserve the original and register a new model path.")
  state$experiment_classifier_registry <- c(Filter(function(item) !identical(item$path, model_path), current), checked)
  invisible(checked[[1L]])
}

wsi_experiment_verify_classifier_reference <- function(state, experiment, model_path) {
  path <- normalizePath(wsi_experiment_text(model_path, "model_path"), winslash = "/", mustWork = FALSE)
  current <- wsi_experiment_classifier_registry(state$experiment_classifier_registry %||% list())
  selected <- Filter(function(record) identical(record$path, path), current)
  # An explicitly chosen legacy file has no stored baseline digest. Its normal
  # model/source checks still run, then successful application registers it.
  if (!length(selected)) return(NULL)
  wsi_experiment_classifier_registry(selected, experiment, verify_files = TRUE)[[1L]]
}

wsi_experiment_project_registry <- function(project, experiment = NULL) {
  saved <- project$viewer_state %||% list()
  records <- saved$experiment_workbench$classifiers %||% list()
  if (!length(records)) return(list())
  if (is.null(experiment) && !is.null(project$metadata$cellphenotyper_experiment))
    experiment <- wsi_read_experiment(project)
  if (is.null(experiment)) wsi_abort("Saved classifiers require the matching experiment source reference.")
  wsi_experiment_classifier_registry(records, experiment, verify_files = FALSE)
}
