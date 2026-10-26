wsi_experiment_review_defaults <- function(experiment) {
  defaults <- experiment$settings$review_defaults
  if (is.null(defaults)) return(list())
  if (!is.list(defaults) || (length(defaults) && is.null(names(defaults))))
    wsi_abort("Experiment settings.review_defaults must be a JSON object.")
  if (!"show_bora_predictions" %in% names(defaults)) return(list())
  value <- defaults$show_bora_predictions
  if (!is.logical(value) || length(value) != 1L || is.na(value))
    wsi_abort("Experiment settings.review_defaults.show_bora_predictions must be a strict boolean.")
  list(show_bora_predictions = value)
}
