\name{wsi_experiment_train_classifier}
\alias{wsi_experiment_train_classifier}
\alias{wsi_experiment_predict_classifier}
\alias{wsi_write_experiment_classifier}
\alias{wsi_read_experiment_classifier}
\title{Fit and persist a training-only CPU experiment classifier}
\usage{
wsi_experiment_train_classifier(experiment, feature_set_id, labels,
  train_ids = NULL, train_tissue_ids = NULL, ncomp = 2L,
  max_features = NULL, scaling = "autoscaling", seed = 4L, groups = NULL)
wsi_experiment_predict_classifier(model, experiment, tissue_ids = NULL)
wsi_write_experiment_classifier(model, path, overwrite = FALSE)
wsi_read_experiment_classifier(path)
}
\arguments{
\item{experiment}{An experiment, path, or typed feature-provider result.}
\item{feature_set_id}{Explicit feature-set identifier.}
\item{labels}{Named character labels or an id/label data frame.}
\item{train_ids}{Explicit training observation IDs.}
\item{train_tissue_ids}{Alternative explicit training tissue scope.}
\item{ncomp}{PLS component count.}
\item{max_features}{Optional training-variance feature limit.}
\item{scaling}{Training-only autoscaling, centering, or none.}
\item{seed}{Fixed solver seed.}
\item{groups}{Optional named training groups, recorded without a CV claim.}
\item{model}{A fitted CPU experiment classifier.}
\item{tissue_ids}{Optional application tissue scope.}
\item{path}{Local RDS path. Read only trusted saved model files.}
\item{overwrite}{Replace an existing model file.}
}
\description{Fits imputation, feature selection and scaling only on explicitly selected training rows, then retains the compact CPU fastPLS SIMPLS-LDA prediction state. Saved models replay preprocessing and prediction without refitting.}
\value{A wsi_experiment_classifier, typed prediction data frame, or saved path.}
\details{Requires the optional fastPLS package. Every training class needs at least two observations. Prediction schema and length mismatches fail instead of recycling labels. Scores are uncalibrated; fitting is not independent validation.}
