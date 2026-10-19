\name{wsi_read_experiment}
\alias{wsi_read_experiment}
\alias{wsi_experiment_features}
\alias{wsi_experiment_prediction_context}
\alias{wsi_experiment_project}
\alias{wsi_viewer_experiment}
\title{Read and view typed CellPhenotyper experiments}
\usage{
wsi_read_experiment(path, verify_hashes = TRUE)
wsi_experiment_features(experiment, feature_set_id, tissue_ids = NULL)
wsi_experiment_prediction_context(experiment, tissue_ids = NULL)
wsi_experiment_project(experiment, output = NULL, overwrite = FALSE)
wsi_viewer_experiment(experiment, open = interactive(), wait = FALSE, ...)
}
\arguments{
\item{path}{Experiment manifest JSON, its directory, or an experiment wsi_project.}
\item{verify_hashes}{Verify referenced images and feature SHA-256 digests.}
\item{experiment}{A wsi_experiment or manifest path.}
\item{feature_set_id}{Explicit typed feature-set identifier.}
\item{tissue_ids}{Optional tissue scope.}
\item{output}{Optional project directory.}
\item{overwrite}{Allow replacement of existing project output.}
\item{open}{Open the live viewer browser.}
\item{wait}{Run the live viewer event loop before returning.}
\item{...}{Additional arguments to wsi_viewer_live.}
}
\description{Validates version 1 cellphenotyper-experiment manifests, calibrated native image frames, ordered feature contracts and hashes. Cached 2560-dimensional pooled tile vectors and 1280-dimensional spatial tokens remain separate observation types. No encoder extraction or GPU task is started.}
\value{An experiment, typed feature matrix/observation list, prediction context, project, or live viewer session according to the called function.}
\details{Feature retrieval is bounded by wsiTools.experiment_max_rows (10000) and wsiTools.experiment_max_matrix_bytes (256 MiB). Project metadata retains the source manifest identity and image references. Relative paths resolve against the manifest. Features must already use native image pixel-centre coordinates.}
