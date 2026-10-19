\name{wsi_experiment_appearance}
\alias{wsi_experiment_appearance}
\title{Fit or replay a frozen Bora discriminative appearance recipe}
\usage{
wsi_experiment_appearance(experiment, feature_set_id, labels = NULL,
  train_ids = NULL, tissue_ids = NULL, mode = c("local", "shared"),
  active_tissue_id = NULL, recipe_path = NULL, output_dir = NULL, python = NULL)
}
\arguments{
\item{experiment}{Experiment object or manifest path.}
\item{feature_set_id}{Typed feature set.}
\item{labels}{Named training labels; query rows remain unlabelled.}
\item{train_ids}{Explicit training observation IDs.}
\item{tissue_ids}{Tissue application scope.}
\item{mode}{Local appearance or one shared frozen recipe.}
\item{active_tissue_id}{Required single tissue for local mode.}
\item{recipe_path}{Frozen JSON recipe to replay without labels or refitting.}
\item{output_dir}{Optional new derivative directory.}
\item{python}{Existing Python interpreter containing Bora. Defaults to option wsiTools.bora_python, environment variable WSITOOLS_BORA_PYTHON, or python3.}
}
\description{Calls the optional installed bora.appearance_bridge CPU JSON/CSV interface. Returns bounded native-coordinate colour and probability rows plus persistent recipe/artifact references. No installation or encoder inference is performed.}
\value{A list with recipe_path, recipe_id, scope, feature_source, classes, points, grids and derivative paths.}
\details{The discriminative-appearance-v1 representation is not the full Bora RGB9+19 procedure. Colours are display-only and probabilities are uncalibrated. Local mode cannot include another tissue. Pooled features remain tile-resolution observations.}
