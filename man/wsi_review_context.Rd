\name{wsi_review_context}
\alias{wsi_review_context}
\alias{wsi_validate_review_context}
\alias{wsi_review_select}
\title{Construct and select a typed same-section review context}
\usage{
wsi_review_context(images, observations, reductions = list(),
  context_id = NULL, experiment_id = NULL, provenance = list())

wsi_validate_review_context(context, verify_hashes = FALSE,
  require_calibration = FALSE)

wsi_review_select(context, ids = NULL, cluster_field = NULL,
  cluster_values = NULL, tissue_ids = NULL, observation_unit = NULL,
  dataset_ids = NULL)
}
\arguments{
\item{images}{Image record list or data frame. Each record declares
\code{tissue_id}, \code{frame_id}, SHA-256 \code{sha256}, positive integer
\code{width} and \code{height}, and \code{coordinate_convention =
"pixel-edge-origin_centres-in-native-pixels"}. Declare positive \code{mpp_x}
and \code{mpp_y}, or explicitly set \code{calibration_status = "unavailable"}
with both values absent. Optional \code{path} or \code{resolved_path} supports
explicit file hash verification.}
\item{observations}{Observation record list or data frame. Required fields are
globally unique canonical \code{id}, \code{original_id}, \code{tissue_id},
\code{frame_id}, \code{observation_unit} (spot, cell, tile or token), and finite
native-pixel centre coordinates \code{x} and \code{y}. Optional
\code{dataset_id}, \code{feature_set_id}, named categorical \code{cluster_values},
and rectangular footprint metadata are retained.}
\item{reductions}{Explicit embedding records with \code{id}, optional
\code{label}, \code{observation_unit}, and \code{points}. Each point has
canonical observation \code{id} and finite embedding coordinates \code{x},
\code{y}. Optional tissue/frame fields must match the observation.}
\item{context_id, experiment_id}{Optional explicit stable identities. An
omitted context identity is derived from the source signature.}
\item{provenance}{JSON-compatible source and coordinate-mapping metadata.
Source references and spatial attachments participate in source integrity.}
\item{context}{A canonical review context.}
\item{verify_hashes}{Verify accessible image files against declared SHA-256 hashes.}
\item{require_calibration}{Require positive physical calibration for every image.}
\item{ids}{Explicit canonical observation IDs. Empty or omitted selection selects none.}
\item{cluster_field, cluster_values}{An explicit categorical metadata field and
values to select. Mutually exclusive with explicit IDs.}
\item{tissue_ids}{Optional explicit tissue scope.}
\item{dataset_ids}{Optional explicit dataset identities, keeping equally named clusters from different spatial objects separate.}

\item{observation_unit}{Optional observation unit scope.}
}
\description{
Normalizes explicit image and observation identities without inferring units,
transforming coordinates or treating spots, cells, tiles and tokens as equivalent.
Reductions join by canonical IDs rather than row order.}
\details{
The optional footprint is a native-pixel edge rectangle with \code{x0},
\code{y0}, \code{x1}, \code{y1}; it must have positive area and contain the
observation centre. Optional \code{footprint_width_px} and
\code{footprint_height_px} must agree with it. Dimensions alone specify a
rectangle centred at the observation. Full footprints may extend beyond image
edges; they are preserved without clipping. Observation centres must be inside
the declared image.

The context carries \code{schema = "wsitools-review-context"}, version 1,
and a deterministic SHA-256 \code{source_signature}. The signature covers
image identities, frames, dimensions, calibration, observation and reduction
data, and declared source references. Relocating an image path does not change
its identity. Validation rejects modified contexts instead of silently
re-signing them. Same-section mappings must be explicit upstream; a declared
\code{same_section = FALSE} is rejected.

The default bound is 10000 observations, configurable with
\code{options(wsiTools.review_max_observations = ...)}. Oversized contexts fail
without sampling. Empty observation arrays are permitted for explicitly
unavailable data sources. The optional \pkg{openssl} package is needed for
integrity signatures.}
\value{
The constructor and validator return a plain JSON-compatible context list.
\code{wsi_review_select()} returns canonical IDs in context observation order.
Unknown or duplicate requested IDs are errors. Source clusters and predictions
are not modified by selection or human review.}
\seealso{\code{\link{wsi_review_checkpoint}}, \code{\link{wsi_review_tile_plan}}}
