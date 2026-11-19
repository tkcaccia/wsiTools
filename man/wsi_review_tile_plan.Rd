\name{wsi_review_tile_plan}
\alias{wsi_review_tile_plan}
\title{Plan deduplicated image-frame tiles from a review selection}
\usage{
wsi_review_tile_plan(context, selected_ids, tile_size = 112,
  units = c("px", "um"), bounds = c("drop", "trim", "error"),
  context_px = 0, max_tiles = 10000L)
}
\arguments{
\item{context}{A canonical \code{wsi_review_context()} list.}
\item{selected_ids}{Explicit unique canonical observation IDs. Empty selects none.}
\item{tile_size}{Positive nominal tile side length. Pixel values must be integers.}
\item{units}{Native pixels or microns. Microns require positive image calibration.}
\item{bounds}{Drop incomplete edge tiles, trim them to image bounds, or error.}
\item{context_px}{Non-negative integer context margin in native pixels. It
affects declared read bounds, clipped explicitly to image bounds.}
\item{max_tiles}{Positive integer bound on deduplicated work tiles.}
}
\description{
Maps selected observation centres onto a deterministic native image grid anchored
at edge origin zero. Observations in the same image-frame tile share one work
record and retain all selecting canonical observation IDs.}
\details{
Membership is explicitly \code{observation_centres}, not spot-footprint
coverage or biological ground truth. Distinct tissue identities are retained
even if image hashes match. Physical tile sizes are converted separately along
the calibrated x and y axes and rounded to native pixels. Tile IDs include
image identity, frame, read geometry and context margin, independently of the
selection order. No pixels are read, features computed, predictions performed,
files exported, or jobs submitted. No compatibility with a particular encoder
context recipe is implied by the context margin.

The maximum is checked before returning any plan; exceeding it is an error.
Bounds exclusions are reported by canonical observation ID.}
\value{A JSON-compatible version 1 \code{wsitools-review-tile-plan} list with
\code{context_id}, \code{source_signature}, \code{selected_ids}, selected
\code{row_count}, deduplicated \code{work_count}, image metadata, \code{tiles}
and \code{excluded_ids}. Each tile has native x/y/width/height, read bounds,
image/tissue/frame identity, and \code{observation_ids}.}
\seealso{\code{\link{wsi_review_context}},
\code{\link{wsi_rectangular_footprints_in_roi}}}
