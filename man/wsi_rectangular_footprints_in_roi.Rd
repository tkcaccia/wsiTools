\name{wsi_rectangular_footprints_in_roi}
\alias{wsi_rectangular_footprints_in_roi}
\title{Test complete rectangular footprints against polygon annotations}
\usage{
wsi_rectangular_footprints_in_roi(rectangles, rois, index = 1L)
}
\arguments{
\item{rectangles}{Data frame with finite native-pixel edge columns
\code{x0}, \code{y0}, \code{x1}, \code{y1}, and positive rectangle areas.}
\item{rois}{A \code{wsi_roi} object containing Polygon or MultiPolygon geometry.}
\item{index}{One valid annotation row.}
}
\description{
Tests full rectangular containment while retaining polygon holes and concavity.
Checks rectangle interiors against every polygon boundary segment as well as
corners and centres, rejecting holes and notches missed by point sampling.}
\details{
Touching an annotation boundary is allowed within a relative tolerance of
\code{1e-9} times the smaller footprint side. Crossing into a hole or outside
a concave boundary is rejected. This is a rectangular containment test, not
an estimated overlap fraction; it does not require \pkg{sf}. Coordinates must
already use the same native image frame.}
\value{One logical containment value per rectangle.}
\seealso{\code{\link{wsi_review_tile_plan}}, \code{\link{read_geojson}}}
