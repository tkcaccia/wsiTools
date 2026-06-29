\name{wsi_associate_annotations}
\alias{wsi_associate_annotations}
\alias{wsi_annotation_association_matrix}
\alias{wsi_write_annotation_associations}
\title{Associate spatial spots or cells with annotation polygons}
\usage{
wsi_associate_annotations(
  points,
  rois,
  ids = NULL,
  include_all = TRUE,
  output = c("data.frame", "matrix"),
  file = NULL,
  engine = c("auto", "native", "r")
)

wsi_annotation_association_matrix(
  x,
  rois = NULL,
  by = c("annotation", "class"),
  include_unassigned = FALSE
)

wsi_write_annotation_associations(x, file)
}
\arguments{
\item{points}{A data frame with point coordinates. Coordinate columns may be
named \code{x}/\code{y}, \code{spot_x}/\code{spot_y}, \code{cell_x}/\code{cell_y},
or \code{centroid_x}/\code{centroid_y}.}

\item{rois}{A \code{wsi_roi} object, usually returned by \code{read_geojson()} or
\code{wsi_read_geojson()}.}

\item{ids}{Optional annotation selectors. Values may be annotation ids, names,
classes, \code{roi:<id>}, \code{roi_index:<zero_based_index>}, or
\code{class:<class>}.}

\item{include_all}{If \code{TRUE}, use all annotations when \code{ids} is empty.}

\item{output}{Return a long \code{data.frame} or a point-by-annotation
\code{matrix}.}

\item{file}{Optional CSV path.}

\item{engine}{Use the native C++ assignment engine, the R fallback, or choose
automatically.}

\item{x}{An association table returned by \code{wsi_associate_annotations()} or
\code{viewer$get_spot_annotation_table()}.}

\item{by}{Build matrix columns by individual annotation or annotation class.}

\item{include_unassigned}{Include an \code{unassigned} matrix column for points
outside all annotations.}
}
\value{
\code{wsi_associate_annotations()} returns a \code{wsi_annotation_association}
data frame by default, or an integer matrix when \code{output = "matrix"}.
\code{wsi_annotation_association_matrix()} returns an integer matrix.
\code{wsi_write_annotation_associations()} returns the normalized output path
invisibly.
}
\description{
Assign each spatial spot or cell coordinate to the first annotation polygon
that contains it. The default engine uses the package native C++ point-in-polygon
routine when available and falls back to the R implementation otherwise.
}
\details{
This function is intended for spatial omics and CellPhenotyper workflows where
annotations drawn in the viewer need to be linked back to spot or cell tables.
The native engine avoids loading image pixels and only evaluates point
coordinates against annotation geometries.
}
\examples{
rois <- data.frame(
  roi_id = "roi_1",
  name = "Tumour",
  class = "tumour",
  geometry_type = "Polygon",
  xmin = 0, ymin = 0, xmax = 10, ymax = 10,
  stringsAsFactors = FALSE
)
rois$coordinates <- list(list(rbind(
  c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)
)))
class(rois) <- c("wsi_roi", class(rois))
points <- data.frame(spot_id = c("a", "b"), spot_x = c(5, 20), spot_y = c(5, 20))
wsi_associate_annotations(points, rois, engine = "r")
}
