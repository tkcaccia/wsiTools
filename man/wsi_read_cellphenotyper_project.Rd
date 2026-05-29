\name{wsi_read_cellphenotyper_project}
\alias{wsi_read_cellphenotyper_project}
\alias{read_cellphenotyper_project}
\alias{wsi_viewer_cellphenotyper}
\alias{viewer_cellphenotyper}
\title{Open a CellPhenotyper project}
\usage{
wsi_read_cellphenotyper_project(path, load_cells = TRUE)

read_cellphenotyper_project(path, load_cells = TRUE)

wsi_viewer_cellphenotyper(
  project,
  output = NULL,
  open = interactive(),
  overwrite = FALSE,
  mode = c("tiles", "thumbnail"),
  backend = c("auto", "openslide", "vips"),
  cell_radius = 6,
  cell_colour = "#38BDF8",
  cell_opacity = 0.75,
  ...
)

viewer_cellphenotyper(
  project,
  output = NULL,
  open = interactive(),
  overwrite = FALSE,
  mode = c("tiles", "thumbnail"),
  backend = c("auto", "openslide", "vips"),
  cell_radius = 6,
  cell_colour = "#38BDF8",
  cell_opacity = 0.75,
  ...
)
}
\arguments{
\item{path}{CellPhenotyper output directory or path to
\code{00_execution/project_outputs.tsv}.}

\item{load_cells}{Whether to read the StarDist/cell-assignment centroid table.}

\item{project}{A project object returned by
\code{wsi_read_cellphenotyper_project()}, or a CellPhenotyper output directory.}

\item{output}{Output HTML file for the viewer.}

\item{open}{Whether to open the viewer in a browser.}

\item{overwrite}{Whether to overwrite \code{output} if it already exists.}

\item{mode}{Viewer mode passed to \code{\link[=wsi_viewer]{wsi_viewer()}}.
Use \code{"tiles"} for full-resolution Deep Zoom viewing when libvips is
available.}

\item{backend}{Backend passed to \code{\link[=wsi_open]{wsi_open()}}.}

\item{cell_radius}{Display radius, in slide pixels, for StarDist cell
centroids.}

\item{cell_colour}{Cell overlay colour.}

\item{cell_opacity}{Cell overlay opacity.}

\item{...}{Additional arguments passed to \code{\link[=wsi_viewer]{wsi_viewer()}}.}
}
\value{
\code{wsi_read_cellphenotyper_project()} returns a
\code{wsi_cellphenotyper_project} object. \code{wsi_viewer_cellphenotyper()}
returns the HTML viewer path invisibly.
}
\description{
\code{wsi_read_cellphenotyper_project()} reads a CellPhenotyper output directory
by using \code{00_execution/project_outputs.tsv} as the project manifest. It
resolves local output paths, identifies the input image, and loads the StarDist
centroid table when available. The large label image is not loaded into memory.
}
\details{
\code{wsi_viewer_cellphenotyper()} opens the input image in the interactive
wsiTools viewer and adds a top \strong{Cells} menu that can show or hide the
CellPhenotyper/StarDist cell segmentation overlay.
}
\examples{
\dontrun{
project <- wsi_read_cellphenotyper_project("CellPhenotyper_outputs")
project

html <- wsi_viewer_cellphenotyper(
  project,
  output = "cellphenotyper_viewer.html",
  mode = "tiles",
  open = TRUE
)
}
}
