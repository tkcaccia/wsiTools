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
  gigatime_overlay = TRUE,
  gigatime_colours = NULL,
  gigatime_opacity = 0.55,
  gigatime_visible = TRUE,
  live = NULL,
  dynamic_tiles = FALSE,
  transport = c("auto", "websocket", "polling"),
  wait = FALSE,
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
  gigatime_overlay = TRUE,
  gigatime_colours = NULL,
  gigatime_opacity = 0.55,
  gigatime_visible = TRUE,
  live = NULL,
  dynamic_tiles = FALSE,
  transport = c("auto", "websocket", "polling"),
  wait = FALSE,
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

\item{gigatime_overlay}{Whether to overlay the GigaTIME probability OME-TIFF
as tiled mIHC channels when available.}

\item{gigatime_colours}{Optional display colours for the GigaTIME channels.}

\item{gigatime_opacity}{Initial GigaTIME channel opacity.}

\item{gigatime_visible}{Initial GigaTIME channel visibility.}

\item{live}{Whether to use the live \code{httpuv} bridge. The default uses live
mode automatically when a GigaTIME OME-TIFF overlay can be served.}

\item{dynamic_tiles}{Whether the H&E base image should also use live dynamic
tiles. The default keeps the base image on static Deep Zoom tiles when possible
and uses live tiles only for the GigaTIME channels.}

\item{transport}{Live viewer transport.}

\item{wait}{Whether to run the live viewer event loop before returning.}

\item{...}{Additional arguments passed to \code{\link[=wsi_viewer]{wsi_viewer()}}.}
}
\value{
\code{wsi_read_cellphenotyper_project()} returns a
\code{wsi_cellphenotyper_project} object. \code{wsi_viewer_cellphenotyper()}
returns a live viewer session when GigaTIME channels are overlaid, otherwise
the HTML viewer path invisibly.
}
\description{
\code{wsi_read_cellphenotyper_project()} reads a CellPhenotyper output directory
by using \code{00_execution/project_outputs.tsv} as the project manifest. It
resolves local output paths, identifies the input image, and loads the StarDist
centroid table when available. The large label image is not loaded into memory.
GigaTIME OME-TIFF probability output, preview files, MedSAM-refined KODAMA
GeoJSON annotations, KODAMA membership plot PNGs, KODAMA RData embeddings, and
cluster CSV files are resolved from the same manifest when present.
}
\details{
\code{wsi_viewer_cellphenotyper()} opens the input image in the interactive
wsiTools viewer and adds a top \strong{Cells} menu that can show or hide the
CellPhenotyper/StarDist cell segmentation overlay. When
\code{gigatime_probs.ome.tif} is available, it is shown as live tiled mIHC
channel overlays on top of the H&E image and controlled from the top
\strong{Stains} menu. When refined KODAMA GeoJSON is available, the top
\strong{KODAMA} menu can import it as editable viewer annotations and can open
membership plots in a floating window. The default KODAMA plot view redraws
the KODAMA embedding points using the same cluster colours as the imported
GeoJSON annotations, with a spatial GeoJSON redraw fallback. When GrandQC
GeoJSON is available, the top \strong{Artifacts} menu imports those QC regions
instead of running browser-side artifact detection.
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
