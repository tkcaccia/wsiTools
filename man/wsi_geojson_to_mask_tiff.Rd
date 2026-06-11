\name{wsi_geojson_to_mask_tiff}
\alias{wsi_geojson_to_mask_tiff}
\alias{geojson_to_mask_tiff}
\title{Convert GeoJSON annotations to a TIFF mask}
\usage{
wsi_geojson_to_mask_tiff(
  geojson,
  output,
  slide = NULL,
  width = NULL,
  height = NULL,
  downsample = 1,
  origin = c(x = 0, y = 0),
  label_by = c("constant", "class", "index", "roi_id", "name"),
  values = NULL,
  background = 0,
  overlap = c("last", "first", "error"),
  smooth = FALSE,
  smooth_iterations = 1,
  smooth_preserve_area = TRUE,
  smooth_max_vertices = 2000,
  format = c("auto", "tiff", "ome-tiff"),
  pyramid = NULL,
  tile_size = 512,
  compression = c("lzw", "jpeg", "deflate", "zstd", "webp", "packbits", "none"),
  bigtiff = TRUE,
  colour = FALSE,
  colour_map = NULL,
  background_colour = "#000000",
  legend_output = NULL,
  overwrite = FALSE,
  return_mask = FALSE
)

geojson_to_mask_tiff(
  geojson,
  output,
  slide = NULL,
  width = NULL,
  height = NULL,
  downsample = 1,
  origin = c(x = 0, y = 0),
  label_by = c("constant", "class", "index", "roi_id", "name"),
  values = NULL,
  background = 0,
  overlap = c("last", "first", "error"),
  smooth = FALSE,
  smooth_iterations = 1,
  smooth_preserve_area = TRUE,
  smooth_max_vertices = 2000,
  format = c("auto", "tiff", "ome-tiff"),
  pyramid = NULL,
  tile_size = 512,
  compression = c("lzw", "jpeg", "deflate", "zstd", "webp", "packbits", "none"),
  bigtiff = TRUE,
  colour = FALSE,
  colour_map = NULL,
  background_colour = "#000000",
  legend_output = NULL,
  overwrite = FALSE,
  return_mask = FALSE
)
}
\arguments{
\item{geojson}{GeoJSON annotation file path.}

\item{output}{Output TIFF path. Use \code{.ome.tif} or \code{.ome.tiff} for OME-TIFF.}

\item{slide}{Optional \code{wsi_slide} object or image path used to derive the full-resolution image width and height.}

\item{width, height}{Full-resolution slide width and height in pixels. Ignored when \code{slide} is supplied. If neither \code{slide} nor dimensions are supplied, the extent is inferred from the ROI bounds.}

\item{downsample}{One or two positive numbers giving the full-resolution pixel size represented by each mask pixel. For example, \code{downsample = 4} creates a mask at one quarter of the slide width and height.}

\item{origin}{Full-resolution x/y coordinate of the top-left mask corner.}

\item{label_by}{How ROI values are assigned: \code{"constant"} burns all ROIs as foreground, \code{"class"} gives one value per class, \code{"index"} one per ROI, and \code{"roi_id"}/\code{"name"} one per ROI id/name.}

\item{values}{Optional mask values passed to \code{\link[=rois_to_mask]{rois_to_mask()}}.}

\item{background}{Background mask value.}

\item{overlap}{How overlapping ROIs are handled.}

\item{smooth}{Whether to smooth polygon boundaries before rasterisation. This uses a lightweight Chaikin corner-cutting pass and is useful for turning jagged cell-segmentation GeoJSON into cleaner mask borders.}

\item{smooth_iterations}{Number of smoothing passes when \code{smooth = TRUE}. One pass is usually enough for cell masks; higher values create more rounded boundaries and more vertices.}

\item{smooth_preserve_area}{Whether to rescale each smoothed ring back toward its original area. This reduces shrinkage of small segmented cells.}

\item{smooth_max_vertices}{Maximum vertices allowed per ring after smoothing. The number of passes is reduced for very large rings to avoid memory blowup.}

\item{format}{\code{"auto"}, \code{"tiff"}, or \code{"ome-tiff"}. \code{"auto"} selects OME-TIFF when \code{output} ends in \code{.ome.tif} or \code{.ome.tiff}.}

\item{pyramid}{Whether to write a tiled pyramid. Enabled automatically for OME-TIFF output.}

\item{tile_size}{TIFF tile size for libvips output.}

\item{compression}{TIFF compression for libvips output.}

\item{bigtiff}{Whether libvips should write BigTIFF.}

\item{colour}{Whether to write an RGB mask coloured by the annotation label table instead of a single-band labelled mask.}

\item{colour_map}{Optional colour map keyed by class/name/ROI id, depending on \code{label_by}, or by mask value.}

\item{background_colour}{Background colour for RGB masks.}

\item{legend_output}{Optional CSV path for the value-to-label table.}

\item{overwrite}{Whether existing output files may be overwritten.}

\item{return_mask}{Whether to include the in-memory mask matrix in the return value. Leave this \code{FALSE} for large slides.}
}
\value{
A \code{wsi_geojson_mask_tiff} list with output path, dimensions, downsample,
label table, and optional mask.
}
\description{
Reads QuPath-style GeoJSON polygon annotations and rasterises them into a
labelled TIFF mask. This function does not read whole-slide pixels; it only
burns annotation geometry into a mask grid. When \code{format = "ome-tiff"} or
\code{pyramid = TRUE}, libvips is used at runtime to write a tiled/pyramidal
TIFF suitable for use as a viewer layer.
}
\examples{
geojson <- system.file("extdata", "example-qupath-annotations.geojson", package = "wsiTools")
if (nzchar(geojson) && requireNamespace("magick", quietly = TRUE)) {
  out <- tempfile(fileext = ".tif")
  result <- wsi_geojson_to_mask_tiff(geojson, out, width = 1000, height = 1000)
  file.exists(result$output)
}
}
