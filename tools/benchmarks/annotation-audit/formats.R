args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
input <- normalizePath(args[[1L]], mustWork = TRUE)
output <- args[[2L]]
library(sf)
sf_use_s2(FALSE)
clock <- function(fun, n = 3L) {
  times <- numeric(n)
  value <- NULL
  for (i in seq_len(n)) {
    gc()
    start <- proc.time()[["elapsed"]]
    value <- fun()
    times[[i]] <- 1000 * (proc.time()[["elapsed"]] - start)
  }
  list(median_ms = median(times), min_ms = min(times), max_ms = max(times), n = n, value = value)
}
metrics <- function(x) x[setdiff(names(x), "value")]
result <- list(file = basename(input), R = R.version.string, sf = as.character(packageVersion("sf")), libraries = sf_extSoftVersion())
sfread <- clock(function() st_read(input, quiet = TRUE))
x <- sfread$value
st_crs(x) <- NA
result$sf_geojson_read <- metrics(sfread)
result$features <- nrow(x)
result$sf_object_bytes <- as.numeric(object.size(x))
directory <- tempfile("wsitools-fgb-audit-")
dir.create(directory)
fgb <- file.path(directory, "annotations.fgb")
result$fgb_write <- metrics(clock(function() {
  if (file.exists(fgb)) unlink(fgb)
  st_write(x, fgb, quiet = TRUE, layer_options = "SPATIAL_INDEX=YES")
}))
result$fgb_bytes <- file.info(fgb)$size
result$fgb_full_read <- metrics(clock(function() st_read(fgb, quiet = TRUE)))
b <- st_bbox(x)
cx <- (b[["xmin"]] + b[["xmax"]]) / 2
cy <- (b[["ymin"]] + b[["ymax"]]) / 2
q <- st_as_sfc(st_bbox(c(xmin = cx - 1024, ymin = cy - 1024, xmax = cx + 1024, ymax = cy + 1024)))
filter <- st_as_text(q)
subset <- clock(function() st_read(fgb, wkt_filter = filter, quiet = TRUE), 5L)
result$fgb_query_2048 <- metrics(subset)
result$fgb_query_features <- nrow(subset$value)
result$fgb_query_vertices <- nrow(st_coordinates(subset$value))
if (grepl("290325", input)) {
  jsread <- clock(function() jsonlite::fromJSON(input, simplifyVector = FALSE))
  result$jsonlite_read <- metrics(jsread)
  normalized <- clock(function() wsiTools:::wsi_roi_from_geojson(jsread$value))
  result$wsi_normalize <- metrics(normalized)
  result$wsi_object_size_logical_bytes <- as.numeric(object.size(normalized$value))
  raw <- file.path(directory, "annotations.rds")
  result$rds_write_compressed <- metrics(clock(function() saveRDS(normalized$value, raw)))
  result$rds_bytes <- file.info(raw)$size
  result$rds_read <- metrics(clock(function() readRDS(raw)))
}
result$note <- "Local file tests with warm OS cache; indexed FGB via GDAL, no HTTP Range latency. CRS removed explicitly: coordinates are original WSI pixels. object.size may count shared R geometry references more than once."
dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(result, output, pretty = TRUE, auto_unbox = TRUE, digits = NA)
unlink(directory, recursive = TRUE)
cat(output, "\n")
