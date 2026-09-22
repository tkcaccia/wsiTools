# Independent GEOS check of a live-priority-edit.cjs report, in slide pixels.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
report <- jsonlite::fromJSON(args[[1L]], simplifyVector = FALSE)
geometry <- function(entry) {
  if (is.null(entry$geometry)) return(sf::st_multipolygon())
  sf::st_multipolygon(lapply(entry$geometry, function(polygon) {
    lapply(polygon, function(ring) do.call(rbind, lapply(ring, unlist)))
  }))
}
before <- sf::st_sfc(lapply(report$before, geometry))
after <- sf::st_sfc(lapply(report$after, geometry))
stopifnot(all(sf::st_is_valid(after)))
target <- which(vapply(report$after, `[[`, character(1), "id") == report$input$selected)
neighbours <- setdiff(seq_along(after), target)
overlap <- sum(as.numeric(sf::st_area(sf::st_intersection(after[target], sf::st_union(after[neighbours])))))
stopifnot(overlap < 0.01)
stopifnot(as.numeric(sf::st_area(after[target])) > as.numeric(sf::st_area(before[target])))
stopifnot(sum(as.numeric(sf::st_area(after[neighbours]))) < sum(as.numeric(sf::st_area(before[neighbours]))))
cat("GEOS: valid polygons, selected area enlarged, neighbours reduced, overlap =", overlap, "square pixels\n")
