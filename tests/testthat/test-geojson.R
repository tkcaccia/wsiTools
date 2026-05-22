test_that("GeoJSON parser reads a small QuPath-style polygon", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {
            "name": "Tumor",
            "classification": {"name": "tumor"}
          },
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 20], [0, 20], [0, 0]]]
          }
        }
      ]
    }',
    path
  )

  roi <- wsi_read_geojson(path)
  expect_s3_class(roi, "wsi_roi")
  expect_equal(nrow(roi), 1)
  expect_equal(roi$roi_id, "roi-1")
  expect_equal(roi$name, "Tumor")
  expect_equal(roi$class, "tumor")
  expect_equal(roi$xmax, 10)
  expect_equal(roi$ymax, 20)
  expect_true(is.list(roi$coordinates))
})

test_that("GeoJSON writer preserves ROI classes and blocks accidental overwrite", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {"name": "Tumor", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 20], [0, 20], [0, 0]]]
          }
        }
      ]
    }',
    path
  )

  roi <- read_geojson(path)
  roi <- wsi_set_roi_class(roi, "necrosis")
  output <- tempfile(fileext = ".geojson")

  expect_invisible(write_geojson(roi, output))
  expect_error(write_geojson(roi, output), "overwrite = FALSE")

  roundtrip <- wsi_read_geojson(output)
  expect_equal(roundtrip$class, "necrosis")
  expect_equal(roundtrip$geometry_type, "Polygon")
  written <- paste(readLines(output, warn = FALSE), collapse = "\n")
  expect_match(written, '"label": "Tumor"', fixed = TRUE)
})

test_that("GeoJSON parser uses label property when name is absent", {
  path <- tempfile(fileext = ".geojson")
  writeLines(
    '{
      "type": "FeatureCollection",
      "features": [
        {
          "type": "Feature",
          "id": "roi-1",
          "properties": {"label": "User drawn region", "classification": {"name": "tumour"}},
          "geometry": {
            "type": "Polygon",
            "coordinates": [[[0, 0], [10, 0], [10, 20], [0, 20], [0, 0]]]
          }
        }
      ]
    }',
    path
  )

  roi <- wsi_read_geojson(path)

  expect_equal(roi$name, "User drawn region")
  expect_equal(roi$class, "tumour")
})

test_that("QuPath polygon and multipolygon metadata round-trip", {
  path <- tempfile(fileext = ".geojson")
  geojson <- list(
    type = "FeatureCollection",
    name = "QuPath annotations",
    crs = list(type = "name", properties = list(name = "pixel")),
    features = list(
      list(
        type = "Feature",
        id = "qp-annotation-1",
        bbox = c(0, 0, 20, 20),
        properties = list(
          objectType = "annotation",
          name = "Tumour island",
          classification = list(name = "tumour", color = 16711680),
          isLocked = TRUE,
          measurements = list(
            list(name = "Area um^2", value = 123.4),
            list(name = "DAB mean", value = 0.42)
          ),
          custom = list(nested = TRUE)
        ),
        geometry = list(
          type = "Polygon",
          coordinates = list(list(c(0, 0), c(20, 0), c(20, 20), c(0, 20), c(0, 0)))
        )
      ),
      list(
        type = "Feature",
        properties = list(
          id = "qp-multipolygon-1",
          objectType = "annotation",
          label = "Stroma areas",
          classification = list(name = "stroma", color = "#00AAFF"),
          measurements = list(list(name = "Cell count", value = 3))
        ),
        geometry = list(
          type = "MultiPolygon",
          coordinates = list(
            list(list(c(30, 30), c(40, 30), c(40, 40), c(30, 40), c(30, 30))),
            list(list(c(50, 50), c(60, 50), c(60, 60), c(50, 60), c(50, 50)))
          )
        )
      )
    )
  )
  jsonlite::write_json(geojson, path, auto_unbox = TRUE, pretty = TRUE)

  rois <- read_geojson(path)

  expect_s3_class(rois, "wsi_roi")
  expect_equal(nrow(rois), 2)
  expect_equal(rois$roi_id, c("qp-annotation-1", "qp-multipolygon-1"))
  expect_equal(rois$geometry_type, c("Polygon", "MultiPolygon"))
  expect_equal(rois$class, c("tumour", "stroma"))
  expect_equal(rois$object_type, c("annotation", "annotation"))
  expect_equal(rois$color, c("#FF0000", "#00AAFF"))
  expect_true(rois$is_locked[[1]])
  expect_equal(rois$measurements[[1]][[1]]$name, "Area um^2")
  expect_true(rois$properties[[1]]$custom$nested)
  expect_equal(attr(rois, "geojson_crs")$properties$name, "pixel")
  expect_equal(attr(rois, "geojson_foreign_members")$name, "QuPath annotations")

  output <- tempfile(fileext = ".geojson")
  expect_invisible(write_geojson(rois, output))
  written <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  expect_equal(written$type, "FeatureCollection")
  expect_equal(written$name, "QuPath annotations")
  expect_equal(written$crs$properties$name, "pixel")
  expect_equal(written$features[[1]]$id, "qp-annotation-1")
  expect_equal(written$features[[1]]$properties$objectType, "annotation")
  expect_equal(written$features[[1]]$properties$classification$name, "tumour")
  expect_equal(written$features[[1]]$properties$classification$color, 16711680)
  expect_equal(written$features[[1]]$properties$measurements[[2]]$name, "DAB mean")
  expect_true(written$features[[1]]$properties$custom$nested)
  expect_equal(written$features[[2]]$id, "qp-multipolygon-1")
  expect_equal(written$features[[2]]$geometry$type, "MultiPolygon")

  changed <- wsi_set_roi_class(rois, "necrosis", roi_id = "qp-annotation-1")
  changed_output <- tempfile(fileext = ".geojson")
  write_geojson(changed, changed_output)
  changed_json <- jsonlite::fromJSON(changed_output, simplifyVector = FALSE)
  expect_equal(changed_json$features[[1]]$properties$classification$name, "necrosis")
  expect_equal(changed_json$features[[1]]$properties$class, "necrosis")
  expect_equal(changed_json$features[[1]]$properties$measurements[[1]]$value, 123.4)
  expect_equal(changed_json$features[[2]]$properties$classification$name, "stroma")
})

test_that("QuPath annotations survive viewer add/get/write style round-trip", {
  geojson <- list(
    type = "FeatureCollection",
    name = "QuPath project export",
    crs = list(type = "name", properties = list(name = "pixel")),
    features = list(
      list(
        type = "Feature",
        id = "feature-1",
        bbox = c(0, 0, 50, 50),
        properties = list(
          objectId = "qupath-object-1",
          objectType = "annotation",
          name = "Tumour multi",
          classification = list(name = "tumour", colorRGB = -65536),
          isLocked = FALSE,
          measurements = list(
            list(name = "Area um^2", value = 500),
            list(name = "Positive cells", value = 7)
          ),
          custom_qupath_payload = list(flag = TRUE, note = "keep me")
        ),
        geometry = list(
          type = "MultiPolygon",
          coordinates = list(
            list(list(c(0, 0), c(20, 0), c(20, 20), c(0, 20), c(0, 0))),
            list(list(c(30, 30), c(50, 30), c(50, 50), c(30, 50), c(30, 30)))
          )
        )
      )
    )
  )

  rois <- wsiTools:::wsi_roi_from_geojson(geojson)
  viewer_rois <- wsiTools:::wsi_viewer_roi_features(rois)

  expect_length(viewer_rois, 1)
  expect_equal(viewer_rois[[1]]$geometry_type, "MultiPolygon")
  expect_equal(viewer_rois[[1]]$properties$objectId, "qupath-object-1")
  expect_equal(viewer_rois[[1]]$properties$classification$colorRGB, -65536)
  expect_equal(length(viewer_rois[[1]]$add_groups), 1)
  expect_equal(viewer_rois[[1]]$feature$id, "feature-1")

  state <- wsiTools:::wsi_new_viewer_state(name = "roundtrip", envir = new.env(parent = emptyenv()))
  payload <- list(
    event = "viewer_state",
    selected_index = 0,
    selected_roi = wsiTools:::wsi_viewer_rois_to_geojson(rois)$features[[1]],
    selected_rois = wsiTools:::wsi_viewer_rois_to_geojson(rois),
    rois = wsiTools:::wsi_viewer_rois_to_geojson(rois),
    segmentation = list(type = "FeatureCollection", features = list()),
    measurements = list(),
    view = list(),
    detail = list()
  )
  wsiTools:::wsi_viewer_state_apply(state, payload)
  edited <- wsi_viewer_state(state)$rois

  output <- tempfile(fileext = ".geojson")
  write_geojson(edited, output)
  written <- jsonlite::fromJSON(output, simplifyVector = FALSE)

  expect_equal(written$name, "QuPath project export")
  expect_equal(written$crs$properties$name, "pixel")
  expect_equal(written$features[[1]]$id, "feature-1")
  expect_equal(written$features[[1]]$properties$objectId, "qupath-object-1")
  expect_equal(written$features[[1]]$properties$objectType, "annotation")
  expect_equal(written$features[[1]]$properties$classification$name, "tumour")
  expect_equal(written$features[[1]]$properties$classification$colorRGB, -65536)
  expect_equal(written$features[[1]]$properties$measurements[[2]]$value, 7)
  expect_true(written$features[[1]]$properties$custom_qupath_payload$flag)
  expect_equal(written$features[[1]]$geometry$type, "MultiPolygon")
  expect_equal(length(written$features[[1]]$geometry$coordinates), 2)

  edited$color[[1]] <- "#00FF00"
  edited$classification_color[[1]] <- "#00FF00"
  recolored <- tempfile(fileext = ".geojson")
  write_geojson(edited, recolored)
  recolored_json <- jsonlite::fromJSON(recolored, simplifyVector = FALSE)
  expect_equal(recolored_json$features[[1]]$properties$classification$color, "#00FF00")
  expect_null(recolored_json$features[[1]]$properties$classification$colorRGB)
})
