test_that("GeoJSON polygon coordinates survive import and export unchanged", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  function_line <- function(name) {
    matches <- grep(paste0("^function ", name, "\\("), lines, value = TRUE)
    expect_true(length(matches) > 0L)
    tail(matches, 1L)
  }
  js <- V8::v8()
  js$eval("var rois=[]; var cfg={slide_width:100,slide_height:80};")
  for (name in c("pointFromGeojsonCoord", "ringFromGeojsonCoords",
                 "ringCoordinates")) js$eval(function_line(name))

  original <- list(c(-1.25, 5.5), c(101.75, 5.5),
                   c(101.75, 81.125), c(-1.25, 5.5))
  js$assign("inputRing", original)
  exported <- jsonlite::fromJSON(js$eval(
    "JSON.stringify(ringCoordinates(ringFromGeojsonCoords(inputRing)))"
  ))
  expect_equal(unname(exported), do.call(rbind, original))

  js$eval(paste(
    "function roiDrawGroups(roi){return [{rings:roi.rings,holes:[]}];}",
    "function isDrawable(){return true;}",
    "function roiLabelText(){return 'Region';}",
    "function ensureClassPreset(){return null;}",
    "function classColour(){return '#22c55e';}",
    "function normaliseHexColour(){return '';}",
    "function classPresetExportable(){return true;}",
    "function classPresetExportRule(){return 'include';}",
    "function clonePlain(x){return JSON.parse(JSON.stringify(x||{}));}",
    "function lockedRoi(){return false;}",
    "function visibleRoi(){return true;}",
    "function roiBounds(roi){return roi.bbox;}"
  ))
  js$eval(function_line("roiCompositeGeometry"))
  js$eval(function_line("wsiUncachedRoiFeature"))
  js$eval(paste0(
    "var imported={id:'r1',class:'tumour',rings:[ringFromGeojsonCoords(inputRing)],",
    "bbox:{xmin:-1.25,ymin:5.5,xmax:101.75,ymax:81.125}};",
    "var feature=wsiUncachedRoiFeature(imported,0);"
  ))
  geometry <- jsonlite::fromJSON(js$eval("JSON.stringify(feature.geometry.coordinates[0])"))
  expect_equal(unname(geometry), do.call(rbind, original))
  bbox <- jsonlite::fromJSON(js$eval("JSON.stringify(feature.bbox)"))
  expect_equal(bbox, c(-1.25, 5.5, 101.75, 81.125))
})

test_that("Saved browser projects keep current image sources and reject other slides", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  definition <- grep("^function restoreBrowserProjectInLiveSession\\(", lines, value = TRUE)
  expect_length(definition, 1L)
  js <- V8::v8()
  js$eval(paste(
    "var projectItems=[{id:'live',path:'/current/slide.tif',width:100,height:80,tile_url_base:'fresh'}];",
    "var layers=[]; var restored=null;",
    "function cloneProjectValue(x){return JSON.parse(JSON.stringify(x));}",
    "function projectItemBaseKey(item){return item.id;}",
    "function restoreBrowserProject(x){restored=x;}"
  ))
  js$eval(definition)
  snapshot <- list(
    schema = "wsiTools-viewer-project",
    project = list(items = list(list(id = "old", path = "/old/slide.tif",
                                   width = 100, height = 80,
                                   tile_url_base = "stale")),
                   annotation_sets = list(list(key = "old::image", rois = list())))
  )
  js$assign("saved", snapshot)
  js$eval("restoreBrowserProjectInLiveSession(saved)")
  expect_identical(js$eval("restored.project.items[0].tile_url_base"), "fresh")
  expect_identical(js$eval("restored.project.annotation_sets[0].key"), "live::image")
  js$eval("saved.project.items[0].path='/other/different.tif'")
  expect_error(js$eval("restoreBrowserProjectInLiveSession(saved)"), "different images")
})
