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
    "var layers=[]; var restored=null; var preserved=null;",
    "var activeProjectIndex=0; var activeProjectSectionIndex=-1;",
    "function cloneProjectValue(x){return JSON.parse(JSON.stringify(x));}",
    "function projectItemBaseKey(item){return item.id;}",
    "function clamp(x,lo,hi){return Math.min(hi,Math.max(lo,x));}",
    "function defaultProjectSectionIndex(){return -1;}",
    "function restoreBrowserProject(x,keep){restored=x;preserved=keep;}"
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
  expect_identical(js$eval("preserved"), "true")
  js$eval("saved.project.items[0].path='/other/different.tif'")
  expect_error(js$eval("restoreBrowserProjectInLiveSession(saved)"), "different images")
})

test_that("Restoring annotations on the active slide does not reopen its tiles", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  definition <- grep("^function restoreBrowserProject\\(", readLines(path, warn = FALSE), value = TRUE)
  expect_length(definition, 1L)
  js <- V8::v8()
  js$eval(paste(
    "var projectItems=[{id:'live',path:'/slide.tif'}];",
    "var activeProjectIndex=0,activeProjectSectionIndex=-1;",
    "var projectAnnotationStore=new Map(),projectAnnotationStorePreloaded=false;",
    "var layers=[],measures=[],previewOpens=0,sectionFinishes=0;",
    "function saveActiveProjectAnnotations(){}",
    "function clamp(x,lo,hi){return Math.min(hi,Math.max(lo,x));}",
    "function defaultProjectSectionIndex(){return -1;}",
    "function projectAnnotationKey(){return 'live::image';}",
    "function cloneProjectValue(x){return JSON.parse(JSON.stringify(x));}",
    "function setProjectDirty(){} function loadProjectAnnotations(){}",
    "function openProjectPanel(){} function renderProjectPanel(){}",
    "function activeProjectSection(){return null;}",
    "function finishProjectSwitch(){sectionFinishes++;}",
    "function applyProjectPreview(){previewOpens++;}",
    "function recordAnnotationHistory(){} function scheduleViewerStateSync(){}",
    "function projectMenuStatus(){} function notify(){}"
  ))
  js$eval(definition)
  js$eval("var saved={schema:'wsiTools-viewer-project',project:{items:[{id:'live',path:'/slide.tif'}],annotation_sets:[{key:'live::image',rois:[]}]}}")
  js$eval("restoreBrowserProject(saved,true)")
  expect_identical(js$eval("previewOpens"), "0")
  expect_identical(js$eval("sectionFinishes"), "1")
  js$eval("restoreBrowserProject(saved,false)")
  expect_identical(js$eval("previewOpens"), "1")
})

test_that("canceled old tile requests are not reported as image failures", {
  skip_if_not_installed("V8")
  slide <- wsiTools:::wsi_mock_slide(width = 100, height = 80, levels = c(1, 4))
  path <- tempfile(fileext = ".html")
  wsi_viewer(slide, output = path, open = FALSE, overwrite = TRUE)
  lines <- readLines(path, warn = FALSE)
  js <- V8::v8()
  for (name in c("viewerTileWasCancelled", "benignViewerConsoleError")) {
    definition <- grep(paste0("^function ", name, "\\("), lines, value = TRUE)
    expect_length(definition, 1L)
    js$eval(definition)
  }
  expect_identical(js$eval("viewerTileWasCancelled({message:'Image load aborted.'})"), "true")
  expect_identical(js$eval("viewerTileWasCancelled({message:'Image load exceeded timeout (30000 ms)'})"), "false")
  expect_identical(js$eval("benignViewerConsoleError('Tile %s failed to load: %s - error: %s {} /tiles/12/2/1.jpg Image load aborted.')"), "true")
  expect_identical(js$eval("benignViewerConsoleError('Tile %s failed to load: %s - error: %s {} /tiles/12/2/1.jpg Image load exceeded timeout (30000 ms)')"), "false")
})
