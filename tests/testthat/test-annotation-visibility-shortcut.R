test_that("Shift+A toggles tissue annotation visibility without hiding coordinates", {
  skip_if_not_installed("V8")
  code <- strsplit(wsiTools:::wsi_viewer_stain_js(), "\n", fixed = TRUE)[[1L]]
  definitions <- code[grepl(
    "^(let (overlayFocusMode|tissueAnnotationsVisible)=|function (overlayFocusActive|toggleTissueAnnotations)\\()",
    code
  )]
  expect_length(definitions, 4L)
  js <- V8::v8()
  js$eval(paste0(
    "var showRois=true, redraws=0;",
    "function updateOverlayFocusControls(){}",
    "function updateButtons(){}",
    "function applyOverlayFocus(){redraws++;}",
    "function notify(){}"
  ))
  js$eval(paste(definitions, collapse = "\n"))
  expect_identical(js$eval("overlayFocusActive('annotation')"), "true")
  expect_identical(js$eval("toggleTissueAnnotations()"), "false")
  expect_identical(js$eval("overlayFocusActive('annotation')"), "false")
  expect_identical(js$eval("overlayFocusActive('coordinates')"), "true")
  expect_identical(js$eval("toggleTissueAnnotations()"), "true")
  expect_identical(js$eval("overlayFocusActive('annotation')"), "true")
  expect_identical(js$eval("redraws"), "2")
  js$eval("overlayFocusMode='coordinates'; toggleTissueAnnotations(); toggleTissueAnnotations()")
  expect_identical(js$eval("overlayFocusMode"), "coordinates_annotation")
})

test_that("Shift+A shortcut ignores typing and repeated keydown events", {
  skip_if_not_installed("V8")
  js <- V8::v8()
  js$eval(paste0(
    "var handlers=[],toggles=0;",
    "var document={querySelectorAll:()=>[],querySelector:()=>null,",
    "addEventListener:(name,fn)=>{if(name==='keydown')handlers.push(fn)}};",
    "function el(){return null;}",
    "function toggleTissueAnnotations(){toggles++;}",
    "function setMode(){}"
  ))
  js$eval(wsiTools:::wsi_viewer_shortcuts_js())
  js$eval("bindShortcutHelp()")
  js$eval(paste0(
    "function fire(opts){var e=Object.assign({key:'A',shiftKey:true,",
    "ctrlKey:false,metaKey:false,altKey:false,repeat:false,",
    "target:{tagName:'BODY',isContentEditable:false},",
    "preventDefault(){},stopPropagation(){}},opts);",
    "handlers.forEach(fn=>fn(e));}"
  ))
  js$eval("fire({});fire({repeat:true});fire({target:{tagName:'INPUT'}});fire({ctrlKey:true});fire({shiftKey:false})")
  expect_identical(js$eval("toggles"), "1")
})
