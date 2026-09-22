wsi_viewer_runtime_asset <- function(name) {
  path <- system.file("viewer", name, package = "wsiTools")
  if (!nzchar(path) || !file.exists(path)) {
    wsi_abort(sprintf("Viewer runtime asset is missing: %s. Reinstall wsiTools.", name))
  }
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

wsi_viewer_runtime_js <- function() {
  wasm_path <- system.file("viewer", "clipper2.wasm", package = "wsiTools")
  if (!nzchar(wasm_path)) wsi_abort("Viewer geometry kernel is missing. Reinstall wsiTools.")
  worker <- paste(
    paste0("const wsiClipper2Binary = ", jsonlite::toJSON(
      jsonlite::base64_enc(readBin(wasm_path, "raw", n = file.info(wasm_path)$size)),
      auto_unbox = TRUE), ";"),
    wsi_viewer_runtime_asset("clipper2.js"),
    wsi_viewer_runtime_asset("clipper.js"),
    wsi_viewer_runtime_asset("geometry-codec.js"),
    wsi_viewer_runtime_asset("geometry-kernel.js"),
    wsi_viewer_runtime_asset("geometry-local.js"),
    wsi_viewer_runtime_asset("geometry-worker.js"), sep = "\n"
  )
  paste0(
    "const wsiGeometryWorkerSource = ",
    jsonlite::toJSON(worker, auto_unbox = TRUE), ";\n",
    wsi_viewer_runtime_asset("geometry-codec.js"), "\n",
    wsi_viewer_runtime_asset("performance.js"), "\n",
    wsi_viewer_runtime_asset("edit-transactions.js"), "\n",
    wsi_viewer_runtime_asset("sync.js"), "\n",
    wsi_viewer_runtime_asset("annotation-selection.js"), "\n"
  )
}
