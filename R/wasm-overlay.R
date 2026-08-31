# Browser-side WebAssembly support for dense annotation overlay culling.
# The viewer remains fully functional when this optional optimization is absent.

wsi_overlay_wasm_base64 <- local({
  cached <- NULL

  function() {
    if (!is.null(cached)) {
      return(cached)
    }

    path <- system.file("wasm", "wsi_overlay_core.wasm", package = "wsiTools")
    if (!nzchar(path) || !file.exists(path)) {
      return(NULL)
    }

    size <- file.info(path)$size
    if (!is.finite(size) || size <= 0L || size > 1024 * 1024) {
      return(NULL)
    }

    cached <<- jsonlite::base64_enc(readBin(path, what = "raw", n = size))
    cached
  }
})
