#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern "C" SEXP wsi_native_czi_available() {
  return Rf_ScalarLogical(FALSE);
}

extern "C" SEXP wsi_native_czi_version() {
  return Rf_ScalarString(NA_STRING);
}

extern "C" SEXP wsi_native_czi_info(SEXP path_) {
  (void) path_;
  Rf_error("Native CZI support was not compiled for this wsiTools build.");
}

extern "C" SEXP wsi_native_czi_read_region(SEXP path_, SEXP x_, SEXP y_,
                                           SEXP width_, SEXP height_,
                                           SEXP zoom_, SEXP channel_,
                                           SEXP scene_) {
  (void) path_;
  (void) x_;
  (void) y_;
  (void) width_;
  (void) height_;
  (void) zoom_;
  (void) channel_;
  (void) scene_;
  Rf_error("Native CZI support was not compiled for this wsiTools build.");
}

static const R_CallMethodDef CallEntries[] = {
  {"wsi_native_czi_available", reinterpret_cast<DL_FUNC>(&wsi_native_czi_available), 0},
  {"wsi_native_czi_version", reinterpret_cast<DL_FUNC>(&wsi_native_czi_version), 0},
  {"wsi_native_czi_info", reinterpret_cast<DL_FUNC>(&wsi_native_czi_info), 1},
  {"wsi_native_czi_read_region", reinterpret_cast<DL_FUNC>(&wsi_native_czi_read_region), 8},
  {NULL, NULL, 0}
};

extern "C" void R_init_wsiTools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, static_cast<Rboolean>(FALSE));
}
