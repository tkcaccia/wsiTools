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

extern "C" SEXP wsi_native_czi_open_handle(SEXP path_) {
  (void) path_;
  Rf_error("Native CZI support was not compiled for this wsiTools build.");
}

extern "C" SEXP wsi_native_czi_close_handle(SEXP handle_) {
  (void) handle_;
  return Rf_ScalarLogical(FALSE);
}

extern "C" SEXP wsi_native_czi_handle_read_region(SEXP handle_, SEXP x_,
                                                  SEXP y_, SEXP width_,
                                                  SEXP height_, SEXP zoom_,
                                                  SEXP channel_, SEXP scene_) {
  (void) handle_;
  (void) x_;
  (void) y_;
  (void) width_;
  (void) height_;
  (void) zoom_;
  (void) channel_;
  (void) scene_;
  Rf_error("Native CZI support was not compiled for this wsiTools build.");
}

static bool wsi_point_in_ring_cpp(double x, double y, SEXP ring) {
  if (!Rf_isMatrix(ring)) {
    return false;
  }
  SEXP dim = Rf_getAttrib(ring, R_DimSymbol);
  if (Rf_length(dim) < 2) {
    return false;
  }
  int rows = INTEGER(dim)[0];
  int cols = INTEGER(dim)[1];
  if (rows < 3 || cols < 2) {
    return false;
  }
  const double* pts = REAL(ring);
  int n = rows;
  int last = n - 1;
  if (pts[0] == pts[last] && pts[rows] == pts[last + rows] && n > 3) {
    n -= 1;
  }
  bool inside = false;
  int j = n - 1;
  for (int i = 0; i < n; ++i) {
    double xi = pts[i], yi = pts[i + rows], xj = pts[j], yj = pts[j + rows];
    bool crosses = ((yi > y) != (yj > y));
    if (crosses) {
      double denom = yj - yi;
      if (denom == 0.0) denom = 2.220446049250313e-16;
      if (x < (xj - xi) * (y - yi) / denom + xi) {
        inside = !inside;
      }
    }
    j = i;
  }
  return inside;
}

static bool wsi_point_in_polygon_cpp(double x, double y, SEXP polygon) {
  if (!Rf_isVectorList(polygon) || Rf_length(polygon) < 1) {
    return false;
  }
  if (!wsi_point_in_ring_cpp(x, y, VECTOR_ELT(polygon, 0))) {
    return false;
  }
  for (R_xlen_t i = 1; i < Rf_xlength(polygon); ++i) {
    if (wsi_point_in_ring_cpp(x, y, VECTOR_ELT(polygon, i))) {
      return false;
    }
  }
  return true;
}

extern "C" SEXP wsi_assign_points_to_polygons_cpp(SEXP x_, SEXP y_, SEXP polygons_,
                                                  SEXP bbox_) {
  if (!Rf_isReal(x_) || !Rf_isReal(y_) || Rf_xlength(x_) != Rf_xlength(y_) ||
      !Rf_isVectorList(polygons_)) {
    Rf_error("Invalid native annotation-association input.");
  }
  R_xlen_t n_points = Rf_xlength(x_);
  R_xlen_t n_rois = Rf_xlength(polygons_);
  const double* x = REAL(x_);
  const double* y = REAL(y_);
  const double* bbox = nullptr;
  int bbox_rows = 0;
  if (!Rf_isNull(bbox_)) {
    SEXP dim = Rf_getAttrib(bbox_, R_DimSymbol);
    bbox_rows = INTEGER(dim)[0];
    bbox = REAL(bbox_);
  }
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n_points));
  int* assigned = INTEGER(out);
  for (R_xlen_t p = 0; p < n_points; ++p) {
    assigned[p] = NA_INTEGER;
    for (R_xlen_t r = 0; r < n_rois; ++r) {
      if (bbox != nullptr) {
        double xmin = bbox[r], ymin = bbox[r + bbox_rows];
        double xmax = bbox[r + 2 * bbox_rows], ymax = bbox[r + 3 * bbox_rows];
        if (R_finite(xmin) && R_finite(ymin) && R_finite(xmax) && R_finite(ymax) &&
            (x[p] < xmin || x[p] > xmax || y[p] < ymin || y[p] > ymax)) {
          continue;
        }
      }
      SEXP roi = VECTOR_ELT(polygons_, r);
      bool inside = false;
      for (R_xlen_t poly = 0; Rf_isVectorList(roi) && poly < Rf_xlength(roi); ++poly) {
        if (wsi_point_in_polygon_cpp(x[p], y[p], VECTOR_ELT(roi, poly))) {
          inside = true;
          break;
        }
      }
      if (inside) {
        assigned[p] = static_cast<int>(r + 1);
        break;
      }
    }
  }
  UNPROTECT(1);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
  {"wsi_native_czi_available", reinterpret_cast<DL_FUNC>(&wsi_native_czi_available), 0},
  {"wsi_native_czi_version", reinterpret_cast<DL_FUNC>(&wsi_native_czi_version), 0},
  {"wsi_native_czi_info", reinterpret_cast<DL_FUNC>(&wsi_native_czi_info), 1},
  {"wsi_native_czi_read_region", reinterpret_cast<DL_FUNC>(&wsi_native_czi_read_region), 8},
  {"wsi_native_czi_open_handle", reinterpret_cast<DL_FUNC>(&wsi_native_czi_open_handle), 1},
  {"wsi_native_czi_close_handle", reinterpret_cast<DL_FUNC>(&wsi_native_czi_close_handle), 1},
  {"wsi_native_czi_handle_read_region", reinterpret_cast<DL_FUNC>(&wsi_native_czi_handle_read_region), 8},
  {"wsi_assign_points_to_polygons_cpp", reinterpret_cast<DL_FUNC>(&wsi_assign_points_to_polygons_cpp), 4},
  {NULL, NULL, 0}
};

extern "C" void R_init_wsiTools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, static_cast<Rboolean>(FALSE));
}
