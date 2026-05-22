#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <algorithm>
#include <cmath>
#include <vector>

static void wsi_check_matrix(SEXP x, const char *name) {
  SEXP dim = Rf_getAttrib(x, R_DimSymbol);
  if (dim == R_NilValue || Rf_length(dim) != 2) {
    Rf_error("`%s` must be a matrix.", name);
  }
}

extern "C" SEXP wsi_cpp_edge_magnitude(SEXP gray) {
  SEXP gray_real = PROTECT(Rf_coerceVector(gray, REALSXP));
  wsi_check_matrix(gray_real, "gray");
  SEXP dim = Rf_getAttrib(gray_real, R_DimSymbol);
  const int nr = INTEGER(dim)[0];
  const int nc = INTEGER(dim)[1];
  if (nr < 1 || nc < 1) {
    UNPROTECT(1);
    Rf_error("`gray` must have positive dimensions.");
  }

  SEXP out = PROTECT(Rf_allocMatrix(REALSXP, nr, nc));
  double *input = REAL(gray_real);
  double *edge = REAL(out);
  std::fill(edge, edge + static_cast<size_t>(nr) * static_cast<size_t>(nc), 0.0);

  if (nc >= 2) {
    for (int c = 0; c < nc - 1; ++c) {
      for (int r = 0; r < nr; ++r) {
        const int left = r + c * nr;
        const int right = r + (c + 1) * nr;
        const double dx = std::fabs(input[right] - input[left]);
        if (dx > edge[left]) edge[left] = dx;
        if (dx > edge[right]) edge[right] = dx;
      }
    }
  }
  if (nr >= 2) {
    for (int c = 0; c < nc; ++c) {
      for (int r = 0; r < nr - 1; ++r) {
        const int top = r + c * nr;
        const int bottom = r + 1 + c * nr;
        const double dy = std::fabs(input[bottom] - input[top]);
        if (dy > edge[top]) edge[top] = dy;
        if (dy > edge[bottom]) edge[bottom] = dy;
      }
    }
  }

  UNPROTECT(2);
  return out;
}

extern "C" SEXP wsi_cpp_binary_dilate(SEXP mask, SEXP radius_sexp) {
  SEXP mask_logical = PROTECT(Rf_coerceVector(mask, LGLSXP));
  wsi_check_matrix(mask_logical, "mask");
  SEXP dim = Rf_getAttrib(mask_logical, R_DimSymbol);
  const int nr = INTEGER(dim)[0];
  const int nc = INTEGER(dim)[1];
  if (nr < 1 || nc < 1) {
    UNPROTECT(1);
    Rf_error("`mask` must have positive dimensions.");
  }

  int radius = Rf_asInteger(radius_sexp);
  if (radius == NA_INTEGER) {
    UNPROTECT(1);
    Rf_error("`radius` must be an integer.");
  }
  if (radius < 1) {
    UNPROTECT(1);
    return Rf_duplicate(mask_logical);
  }

  SEXP out = PROTECT(Rf_allocMatrix(LGLSXP, nr, nc));
  const int *input = LOGICAL(mask_logical);
  int *result = LOGICAL(out);
  const size_t total = static_cast<size_t>(nr) * static_cast<size_t>(nc);
  std::fill(result, result + total, FALSE);

  for (int dr = -radius; dr <= radius; ++dr) {
    for (int dc = -radius; dc <= radius; ++dc) {
      if (dr * dr + dc * dc > radius * radius) {
        continue;
      }
      const int r_start = std::max(0, -dr);
      const int r_end = std::min(nr - 1, nr - 1 - dr);
      const int c_start = std::max(0, -dc);
      const int c_end = std::min(nc - 1, nc - 1 - dc);
      if (r_start > r_end || c_start > c_end) {
        continue;
      }
      for (int c = c_start; c <= c_end; ++c) {
        const int out_c = c + dc;
        for (int r = r_start; r <= r_end; ++r) {
          if (input[r + c * nr] == TRUE) {
            result[(r + dr) + out_c * nr] = TRUE;
          }
        }
      }
    }
  }

  UNPROTECT(2);
  return out;
}

extern "C" SEXP wsi_cpp_mask_components(SEXP binary, SEXP connectivity_sexp, SEXP min_area_sexp) {
  SEXP binary_logical = PROTECT(Rf_coerceVector(binary, LGLSXP));
  wsi_check_matrix(binary_logical, "binary");
  SEXP dim = Rf_getAttrib(binary_logical, R_DimSymbol);
  const int nr = INTEGER(dim)[0];
  const int nc = INTEGER(dim)[1];
  if (nr < 1 || nc < 1) {
    UNPROTECT(1);
    Rf_error("`binary` must have positive dimensions.");
  }

  const char *connectivity = CHAR(STRING_ELT(connectivity_sexp, 0));
  const bool use_eight = connectivity[0] == '8';
  int min_area = Rf_asInteger(min_area_sexp);
  if (min_area == NA_INTEGER || min_area < 1) {
    UNPROTECT(1);
    Rf_error("`min_area` must be a positive integer.");
  }

  const int *input = LOGICAL(binary_logical);
  const size_t total = static_cast<size_t>(nr) * static_cast<size_t>(nc);
  std::vector<unsigned char> visited(total, 0);
  std::vector< std::vector<int> > components;
  std::vector<int> queue;
  std::vector<int> component;
  queue.reserve(1024);
  component.reserve(1024);

  const int dirs4[4][2] = {{-1, 0}, {1, 0}, {0, -1}, {0, 1}};
  const int dirs8[8][2] = {
    {-1, 0}, {1, 0}, {0, -1}, {0, 1},
    {-1, -1}, {-1, 1}, {1, -1}, {1, 1}
  };
  const int ndirs = use_eight ? 8 : 4;

  for (size_t start = 0; start < total; ++start) {
    if (visited[start] || input[start] != TRUE) {
      continue;
    }
    visited[start] = 1;
    queue.clear();
    component.clear();
    queue.push_back(static_cast<int>(start));
    size_t head = 0;
    while (head < queue.size()) {
      const int idx = queue[head++];
      component.push_back(idx);
      const int r = idx % nr;
      const int c = idx / nr;
      for (int d = 0; d < ndirs; ++d) {
        const int rr = r + (use_eight ? dirs8[d][0] : dirs4[d][0]);
        const int cc = c + (use_eight ? dirs8[d][1] : dirs4[d][1]);
        if (rr < 0 || rr >= nr || cc < 0 || cc >= nc) {
          continue;
        }
        const int nidx = rr + cc * nr;
        if (!visited[nidx] && input[nidx] == TRUE) {
          visited[nidx] = 1;
          queue.push_back(nidx);
        }
      }
    }
    if (static_cast<int>(component.size()) >= min_area) {
      components.push_back(component);
    }
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, static_cast<R_xlen_t>(components.size())));
  for (size_t i = 0; i < components.size(); ++i) {
    const std::vector<int> &component_i = components[i];
    const R_xlen_t n = static_cast<R_xlen_t>(component_i.size());
    SEXP mat = PROTECT(Rf_allocMatrix(INTSXP, n, 2));
    int *values = INTEGER(mat);
    for (R_xlen_t j = 0; j < n; ++j) {
      const int idx = component_i[static_cast<size_t>(j)];
      values[j] = (idx % nr) + 1;
      values[j + n] = (idx / nr) + 1;
    }
    SEXP names = PROTECT(Rf_allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, Rf_mkChar("row"));
    SET_STRING_ELT(names, 1, Rf_mkChar("col"));
    SEXP dimnames = PROTECT(Rf_allocVector(VECSXP, 2));
    SET_VECTOR_ELT(dimnames, 1, names);
    Rf_setAttrib(mat, R_DimNamesSymbol, dimnames);
    SET_VECTOR_ELT(out, static_cast<R_xlen_t>(i), mat);
    UNPROTECT(3);
  }

  UNPROTECT(2);
  return out;
}

static const R_CallMethodDef CallEntries[] = {
  {"wsi_cpp_edge_magnitude", (DL_FUNC) &wsi_cpp_edge_magnitude, 1},
  {"wsi_cpp_binary_dilate", (DL_FUNC) &wsi_cpp_binary_dilate, 2},
  {"wsi_cpp_mask_components", (DL_FUNC) &wsi_cpp_mask_components, 3},
  {NULL, NULL, 0}
};

extern "C" void R_init_wsiTools(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
