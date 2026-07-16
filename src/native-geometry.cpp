#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#include <algorithm>
#include <cmath>
#include <cstdint>
#include <limits>
#include <unordered_map>
#include <vector>

namespace {

struct BboxIndex {
  std::vector<double> xmin;
  std::vector<double> ymin;
  std::vector<double> xmax;
  std::vector<double> ymax;
  std::vector<uint32_t> stamps;
  std::unordered_map<uint64_t, std::vector<int>> cells;
  std::vector<int> large;
  double origin_x = 0.0;
  double origin_y = 0.0;
  double cell_width = 1.0;
  double cell_height = 1.0;
  int cols = 1;
  int rows = 1;
  uint32_t stamp = 0;
};

uint64_t cell_key(int col, int row) {
  return (static_cast<uint64_t>(static_cast<uint32_t>(row)) << 32U) |
         static_cast<uint32_t>(col);
}

int clamp_cell(double value, double origin, double size, int count) {
  if (count <= 1 || !R_finite(value) || !R_finite(origin) || !R_finite(size) || size <= 0) {
    return 0;
  }
  const double raw = std::floor((value - origin) / size);
  if (raw <= 0) return 0;
  if (raw >= count - 1) return count - 1;
  return static_cast<int>(raw);
}

void bbox_index_finalizer(SEXP pointer) {
  auto* index = static_cast<BboxIndex*>(R_ExternalPtrAddr(pointer));
  if (index != nullptr) {
    delete index;
    R_ClearExternalPtr(pointer);
  }
}

BboxIndex* checked_index(SEXP pointer) {
  if (TYPEOF(pointer) != EXTPTRSXP) {
    Rf_error("`index` must be a native wsiTools bounding-box index.");
  }
  auto* index = static_cast<BboxIndex*>(R_ExternalPtrAddr(pointer));
  if (index == nullptr) {
    Rf_error("The native wsiTools bounding-box index is no longer available.");
  }
  return index;
}

double scalar_number(SEXP value, const char* name) {
  if (!Rf_isNumeric(value) || Rf_xlength(value) != 1) {
    Rf_error("`%s` must be one finite number.", name);
  }
  const double out = Rf_asReal(value);
  if (!R_finite(out)) {
    Rf_error("`%s` must be one finite number.", name);
  }
  return out;
}

}  // namespace

extern "C" SEXP wsi_bbox_index_build_cpp(SEXP bbox_) {
  if (!Rf_isMatrix(bbox_) || !Rf_isReal(bbox_)) {
    Rf_error("`bbox` must be a numeric matrix with four columns.");
  }
  SEXP dim = Rf_getAttrib(bbox_, R_DimSymbol);
  const int n = INTEGER(dim)[0];
  const int columns = INTEGER(dim)[1];
  if (columns < 4) {
    Rf_error("`bbox` must have at least four columns: xmin, ymin, xmax, ymax.");
  }

  auto* index = new BboxIndex();
  index->xmin.resize(n);
  index->ymin.resize(n);
  index->xmax.resize(n);
  index->ymax.resize(n);
  index->stamps.assign(n, 0U);
  const double* bbox = REAL(bbox_);
  double global_xmin = std::numeric_limits<double>::infinity();
  double global_ymin = std::numeric_limits<double>::infinity();
  double global_xmax = -std::numeric_limits<double>::infinity();
  double global_ymax = -std::numeric_limits<double>::infinity();
  int valid = 0;

  for (int i = 0; i < n; ++i) {
    double xmin = bbox[i];
    double ymin = bbox[i + n];
    double xmax = bbox[i + 2 * n];
    double ymax = bbox[i + 3 * n];
    if (R_finite(xmin) && R_finite(ymin) && R_finite(xmax) && R_finite(ymax)) {
      if (xmax < xmin) std::swap(xmin, xmax);
      if (ymax < ymin) std::swap(ymin, ymax);
      global_xmin = std::min(global_xmin, xmin);
      global_ymin = std::min(global_ymin, ymin);
      global_xmax = std::max(global_xmax, xmax);
      global_ymax = std::max(global_ymax, ymax);
      ++valid;
    }
    index->xmin[i] = xmin;
    index->ymin[i] = ymin;
    index->xmax[i] = xmax;
    index->ymax[i] = ymax;
  }

  if (valid > 0) {
    const double width = std::max(1.0, global_xmax - global_xmin);
    const double height = std::max(1.0, global_ymax - global_ymin);
    const double target = std::sqrt(std::max(1.0, valid / 24.0));
    const double aspect = width / height;
    index->cols = std::max(1, std::min(512, static_cast<int>(std::round(target * std::sqrt(aspect)))));
    index->rows = std::max(1, std::min(512, static_cast<int>(std::round(target / std::sqrt(aspect)))));
    index->origin_x = global_xmin;
    index->origin_y = global_ymin;
    index->cell_width = width / index->cols;
    index->cell_height = height / index->rows;

    for (int i = 0; i < n; ++i) {
      if (!R_finite(index->xmin[i]) || !R_finite(index->ymin[i]) ||
          !R_finite(index->xmax[i]) || !R_finite(index->ymax[i])) {
        continue;
      }
      const int c0 = clamp_cell(index->xmin[i], index->origin_x, index->cell_width, index->cols);
      const int c1 = clamp_cell(index->xmax[i], index->origin_x, index->cell_width, index->cols);
      const int r0 = clamp_cell(index->ymin[i], index->origin_y, index->cell_height, index->rows);
      const int r1 = clamp_cell(index->ymax[i], index->origin_y, index->cell_height, index->rows);
      const int covered = (c1 - c0 + 1) * (r1 - r0 + 1);
      if (covered > 256) {
        index->large.push_back(i);
        continue;
      }
      for (int row = r0; row <= r1; ++row) {
        for (int col = c0; col <= c1; ++col) {
          index->cells[cell_key(col, row)].push_back(i);
        }
      }
    }
  }

  SEXP pointer = PROTECT(R_MakeExternalPtr(index, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(pointer, bbox_index_finalizer, TRUE);
  SEXP klass = PROTECT(Rf_mkString("wsi_bbox_index"));
  Rf_classgets(pointer, klass);
  UNPROTECT(2);
  return pointer;
}

extern "C" SEXP wsi_bbox_index_query_cpp(SEXP pointer_, SEXP xmin_, SEXP ymin_,
                                           SEXP xmax_, SEXP ymax_) {
  BboxIndex* index = checked_index(pointer_);
  double xmin = scalar_number(xmin_, "xmin");
  double ymin = scalar_number(ymin_, "ymin");
  double xmax = scalar_number(xmax_, "xmax");
  double ymax = scalar_number(ymax_, "ymax");
  if (xmax < xmin) std::swap(xmin, xmax);
  if (ymax < ymin) std::swap(ymin, ymax);

  if (++index->stamp == 0U) {
    std::fill(index->stamps.begin(), index->stamps.end(), 0U);
    index->stamp = 1U;
  }
  std::vector<int> candidates;
  const auto add = [&](int value) {
    if (value < 0 || static_cast<size_t>(value) >= index->stamps.size()) return;
    if (index->stamps[value] == index->stamp) return;
    index->stamps[value] = index->stamp;
    candidates.push_back(value);
  };

  const int c0 = clamp_cell(xmin, index->origin_x, index->cell_width, index->cols);
  const int c1 = clamp_cell(xmax, index->origin_x, index->cell_width, index->cols);
  const int r0 = clamp_cell(ymin, index->origin_y, index->cell_height, index->rows);
  const int r1 = clamp_cell(ymax, index->origin_y, index->cell_height, index->rows);
  for (int row = r0; row <= r1; ++row) {
    for (int col = c0; col <= c1; ++col) {
      const auto found = index->cells.find(cell_key(col, row));
      if (found == index->cells.end()) continue;
      for (int value : found->second) add(value);
    }
  }
  for (int value : index->large) add(value);

  std::vector<int> matches;
  matches.reserve(candidates.size());
  for (int value : candidates) {
    if (R_finite(index->xmin[value]) && R_finite(index->ymin[value]) &&
        R_finite(index->xmax[value]) && R_finite(index->ymax[value]) &&
        index->xmin[value] <= xmax && index->xmax[value] >= xmin &&
        index->ymin[value] <= ymax && index->ymax[value] >= ymin) {
      matches.push_back(value + 1);
    }
  }
  std::sort(matches.begin(), matches.end());
  SEXP out = PROTECT(Rf_allocVector(INTSXP, matches.size()));
  std::copy(matches.begin(), matches.end(), INTEGER(out));
  UNPROTECT(1);
  return out;
}
