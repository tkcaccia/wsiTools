#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <algorithm>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <string>
#include <vector>

#ifdef _WIN32
// Avoid including windows.h here. R defines macros such as Realloc and Free,
// while Windows headers pull in COM interfaces with methods of the same names.
// The native CZI bridge only needs the three dynamic-loader calls below.
#ifndef WSITOOLS_WINAPI
#define WSITOOLS_WINAPI __stdcall
#endif
extern "C" {
__declspec(dllimport) void* WSITOOLS_WINAPI LoadLibraryA(const char* path);
__declspec(dllimport) int WSITOOLS_WINAPI FreeLibrary(void* handle);
__declspec(dllimport) void* WSITOOLS_WINAPI GetProcAddress(void* handle, const char* name);
}
#else
#include <dlfcn.h>
#endif

// Minimal ABI structs mirrored from the public ZEISS libCZIAPI C interface.
// Keeping them here lets wsiTools load libCZIAPI at runtime without making the
// LGPL libCZI shared library a mandatory installation dependency.
#pragma pack(push, 4)
struct LibCZIVersionInfoInterop {
  int32_t major;
  int32_t minor;
  int32_t patch;
  int32_t tweak;
};

typedef intptr_t ObjectHandle;
typedef ObjectHandle CziReaderObjectHandle;
typedef ObjectHandle InputStreamObjectHandle;
typedef ObjectHandle MetadataSegmentObjectHandle;
typedef ObjectHandle SingleChannelScalingTileAccessorObjectHandle;
typedef ObjectHandle BitmapObjectHandle;

struct ReaderOpenInfoInterop {
  InputStreamObjectHandle streamObject;
};

struct IntRectInterop {
  int32_t x;
  int32_t y;
  int32_t w;
  int32_t h;
};

struct IntSizeInterop {
  int32_t w;
  int32_t h;
};

struct DimBoundsInterop {
  uint32_t dimensions_valid;
  int32_t start[9];
  int32_t size[9];
};

struct CoordinateInterop {
  uint32_t dimensions_valid;
  int32_t value[9];
};

struct SubBlockStatisticsInterop {
  int32_t sub_block_count;
  int32_t min_m_index;
  int32_t max_m_index;
  IntRectInterop bounding_box;
  IntRectInterop bounding_box_layer0;
  DimBoundsInterop dim_bounds;
};

struct BoundingBoxesInterop {
  int32_t sceneIndex;
  IntRectInterop bounding_box;
  IntRectInterop bounding_box_layer0_only;
};

struct SubBlockStatisticsInteropEx {
  int32_t sub_block_count;
  int32_t min_m_index;
  int32_t max_m_index;
  IntRectInterop bounding_box;
  IntRectInterop bounding_box_layer0;
  DimBoundsInterop dim_bounds;
  int32_t number_of_per_scenes_bounding_boxes;
  BoundingBoxesInterop per_scenes_bounding_boxes[1];
};

struct MetadataAsXmlInterop {
  void* data;
  uint64_t size;
};

struct AccessorOptionsInterop {
  float back_ground_color_r;
  float back_ground_color_g;
  float back_ground_color_b;
  bool sort_by_m;
  bool use_visibility_check_optimization;
  const char* additional_parameters;
};

struct BitmapInfoInterop {
  uint32_t width;
  uint32_t height;
  int32_t pixelType;
};

struct BitmapLockInfoInterop {
  void* ptrData;
  void* ptrDataRoi;
  uint32_t stride;
  uint64_t size;
};
#pragma pack(pop)

namespace {

class DynamicLibrary {
 public:
  DynamicLibrary() : handle_(nullptr) {}
  ~DynamicLibrary() { close(); }

  bool open(const std::string& path) {
    close();
#ifdef _WIN32
    handle_ = LoadLibraryA(path.c_str());
#else
    handle_ = dlopen(path.c_str(), RTLD_LAZY | RTLD_LOCAL);
#endif
    name_ = path;
    return handle_ != nullptr;
  }

  void close() {
    if (!handle_) return;
#ifdef _WIN32
    FreeLibrary(handle_);
#else
    dlclose(handle_);
#endif
    handle_ = nullptr;
  }

  void* symbol(const char* name) const {
    if (!handle_) return nullptr;
#ifdef _WIN32
    return GetProcAddress(handle_, name);
#else
    return dlsym(handle_, name);
#endif
  }

  bool loaded() const { return handle_ != nullptr; }
  const std::string& name() const { return name_; }

 private:
  void* handle_;
  std::string name_;
};

std::vector<std::string> candidate_libraries() {
  std::vector<std::string> out;
  const char* env_api = std::getenv("WSITOOLS_LIBCZIAPI");
  const char* env_czi = std::getenv("WSITOOLS_LIBCZI");
  if (env_api && env_api[0]) out.push_back(env_api);
  if (env_czi && env_czi[0]) out.push_back(env_czi);
#ifdef _WIN32
  out.push_back("libCZIAPI.dll");
  out.push_back("libCZIApi.dll");
  out.push_back("libCZI.dll");
#elif defined(__APPLE__)
  out.push_back("libCZIAPI.dylib");
  out.push_back("libCZIApi.dylib");
  out.push_back("libCZI.dylib");
#else
  out.push_back("libCZIAPI.so");
  out.push_back("libCZIApi.so");
  out.push_back("libCZI.so");
#endif
  return out;
}

bool load_libczi(DynamicLibrary& lib) {
  std::vector<std::string> candidates = candidate_libraries();
  for (std::vector<std::string>::const_iterator it = candidates.begin(); it != candidates.end(); ++it) {
    if (lib.open(*it)) return true;
  }
  return false;
}

template <typename T>
T sym(DynamicLibrary& lib, const char* name, bool required = true) {
  void* ptr = lib.symbol(name);
  if (!ptr && required) {
    Rf_error("Native CZI library `%s` is missing required symbol `%s`.", lib.name().c_str(), name);
  }
  return reinterpret_cast<T>(ptr);
}

typedef int32_t (*GetVersionFn)(LibCZIVersionInfoInterop*);
typedef void (*FreeFn)(void*);
typedef int32_t (*CreateReaderFn)(CziReaderObjectHandle*);
typedef int32_t (*ReleaseReaderFn)(CziReaderObjectHandle);
typedef int32_t (*CreateInputStreamFromFileUTF8Fn)(const char*, InputStreamObjectHandle*);
typedef int32_t (*ReleaseInputStreamFn)(InputStreamObjectHandle);
typedef int32_t (*ReaderOpenFn)(CziReaderObjectHandle, const ReaderOpenInfoInterop*);
typedef int32_t (*ReaderGetStatisticsSimpleFn)(CziReaderObjectHandle, SubBlockStatisticsInterop*);
typedef int32_t (*ReaderGetStatisticsExFn)(CziReaderObjectHandle, SubBlockStatisticsInteropEx*, int32_t*);
typedef int32_t (*ReaderGetPyramidStatisticsFn)(CziReaderObjectHandle, char**);
typedef int32_t (*ReaderGetMetadataSegmentFn)(CziReaderObjectHandle, MetadataSegmentObjectHandle*);
typedef int32_t (*MetadataSegmentGetMetadataAsXmlFn)(MetadataSegmentObjectHandle, MetadataAsXmlInterop*);
typedef int32_t (*ReleaseMetadataSegmentFn)(MetadataSegmentObjectHandle);
typedef int32_t (*ReaderGetAttachmentCountFn)(CziReaderObjectHandle, int32_t*);
typedef int32_t (*CreateSingleChannelTileAccessorFn)(CziReaderObjectHandle, SingleChannelScalingTileAccessorObjectHandle*);
typedef int32_t (*SingleChannelTileAccessorGetFn)(SingleChannelScalingTileAccessorObjectHandle, const CoordinateInterop*, const IntRectInterop*, float, const AccessorOptionsInterop*, BitmapObjectHandle*);
typedef int32_t (*ReleaseSingleChannelTileAccessorFn)(SingleChannelScalingTileAccessorObjectHandle);
typedef int32_t (*BitmapGetInfoFn)(BitmapObjectHandle, BitmapInfoInterop*);
typedef int32_t (*BitmapLockFn)(BitmapObjectHandle, BitmapLockInfoInterop*);
typedef int32_t (*BitmapUnlockFn)(BitmapObjectHandle);
typedef int32_t (*ReleaseBitmapFn)(BitmapObjectHandle);

std::string version_string(DynamicLibrary& lib) {
  GetVersionFn get_version = sym<GetVersionFn>(lib, "libCZI_GetLibCZIVersionInfo");
  LibCZIVersionInfoInterop version;
  std::memset(&version, 0, sizeof(version));
  int32_t err = get_version(&version);
  if (err != 0) return std::string();
  std::ostringstream os;
  os << "libCZIAPI " << version.major << "." << version.minor << "." << version.patch << "." << version.tweak;
  return os.str();
}

bool has_required_symbols(DynamicLibrary& lib) {
  const char* required[] = {
    "libCZI_GetLibCZIVersionInfo",
    "libCZI_CreateReader",
    "libCZI_ReleaseReader",
    "libCZI_CreateInputStreamFromFileUTF8",
    "libCZI_ReleaseInputStream",
    "libCZI_ReaderOpen",
    "libCZI_ReaderGetStatisticsSimple",
    "libCZI_CreateSingleChannelTileAccessor",
    "libCZI_SingleChannelTileAccessorGet",
    "libCZI_ReleaseCreateSingleChannelTileAccessor",
    "libCZI_BitmapGetInfo",
    "libCZI_BitmapLock",
    "libCZI_BitmapUnlock",
    "libCZI_ReleaseBitmap"
  };
  for (size_t i = 0; i < sizeof(required) / sizeof(required[0]); ++i) {
    if (!lib.symbol(required[i])) return false;
  }
  return true;
}

const char* dim_label(int dim_index) {
  switch (dim_index) {
    case 1: return "Z";
    case 2: return "C";
    case 3: return "T";
    case 4: return "R";
    case 5: return "S";
    case 6: return "I";
    case 7: return "H";
    case 8: return "V";
    case 9: return "B";
    default: return "?";
  }
}

SEXP dim_bounds_to_r(const DimBoundsInterop& dims) {
  int n = 0;
  for (int dim = 1; dim <= 9; ++dim) {
    if (dims.dimensions_valid & (1u << (dim - 1))) ++n;
  }
  SEXP dimension = PROTECT(Rf_allocVector(STRSXP, n));
  SEXP start = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP size = PROTECT(Rf_allocVector(INTSXP, n));
  int pos = 0;
  int compact = 0;
  for (int dim = 1; dim <= 9; ++dim) {
    if (dims.dimensions_valid & (1u << (dim - 1))) {
      SET_STRING_ELT(dimension, pos, Rf_mkChar(dim_label(dim)));
      INTEGER(start)[pos] = dims.start[compact];
      INTEGER(size)[pos] = dims.size[compact];
      ++pos;
      ++compact;
    }
  }
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(out, 0, dimension);
  SET_VECTOR_ELT(out, 1, start);
  SET_VECTOR_ELT(out, 2, size);
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 3));
  SET_STRING_ELT(names, 0, Rf_mkChar("dimension"));
  SET_STRING_ELT(names, 1, Rf_mkChar("start"));
  SET_STRING_ELT(names, 2, Rf_mkChar("size"));
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(5);
  return out;
}

SEXP scene_boxes_to_r(DynamicLibrary& lib, CziReaderObjectHandle reader) {
  ReaderGetStatisticsExFn get_stats_ex =
      sym<ReaderGetStatisticsExFn>(lib, "libCZI_ReaderGetStatisticsEx", false);
  if (!get_stats_ex) {
    return R_NilValue;
  }

  int32_t count = 0;
  std::vector<unsigned char> probe(sizeof(SubBlockStatisticsInteropEx));
  std::memset(probe.data(), 0, probe.size());
  SubBlockStatisticsInteropEx* probe_stats =
      reinterpret_cast<SubBlockStatisticsInteropEx*>(probe.data());
  int32_t err = get_stats_ex(reader, probe_stats, &count);
  if (err != 0 || count <= 0) {
    return R_NilValue;
  }

  size_t bytes = sizeof(SubBlockStatisticsInteropEx) +
    static_cast<size_t>(std::max<int32_t>(count, 1) - 1) * sizeof(BoundingBoxesInterop);
  std::vector<unsigned char> buffer(bytes);
  std::memset(buffer.data(), 0, buffer.size());
  SubBlockStatisticsInteropEx* stats =
      reinterpret_cast<SubBlockStatisticsInteropEx*>(buffer.data());
  int32_t requested = count;
  err = get_stats_ex(reader, stats, &requested);
  if (err != 0 || stats->number_of_per_scenes_bounding_boxes <= 0) {
    return R_NilValue;
  }

  int n = stats->number_of_per_scenes_bounding_boxes;
  SEXP scene = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP x = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP y = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP width = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP height = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP pyramid_x = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP pyramid_y = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP pyramid_width = PROTECT(Rf_allocVector(INTSXP, n));
  SEXP pyramid_height = PROTECT(Rf_allocVector(INTSXP, n));

  for (int i = 0; i < n; ++i) {
    const BoundingBoxesInterop& box = stats->per_scenes_bounding_boxes[i];
    IntRectInterop layer0 = box.bounding_box_layer0_only.w > 0 && box.bounding_box_layer0_only.h > 0 ?
      box.bounding_box_layer0_only : box.bounding_box;
    INTEGER(scene)[i] = box.sceneIndex;
    INTEGER(x)[i] = layer0.x;
    INTEGER(y)[i] = layer0.y;
    INTEGER(width)[i] = layer0.w;
    INTEGER(height)[i] = layer0.h;
    INTEGER(pyramid_x)[i] = box.bounding_box.x;
    INTEGER(pyramid_y)[i] = box.bounding_box.y;
    INTEGER(pyramid_width)[i] = box.bounding_box.w;
    INTEGER(pyramid_height)[i] = box.bounding_box.h;
  }

  SEXP out = PROTECT(Rf_allocVector(VECSXP, 9));
  SET_VECTOR_ELT(out, 0, scene);
  SET_VECTOR_ELT(out, 1, x);
  SET_VECTOR_ELT(out, 2, y);
  SET_VECTOR_ELT(out, 3, width);
  SET_VECTOR_ELT(out, 4, height);
  SET_VECTOR_ELT(out, 5, pyramid_x);
  SET_VECTOR_ELT(out, 6, pyramid_y);
  SET_VECTOR_ELT(out, 7, pyramid_width);
  SET_VECTOR_ELT(out, 8, pyramid_height);

  SEXP names = PROTECT(Rf_allocVector(STRSXP, 9));
  const char* name_values[] = {
    "scene", "x", "y", "width", "height",
    "pyramid_x", "pyramid_y", "pyramid_width", "pyramid_height"
  };
  for (int i = 0; i < 9; ++i) SET_STRING_ELT(names, i, Rf_mkChar(name_values[i]));
  Rf_setAttrib(out, R_NamesSymbol, names);

  SEXP row_names = PROTECT(Rf_allocVector(INTSXP, 2));
  INTEGER(row_names)[0] = NA_INTEGER;
  INTEGER(row_names)[1] = -n;
  Rf_setAttrib(out, R_RowNamesSymbol, row_names);
  Rf_setAttrib(out, R_ClassSymbol, Rf_mkString("data.frame"));

  UNPROTECT(12);
  return out;
}

struct OpenedCzi {
  CziReaderObjectHandle reader;
  InputStreamObjectHandle stream;
};

OpenedCzi open_czi(DynamicLibrary& lib, const char* path) {
  CreateReaderFn create_reader = sym<CreateReaderFn>(lib, "libCZI_CreateReader");
  CreateInputStreamFromFileUTF8Fn create_stream = sym<CreateInputStreamFromFileUTF8Fn>(lib, "libCZI_CreateInputStreamFromFileUTF8");
  ReaderOpenFn reader_open = sym<ReaderOpenFn>(lib, "libCZI_ReaderOpen");

  OpenedCzi out;
  out.reader = 0;
  out.stream = 0;
  int32_t err = create_stream(path, &out.stream);
  if (err != 0 || out.stream == 0) {
    Rf_error("libCZIAPI could not create an input stream for `%s` (error %d).", path, err);
  }
  err = create_reader(&out.reader);
  if (err != 0 || out.reader == 0) {
    ReleaseInputStreamFn release_stream = sym<ReleaseInputStreamFn>(lib, "libCZI_ReleaseInputStream", false);
    if (release_stream) release_stream(out.stream);
    Rf_error("libCZIAPI could not create a CZI reader (error %d).", err);
  }
  ReaderOpenInfoInterop open_info;
  open_info.streamObject = out.stream;
  err = reader_open(out.reader, &open_info);
  if (err != 0) {
    ReleaseReaderFn release_reader = sym<ReleaseReaderFn>(lib, "libCZI_ReleaseReader", false);
    ReleaseInputStreamFn release_stream = sym<ReleaseInputStreamFn>(lib, "libCZI_ReleaseInputStream", false);
    if (release_reader) release_reader(out.reader);
    if (release_stream) release_stream(out.stream);
    Rf_error("libCZIAPI could not open `%s` (error %d).", path, err);
  }
  return out;
}

void close_czi(DynamicLibrary& lib, OpenedCzi& czi) {
  ReleaseReaderFn release_reader = sym<ReleaseReaderFn>(lib, "libCZI_ReleaseReader", false);
  ReleaseInputStreamFn release_stream = sym<ReleaseInputStreamFn>(lib, "libCZI_ReleaseInputStream", false);
  if (release_reader && czi.reader != 0) release_reader(czi.reader);
  if (release_stream && czi.stream != 0) release_stream(czi.stream);
  czi.reader = 0;
  czi.stream = 0;
}

double pixel_to_unit_gray(const unsigned char* p, int pixel_type) {
  switch (pixel_type) {
    case 0: return static_cast<double>(p[0]) / 255.0;  // Gray8
    case 1: {  // Gray16
      const uint16_t* q = reinterpret_cast<const uint16_t*>(p);
      return static_cast<double>(*q) / 65535.0;
    }
    case 2: {  // Gray32Float
      const float* q = reinterpret_cast<const float*>(p);
      double v = static_cast<double>(*q);
      if (v < 0) v = 0;
      if (v > 1) v = 1;
      return v;
    }
    default:
      return 0;
  }
}

void set_rgb(SEXP out, int height, int width, int row, int col, double r, double g, double b) {
  double* ptr = REAL(out);
  size_t plane = static_cast<size_t>(height) * static_cast<size_t>(width);
  size_t idx = static_cast<size_t>(row) + static_cast<size_t>(height) * static_cast<size_t>(col);
  ptr[idx] = r;
  ptr[idx + plane] = g;
  ptr[idx + 2 * plane] = b;
}

SEXP bitmap_to_rgb_array(BitmapObjectHandle bitmap,
                         BitmapGetInfoFn bitmap_info_fn,
                         BitmapLockFn bitmap_lock,
                         BitmapUnlockFn bitmap_unlock,
                         ReleaseBitmapFn release_bitmap) {
  BitmapInfoInterop info;
  std::memset(&info, 0, sizeof(info));
  int32_t err = bitmap_info_fn(bitmap, &info);
  if (err != 0 || info.width == 0 || info.height == 0) {
    release_bitmap(bitmap);
    Rf_error("libCZIAPI could not inspect the decoded CZI bitmap (error %d).", err);
  }

  BitmapLockInfoInterop lock;
  std::memset(&lock, 0, sizeof(lock));
  err = bitmap_lock(bitmap, &lock);
  if (err != 0 || !lock.ptrDataRoi) {
    release_bitmap(bitmap);
    Rf_error("libCZIAPI could not lock the decoded CZI bitmap (error %d).", err);
  }

  int out_w = static_cast<int>(info.width);
  int out_h = static_cast<int>(info.height);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, static_cast<R_xlen_t>(out_w) * out_h * 3));
  SEXP dim = PROTECT(Rf_allocVector(INTSXP, 3));
  INTEGER(dim)[0] = out_h;
  INTEGER(dim)[1] = out_w;
  INTEGER(dim)[2] = 3;
  Rf_setAttrib(out, R_DimSymbol, dim);

  const unsigned char* base = static_cast<const unsigned char*>(lock.ptrDataRoi);
  for (int row = 0; row < out_h; ++row) {
    const unsigned char* line = base + static_cast<size_t>(row) * lock.stride;
    for (int col = 0; col < out_w; ++col) {
      if (info.pixelType == 0 || info.pixelType == 1 || info.pixelType == 2) {
        int bytes = info.pixelType == 0 ? 1 : (info.pixelType == 1 ? 2 : 4);
        double v = pixel_to_unit_gray(line + static_cast<size_t>(col) * bytes, info.pixelType);
        set_rgb(out, out_h, out_w, row, col, v, v, v);
      } else if (info.pixelType == 3) {  // Bgr24
        const unsigned char* p = line + static_cast<size_t>(col) * 3;
        set_rgb(out, out_h, out_w, row, col, p[2] / 255.0, p[1] / 255.0, p[0] / 255.0);
      } else if (info.pixelType == 4) {  // Bgr48
        const uint16_t* p = reinterpret_cast<const uint16_t*>(line + static_cast<size_t>(col) * 6);
        set_rgb(out, out_h, out_w, row, col, p[2] / 65535.0, p[1] / 65535.0, p[0] / 65535.0);
      } else if (info.pixelType == 9) {  // Bgra32
        const unsigned char* p = line + static_cast<size_t>(col) * 4;
        set_rgb(out, out_h, out_w, row, col, p[2] / 255.0, p[1] / 255.0, p[0] / 255.0);
      } else {
        bitmap_unlock(bitmap);
        release_bitmap(bitmap);
        UNPROTECT(2);
        Rf_error("Unsupported CZI pixel type in native preview: %d.", info.pixelType);
      }
    }
  }

  bitmap_unlock(bitmap);
  release_bitmap(bitmap);
  UNPROTECT(2);
  return out;
}

struct CziTileHandle {
  DynamicLibrary* lib;
  OpenedCzi czi;
  SingleChannelScalingTileAccessorObjectHandle accessor;
  SingleChannelTileAccessorGetFn accessor_get;
  ReleaseSingleChannelTileAccessorFn release_accessor;
  BitmapGetInfoFn bitmap_info_fn;
  BitmapLockFn bitmap_lock;
  BitmapUnlockFn bitmap_unlock;
  ReleaseBitmapFn release_bitmap;
};

void czi_tile_handle_finalizer(SEXP ext) {
  CziTileHandle* handle = reinterpret_cast<CziTileHandle*>(R_ExternalPtrAddr(ext));
  if (!handle) return;
  if (handle->release_accessor && handle->accessor != 0) {
    handle->release_accessor(handle->accessor);
    handle->accessor = 0;
  }
  if (handle->lib) {
    close_czi(*handle->lib, handle->czi);
    delete handle->lib;
    handle->lib = nullptr;
  }
  delete handle;
  R_ClearExternalPtr(ext);
}

SEXP czi_read_region_from_accessor(SingleChannelScalingTileAccessorObjectHandle accessor,
                                   SingleChannelTileAccessorGetFn accessor_get,
                                   BitmapGetInfoFn bitmap_info_fn,
                                   BitmapLockFn bitmap_lock,
                                   BitmapUnlockFn bitmap_unlock,
                                   ReleaseBitmapFn release_bitmap,
                                   int x,
                                   int y,
                                   int width,
                                   int height,
                                   double zoom_d,
                                   int channel,
                                   int scene) {
  CoordinateInterop coord;
  std::memset(&coord, 0, sizeof(coord));
  int coord_pos = 0;
  if (channel != NA_INTEGER && channel >= 0) {
    coord.dimensions_valid |= (1u << (2 - 1));
    coord.value[coord_pos++] = channel;
  }
  if (scene != NA_INTEGER && scene >= 0) {
    coord.dimensions_valid |= (1u << (5 - 1));
    coord.value[coord_pos++] = scene;
  }

  IntRectInterop roi;
  roi.x = x;
  roi.y = y;
  roi.w = width;
  roi.h = height;
  AccessorOptionsInterop options;
  options.back_ground_color_r = 1.0f;
  options.back_ground_color_g = 1.0f;
  options.back_ground_color_b = 1.0f;
  options.sort_by_m = true;
  options.use_visibility_check_optimization = true;
  options.additional_parameters = nullptr;

  BitmapObjectHandle bitmap = 0;
  int32_t err = accessor_get(accessor, &coord, &roi, static_cast<float>(zoom_d), &options, &bitmap);
  if (err != 0 || bitmap == 0) {
    Rf_error("libCZIAPI could not read the requested CZI region (error %d).", err);
  }
  return bitmap_to_rgb_array(bitmap, bitmap_info_fn, bitmap_lock, bitmap_unlock, release_bitmap);
}

}  // namespace

extern "C" SEXP wsi_native_czi_available() {
  DynamicLibrary lib;
  if (!load_libczi(lib)) return Rf_ScalarLogical(FALSE);
  return Rf_ScalarLogical(has_required_symbols(lib));
}

extern "C" SEXP wsi_native_czi_version() {
  DynamicLibrary lib;
  if (!load_libczi(lib)) return Rf_ScalarString(NA_STRING);
  if (!lib.symbol("libCZI_GetLibCZIVersionInfo")) return Rf_ScalarString(NA_STRING);
  std::string version = version_string(lib);
  if (version.empty()) return Rf_ScalarString(NA_STRING);
  return Rf_mkString(version.c_str());
}

extern "C" SEXP wsi_native_czi_info(SEXP path_) {
  if (!Rf_isString(path_) || Rf_length(path_) != 1 || STRING_ELT(path_, 0) == NA_STRING) {
    Rf_error("`path` must be a single file path.");
  }
  const char* path = CHAR(STRING_ELT(path_, 0));
  DynamicLibrary lib;
  if (!load_libczi(lib) || !has_required_symbols(lib)) {
    Rf_error("Native CZI support requires libCZIAPI on the dynamic library path or `WSITOOLS_LIBCZIAPI`.");
  }

  OpenedCzi czi = open_czi(lib, path);
  ReaderGetStatisticsSimpleFn get_stats = sym<ReaderGetStatisticsSimpleFn>(lib, "libCZI_ReaderGetStatisticsSimple");
  SubBlockStatisticsInterop stats;
  std::memset(&stats, 0, sizeof(stats));
  int32_t err = get_stats(czi.reader, &stats);
  if (err != 0) {
    close_czi(lib, czi);
    Rf_error("libCZIAPI could not read CZI statistics (error %d).", err);
  }

  int32_t attachment_count = NA_INTEGER;
  ReaderGetAttachmentCountFn attachment_fn = sym<ReaderGetAttachmentCountFn>(lib, "libCZI_ReaderGetAttachmentCount", false);
  if (attachment_fn) {
    int32_t count = 0;
    if (attachment_fn(czi.reader, &count) == 0) attachment_count = count;
  }

  char* pyramid_json_ptr = nullptr;
  ReaderGetPyramidStatisticsFn pyramid_fn = sym<ReaderGetPyramidStatisticsFn>(lib, "libCZI_ReaderGetPyramidStatistics", false);
  FreeFn free_fn = sym<FreeFn>(lib, "libCZI_Free", false);
  SEXP pyramid_json = PROTECT(Rf_ScalarString(NA_STRING));
  if (pyramid_fn && free_fn && pyramid_fn(czi.reader, &pyramid_json_ptr) == 0 && pyramid_json_ptr) {
    UNPROTECT(1);
    pyramid_json = PROTECT(Rf_mkString(pyramid_json_ptr));
    free_fn(pyramid_json_ptr);
  }

  SEXP metadata_xml = PROTECT(Rf_ScalarString(NA_STRING));
  ReaderGetMetadataSegmentFn metadata_segment_fn = sym<ReaderGetMetadataSegmentFn>(lib, "libCZI_ReaderGetMetadataSegment", false);
  MetadataSegmentGetMetadataAsXmlFn xml_fn = sym<MetadataSegmentGetMetadataAsXmlFn>(lib, "libCZI_MetadataSegmentGetMetadataAsXml", false);
  ReleaseMetadataSegmentFn release_metadata_fn = sym<ReleaseMetadataSegmentFn>(lib, "libCZI_ReleaseMetadataSegment", false);
  if (metadata_segment_fn && xml_fn && release_metadata_fn && free_fn) {
    MetadataSegmentObjectHandle metadata_handle = 0;
    if (metadata_segment_fn(czi.reader, &metadata_handle) == 0 && metadata_handle != 0) {
      MetadataAsXmlInterop xml;
      std::memset(&xml, 0, sizeof(xml));
      if (xml_fn(metadata_handle, &xml) == 0 && xml.data && xml.size > 0) {
        UNPROTECT(1);
        metadata_xml = PROTECT(Rf_ScalarString(Rf_mkCharLenCE(static_cast<const char*>(xml.data), static_cast<int>(xml.size), CE_UTF8)));
        free_fn(xml.data);
      }
      release_metadata_fn(metadata_handle);
    }
  }

  SEXP scenes = PROTECT(scene_boxes_to_r(lib, czi.reader));

  std::string version = version_string(lib);
  close_czi(lib, czi);

  IntRectInterop box = stats.bounding_box_layer0.w > 0 && stats.bounding_box_layer0.h > 0 ?
    stats.bounding_box_layer0 : stats.bounding_box;

  SEXP dims = PROTECT(dim_bounds_to_r(stats.dim_bounds));
  SEXP out = PROTECT(Rf_allocVector(VECSXP, 16));
  SEXP names = PROTECT(Rf_allocVector(STRSXP, 16));
  const char* name_values[] = {
    "path", "library", "version", "x", "y", "width", "height",
    "sub_block_count", "min_m_index", "max_m_index", "dimensions",
    "pyramid_json", "metadata_xml", "attachment_count", "backend", "scenes"
  };
  for (int i = 0; i < 16; ++i) SET_STRING_ELT(names, i, Rf_mkChar(name_values[i]));
  SET_VECTOR_ELT(out, 0, Rf_mkString(path));
  SET_VECTOR_ELT(out, 1, Rf_mkString(lib.name().c_str()));
  SET_VECTOR_ELT(out, 2, version.empty() ? Rf_ScalarString(NA_STRING) : Rf_mkString(version.c_str()));
  SET_VECTOR_ELT(out, 3, Rf_ScalarInteger(box.x));
  SET_VECTOR_ELT(out, 4, Rf_ScalarInteger(box.y));
  SET_VECTOR_ELT(out, 5, Rf_ScalarInteger(box.w));
  SET_VECTOR_ELT(out, 6, Rf_ScalarInteger(box.h));
  SET_VECTOR_ELT(out, 7, Rf_ScalarInteger(stats.sub_block_count));
  SET_VECTOR_ELT(out, 8, Rf_ScalarInteger(stats.min_m_index));
  SET_VECTOR_ELT(out, 9, Rf_ScalarInteger(stats.max_m_index));
  SET_VECTOR_ELT(out, 10, dims);
  SET_VECTOR_ELT(out, 11, pyramid_json);
  SET_VECTOR_ELT(out, 12, metadata_xml);
  SET_VECTOR_ELT(out, 13, Rf_ScalarInteger(attachment_count));
  SET_VECTOR_ELT(out, 14, Rf_mkString("native_czi"));
  SET_VECTOR_ELT(out, 15, scenes);
  Rf_setAttrib(out, R_NamesSymbol, names);
  UNPROTECT(6);
  return out;
}

extern "C" SEXP wsi_native_czi_read_region(SEXP path_, SEXP x_, SEXP y_, SEXP width_, SEXP height_,
                                           SEXP zoom_, SEXP channel_, SEXP scene_) {
  if (!Rf_isString(path_) || Rf_length(path_) != 1 || STRING_ELT(path_, 0) == NA_STRING) {
    Rf_error("`path` must be a single file path.");
  }
  const char* path = CHAR(STRING_ELT(path_, 0));
  int x = Rf_asInteger(x_);
  int y = Rf_asInteger(y_);
  int width = Rf_asInteger(width_);
  int height = Rf_asInteger(height_);
  double zoom_d = Rf_asReal(zoom_);
  int channel = Rf_asInteger(channel_);
  int scene = Rf_asInteger(scene_);
  if (width <= 0 || height <= 0 || !R_finite(zoom_d) || zoom_d <= 0) {
    Rf_error("Invalid CZI region request.");
  }

  DynamicLibrary lib;
  if (!load_libczi(lib) || !has_required_symbols(lib)) {
    Rf_error("Native CZI support requires libCZIAPI on the dynamic library path or `WSITOOLS_LIBCZIAPI`.");
  }
  OpenedCzi czi = open_czi(lib, path);

  CreateSingleChannelTileAccessorFn create_accessor = sym<CreateSingleChannelTileAccessorFn>(lib, "libCZI_CreateSingleChannelTileAccessor");
  SingleChannelTileAccessorGetFn accessor_get = sym<SingleChannelTileAccessorGetFn>(lib, "libCZI_SingleChannelTileAccessorGet");
  ReleaseSingleChannelTileAccessorFn release_accessor = sym<ReleaseSingleChannelTileAccessorFn>(lib, "libCZI_ReleaseCreateSingleChannelTileAccessor");
  BitmapGetInfoFn bitmap_info_fn = sym<BitmapGetInfoFn>(lib, "libCZI_BitmapGetInfo");
  BitmapLockFn bitmap_lock = sym<BitmapLockFn>(lib, "libCZI_BitmapLock");
  BitmapUnlockFn bitmap_unlock = sym<BitmapUnlockFn>(lib, "libCZI_BitmapUnlock");
  ReleaseBitmapFn release_bitmap = sym<ReleaseBitmapFn>(lib, "libCZI_ReleaseBitmap");

  SingleChannelScalingTileAccessorObjectHandle accessor = 0;
  int32_t err = create_accessor(czi.reader, &accessor);
  if (err != 0 || accessor == 0) {
    close_czi(lib, czi);
    Rf_error("libCZIAPI could not create a tile accessor (error %d).", err);
  }

  SEXP out = R_NilValue;
  out = PROTECT(czi_read_region_from_accessor(
    accessor, accessor_get, bitmap_info_fn, bitmap_lock, bitmap_unlock, release_bitmap,
    x, y, width, height, zoom_d, channel, scene
  ));
  release_accessor(accessor);
  close_czi(lib, czi);
  UNPROTECT(1);
  return out;
}

extern "C" SEXP wsi_native_czi_open_handle(SEXP path_) {
  if (!Rf_isString(path_) || Rf_length(path_) != 1 || STRING_ELT(path_, 0) == NA_STRING) {
    Rf_error("`path` must be a single file path.");
  }
  const char* path = CHAR(STRING_ELT(path_, 0));
  DynamicLibrary* lib = new DynamicLibrary();
  if (!load_libczi(*lib) || !has_required_symbols(*lib)) {
    delete lib;
    Rf_error("Native CZI support requires libCZIAPI on the dynamic library path or `WSITOOLS_LIBCZIAPI`.");
  }
  OpenedCzi czi = open_czi(*lib, path);
  CreateSingleChannelTileAccessorFn create_accessor = sym<CreateSingleChannelTileAccessorFn>(*lib, "libCZI_CreateSingleChannelTileAccessor");
  SingleChannelTileAccessorGetFn accessor_get = sym<SingleChannelTileAccessorGetFn>(*lib, "libCZI_SingleChannelTileAccessorGet");
  ReleaseSingleChannelTileAccessorFn release_accessor = sym<ReleaseSingleChannelTileAccessorFn>(*lib, "libCZI_ReleaseCreateSingleChannelTileAccessor");
  BitmapGetInfoFn bitmap_info_fn = sym<BitmapGetInfoFn>(*lib, "libCZI_BitmapGetInfo");
  BitmapLockFn bitmap_lock = sym<BitmapLockFn>(*lib, "libCZI_BitmapLock");
  BitmapUnlockFn bitmap_unlock = sym<BitmapUnlockFn>(*lib, "libCZI_BitmapUnlock");
  ReleaseBitmapFn release_bitmap = sym<ReleaseBitmapFn>(*lib, "libCZI_ReleaseBitmap");

  SingleChannelScalingTileAccessorObjectHandle accessor = 0;
  int32_t err = create_accessor(czi.reader, &accessor);
  if (err != 0 || accessor == 0) {
    close_czi(*lib, czi);
    delete lib;
    Rf_error("libCZIAPI could not create a persistent tile accessor (error %d).", err);
  }

  CziTileHandle* handle = new CziTileHandle();
  handle->lib = lib;
  handle->czi = czi;
  handle->accessor = accessor;
  handle->accessor_get = accessor_get;
  handle->release_accessor = release_accessor;
  handle->bitmap_info_fn = bitmap_info_fn;
  handle->bitmap_lock = bitmap_lock;
  handle->bitmap_unlock = bitmap_unlock;
  handle->release_bitmap = release_bitmap;

  SEXP ext = PROTECT(R_MakeExternalPtr(handle, Rf_install("wsi_native_czi_handle"), R_NilValue));
  R_RegisterCFinalizerEx(ext, czi_tile_handle_finalizer, TRUE);
  SEXP cls = PROTECT(Rf_mkString("wsi_native_czi_handle"));
  Rf_setAttrib(ext, R_ClassSymbol, cls);
  UNPROTECT(2);
  return ext;
}

extern "C" SEXP wsi_native_czi_close_handle(SEXP handle_) {
  czi_tile_handle_finalizer(handle_);
  return Rf_ScalarLogical(TRUE);
}

extern "C" SEXP wsi_native_czi_handle_read_region(SEXP handle_, SEXP x_, SEXP y_, SEXP width_,
                                                  SEXP height_, SEXP zoom_, SEXP channel_, SEXP scene_) {
  CziTileHandle* handle = reinterpret_cast<CziTileHandle*>(R_ExternalPtrAddr(handle_));
  if (!handle || handle->accessor == 0) {
    Rf_error("The native CZI handle is closed or invalid.");
  }
  int x = Rf_asInteger(x_);
  int y = Rf_asInteger(y_);
  int width = Rf_asInteger(width_);
  int height = Rf_asInteger(height_);
  double zoom_d = Rf_asReal(zoom_);
  int channel = Rf_asInteger(channel_);
  int scene = Rf_asInteger(scene_);
  if (width <= 0 || height <= 0 || !R_finite(zoom_d) || zoom_d <= 0) {
    Rf_error("Invalid CZI region request.");
  }
  SEXP out = czi_read_region_from_accessor(
    handle->accessor, handle->accessor_get, handle->bitmap_info_fn,
    handle->bitmap_lock, handle->bitmap_unlock, handle->release_bitmap,
    x, y, width, height, zoom_d, channel, scene
  );
  return out;
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
  double x0 = pts[0];
  double y0 = pts[rows];
  double xl = pts[last];
  double yl = pts[last + rows];
  if (x0 == xl && y0 == yl && n > 3) {
    n -= 1;
  }
  bool inside = false;
  int j = n - 1;
  for (int i = 0; i < n; ++i) {
    double xi = pts[i];
    double yi = pts[i + rows];
    double xj = pts[j];
    double yj = pts[j + rows];
    bool crosses = ((yi > y) != (yj > y));
    if (crosses) {
      double denom = yj - yi;
      if (denom == 0.0) {
        denom = 2.220446049250313e-16;
      }
      double x_intersect = (xj - xi) * (y - yi) / denom + xi;
      if (x < x_intersect) {
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
  if (!Rf_isReal(x_) || !Rf_isReal(y_) || Rf_xlength(x_) != Rf_xlength(y_)) {
    Rf_error("`x` and `y` must be numeric vectors with the same length.");
  }
  if (!Rf_isVectorList(polygons_)) {
    Rf_error("`polygons` must be a list.");
  }
  R_xlen_t n_points = Rf_xlength(x_);
  R_xlen_t n_rois = Rf_xlength(polygons_);
  if (n_rois > INT_MAX) {
    Rf_error("Too many annotations for native assignment.");
  }
  const double* x = REAL(x_);
  const double* y = REAL(y_);
  const double* bbox = nullptr;
  int bbox_rows = 0;
  if (!Rf_isNull(bbox_)) {
    if (!Rf_isMatrix(bbox_) || !Rf_isReal(bbox_)) {
      Rf_error("`bbox` must be a numeric matrix.");
    }
    SEXP dim = Rf_getAttrib(bbox_, R_DimSymbol);
    bbox_rows = INTEGER(dim)[0];
    int bbox_cols = INTEGER(dim)[1];
    if (bbox_rows < n_rois || bbox_cols < 4) {
      Rf_error("`bbox` must have one row per annotation and at least four columns.");
    }
    bbox = REAL(bbox_);
  }
  SEXP out = PROTECT(Rf_allocVector(INTSXP, n_points));
  int* assigned = INTEGER(out);
  for (R_xlen_t p = 0; p < n_points; ++p) {
    assigned[p] = NA_INTEGER;
    if (!R_finite(x[p]) || !R_finite(y[p])) {
      continue;
    }
    for (R_xlen_t r = 0; r < n_rois; ++r) {
      if (bbox != nullptr) {
        double xmin = bbox[r];
        double ymin = bbox[r + bbox_rows];
        double xmax = bbox[r + 2 * bbox_rows];
        double ymax = bbox[r + 3 * bbox_rows];
        if (R_finite(xmin) && R_finite(ymin) && R_finite(xmax) && R_finite(ymax) &&
            (x[p] < xmin || x[p] > xmax || y[p] < ymin || y[p] > ymax)) {
          continue;
        }
      }
      SEXP roi = VECTOR_ELT(polygons_, r);
      bool inside = false;
      if (Rf_isVectorList(roi)) {
        for (R_xlen_t poly = 0; poly < Rf_xlength(roi); ++poly) {
          if (wsi_point_in_polygon_cpp(x[p], y[p], VECTOR_ELT(roi, poly))) {
            inside = true;
            break;
          }
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

extern "C" SEXP wsi_bbox_index_build_cpp(SEXP bbox_);
extern "C" SEXP wsi_bbox_index_query_cpp(SEXP pointer_, SEXP xmin_, SEXP ymin_,
                                           SEXP xmax_, SEXP ymax_);

static const R_CallMethodDef CallEntries[] = {
  {"wsi_native_czi_available", reinterpret_cast<DL_FUNC>(&wsi_native_czi_available), 0},
  {"wsi_native_czi_version", reinterpret_cast<DL_FUNC>(&wsi_native_czi_version), 0},
  {"wsi_native_czi_info", reinterpret_cast<DL_FUNC>(&wsi_native_czi_info), 1},
  {"wsi_native_czi_read_region", reinterpret_cast<DL_FUNC>(&wsi_native_czi_read_region), 8},
  {"wsi_native_czi_open_handle", reinterpret_cast<DL_FUNC>(&wsi_native_czi_open_handle), 1},
  {"wsi_native_czi_close_handle", reinterpret_cast<DL_FUNC>(&wsi_native_czi_close_handle), 1},
  {"wsi_native_czi_handle_read_region", reinterpret_cast<DL_FUNC>(&wsi_native_czi_handle_read_region), 8},
  {"wsi_assign_points_to_polygons_cpp", reinterpret_cast<DL_FUNC>(&wsi_assign_points_to_polygons_cpp), 4},
  {"wsi_bbox_index_build_cpp", reinterpret_cast<DL_FUNC>(&wsi_bbox_index_build_cpp), 1},
  {"wsi_bbox_index_query_cpp", reinterpret_cast<DL_FUNC>(&wsi_bbox_index_query_cpp), 5},
  {NULL, NULL, 0}
};

extern "C" void R_init_wsiTools(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, static_cast<Rboolean>(FALSE));
}
