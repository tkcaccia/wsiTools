#define R_NO_REMAP
#include <Rinternals.h>

#include <cstdio>
#include <string>

#ifdef _WIN32
// Declare only the required Win32 calls: windows.h conflicts with R's memory macros.
extern "C" {
__declspec(dllimport) int __stdcall MultiByteToWideChar(unsigned int, unsigned long,
                                                        const char*, int, wchar_t*, int);
__declspec(dllimport) int __stdcall MoveFileExW(const wchar_t*, const wchar_t*, unsigned long);
}

static std::wstring wsi_wide_path(SEXP value) {
  const char* utf8 = Rf_translateCharUTF8(STRING_ELT(value, 0));
  const int count = MultiByteToWideChar(65001, 0, utf8, -1, nullptr, 0);
  if (count <= 0) return std::wstring();
  std::wstring result(static_cast<size_t>(count), L'\0');
  if (!MultiByteToWideChar(65001, 0, utf8, -1, &result[0], count)) return std::wstring();
  return result;
}
#endif

extern "C" SEXP wsi_atomic_replace_file(SEXP staged, SEXP output) {
  if (TYPEOF(staged) != STRSXP || Rf_length(staged) != 1 ||
      TYPEOF(output) != STRSXP || Rf_length(output) != 1 ||
      STRING_ELT(staged, 0) == NA_STRING || STRING_ELT(output, 0) == NA_STRING) {
    Rf_error("Atomic replacement requires two file paths.");
  }
#ifdef _WIN32
  const std::wstring from = wsi_wide_path(staged);
  const std::wstring to = wsi_wide_path(output);
  if (from.empty() || to.empty()) return Rf_ScalarLogical(FALSE);
  // Staging in the destination directory keeps this an in-volume replacement.
  return Rf_ScalarLogical(MoveFileExW(from.c_str(), to.c_str(), 0x1 | 0x8) != 0);
#else
  const std::string from(Rf_translateCharUTF8(STRING_ELT(staged, 0)));
  const std::string to(Rf_translateCharUTF8(STRING_ELT(output, 0)));
  return Rf_ScalarLogical(std::rename(from.c_str(), to.c_str()) == 0);
#endif
}
