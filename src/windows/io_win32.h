#ifndef Io_win32_H
#define Io_win32_H

#if defined(_WIN32)

#include <functional>
#include <string>

// Compilers on Windows other than MSVC (e.g. Cygwin, MinGW32) define the
// following functions already, except for mkdir.
namespace io {
namespace win32 {

FILE* fopen(const char* path, const char* mode);
int access(const char* path, int mode);
int chdir(const char* path);
int close(int fd);
int dup(int fd);
int dup2(int fd1, int fd2);
int mkdir(const char* path, int _mode);
int open(const char* path, int flags, int mode = 0);
int read(int fd, void* buffer, size_t size);
int setmode(int fd, int mode);
int stat(const char* path, struct _stat* buffer);
int write(int fd, const void* buffer, size_t size);
std::wstring testonly_utf8_to_winpath(const char* path);

enum class ExpandWildcardsResult {
  kSuccess = 0,
  kErrorNoMatchingFile = 1,
  kErrorInputPathConversion = 2,
  kErrorOutputPathConversion = 3,
};

// Expand wildcards in a path pattern, feed the result to a consumer function.
//
// `path` must be a valid, Windows-style path. It may be absolute, or relative
// to the current working directory, and it may contain wildcards ("*" and "?")
// in the last path segment. This function passes all matching file names to
// `consume`. The resulting paths may not be absolute nor normalized.
//
// The function returns a value from `ExpandWildcardsResult`.
ExpandWildcardsResult ExpandWildcards(
    const std::string& path, std::function<void(const std::string&)> consume);

namespace strings {

// Convert from UTF-16 to Active-Code-Page-encoded or to UTF-8-encoded text.
bool wcs_to_mbs(const wchar_t* s, std::string* out,
                                bool outUtf8);

// Convert from Active-Code-Page-encoded or UTF-8-encoded text to UTF-16.
bool mbs_to_wcs(const char* s, std::wstring* out, bool inUtf8);

// Convert from UTF-8-encoded text to UTF-16.
bool utf8_to_wcs(const char* input, std::wstring* out);

// Convert from UTF-16-encoded text to UTF-8.
bool wcs_to_utf8(const wchar_t* input, std::string* out);

}  // namespace strings

}  // namespace win32
}  // namespace io


#ifndef W_OK
#define W_OK 02  // not defined by MSVC for whatever reason
#endif

#ifndef F_OK
#define F_OK 00  // not defined by MSVC for whatever reason
#endif

#ifndef STDIN_FILENO
#define STDIN_FILENO 0
#endif

#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

#endif  // defined(_WIN32)

#endif  // defined(Io_win32_H)
