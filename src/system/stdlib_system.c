/*
 * Feature-test macros must be defined before *any* system header is included:
 * once the C library's own configuration header has been pulled in, it has
 * already decided which symbols to expose and a later definition has no effect
 * (and is reported as a redefinition). Without them, a strict-conformance build
 * (`-std=c99`, or the Intel compilers in strict mode) hides POSIX declarations
 * such as `lstat`, while the GNU default dialect happens to expose them.
 *
 * Each macro is guarded so that a build system which already supplies its own
 * conformance level wins instead of clashing with the values chosen here.
 */
#if !defined(_WIN32)
#  if !defined(_POSIX_C_SOURCE)
#    define _POSIX_C_SOURCE 200809L /* POSIX.1-2008: lstat, getcwd, nanosleep, ... */
#  endif
#  if !defined(_XOPEN_SOURCE)
#    define _XOPEN_SOURCE 700 /* XSI extensions of the same revision */
#  endif
#  if !defined(_DEFAULT_SOURCE)
#    define _DEFAULT_SOURCE 1 /* glibc: keep the BSD/misc declarations visible */
#  endif
#  if defined(__APPLE__) && !defined(_DARWIN_C_SOURCE)
#    define _DARWIN_C_SOURCE 1 /* Darwin: ditto, restricted by _POSIX_C_SOURCE */
#  endif
#endif /* !defined(_WIN32) */

#include <stdbool.h>
#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>
#ifdef _WIN32
#include <direct.h>
#include <windows.h>
#ifndef S_ISREG
#if defined(S_IFMT) && defined(S_IFREG)
#define S_ISREG(mode) (((mode) & S_IFMT) == S_IFREG)
#elif defined(_S_IFMT) && defined(_S_IFREG)
#define S_ISREG(mode) (((mode) & _S_IFMT) == _S_IFREG)
#endif
#endif /* ifndef S_ISREG */
#else
#include <unistd.h>
#endif /* ifdef _WIN32 */

// Wrapper to get the string describing a system syscall error.
// Always Uses `strerr` on unix.
// if `winapi` is `false`, uses the usual `strerr` on windows.
// If `winapi` is `true`, uses `FormatMessageA`(from windows.h) on windows.
char* stdlib_strerror(size_t* len, bool winapi){

    if (winapi) {
#ifdef _WIN32
    LPSTR err = NULL;
    DWORD dw = GetLastError();

    FormatMessageA(
    FORMAT_MESSAGE_ALLOCATE_BUFFER |
    FORMAT_MESSAGE_FROM_SYSTEM |
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    dw,
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
    (LPSTR) &err,
    0,
    NULL);

    *len = strlen(err);
    return (char*) err;

#endif /* ifdef _WIN32 */
    }

    char* err = strerror(errno);
    *len = strlen(err);
    return err;
}

// Wrapper to the platform's `mkdir`(make directory) call.
// Uses `mkdir` on unix, `_mkdir` on windows.
// Returns 0 if successful, otherwise returns the `errno`.
int stdlib_make_directory(const char* path){
    int code;
#ifdef _WIN32
    code = _mkdir(path);
#else
    // Default mode 0777
    code = mkdir(path, 0777);
#endif /* ifdef _WIN32 */
    
    return (!code) ? 0 : errno;
}

// Wrapper to the platform's `rmdir`(remove directory) call.
// Uses `rmdir` on unix, `_rmdir` on windows.
// Returns 0 if successful, otherwise returns the `errno`.
int stdlib_remove_directory(const char* path){
    int code;
#ifdef _WIN32
    code = _rmdir(path);
#else
    code = rmdir(path);
#endif /* ifdef _WIN32 */

    return (!code) ? 0 : errno;
}
// Wrapper to the platform's `getcwd`(get current working directory) call.
// Uses `getcwd` on unix, `_getcwd` on windows.
// Returns the cwd, sets the length of cwd and the `stat` of the operation.
char* stdlib_get_cwd(size_t* len, int* stat){
    // Always leave the outputs in a defined state: callers read `len` even when
    // the call fails, so it must never be left uninitialized.
    *len = 0;
    *stat = 0;
#ifdef _WIN32
    char* buffer;
    buffer = _getcwd(NULL, 0);

    if (buffer == NULL) {
        *stat = errno;
        return NULL;
    }

    *len = strlen(buffer);
    return buffer;
#else
    // `PATH_MAX` is optional in POSIX: it is left undefined whenever the limit is
    // indeterminate (GNU/Hurd) and, where it is defined, it is not necessarily an
    // upper bound for the working directory. Grow the buffer until `getcwd` fits.
    size_t size = 256;
    char* buffer = NULL;

    for (;;) {
        char* grown = realloc(buffer, size);

        if (grown == NULL) {
            free(buffer);
            *stat = ENOMEM;  // Memory allocation failure
            return NULL;
        }
        buffer = grown;

        if (getcwd(buffer, size) != NULL) break;

        if (errno != ERANGE || size > ((size_t) -1) / 2) {
            // Either a hard failure, or a path that cannot be represented
            *stat = (errno == ERANGE) ? ENAMETOOLONG : errno;
            free(buffer);
            return NULL;
        }
        size *= 2;
    }

    *len = strlen(buffer);
    return buffer;
#endif /* ifdef _WIN32 */
}

// Releases a buffer returned by `stdlib_get_cwd`.
// The release happens on the C side on purpose: it keeps the allocation and the
// matching `free` inside the same C runtime, which matters on Windows where the
// Fortran and C objects may well be linked against different CRTs.
// Passing NULL is safe, so callers need not check the failure path.
void stdlib_free_cstr(char* ptr){
    free(ptr);
}

// Wrapper to the platform's `chdir`(change directory) call.
// Uses `chdir` on unix, `_chdir` on windows.
// Returns 0 if successful, otherwise returns the `errno`.
int stdlib_set_cwd(const char* path) {
    int code;
#ifdef _WIN32
    code = _chdir(path);
#else
    code = chdir(path);
#endif /* ifdef _WIN32 */
    
    return (code == -1) ? errno : 0;
}

// Wrapper to the platform's `stat`(status of path) call.
// Uses `lstat` on unix, `GetFileAttributesA` on windows.
// Returns the `type` of the path, and sets the `stat`(if any errors).
int stdlib_exists(const char* path, int* stat){
    // All the valid types
    const int fs_type_unknown = 0;
    const int fs_type_regular_file = 1;
    const int fs_type_directory = 2;
    const int fs_type_symlink = 3;

    int type = fs_type_unknown;
    *stat = 0;

#ifdef _WIN32
    DWORD attrs = GetFileAttributesA(path);

    if (attrs == INVALID_FILE_ATTRIBUTES) {
        *stat = (int) GetLastError();
        return fs_type_unknown;
    }

    // Let's assume it is a regular file
    type = fs_type_regular_file;

    if (attrs & FILE_ATTRIBUTE_REPARSE_POINT) type = fs_type_symlink;
    if (attrs & FILE_ATTRIBUTE_DIRECTORY) type = fs_type_directory;
#else
    struct stat buf = {0};
    int status;
    status = lstat(path, &buf);

    if (status == -1) {
        // `lstat` failed
        *stat = errno;
        return fs_type_unknown;
    }

    // Use the `S_IS*` predicates rather than masking with `S_IFMT`: the former are
    // required by POSIX and always visible, whereas `S_IFMT` and friends are
    // XSI-only and stay hidden in a strict-conformance build.
    if      (S_ISREG(buf.st_mode)) type = fs_type_regular_file;
    else if (S_ISDIR(buf.st_mode)) type = fs_type_directory;
    else if (S_ISLNK(buf.st_mode)) type = fs_type_symlink;
    else                           type = fs_type_unknown;
#endif /* ifdef _WIN32 */
    return type;
}

// `stat` and `_stat` follow symlinks automatically.
// so no need for winapi functions.
bool stdlib_is_file(const char* path) {
#ifdef _WIN32
    struct _stat buf = {0};
    return _stat(path, &buf) == 0 && S_ISREG(buf.st_mode);
#else
    struct stat buf = {0};
    return stat(path, &buf) == 0 && S_ISREG(buf.st_mode);
#endif /* ifdef _WIN32 */
}
