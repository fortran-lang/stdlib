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
    char buffer[PATH_MAX + 1];
    if (!getcwd(buffer, sizeof(buffer))) {
        *stat = errno;
    }

    *len = strlen(buffer);

    char* res = malloc(*len + 1);  // Allocate space for null terminator
    if (res == NULL) {
        *stat = ENOMEM;  // Set error code for memory allocation failure
        return NULL;
    }
    strncpy(res, buffer, *len);
    res[*len] = '\0';  // Ensure null termination

    return res;
#endif /* ifdef _WIN32 */
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

    switch (buf.st_mode & S_IFMT) {
        case S_IFREG: type = fs_type_regular_file; break;
        case S_IFDIR: type = fs_type_directory;    break;
        case S_IFLNK: type = fs_type_symlink;      break;
        default:      type = fs_type_unknown;      break;
    }
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
