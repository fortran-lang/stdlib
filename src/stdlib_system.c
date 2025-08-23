#include <limits.h>
#include <stddef.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <string.h>
#include <errno.h>
#ifdef _WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif /* ifdef _WIN32 */

// Returns the string describing the meaning of `errno` code (by calling `strerror`).
char* stdlib_strerror(size_t* len){
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

    char* res = malloc(*len);
    strncpy(res, buffer, *len);

    return res;
#endif /* ifdef _WIN32 */
}

// Wrapper to the platform's `chdir`(change directory) call.
// Uses `chdir` on unix, `_chdir` on windows.
// Returns 0 if successful, otherwise returns the `errno`.
int stdlib_set_cwd(char* path) {
    int code;
#ifdef _WIN32
    code = _chdir(path);
#else
    code = chdir(path);
#endif /* ifdef _WIN32 */
    
    return (code == -1) ? errno : 0;
}
