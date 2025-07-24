#include <stddef.h>
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
