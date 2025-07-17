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

char* stdlib_strerror(size_t* len){
    char* err = strerror(errno);
    *len = strlen(err);
    return err;
}

int stdlib_make_directory(const char* path, mode_t mode){
    int code;
#ifdef _WIN32
    code = _mkdir(path);
#else
    code = mkdir(path, mode);
#endif /* ifdef _WIN32 */
    
    if (!code){
        return 0;
    }

    return errno;
}

int stdlib_remove_directory(const char* path){
    int code;
#ifdef _WIN32
    code = _rmdir(path);
#else
    code = rmdir(path);
#endif /* ifdef _WIN32 */

    if (!code){
        return 0;
    }

    return errno;
}

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

int stdlib_set_cwd(char* path) {
    int code;
#ifdef _WIN32
    code = _chdir(path);
#else
    code = chdir(path);
#endif /* ifdef _WIN32 */
    
    if (code == -1) return errno;
    return 0;
}
