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

char* stdlib_strerror(size_t* len){
    char* err = strerror(errno);
    *len = strlen(err);
    return err;
}

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

int stdlib_remove_directory(const char* path){
    int code;
#ifdef _WIN32
    code = _rmdir(path);
#else
    code = rmdir(path);
#endif /* ifdef _WIN32 */

    return (!code) ? 0 : errno;
}
