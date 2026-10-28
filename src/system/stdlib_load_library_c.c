/*
 * OS layer of stdlib_load_library: dlopen & co. on POSIX systems,
 * LoadLibraryExW & co. on Windows.
 *
 * The Fortran module only sees the four functions below, with the same
 * signature on every platform. All platform detection happens here, with
 * the usual C predefined macros, so the Fortran source needs no
 * preprocessing. Platform naming conventions (lib prefix, .so/.dll/.dylib
 * suffix) are not handled here: they are resolved in Fortran through
 * stdlib_system.
 *
 * Error messages are written, NUL-terminated and UTF-8 encoded, to the
 * caller's buffer `err` of `errlen` bytes. They are truncated if needed.
 */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(_WIN32)
#  ifndef WIN32_LEAN_AND_MEAN
#    define WIN32_LEAN_AND_MEAN
#  endif
#  include <windows.h>
#else
#  include <dlfcn.h>
#endif

typedef void (*loadlib_funptr)(void);

/* Data and function addresses are converted to each other with memcpy,
   which requires them to have the same size (true on every supported
   platform, and required by POSIX for dlsym). */
typedef char loadlib_check_pointer_sizes[
    sizeof(void *) == sizeof(loadlib_funptr) ? 1 : -1];

static void set_error(char *err, size_t errlen, const char *msg)
{
    size_t i;
    if (err == NULL || errlen == 0) return;
    for (i = 0; i + 1 < errlen && msg[i] != '\0'; i++) err[i] = msg[i];
    err[i] = '\0';
}

#if defined(_WIN32)

/* "error <code>: <system message in the user's language>", in UTF-8 */
static void set_win_error(DWORD code, char *err, size_t errlen)
{
    wchar_t wmsg[1024];
    DWORD n;
    int prefix, m;

    if (err == NULL || errlen == 0) return;
    prefix = snprintf(err, errlen, "error %lu: ", (unsigned long)code);
    if (prefix < 0 || (size_t)prefix >= errlen) return;

    n = FormatMessageW(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS,
                       NULL, code, 0, wmsg, (DWORD)(sizeof wmsg / sizeof wmsg[0]), NULL);
    while (n > 0 && (wmsg[n - 1] == L'\r' || wmsg[n - 1] == L'\n' || wmsg[n - 1] == L' '))
        n--;
    m = n == 0 ? 0 : WideCharToMultiByte(CP_UTF8, 0, wmsg, (int)n, err + prefix,
                                         (int)(errlen - (size_t)prefix - 1), NULL, NULL);
    err[prefix + (m > 0 ? m : 0)] = '\0';
}

/* UTF-8, or failing that the ANSI code page, to a malloc'ed UTF-16 string */
static wchar_t *to_wide(const char *s)
{
    UINT cp = CP_UTF8;
    DWORD flags = MB_ERR_INVALID_CHARS;
    wchar_t *w;
    int n = MultiByteToWideChar(cp, flags, s, -1, NULL, 0);

    if (n <= 0) {
        cp = CP_ACP;
        flags = 0;
        n = MultiByteToWideChar(cp, flags, s, -1, NULL, 0);
        if (n <= 0) return NULL;
    }
    w = (wchar_t *)malloc((size_t)n * sizeof *w);
    if (w != NULL && MultiByteToWideChar(cp, flags, s, -1, w, n) <= 0) {
        free(w);
        w = NULL;
    }
    return w;
}

/* Absolute form of w, malloc'ed; w itself (not a copy) if that fails */
static wchar_t *full_path(wchar_t *w)
{
    DWORD n = GetFullPathNameW(w, 0, NULL, NULL), m;
    wchar_t *f;

    if (n == 0) return w;
    f = (wchar_t *)malloc((size_t)n * sizeof *f);
    if (f == NULL) return w;
    m = GetFullPathNameW(w, n, f, NULL);
    if (m == 0 || m >= n) {
        free(f);
        return w;
    }
    free(w);
    return f;
}

void *stdlib_loadlib_open(const char *filename, int global, char *err, size_t errlen)
{
    wchar_t *w;
    DWORD flags = 0, code = 0, old_mode;
    HMODULE h;

    (void)global;                   /* no RTLD_GLOBAL equivalent on Windows */

    w = to_wide(filename);
    if (w == NULL) {
        set_error(err, errlen, "file name is not valid UTF-8 or ANSI text");
        return NULL;
    }
    if (wcschr(w, L'/') != NULL || wcschr(w, L'\\') != NULL) {
        /* Explicit location: resolve the DLL's own dependencies from its
           directory first, then with the standard order, PATH included.
           (LOAD_LIBRARY_SEARCH_* flags would exclude PATH, so a plugin
           could not find a compiler runtime that the host does not already
           have loaded, e.g. libifcoremd.dll from a gfortran host.)
           LOAD_WITH_ALTERED_SEARCH_PATH needs an absolute path. */
        w = full_path(w);
        flags = LOAD_WITH_ALTERED_SEARCH_PATH;
    }

    /* No "bad image" / "missing drive" dialog boxes: report errors instead */
    if (!SetThreadErrorMode(SEM_FAILCRITICALERRORS | SEM_NOOPENFILEERRORBOX, &old_mode))
        old_mode = (DWORD)-1;
    h = LoadLibraryExW(w, NULL, flags);
    if (h == NULL) code = GetLastError();
    if (old_mode != (DWORD)-1) SetThreadErrorMode(old_mode, NULL);

    free(w);
    if (h == NULL) set_win_error(code, err, errlen);
    return (void *)h;
}

static void *lookup(void *handle, const char *name, char *err, size_t errlen)
{
    FARPROC f = GetProcAddress((HMODULE)handle, name);
    void *p;

    if (f == NULL) {
        set_win_error(GetLastError(), err, errlen);
        return NULL;
    }
    memcpy(&p, &f, sizeof p);
    return p;
}

int stdlib_loadlib_close(void *handle, char *err, size_t errlen)
{
    if (FreeLibrary((HMODULE)handle)) return 0;
    set_win_error(GetLastError(), err, errlen);
    return -1;
}

#else /* POSIX */

static void set_dl_error(const char *fallback, char *err, size_t errlen)
{
    const char *msg = dlerror();
    set_error(err, errlen, msg != NULL ? msg : fallback);
}

void *stdlib_loadlib_open(const char *filename, int global, char *err, size_t errlen)
{
    void *h = dlopen(filename, RTLD_NOW | (global ? RTLD_GLOBAL : RTLD_LOCAL));
    if (h == NULL) set_dl_error("unknown dlopen error", err, errlen);
    return h;
}

static void *lookup(void *handle, const char *name, char *err, size_t errlen)
{
    void *p;

    (void)dlerror();                /* clear any pending error */
    p = dlsym(handle, name);
    if (p == NULL) set_dl_error("the symbol resolves to a null address", err, errlen);
    return p;
}

int stdlib_loadlib_close(void *handle, char *err, size_t errlen)
{
    if (dlclose(handle) == 0) return 0;
    set_dl_error("unknown dlclose error", err, errlen);
    return -1;
}

#endif

/* Address of an exported variable */
void *stdlib_loadlib_data(void *handle, const char *name, char *err, size_t errlen)
{
    return lookup(handle, name, err, errlen);
}

/* Address of an exported procedure */
loadlib_funptr stdlib_loadlib_symbol(void *handle, const char *name, char *err, size_t errlen)
{
    void *p = lookup(handle, name, err, errlen);
    loadlib_funptr f;

    memcpy(&f, &p, sizeof f);
    return f;
}
