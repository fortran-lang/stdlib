---
title: load_library
---

# Run-time loading of shared libraries

[TOC]

The `stdlib_load_library` module loads shared libraries (`.so`, `.dll`, `.dylib`) at
run time and resolves the symbols they export, in the spirit of
`numpy.ctypeslib.load_library`. It is the building block for plugin architectures,
for optional back-ends that must not be a link-time dependency, and for calling into
libraries whose location is only known when the program runs.

Two layers are involved: the platform *naming* conventions (`lib` prefix,
`.so`/`.dll`/`.dylib` suffix) are resolved in Fortran through
[[stdlib_system(module)]], while the OS calls themselves (`dlopen` and `dlsym` on
POSIX systems, `LoadLibraryExW` and `GetProcAddress` on Windows) live in the
companion C file `stdlib_load_library_c.c`.

Error handling follows the usual `stdlib` convention: all procedures that can fail
take an optional `err` argument of [[stdlib_error(module):state_type(type)]]. When it
is present the error is returned through it, otherwise the program stops with an
error message.

@note Symbols exported by the loaded library should be declared `bind(C, name="...")`
so that their names do not depend on the compiler that built it.

@warning Intrinsic assignment of a `shared_library_type` copies the OS handle, so
closing one copy invalidates the procedure pointers obtained from any copy. The type
has no finalizer on purpose: unloading is always explicit.

## `shared_library_type` - Handle on a loaded shared library

### Status

Experimental

### Description

A derived type holding the OS handle of a loaded shared library and the path of the
file that was actually loaded. A default-initialized variable carries no library.

### Type-bound procedures

`open(name [, path] [, global] [, err])`: loads a library into the handle, which must
not already carry one. Same arguments as `load_library`.

`symbol(name [, err])`: returns the address of the exported procedure `name`, as a
`type(c_funptr)` to be passed to `c_f_procpointer`. Returns `c_null_funptr` on error.

`data(name [, err])`: returns the address of the exported variable `name`, as a
`type(c_ptr)` to be passed to `c_f_pointer`. Returns `c_null_ptr` on error.

`has_symbol(name)`: returns a `logical` flag, `.true.` if a library is loaded and
exports `name`.

`close([err])`: unloads the library. Any procedure pointer obtained from it becomes
invalid. Closing a handle that carries no library is a no-op.

`is_loaded()`: returns a `logical` flag, `.true.` if the handle carries a library.

`filename()`: returns the `character(:), allocatable` path of the file that was
actually loaded, or an empty string if the handle carries no library.

## `load_library` - Load a shared library

### Status

Experimental

### Description

Loads a shared library and returns a handle on it.

`name` may be a bare name (`"kernels"`), a file name (`"libkernels.so.2"`) or a path
(`"./plugins/kernels.dll"`). A name that carries no extension is decorated with the
platform prefix and suffix, and several candidates are tried in turn, so that the same
call finds `libkernels.so` on Linux, `libkernels.dylib` on macOS and `kernels.dll` on
Windows.

### Syntax

`lib = ` [[stdlib_load_library(module):load_library(function)]] `(name [, path] [, global] [, err])`

### Arguments

`name`: Shall be a `character(*)` library name, file name or path. This is an `intent(in)` argument.

`path` (optional): Shall be a `character(*)` directory. If present and non-empty, the library is searched in that directory only, instead of following the OS search rules (`LD_LIBRARY_PATH`, rpath, `PATH`, ...). It is ignored when `name` already carries a directory component. This is an `intent(in)` argument.

`global` (optional): Shall be a `logical` flag. If `.true.`, the symbols of the library are made available to libraries loaded afterwards (`RTLD_GLOBAL`). The default is `.false.` (`RTLD_LOCAL`). Windows has no equivalent, and ignores it. This is an `intent(in)` argument.

`err` (optional): Shall be a `type(state_type)` value. This is an `intent(out)` argument.

### Return value

Returns a `type(shared_library_type)` handle on the loaded library. On error, the handle carries no library and `err` (if present) holds a `STDLIB_FS_ERROR` state.

## `library_filename` - Platform file name of a library

### Status

Experimental

### Description

Returns the file name a library would have on the current platform, e.g. `fft` becomes
`libfft.so` on Linux, `libfft.dylib` on macOS and `fft.dll` on Windows. A name that
already starts with `lib` is not decorated twice.

### Syntax

`fname = ` [[stdlib_load_library(module):library_filename(function)]] `(name)`

### Arguments

`name`: Shall be a `character(*)` undecorated library name. Trailing blanks are ignored. This is an `intent(in)` argument.

### Return value

Returns a `character(:), allocatable` file name.

## `shared_library_suffix` - Platform shared library extension

### Status

Experimental

### Description

Returns the shared library file extension of the current platform, dot included:
`".so"`, `".dll"` or `".dylib"`.

### Syntax

`suffix = ` [[stdlib_load_library(module):shared_library_suffix(function)]] `()`

### Return value

Returns a `character(:), allocatable` extension.

## Example

```fortran
{!example/system/example_load_library.f90!}
```
