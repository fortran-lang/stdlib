!> Version: experimental
!>
!> Cross-platform run-time loading of shared libraries (`.so` / `.dll` / `.dylib`),
!> in the spirit of `numpy.ctypeslib.load_library`.
!> ([Specification](../page/specs/stdlib_load_library.html))
!>
!> This file is standard Fortran 2008 and needs no preprocessing: the OS-specific
!> calls (`dlopen` / `LoadLibraryExW`, ...) live in `stdlib_load_library_c.c`,
!> which must be compiled and linked with it. Platform *naming* conventions, on
!> the other hand, are resolved in Fortran through `stdlib_system`.
!>
!> ## Example
!>```fortran
!>   type(shared_library_type) :: lib
!>   procedure(my_iface), pointer :: f
!>
!>   lib = load_library("kernels", path="./plugins")   ! libkernels.so / kernels.dll / ...
!>   call c_f_procpointer(lib%symbol("my_func"), f)
!>   call f(...)
!>   call lib%close()
!>```
!>
!> Error handling follows the `stdlib` convention: when `err` is present the error
!> is returned through it, otherwise the program stops with an error message.
!>
!> Caveats:
!>  * Copying a `shared_library_type` (intrinsic assignment) copies the OS handle.
!>    Closing one copy invalidates procedure pointers obtained from any copy.
!>    There is deliberately no finalizer: unloading must be explicit.
!>  * Exported symbols should use `bind(C, name="...")` so their names do not
!>    depend on the compiler that built the library.
!>  * `global` (`RTLD_GLOBAL`) has no meaning on Windows and is ignored there.
module stdlib_load_library
    use, intrinsic :: iso_c_binding, only: c_ptr, c_funptr, c_int, c_size_t, &
        c_null_ptr, c_null_funptr, c_null_char, c_associated
    use stdlib_kinds, only: c_char
    use stdlib_error, only: state_type, STDLIB_VALUE_ERROR
    use stdlib_string_type, only: string_type, char
    use stdlib_strings, only: starts_with, to_c_char
    use stdlib_system, only: OS_TYPE, OS_WINDOWS, OS_MACOS, join_path, FS_ERROR
    implicit none
    private

    public :: shared_library_type
    public :: load_library
    public :: library_filename
    public :: shared_library_suffix

    !> Version: experimental
    !>
    !> Handle on a loaded shared library.
    !> ([Specification](../page/specs/stdlib_load_library.html#shared_library_type))
    type :: shared_library_type
        private
        !> OS handle returned by `dlopen` / `LoadLibraryExW`
        type(c_ptr) :: handle = c_null_ptr
        !> Path of the file that was actually loaded
        character(len=:), allocatable :: path
    contains
        !> Load a shared library into this handle
        procedure :: open       => lib_open
        !> Address of an exported procedure
        procedure :: symbol     => lib_symbol
        !> Address of an exported variable
        procedure :: data       => lib_data
        !> Test whether a symbol is exported
        procedure :: has_symbol => lib_has_symbol
        !> Unload the library
        procedure :: close      => lib_close
        !> Test whether a library is currently loaded
        procedure :: is_loaded  => lib_is_loaded
        !> Path of the file that was actually loaded
        procedure :: filename   => lib_filename
    end type shared_library_type

    !> Size of the error message buffers filled by the C layer
    integer, parameter :: ERRBUF_LEN = 512

    ! ------------------------------------------------------------------
    ! C layer (stdlib_load_library_c.c). Error messages are written,
    ! NUL-terminated, to errbuf.
    ! ------------------------------------------------------------------
    interface
        function c_loadlib_open(filename, global, errbuf, errlen) &
                bind(C, name="stdlib_loadlib_open") result(handle)
            import :: c_ptr, c_char, c_int, c_size_t
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value :: global
            character(kind=c_char), intent(out) :: errbuf(*)
            integer(c_size_t), value :: errlen
            type(c_ptr) :: handle
        end function c_loadlib_open

        function c_loadlib_symbol(handle, name, errbuf, errlen) &
                bind(C, name="stdlib_loadlib_symbol") result(fptr)
            import :: c_ptr, c_funptr, c_char, c_size_t
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(out) :: errbuf(*)
            integer(c_size_t), value :: errlen
            type(c_funptr) :: fptr
        end function c_loadlib_symbol

        function c_loadlib_data(handle, name, errbuf, errlen) &
                bind(C, name="stdlib_loadlib_data") result(ptr)
            import :: c_ptr, c_char, c_size_t
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
            character(kind=c_char), intent(out) :: errbuf(*)
            integer(c_size_t), value :: errlen
            type(c_ptr) :: ptr
        end function c_loadlib_data

        function c_loadlib_close(handle, errbuf, errlen) &
                bind(C, name="stdlib_loadlib_close") result(rc)
            import :: c_ptr, c_char, c_int, c_size_t
            type(c_ptr), value :: handle
            character(kind=c_char), intent(out) :: errbuf(*)
            integer(c_size_t), value :: errlen
            integer(c_int) :: rc
        end function c_loadlib_close
    end interface

contains

    ! ==================================================================
    ! Public API
    ! ==================================================================

    !> Version: experimental
    !>
    !> Load a shared library and return a handle on it.
    !>
    !> `name` may be a bare name (`"kernels"`), a file name (`"libkernels.so.2"`)
    !> or a path (`"./plugins/kernels.dll"`). A name without extension is
    !> decorated with the platform prefix/suffix. If `path` is given, the library
    !> is searched only in that directory; otherwise the OS search rules apply
    !> (`LD_LIBRARY_PATH`, rpath, `PATH`, ...).
    function load_library(name, path, global, err) result(lib)
        !> Library name, file name or path
        character(len=*), intent(in) :: name
        !> Optional directory to search in, instead of the OS search rules
        character(len=*), intent(in), optional :: path
        !> Optional flag to make the library symbols available to libraries
        !> loaded afterwards (`RTLD_GLOBAL`). Ignored on Windows
        logical, intent(in), optional :: global
        !> Optional state return flag. On error, if not requested, the code stops
        type(state_type), intent(out), optional :: err
        !> Handle on the loaded library
        type(shared_library_type) :: lib

        call lib%open(name, path, global, err)
    end function load_library

    !> Version: experimental
    !>
    !> Platform-decorated file name of a library, e.g. `"fft"` -> `"libfft.so"` / `"fft.dll"`.
    !> ([Specification](../page/specs/stdlib_load_library.html#library_filename))
    function library_filename(name) result(fname)
        !> Undecorated library name
        character(len=*), intent(in) :: name
        !> Platform-specific file name
        character(len=:), allocatable :: fname

        if (OS_TYPE() == OS_WINDOWS) then
            fname = trim(name)//".dll"
        else if (starts_with(trim(name), "lib")) then
            fname = trim(name)//shared_library_suffix()
        else
            fname = "lib"//trim(name)//shared_library_suffix()
        end if
    end function library_filename

    !> Version: experimental
    !>
    !> Shared library file extension on this platform (`".so"`, `".dll"`, `".dylib"`).
    !> ([Specification](../page/specs/stdlib_load_library.html#shared_library_suffix))
    function shared_library_suffix() result(suffix)
        !> Platform-specific extension, dot included
        character(len=:), allocatable :: suffix

        select case (OS_TYPE())
        case (OS_WINDOWS)
            suffix = ".dll"
        case (OS_MACOS)
            suffix = ".dylib"
        case default
            suffix = ".so"
        end select
    end function shared_library_suffix

    ! ==================================================================
    ! Type-bound procedures
    ! ==================================================================

    !> Load a shared library into an existing handle.
    subroutine lib_open(self, name, path, global, err)
        !> Handle on the library. Must not be loaded already
        class(shared_library_type), intent(inout) :: self
        !> Library name, file name or path
        character(len=*), intent(in) :: name
        !> Optional directory to search in, instead of the OS search rules
        character(len=*), intent(in), optional :: path
        !> Optional `RTLD_GLOBAL` flag. Ignored on Windows
        logical, intent(in), optional :: global
        !> Optional state return flag. On error, if not requested, the code stops
        type(state_type), intent(out), optional :: err

        type(string_type), allocatable :: candidates(:)
        character(len=:), allocatable :: full, report, msg
        character(kind=c_char) :: errbuf(ERRBUF_LEN)
        type(state_type) :: err0
        type(c_ptr) :: h
        integer(c_int) :: glob
        logical :: use_dir
        integer :: i

        if (c_associated(self%handle)) then
            msg = "shared library '"//self%path//"' is already loaded: call close() first"
            err0 = state_type('open', STDLIB_VALUE_ERROR, msg)
            call err0%handle(err)
            return
        end if

        glob = 0
        if (present(global)) then
            if (global) glob = 1
        end if

        use_dir = .false.
        if (present(path)) use_dir = len_trim(path) > 0 .and. .not. has_separator(trim(name))

        candidates = candidate_names(trim(name))   ! blank-padded buffers are common
        report = ""
        do i = 1, size(candidates)
            if (use_dir) then
                full = join_path(trim(path), char(candidates(i)))
            else
                full = char(candidates(i))
            end if

            errbuf = c_null_char
            h = c_loadlib_open(to_c_char(full), glob, errbuf, size(errbuf, kind=c_size_t))
            if (c_associated(h)) then
                self%handle = h
                self%path = full
                return
            end if
            if (i > 1) report = report//","
            msg = from_c_buffer(errbuf)
            report = report//" '"//full//"' ("//msg//")"
        end do

        msg = "cannot load shared library '"//trim(name)//"'; tried"//report
        err0 = FS_ERROR(msg)
        call err0%update_location('open')
        call err0%handle(err)
    end subroutine lib_open

    !> Address of an exported procedure, to be used with `c_f_procpointer`.
    function lib_symbol(self, name, err) result(fptr)
        !> Handle on a loaded library
        class(shared_library_type), intent(in) :: self
        !> Name of the exported procedure
        character(len=*), intent(in) :: name
        !> Optional state return flag. On error, if not requested, the code stops
        type(state_type), intent(out), optional :: err
        !> Address of the procedure, `c_null_funptr` on error
        type(c_funptr) :: fptr

        type(c_ptr) :: unused
        type(state_type) :: err0

        call lookup(self, name, .false., fptr, unused, err0)
        call err0%handle(err)
    end function lib_symbol

    !> Address of an exported variable, to be used with `c_f_pointer`.
    function lib_data(self, name, err) result(ptr)
        !> Handle on a loaded library
        class(shared_library_type), intent(in) :: self
        !> Name of the exported variable
        character(len=*), intent(in) :: name
        !> Optional state return flag. On error, if not requested, the code stops
        type(state_type), intent(out), optional :: err
        !> Address of the variable, `c_null_ptr` on error
        type(c_ptr) :: ptr

        type(c_funptr) :: unused
        type(state_type) :: err0

        call lookup(self, name, .true., unused, ptr, err0)
        call err0%handle(err)
    end function lib_data

    !> `.true.` if the library is loaded and exports `name`.
    function lib_has_symbol(self, name) result(found)
        !> Handle on a library
        class(shared_library_type), intent(in) :: self
        !> Name of the symbol to look for
        character(len=*), intent(in) :: name
        !> Whether the symbol is exported
        logical :: found

        type(c_funptr) :: fptr
        type(c_ptr) :: unused
        type(state_type) :: err0

        call lookup(self, name, .false., fptr, unused, err0)
        found = err0%ok()
    end function lib_has_symbol

    !> Unload the library. Procedure pointers obtained from it become invalid.
    !> Closing an unloaded handle is a no-op.
    subroutine lib_close(self, err)
        !> Handle on the library
        class(shared_library_type), intent(inout) :: self
        !> Optional state return flag. On error, if not requested, the code stops
        type(state_type), intent(out), optional :: err

        character(kind=c_char) :: errbuf(ERRBUF_LEN)
        character(len=:), allocatable :: msg
        type(state_type) :: err0
        integer(c_int) :: rc

        if (.not. c_associated(self%handle)) return

        errbuf = c_null_char
        rc = c_loadlib_close(self%handle, errbuf, size(errbuf, kind=c_size_t))
        msg = "cannot unload shared library '"//self%path//"': "//from_c_buffer(errbuf)

        ! The handle is dropped whatever the outcome: a failed unload leaves it
        ! in an unusable state, and retrying would only leak the reference count
        self%handle = c_null_ptr
        if (allocated(self%path)) deallocate (self%path)

        if (rc /= 0) then
            err0 = FS_ERROR(msg)
            call err0%update_location('close')
            call err0%handle(err)
        end if
    end subroutine lib_close

    !> Whether a library is currently loaded into this handle.
    pure function lib_is_loaded(self) result(loaded)
        !> Handle on a library
        class(shared_library_type), intent(in) :: self
        !> Whether a library is loaded
        logical :: loaded

        loaded = c_associated(self%handle)
    end function lib_is_loaded

    !> Path of the file that was actually loaded (`""` if none).
    pure function lib_filename(self) result(fname)
        !> Handle on a library
        class(shared_library_type), intent(in) :: self
        !> Path of the loaded file
        character(len=:), allocatable :: fname

        if (allocated(self%path)) then
            fname = self%path
        else
            fname = ""
        end if
    end function lib_filename

    ! ==================================================================
    ! Implementation
    ! ==================================================================

    !> Symbol lookup shared by `symbol()`, `data()` and `has_symbol()`.
    !> Only one of `fptr` / `ptr` is set, depending on `is_data`.
    subroutine lookup(self, name, is_data, fptr, ptr, err)
        class(shared_library_type), intent(in) :: self
        character(len=*), intent(in) :: name
        logical, intent(in) :: is_data
        type(c_funptr), intent(out) :: fptr
        type(c_ptr), intent(out) :: ptr
        type(state_type), intent(out) :: err

        character(kind=c_char) :: errbuf(ERRBUF_LEN)
        character(len=:), allocatable :: msg
        logical :: found

        fptr = c_null_funptr
        ptr = c_null_ptr

        if (.not. c_associated(self%handle)) then
            msg = "cannot look up symbol '"//trim(name)//"': no library is loaded"
            err = state_type('symbol', STDLIB_VALUE_ERROR, msg)
            return
        end if

        errbuf = c_null_char
        if (is_data) then
            ptr = c_loadlib_data(self%handle, to_c_char(trim(name)), errbuf, size(errbuf, kind=c_size_t))
            found = c_associated(ptr)
        else
            fptr = c_loadlib_symbol(self%handle, to_c_char(trim(name)), errbuf, size(errbuf, kind=c_size_t))
            found = c_associated(fptr)
        end if

        if (.not. found) then
            msg = "symbol '"//trim(name)//"' not found in '"//self%path//"': "//from_c_buffer(errbuf)
            err = FS_ERROR(msg)
            call err%update_location('symbol')
        end if
    end subroutine lookup

    !> File names to try, in order, for a user-supplied library name.
    function candidate_names(name) result(c)
        character(len=*), intent(in) :: name
        type(string_type), allocatable :: c(:)

        character(len=:), allocatable :: dir, base
        integer :: isep

        isep = last_separator(name)
        dir = name(1:isep)
        base = name(isep + 1:)

        if (index(base, ".") > 0) then
            ! Already has an extension (libfoo.so.2, foo.dll, ...): use as is
            c = [string_type(name)]
            return
        end if

        select case (OS_TYPE())
        case (OS_WINDOWS)
            ! MSVC-style foo.dll first, then MinGW-style libfoo.dll
            c = [string_type(dir//base//".dll")]
            if (.not. starts_with(base, "lib")) c = [c, string_type(dir//"lib"//base//".dll")]
        case (OS_MACOS)
            if (starts_with(base, "lib")) then
                c = [string_type(dir//base//".dylib"), string_type(dir//base//".so")]
            else
                c = [string_type(dir//"lib"//base//".dylib"), string_type(dir//base//".dylib"), &
                     string_type(dir//"lib"//base//".so"), string_type(dir//base//".so")]
            end if
        case default
            if (starts_with(base, "lib")) then
                c = [string_type(dir//base//".so")]
            else
                c = [string_type(dir//"lib"//base//".so"), string_type(dir//base//".so")]
            end if
        end select
    end function candidate_names

    !> Path separators accepted by the OS. Unlike `stdlib_system`'s `path_sep`,
    !> both `"/"` and `"\"` are recognized on Windows, as both are accepted there.
    function is_separator(ch) result(sep)
        character, intent(in) :: ch
        logical :: sep

        sep = ch == "/"
        if (.not. sep .and. ch == achar(92)) sep = OS_TYPE() == OS_WINDOWS
    end function is_separator

    !> Position of the last path separator in `s`, `0` if there is none.
    !> `split_path` is not used here: it normalizes the path, whereas the
    !> directory prefix must be preserved verbatim to rebuild candidate names.
    function last_separator(s) result(isep)
        character(len=*), intent(in) :: s
        integer :: isep

        integer :: i

        isep = 0
        do i = len(s), 1, -1
            if (is_separator(s(i:i))) then
                isep = i
                return
            end if
        end do
    end function last_separator

    !> Whether `s` carries a directory component.
    function has_separator(s) result(found)
        character(len=*), intent(in) :: s
        logical :: found

        found = last_separator(s) > 0
    end function has_separator

    !> NUL-terminated C buffer -> Fortran string.
    !> `stdlib_system`'s `to_f_char` is not reused here: it reads from a `c_ptr`
    !> of known length, while the C layer fills a caller-provided array.
    pure function from_c_buffer(buf) result(s)
        character(kind=c_char), intent(in) :: buf(:)
        character(len=:), allocatable :: s

        integer :: i, n

        n = size(buf)
        do i = 1, size(buf)
            if (buf(i) == c_null_char) then
                n = i - 1
                exit
            end if
        end do

        allocate (character(len=n) :: s)
        do i = 1, n
            s(i:i) = buf(i)
        end do
    end function from_c_buffer

end module stdlib_load_library
