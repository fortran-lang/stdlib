module test_load_library
    use, intrinsic :: iso_c_binding, only: c_double, c_funptr, c_ptr, c_associated, c_f_procpointer
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use stdlib_error, only: state_type
    use stdlib_load_library, only: shared_library_type, load_library, &
                                   library_filename, shared_library_suffix

    implicit none

    abstract interface
        !> Interface of the C function `double cos(double)`
        function cos_t(x) bind(C) result(y)
            import :: c_double
            real(c_double), value :: x
            real(c_double) :: y
        end function cos_t
    end interface

contains

    !> Collect all exported unit tests
    subroutine collect_suite(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest('test_naming', test_naming), &
            new_unittest('test_unloaded_handle', test_unloaded_handle), &
            new_unittest('test_missing_library', test_missing_library), &
            new_unittest('test_system_library', test_system_library) &
            ]
    end subroutine collect_suite

    !> Name of the C runtime library exporting `cos` on this platform
    function c_runtime_name() result(name)
        character(len=:), allocatable :: name

        select case (shared_library_suffix())
        case (".dll")
            name = "msvcrt"               ! bare name: standard Windows search order
        case (".dylib")
            name = "libSystem.B.dylib"    ! in the dyld shared cache, not on disk
        case default
            name = "libm.so.6"
        end select
    end function c_runtime_name

    !> Platform naming conventions, which need no library to be loaded
    subroutine test_naming(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: sfx

        sfx = shared_library_suffix()

        call check(error, sfx == ".so" .or. sfx == ".dll" .or. sfx == ".dylib", &
                   "unknown shared library suffix: "//sfx)
        if (allocated(error)) return

        if (sfx == ".dll") then
            call check(error, library_filename("fft") == "fft.dll", "library_filename(fft)")
            if (allocated(error)) return
            call check(error, library_filename("libfft") == "libfft.dll", "library_filename(libfft)")
        else
            call check(error, library_filename("fft") == "libfft"//sfx, "library_filename(fft)")
            if (allocated(error)) return
            call check(error, library_filename("libfft") == "libfft"//sfx, "library_filename(libfft)")
        end if
        if (allocated(error)) return

        ! Blank-padded fixed-length buffers are a common way to pass names around
        call check(error, library_filename("fft   ") == library_filename("fft"), &
                   "library_filename ignores trailing blanks")
    end subroutine test_naming

    !> Every operation on a handle that carries no library must be diagnosed
    subroutine test_unloaded_handle(error)
        type(error_type), allocatable, intent(out) :: error
        type(shared_library_type) :: lib
        type(state_type) :: err
        type(c_funptr) :: fptr
        type(c_ptr) :: ptr

        call check(error, .not. lib%is_loaded(), "a default handle is not loaded")
        if (allocated(error)) return

        call check(error, lib%filename() == "", "a default handle has an empty file name")
        if (allocated(error)) return

        call check(error, .not. lib%has_symbol("anything"), "has_symbol on an unloaded handle")
        if (allocated(error)) return

        fptr = lib%symbol("anything", err)
        call check(error, err%error() .and. .not. c_associated(fptr), &
                   "symbol on an unloaded handle must fail")
        if (allocated(error)) return

        ptr = lib%data("anything", err)
        call check(error, err%error() .and. .not. c_associated(ptr), &
                   "data on an unloaded handle must fail")
        if (allocated(error)) return

        call lib%close(err)
        call check(error, err%ok(), "close on an unloaded handle is a no-op: "//err%print())
    end subroutine test_unloaded_handle

    !> A library that cannot be found is reported, with its name in the message
    subroutine test_missing_library(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: missing = "stdlib_loadlib_does_not_exist"
        type(shared_library_type) :: lib
        type(state_type) :: err

        lib = load_library(missing, err=err)
        call check(error, err%error() .and. .not. lib%is_loaded(), &
                   "loading a missing library must fail")
        if (allocated(error)) return

        call check(error, index(err%print(), missing) > 0, &
                   "the error message must name the library: "//err%print())
        if (allocated(error)) return

        ! Same, but searching one directory only instead of the OS search rules
        lib = load_library(missing, path=".", err=err)
        call check(error, err%error() .and. .not. lib%is_loaded(), &
                   "loading a missing library from a path must fail")
    end subroutine test_missing_library

    !> Full round trip on the C runtime: load, look up, call, unload
    subroutine test_system_library(error)
        type(error_type), allocatable, intent(out) :: error
        type(shared_library_type) :: libm
        type(state_type) :: err
        procedure(cos_t), pointer :: ccos
        character(len=:), allocatable :: name
        type(c_funptr) :: fptr

        name = c_runtime_name()
        libm = load_library(name, err=err)
        if (err%error()) then
            call skip_test(error, "the C runtime is not available as "//name//" (skipping)")
            return
        end if

        ! Everything below runs on the loaded handle: on failure it is closed
        ! once, at the end, rather than at every early return
        check_loaded: block
            call check(error, libm%is_loaded() .and. len(libm%filename()) > 0, &
                       "a loaded handle reports its file name")
            if (allocated(error)) exit check_loaded

            call check(error, libm%has_symbol("cos"), "the C runtime exports cos")
            if (allocated(error)) exit check_loaded

            call c_f_procpointer(libm%symbol("cos", err), ccos)
            call check(error, err%ok(), "resolving cos: "//err%print())
            if (allocated(error)) exit check_loaded

            call check(error, abs(ccos(0.0_c_double) - 1.0_c_double) < 1.0e-15_c_double, &
                       "calling cos through the resolved pointer")
            if (allocated(error)) exit check_loaded

            ! A symbol that no C runtime exports
            fptr = libm%symbol("stdlib_loadlib_no_such_symbol", err)
            call check(error, err%error() .and. .not. c_associated(fptr), &
                       "looking up a missing symbol must fail")
            if (allocated(error)) exit check_loaded

            ! Loading into a handle that is already in use is a programming error
            call libm%open(name, err=err)
            call check(error, err%error(), "opening an already loaded handle must fail")
        end block check_loaded

        call libm%close(err)
        if (allocated(error)) return

        call check(error, err%ok() .and. .not. libm%is_loaded(), "unloading: "//err%print())
        if (allocated(error)) return

        ! Closing twice is a no-op, and symbols are gone once unloaded
        call libm%close(err)
        call check(error, err%ok(), "closing twice is a no-op: "//err%print())
        if (allocated(error)) return

        call check(error, .not. libm%has_symbol("cos"), "no symbol survives close")
    end subroutine test_system_library

end module test_load_library

program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use testdrive, only: run_testsuite, new_testsuite, testsuite_type
    use test_load_library, only: collect_suite

    implicit none

    integer :: stat, is
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0

    testsuites = [ &
        new_testsuite("load_library", collect_suite) &
        ]

    do is = 1, size(testsuites)
        write (error_unit, fmt) "Testing:", testsuites(is)%name
        call run_testsuite(testsuites(is)%collect, error_unit, stat)
    end do

    if (stat > 0) then
        write (error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop
    end if
end program tester
