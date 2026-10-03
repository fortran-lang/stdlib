! Demonstrate run-time loading of a shared library
program example_load_library
    use, intrinsic :: iso_c_binding, only: c_double, c_f_procpointer
    use stdlib_load_library, only: shared_library_type, load_library, &
                                   library_filename, shared_library_suffix
    use stdlib_error, only: state_type
    implicit none

    abstract interface
        !> Interface of the C function `double cos(double)`
        function cos_t(x) bind(C) result(y)
            import :: c_double
            real(c_double), value :: x
            real(c_double) :: y
        end function cos_t
    end interface

    type(shared_library_type) :: libm
    type(state_type) :: err
    procedure(cos_t), pointer :: ccos
    character(len=:), allocatable :: name

    ! Platform naming conventions, without loading anything
    print *, "shared library suffix : ", shared_library_suffix()
    print *, "library 'fft' is named: ", library_filename("fft")

    ! The C math functions live in a different library on each platform
    select case (shared_library_suffix())
    case (".dll")
        name = "msvcrt"               ! bare name: standard Windows search order
    case (".dylib")
        name = "libSystem.B.dylib"    ! in the dyld shared cache, not on disk
    case default
        name = "libm.so.6"
    end select

    ! With `err` present, a failure is returned instead of stopping the program
    libm = load_library(name, err=err)
    if (err%error()) then
        ! No C runtime under this name here: nothing else to demonstrate
        print *, err%print()
        stop
    end if
    print *, "loaded                : ", libm%filename()

    ! Resolve `cos` at run time and call it through a procedure pointer
    if (libm%has_symbol("cos")) then
        call c_f_procpointer(libm%symbol("cos"), ccos)
        print *, "cos(0.0) = ", ccos(0.0_c_double)
    end if

    ! Unloading is always explicit: there is no finalizer
    call libm%close(err)
    if (err%error()) print *, err%print()

end program example_load_library
