module stdlib_specialmatrices
    !! Provides derived-types and associated specialized linear algebra drivers
    !! for highly-structured matrices commonly encountered in the discretization
    !! of partial differential equations, as well as control and signal processing
    !! applications. ([Specifications](../page/specs/stdlib_specialmatrices.html))
    use stdlib_linalg_constants
    use stdlib_constants
    use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
        LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
    use stdlib_lapack_extended_base
    implicit none
    private
    public :: tridiagonal, sym_tridiagonal
    public :: spmv
    public :: dense, transpose, hermitian
    public :: operator(*), operator(+), operator(-)

    !--------------------------------------
    !-----                           ------
    !-----     TYPE DEFINITIONS      ------
    !-----                           ------
    !--------------------------------------

    !--> Tridiagonal matrices
    type, public :: tridiagonal_sp_type
        !! Base type to define a `tridiagonal` matrix.
        private
        real(sp), allocatable :: dl(:), dv(:), du(:)
        integer(ilp) :: n
    end type
    type, public :: tridiagonal_dp_type
        !! Base type to define a `tridiagonal` matrix.
        private
        real(dp), allocatable :: dl(:), dv(:), du(:)
        integer(ilp) :: n
    end type
    type, public :: tridiagonal_csp_type
        !! Base type to define a `tridiagonal` matrix.
        private
        complex(sp), allocatable :: dl(:), dv(:), du(:)
        integer(ilp) :: n
    end type
    type, public :: tridiagonal_cdp_type
        !! Base type to define a `tridiagonal` matrix.
        private
        complex(dp), allocatable :: dl(:), dv(:), du(:)
        integer(ilp) :: n
    end type

    !--> Symmetric tridiagonal matrices
    type, public :: sym_tridiagonal_sp_type
        !! Base type to define a `symmetric tridiagonal` matrix.
        private
        real(sp), allocatable :: du(:), dv(:)
        integer(ilp) :: n
    end type
    type, public :: sym_tridiagonal_dp_type
        !! Base type to define a `symmetric tridiagonal` matrix.
        private
        real(dp), allocatable :: du(:), dv(:)
        integer(ilp) :: n
    end type
    type, public :: sym_tridiagonal_csp_type
        !! Base type to define a `symmetric tridiagonal` matrix.
        private
        complex(sp), allocatable :: du(:), dv(:)
        integer(ilp) :: n
    end type
    type, public :: sym_tridiagonal_cdp_type
        !! Base type to define a `symmetric tridiagonal` matrix.
        private
        complex(dp), allocatable :: du(:), dv(:)
        integer(ilp) :: n
    end type
    !--------------------------------
    !-----                      -----
    !-----     CONSTRUCTORS     -----
    !-----                      -----
    !--------------------------------

    interface tridiagonal
        !! ([Specifications](../page/specs/stdlib_specialmatrices.html#Tridiagonal)) This
        !! interface provides different methods to construct a `tridiagonal` matrix. Only
        !! the non-zero elements of \( A \) are stored, i.e.
        !!
        !! \[
        !!    A
        !!    =
        !!    \begin{bmatrix}
        !!       a_1   &  b_1  \\
        !!       c_1  &  a_2      &  b_2  \\
        !!             &  \ddots   &  \ddots   &  \ddots   \\
        !!             &           &  c_{n-2} &  a_{n-1}  &  b_{n-1} \\
        !!             &           &           &  c_{n-1} &  a_n
        !!    \end{bmatrix}.
        !! \]
        !!
        !! #### Syntax
        !!
        !! - Construct a real `tridiagonal` matrix from rank-1 arrays:
        !!
        !! ```fortran
        !!    integer, parameter :: n
        !!    real(dp), allocatable :: dl(:), dv(:), du(:)
        !!    type(tridiagonal_rdp_type) :: A
        !!    integer :: i
        !!
        !!    dl = [(i, i=1, n-1)]
        !!    dv = [(2*i, i=1, n)]
        !!    du = [(3*i, i=1, n)]
        !!    A = Tridiagonal(dl, dv, du)
        !! ```
        !!
        !! - Construct a real `tridiagonal` matrix with constant diagonals:
        !!
        !! ```fortran
        !!    integer, parameter :: n
        !!    real(dp), parameter :: a = 1.0_dp, b = 1.0_dp, c = 2.0_dp
        !!    type(tridiagonal_rdp_type) :: A
        !!
        !!    A = Tridiagonal(a, b, c, n)
        !! ```
        pure module function initialize_tridiagonal_pure_sp(dl, dv, du) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            real(sp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(tridiagonal_sp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        pure module function initialize_constant_tridiagonal_pure_sp(dl, dv, du, n) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            real(sp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(tridiagonal_sp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_tridiagonal_impure_sp(dl, dv, du, err) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            real(sp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_sp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_constant_tridiagonal_impure_sp(dl, dv, du, n, err) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            real(sp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_sp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function
        pure module function initialize_tridiagonal_pure_dp(dl, dv, du) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            real(dp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(tridiagonal_dp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        pure module function initialize_constant_tridiagonal_pure_dp(dl, dv, du, n) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            real(dp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(tridiagonal_dp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_tridiagonal_impure_dp(dl, dv, du, err) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            real(dp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_dp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_constant_tridiagonal_impure_dp(dl, dv, du, n, err) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            real(dp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_dp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function
        pure module function initialize_tridiagonal_pure_csp(dl, dv, du) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            complex(sp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(tridiagonal_csp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        pure module function initialize_constant_tridiagonal_pure_csp(dl, dv, du, n) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            complex(sp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(tridiagonal_csp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_tridiagonal_impure_csp(dl, dv, du, err) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            complex(sp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_csp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_constant_tridiagonal_impure_csp(dl, dv, du, n, err) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            complex(sp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_csp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function
        pure module function initialize_tridiagonal_pure_cdp(dl, dv, du) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            complex(dp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(tridiagonal_cdp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        pure module function initialize_constant_tridiagonal_pure_cdp(dl, dv, du, n) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            complex(dp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(tridiagonal_cdp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_tridiagonal_impure_cdp(dl, dv, du, err) result(A)
            !! Construct a `tridiagonal` matrix from the rank-1 arrays
            !! `dl`, `dv` and `du`.
            complex(dp), intent(in) :: dl(:), dv(:), du(:)
            !! Tridiagonal matrix elements.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_cdp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function

        module function initialize_constant_tridiagonal_impure_cdp(dl, dv, du, n, err) result(A)
            !! Construct a `tridiagonal` matrix with scalar elements.
            complex(dp), intent(in) :: dl, dv, du
            !! Tridiagonal matrix elements.
            integer(ilp), intent(in) :: n
            !! Matrix dimension.
            type(linalg_state_type), intent(out) :: err
            !! Error handling.
            type(tridiagonal_cdp_type) :: A
            !! Corresponding Tridiagonal matrix.
        end function
    end interface

    interface sym_tridiagonal
        !! ([Specifications](../page/specs/stdlib_specialmatrices.html#Sym_tridiagonal)) This
        !! interface provides different methods to construct a `symmetric tridiagonal` matrix. Only
        !! the non-zero elements of \( A \) are stored, i.e.
        !!
        !! \[
        !!    A
        !!    =
        !!    \begin{bmatrix}
        !!       a_1   &  b_1  \\
        !!       b_1  &  a_2      &  b_2  \\
        !!             &  \ddots   &  \ddots   &  \ddots   \\
        !!             &           &  b_{n-2} &  a_{n-1}  &  b_{n-1} \\
        !!             &           &           &  b_{n-1} &  a_n
        !!    \end{bmatrix}.
        !! \]
        !!
        !! #### Syntax
        !!
        !! - Construct a real `symmetric tridiagonal` matrix from rank-1 arrays:
        !!
        !! ```fortran
        !!    integer, parameter :: n
        !!    real(dp), allocatable :: du(:), dv(:)
        !!    type(sym_tridiagonal_rdp_type) :: A
        !!    integer :: i
        !!
        !!    du = [(i, i=1, n-1)]
        !!    dv = [(2*i, i=1, n)]
        !!    A = sym_tridiagonal(du, dv)
        !! ```
        !!
        !! - Construct a real `symmetric tridiagonal` matrix with constant diagonals:
        !!
        !! ```fortran
        !!    integer, parameter :: n
        !!    real(dp), parameter :: a = 1.0_dp, b = 1.0_dp
        !!    type(sym_tridiagonal_rdp_type) :: A
        !!
        !!    A = sym_tridiagonal(a, b, n)
        !! ```
        pure module function initialize_sym_tridiagonal_pure_sp(du, dv) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        real(sp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(sym_tridiagonal_sp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        pure module function initialize_constant_sym_tridiagonal_pure_sp(du, dv, n) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        real(sp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(sym_tridiagonal_sp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_sym_tridiagonal_impure_sp(du, dv, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        real(sp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_sp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_constant_sym_tridiagonal_impure_sp(du, dv, n, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        real(sp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_sp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function
        pure module function initialize_sym_tridiagonal_pure_dp(du, dv) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        real(dp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(sym_tridiagonal_dp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        pure module function initialize_constant_sym_tridiagonal_pure_dp(du, dv, n) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        real(dp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(sym_tridiagonal_dp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_sym_tridiagonal_impure_dp(du, dv, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        real(dp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_dp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_constant_sym_tridiagonal_impure_dp(du, dv, n, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        real(dp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_dp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function
        pure module function initialize_sym_tridiagonal_pure_csp(du, dv) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        complex(sp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(sym_tridiagonal_csp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        pure module function initialize_constant_sym_tridiagonal_pure_csp(du, dv, n) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        complex(sp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(sym_tridiagonal_csp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_sym_tridiagonal_impure_csp(du, dv, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        complex(sp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_csp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_constant_sym_tridiagonal_impure_csp(du, dv, n, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        complex(sp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_csp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function
        pure module function initialize_sym_tridiagonal_pure_cdp(du, dv) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        complex(dp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(sym_tridiagonal_cdp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        pure module function initialize_constant_sym_tridiagonal_pure_cdp(du, dv, n) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        complex(dp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(sym_tridiagonal_cdp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_sym_tridiagonal_impure_cdp(du, dv, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` from rank-1 arrays `du` and `dv`.
        complex(dp), intent(in) :: du(:), dv(:)
        !! Symmetric tridiagonal matrix elements.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_cdp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function

        module function initialize_constant_sym_tridiagonal_impure_cdp(du, dv, n, err) result(A)
        !! Construct a `symmetric tridiagonal matrix` with scalar elements.
        complex(dp), intent(in) :: du, dv
        !! Symmetric tridiagonal matrix elements.
        integer(ilp), intent(in) :: n
        !! Matrix dimension.
        type(linalg_state_type), intent(out) :: err
        !! Error Handling.
        type(sym_tridiagonal_cdp_type) :: A
        !! Corresponding symmetric tridiagonal matrix.
        end function
    end interface

    interface build_tridiagonal
    pure module subroutine build_tridiagonal_from_arrays_sp(dl, dv, du, A, err)
        real(sp), intent(in) :: dl(:), dv(:), du(:)
        type(tridiagonal_sp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_tridiagonal_from_constants_sp(dl, dv, du, n, A, err)
        real(sp), intent(in) :: dl, dv, du
        integer(ilp), intent(in) :: n
        type(tridiagonal_sp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    pure module subroutine build_tridiagonal_from_arrays_dp(dl, dv, du, A, err)
        real(dp), intent(in) :: dl(:), dv(:), du(:)
        type(tridiagonal_dp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_tridiagonal_from_constants_dp(dl, dv, du, n, A, err)
        real(dp), intent(in) :: dl, dv, du
        integer(ilp), intent(in) :: n
        type(tridiagonal_dp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    pure module subroutine build_tridiagonal_from_arrays_csp(dl, dv, du, A, err)
        complex(sp), intent(in) :: dl(:), dv(:), du(:)
        type(tridiagonal_csp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_tridiagonal_from_constants_csp(dl, dv, du, n, A, err)
        complex(sp), intent(in) :: dl, dv, du
        integer(ilp), intent(in) :: n
        type(tridiagonal_csp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    pure module subroutine build_tridiagonal_from_arrays_cdp(dl, dv, du, A, err)
        complex(dp), intent(in) :: dl(:), dv(:), du(:)
        type(tridiagonal_cdp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_tridiagonal_from_constants_cdp(dl, dv, du, n, A, err)
        complex(dp), intent(in) :: dl, dv, du
        integer(ilp), intent(in) :: n
        type(tridiagonal_cdp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    end interface

    interface build_sym_tridiagonal
    pure module subroutine build_sym_tridiagonal_from_arrays_sp(du, dv, A, err)
        real(sp), intent(in) :: du(:), dv(:)
        type(sym_tridiagonal_sp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_sym_tridiagonal_from_constants_sp(du, dv, n, A, err)
        real(sp), intent(in) :: du, dv
        integer(ilp), intent(in) :: n
        type(sym_tridiagonal_sp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    pure module subroutine build_sym_tridiagonal_from_arrays_dp(du, dv, A, err)
        real(dp), intent(in) :: du(:), dv(:)
        type(sym_tridiagonal_dp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_sym_tridiagonal_from_constants_dp(du, dv, n, A, err)
        real(dp), intent(in) :: du, dv
        integer(ilp), intent(in) :: n
        type(sym_tridiagonal_dp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    pure module subroutine build_sym_tridiagonal_from_arrays_csp(du, dv, A, err)
        complex(sp), intent(in) :: du(:), dv(:)
        type(sym_tridiagonal_csp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_sym_tridiagonal_from_constants_csp(du, dv, n, A, err)
        complex(sp), intent(in) :: du, dv
        integer(ilp), intent(in) :: n
        type(sym_tridiagonal_csp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    pure module subroutine build_sym_tridiagonal_from_arrays_cdp(du, dv, A, err)
        complex(dp), intent(in) :: du(:), dv(:)
        type(sym_tridiagonal_cdp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        integer(ilp) :: n
        type(linalg_state_type) :: err0
    end subroutine

    pure module subroutine build_sym_tridiagonal_from_constants_cdp(du, dv, n, A, err)
        complex(dp), intent(in) :: du, dv
        integer(ilp), intent(in) :: n
        type(sym_tridiagonal_cdp_type), intent(out) :: A
        type(linalg_state_type), intent(out), optional :: err

        ! Internal variables.
        type(linalg_state_type) :: err0
    end subroutine
    end interface

    !----------------------------------
    !-----                        -----
    !-----     LINEAR ALGEBRA     -----
    !-----                        -----
    !----------------------------------

    interface spmv
        !! ([Specifications](../page/specs/stdlib_specialmatrices.html#spmv)) This
        !! interface provides methods to compute the matrix-vector product
        !!
        !!  $$ y = \alpha \mathrm{op}(A) x + \beta y$$
        !!
        !! for the different matrix types defined by `stdlib_specialmatrices`.
        module subroutine spmv_tridiag_1d_sp(A, x, y, alpha, beta, op)
         type(tridiagonal_sp_type), intent(in) :: A
            real(sp), intent(in), contiguous, target :: x(:)
            real(sp), intent(inout), contiguous, target :: y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_2d_sp(A, x, y, alpha, beta, op)
         type(tridiagonal_sp_type), intent(in) :: A
            real(sp), intent(in), contiguous, target :: x(:,:)
            real(sp), intent(inout), contiguous, target :: y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_1d_dp(A, x, y, alpha, beta, op)
         type(tridiagonal_dp_type), intent(in) :: A
            real(dp), intent(in), contiguous, target :: x(:)
            real(dp), intent(inout), contiguous, target :: y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_2d_dp(A, x, y, alpha, beta, op)
         type(tridiagonal_dp_type), intent(in) :: A
            real(dp), intent(in), contiguous, target :: x(:,:)
            real(dp), intent(inout), contiguous, target :: y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_1d_csp(A, x, y, alpha, beta, op)
         type(tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in), contiguous, target :: x(:)
            complex(sp), intent(inout), contiguous, target :: y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_2d_csp(A, x, y, alpha, beta, op)
         type(tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in), contiguous, target :: x(:,:)
            complex(sp), intent(inout), contiguous, target :: y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_1d_cdp(A, x, y, alpha, beta, op)
         type(tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in), contiguous, target :: x(:)
            complex(dp), intent(inout), contiguous, target :: y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_2d_cdp(A, x, y, alpha, beta, op)
         type(tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in), contiguous, target :: x(:,:)
            complex(dp), intent(inout), contiguous, target :: y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine

        module subroutine spmv_sym_tridiag_1d_sp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            real(sp), intent(in), contiguous, target :: x(:)
            real(sp), intent(inout), contiguous, target :: y(:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_2d_sp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            real(sp), intent(in), contiguous, target :: x(:,:)
            real(sp), intent(inout), contiguous, target :: y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_1d_dp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            real(dp), intent(in), contiguous, target :: x(:)
            real(dp), intent(inout), contiguous, target :: y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_2d_dp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            real(dp), intent(in), contiguous, target :: x(:,:)
            real(dp), intent(inout), contiguous, target :: y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_1d_csp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in), contiguous, target :: x(:)
            complex(sp), intent(inout), contiguous, target :: y(:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_2d_csp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in), contiguous, target :: x(:,:)
            complex(sp), intent(inout), contiguous, target :: y(:,:)
            complex(sp), intent(in), optional :: alpha
            complex(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_1d_cdp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in), contiguous, target :: x(:)
            complex(dp), intent(inout), contiguous, target :: y(:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_sym_tridiag_2d_cdp(A, x, y, alpha, beta, op)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in), contiguous, target :: x(:,:)
            complex(dp), intent(inout), contiguous, target :: y(:,:)
            complex(dp), intent(in), optional :: alpha
            complex(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
    end interface

    !-------------------------------------
    !-----                           -----
    !-----     UTILITY FUNCTIONS     -----
    !-----                           -----
    !-------------------------------------

    interface dense
        !! This interface provides methods to convert a matrix of one of the
        !! types defined by `stdlib_specialmatrices` to a standard rank-2 array.
        !! ([Specifications](../page/specs/stdlib_specialmatrices.html#dense))
        pure module function tridiagonal_to_dense_sp(A) result(B)
            !! Convert a `tridiagonal` matrix to its dense representation.
            type(tridiagonal_sp_type), intent(in) :: A
            !! Input Tridiagonal matrix.
            real(sp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
        pure module function tridiagonal_to_dense_dp(A) result(B)
            !! Convert a `tridiagonal` matrix to its dense representation.
            type(tridiagonal_dp_type), intent(in) :: A
            !! Input Tridiagonal matrix.
            real(dp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
        pure module function tridiagonal_to_dense_csp(A) result(B)
            !! Convert a `tridiagonal` matrix to its dense representation.
            type(tridiagonal_csp_type), intent(in) :: A
            !! Input Tridiagonal matrix.
            complex(sp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
        pure module function tridiagonal_to_dense_cdp(A) result(B)
            !! Convert a `tridiagonal` matrix to its dense representation.
            type(tridiagonal_cdp_type), intent(in) :: A
            !! Input Tridiagonal matrix.
            complex(dp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function

        pure module function sym_tridiagonal_to_dense_sp(A) result(B)
            !! Convert a `symmetric tridiagonal` matrix to its dense representation.
            type(sym_tridiagonal_sp_type), intent(in) :: A
            !! Input Symmetric tridiagonal matrix.
            real(sp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
        pure module function sym_tridiagonal_to_dense_dp(A) result(B)
            !! Convert a `symmetric tridiagonal` matrix to its dense representation.
            type(sym_tridiagonal_dp_type), intent(in) :: A
            !! Input Symmetric tridiagonal matrix.
            real(dp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
        pure module function sym_tridiagonal_to_dense_csp(A) result(B)
            !! Convert a `symmetric tridiagonal` matrix to its dense representation.
            type(sym_tridiagonal_csp_type), intent(in) :: A
            !! Input Symmetric tridiagonal matrix.
            complex(sp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
        pure module function sym_tridiagonal_to_dense_cdp(A) result(B)
            !! Convert a `symmetric tridiagonal` matrix to its dense representation.
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            !! Input Symmetric tridiagonal matrix.
            complex(dp), allocatable :: B(:, :)
            !! Corresponding dense matrix.
        end function
    end interface

    interface transpose
        !! This interface provides methods to compute the transpose operation for
        !! the different matrix types defined by `stdlib_specialmatrices`.
        !! [Specifications](../page/specs/stdlib_specialmatrices.html#transpose)
        pure module function transpose_tridiagonal_sp(A) result(B)
            type(tridiagonal_sp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_sp_type) :: B
        end function
        pure module function transpose_tridiagonal_dp(A) result(B)
            type(tridiagonal_dp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_dp_type) :: B
        end function
        pure module function transpose_tridiagonal_csp(A) result(B)
            type(tridiagonal_csp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_csp_type) :: B
        end function
        pure module function transpose_tridiagonal_cdp(A) result(B)
            type(tridiagonal_cdp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_cdp_type) :: B
        end function

        pure module function transpose_sym_tridiagonal_sp(A) result(B)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_sp_type) :: B
        end function
        pure module function transpose_sym_tridiagonal_dp(A) result(B)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_dp_type) :: B
        end function
        pure module function transpose_sym_tridiagonal_csp(A) result(B)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_csp_type) :: B
        end function
        pure module function transpose_sym_tridiagonal_cdp(A) result(B)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_cdp_type) :: B
        end function
    end interface

    interface hermitian
        !! This interface provides methods to compute the hermitian operation for
        !! the different matrix types defined by `stdlib_specialmatrices`. For
        !! real-valued matrices, this is equivalent to the standard `transpose`.
        !! [Specifications](../page/specs/stdlib_specialmatrices.html#hermitian)
        pure module function hermitian_tridiagonal_sp(A) result(B)
            type(tridiagonal_sp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_sp_type) :: B
        end function
        pure module function hermitian_tridiagonal_dp(A) result(B)
            type(tridiagonal_dp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_dp_type) :: B
        end function
        pure module function hermitian_tridiagonal_csp(A) result(B)
            type(tridiagonal_csp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_csp_type) :: B
        end function
        pure module function hermitian_tridiagonal_cdp(A) result(B)
            type(tridiagonal_cdp_type), intent(in) :: A
            !! Input matrix.
            type(tridiagonal_cdp_type) :: B
        end function

        pure module function hermitian_sym_tridiagonal_sp(A) result(B)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_sp_type) :: B
        end function
        pure module function hermitian_sym_tridiagonal_dp(A) result(B)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_dp_type) :: B
        end function
        pure module function hermitian_sym_tridiagonal_csp(A) result(B)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_csp_type) :: B
        end function
        pure module function hermitian_sym_tridiagonal_cdp(A) result(B)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            !! Input matrix.
            type(sym_tridiagonal_cdp_type) :: B
        end function
    end interface

    !----------------------------------------
    !-----                              -----
    !-----     ARITHMETIC OPERATORS     -----
    !-----                              -----
    !----------------------------------------

    interface operator(*)
        !! Overload the `*` for scalar-matrix multiplications for the different matrix
        !! types provided by `stdlib_specialmatrices`.
        !! [Specifications](../page/specs/stdlib_specialmatrices.html#operators)
        pure module function scalar_multiplication_tridiagonal_sp(alpha, A) result(B)
            real(sp), intent(in) :: alpha
            type(tridiagonal_sp_type), intent(in) :: A
            type(tridiagonal_sp_type) :: B
        end function
        pure module function scalar_multiplication_bis_tridiagonal_sp(A, alpha) result(B)
            type(tridiagonal_sp_type), intent(in) :: A
            real(sp), intent(in) :: alpha
            type(tridiagonal_sp_type) :: B
        end function
        pure module function scalar_multiplication_tridiagonal_dp(alpha, A) result(B)
            real(dp), intent(in) :: alpha
            type(tridiagonal_dp_type), intent(in) :: A
            type(tridiagonal_dp_type) :: B
        end function
        pure module function scalar_multiplication_bis_tridiagonal_dp(A, alpha) result(B)
            type(tridiagonal_dp_type), intent(in) :: A
            real(dp), intent(in) :: alpha
            type(tridiagonal_dp_type) :: B
        end function
        pure module function scalar_multiplication_tridiagonal_csp(alpha, A) result(B)
            complex(sp), intent(in) :: alpha
            type(tridiagonal_csp_type), intent(in) :: A
            type(tridiagonal_csp_type) :: B
        end function
        pure module function scalar_multiplication_bis_tridiagonal_csp(A, alpha) result(B)
            type(tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in) :: alpha
            type(tridiagonal_csp_type) :: B
        end function
        pure module function scalar_multiplication_tridiagonal_cdp(alpha, A) result(B)
            complex(dp), intent(in) :: alpha
            type(tridiagonal_cdp_type), intent(in) :: A
            type(tridiagonal_cdp_type) :: B
        end function
        pure module function scalar_multiplication_bis_tridiagonal_cdp(A, alpha) result(B)
            type(tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in) :: alpha
            type(tridiagonal_cdp_type) :: B
        end function

        pure module function scalar_multiplication_sym_tridiagonal_sp(alpha, A) result(B)
            real(sp), intent(in) :: alpha
            type(sym_tridiagonal_sp_type), intent(in) :: A
            type(sym_tridiagonal_sp_type) :: B
        end function
        pure module function scalar_multiplication_bis_sym_tridiagonal_sp(A, alpha) result(B)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            real(sp), intent(in) :: alpha
            type(sym_tridiagonal_sp_type) :: B
        end function
        pure module function scalar_multiplication_sym_tridiagonal_dp(alpha, A) result(B)
            real(dp), intent(in) :: alpha
            type(sym_tridiagonal_dp_type), intent(in) :: A
            type(sym_tridiagonal_dp_type) :: B
        end function
        pure module function scalar_multiplication_bis_sym_tridiagonal_dp(A, alpha) result(B)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            real(dp), intent(in) :: alpha
            type(sym_tridiagonal_dp_type) :: B
        end function
        pure module function scalar_multiplication_sym_tridiagonal_csp(alpha, A) result(B)
            complex(sp), intent(in) :: alpha
            type(sym_tridiagonal_csp_type), intent(in) :: A
            type(sym_tridiagonal_csp_type) :: B
        end function
        pure module function scalar_multiplication_bis_sym_tridiagonal_csp(A, alpha) result(B)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in) :: alpha
            type(sym_tridiagonal_csp_type) :: B
        end function
        pure module function scalar_multiplication_sym_tridiagonal_cdp(alpha, A) result(B)
            complex(dp), intent(in) :: alpha
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            type(sym_tridiagonal_cdp_type) :: B
        end function
        pure module function scalar_multiplication_bis_sym_tridiagonal_cdp(A, alpha) result(B)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in) :: alpha
            type(sym_tridiagonal_cdp_type) :: B
        end function
    end interface

    interface operator(+)
        !! Overload the `+` operator for matrix-matrix addition. The two matrices need to
        !! be of the same type and kind.
        !! [Specifications](../page/specs/stdlib_specialmatrices.html#operators)
        pure module function matrix_add_tridiagonal_sp(A, B) result(C)
            type(tridiagonal_sp_type), intent(in) :: A
            type(tridiagonal_sp_type), intent(in) :: B
            type(tridiagonal_sp_type) :: C
        end function
        pure module function matrix_add_tridiagonal_dp(A, B) result(C)
            type(tridiagonal_dp_type), intent(in) :: A
            type(tridiagonal_dp_type), intent(in) :: B
            type(tridiagonal_dp_type) :: C
        end function
        pure module function matrix_add_tridiagonal_csp(A, B) result(C)
            type(tridiagonal_csp_type), intent(in) :: A
            type(tridiagonal_csp_type), intent(in) :: B
            type(tridiagonal_csp_type) :: C
        end function
        pure module function matrix_add_tridiagonal_cdp(A, B) result(C)
            type(tridiagonal_cdp_type), intent(in) :: A
            type(tridiagonal_cdp_type), intent(in) :: B
            type(tridiagonal_cdp_type) :: C
        end function

        pure module function matrix_add_sym_tridiagonal_sp(A, B) result(C)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            type(sym_tridiagonal_sp_type), intent(in) :: B
            type(sym_tridiagonal_sp_type) :: C
        end function
        pure module function matrix_add_sym_tridiagonal_dp(A, B) result(C)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            type(sym_tridiagonal_dp_type), intent(in) :: B
            type(sym_tridiagonal_dp_type) :: C
        end function
        pure module function matrix_add_sym_tridiagonal_csp(A, B) result(C)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            type(sym_tridiagonal_csp_type), intent(in) :: B
            type(sym_tridiagonal_csp_type) :: C
        end function
        pure module function matrix_add_sym_tridiagonal_cdp(A, B) result(C)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            type(sym_tridiagonal_cdp_type), intent(in) :: B
            type(sym_tridiagonal_cdp_type) :: C
        end function
    end interface

    interface operator(-)
        !! Overload the `-` operator for matrix-matrix subtraction. The two matrices need to
        !! be of the same type and kind.
        !! [Specifications](../page/specs/stdlib_specialmatrices.html#operators)
        pure module function matrix_sub_tridiagonal_sp(A, B) result(C)
            type(tridiagonal_sp_type), intent(in) :: A
            type(tridiagonal_sp_type), intent(in) :: B
            type(tridiagonal_sp_type) :: C
        end function
        pure module function matrix_sub_tridiagonal_dp(A, B) result(C)
            type(tridiagonal_dp_type), intent(in) :: A
            type(tridiagonal_dp_type), intent(in) :: B
            type(tridiagonal_dp_type) :: C
        end function
        pure module function matrix_sub_tridiagonal_csp(A, B) result(C)
            type(tridiagonal_csp_type), intent(in) :: A
            type(tridiagonal_csp_type), intent(in) :: B
            type(tridiagonal_csp_type) :: C
        end function
        pure module function matrix_sub_tridiagonal_cdp(A, B) result(C)
            type(tridiagonal_cdp_type), intent(in) :: A
            type(tridiagonal_cdp_type), intent(in) :: B
            type(tridiagonal_cdp_type) :: C
        end function

        pure module function matrix_sub_sym_tridiagonal_sp(A, B) result(C)
            type(sym_tridiagonal_sp_type), intent(in) :: A
            type(sym_tridiagonal_sp_type), intent(in) :: B
            type(sym_tridiagonal_sp_type) :: C
        end function
        pure module function matrix_sub_sym_tridiagonal_dp(A, B) result(C)
            type(sym_tridiagonal_dp_type), intent(in) :: A
            type(sym_tridiagonal_dp_type), intent(in) :: B
            type(sym_tridiagonal_dp_type) :: C
        end function
        pure module function matrix_sub_sym_tridiagonal_csp(A, B) result(C)
            type(sym_tridiagonal_csp_type), intent(in) :: A
            type(sym_tridiagonal_csp_type), intent(in) :: B
            type(sym_tridiagonal_csp_type) :: C
        end function
        pure module function matrix_sub_sym_tridiagonal_cdp(A, B) result(C)
            type(sym_tridiagonal_cdp_type), intent(in) :: A
            type(sym_tridiagonal_cdp_type), intent(in) :: B
            type(sym_tridiagonal_cdp_type) :: C
        end function
    end interface

end module stdlib_specialmatrices
