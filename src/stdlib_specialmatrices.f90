module stdlib_specialmatrices
    !! Provides derived-types and associated specialized linear algebra drivers
    !! for highly-structured matrices commonly encountered in the discretization
    !! of partial differential equations, as well as control and signal processing
    !! applications. ([Specifications](../page/specs/stdlib_specialmatrices.html))
    use stdlib_linalg_constants
    use stdlib_constants
    use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling, LINALG_ERROR, &
        LINALG_INTERNAL_ERROR, LINALG_VALUE_ERROR
    implicit none
    private
    public :: tridiagonal
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
        !!    dl = [(i, i=1, n-1)]; dv = [(2*i, i=1, n)]; du = [(3*i, i=1, n)]
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            !! Construct a `tridiagonal` matrix with constant elements.
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
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_2d_csp(A, x, y, alpha, beta, op)
         type(tridiagonal_csp_type), intent(in) :: A
            complex(sp), intent(in), contiguous, target :: x(:,:)
            complex(sp), intent(inout), contiguous, target :: y(:,:)
            real(sp), intent(in), optional :: alpha
            real(sp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_1d_cdp(A, x, y, alpha, beta, op)
         type(tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in), contiguous, target :: x(:)
            complex(dp), intent(inout), contiguous, target :: y(:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
            character(1), intent(in), optional :: op
        end subroutine
        module subroutine spmv_tridiag_2d_cdp(A, x, y, alpha, beta, op)
         type(tridiagonal_cdp_type), intent(in) :: A
            complex(dp), intent(in), contiguous, target :: x(:,:)
            complex(dp), intent(inout), contiguous, target :: y(:,:)
            real(dp), intent(in), optional :: alpha
            real(dp), intent(in), optional :: beta
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
    end interface

end module stdlib_specialmatrices
