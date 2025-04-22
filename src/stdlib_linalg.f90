module stdlib_linalg
  !!Provides a support for various linear algebra procedures
  !! ([Specification](../page/specs/stdlib_linalg.html))
  use stdlib_kinds, only: xdp, int8, int16, int32, int64
  use stdlib_linalg_constants, only: sp, dp, qp, lk, ilp
  use stdlib_error, only: error_stop
  use stdlib_optval, only: optval
  use stdlib_linalg_state, only: linalg_state_type, linalg_error_handling
  implicit none
  private

  public :: chol
  public :: cholesky
  public :: det
  public :: operator(.det.)
  public :: diag
  public :: eig
  public :: eigh
  public :: eigvals
  public :: eigvalsh
  public :: eye
  public :: inv
  public :: invert
  public :: operator(.inv.)
  public :: pinv
  public :: pseudoinvert
  public :: operator(.pinv.)
  public :: lstsq
  public :: lstsq_space
  public :: norm
  public :: mnorm
  public :: get_norm
  public :: solve
  public :: solve_lu  
  public :: solve_lstsq
  public :: trace
  public :: svd
  public :: svdvals
  public :: outer_product
  public :: kronecker_product
  public :: cross_product
  public :: qr
  public :: qr_space
  public :: schur
  public :: schur_space
  public :: is_square
  public :: is_diagonal
  public :: is_symmetric
  public :: is_skew_symmetric
  public :: hermitian
  public :: is_hermitian
  public :: is_triangular
  public :: is_hessenberg
  
  ! Export linalg error handling
  public :: linalg_state_type, linalg_error_handling

  interface chol
    !! version: experimental 
    !!
    !! Computes the Cholesky factorization \( A = L \cdot L^T \), or \( A = U^T \cdot U \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#chol-compute-the-cholesky-factorization-of-a-rank-2-square-array-matrix))
    !! 
    !!### Summary 
    !! Pure function interface for computing the Cholesky triangular factors. 
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the lower- or upper- triangular matrix from the 
    !! Cholesky factorization of a `real` symmetric or `complex` Hermitian matrix.
    !! Supported data types include `real` and `complex`.    
    !! 
    !!@note The solution is based on LAPACK's `*POTRF` methods.
    !!     
     pure module function stdlib_linalg_s_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[m,n]
         real(sp), intent(in) :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> Output matrix with Cholesky factors c[n,n]
         real(sp) :: c(size(a,1),size(a,2))                  
     end function stdlib_linalg_s_cholesky_fun
     pure module function stdlib_linalg_d_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[m,n]
         real(dp), intent(in) :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> Output matrix with Cholesky factors c[n,n]
         real(dp) :: c(size(a,1),size(a,2))                  
     end function stdlib_linalg_d_cholesky_fun
     pure module function stdlib_linalg_c_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[m,n]
         complex(sp), intent(in) :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> Output matrix with Cholesky factors c[n,n]
         complex(sp) :: c(size(a,1),size(a,2))                  
     end function stdlib_linalg_c_cholesky_fun
     pure module function stdlib_linalg_z_cholesky_fun(a,lower,other_zeroed) result(c)
         !> Input matrix a[m,n]
         complex(dp), intent(in) :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> Output matrix with Cholesky factors c[n,n]
         complex(dp) :: c(size(a,1),size(a,2))                  
     end function stdlib_linalg_z_cholesky_fun
  end interface chol  

  interface cholesky
    !! version: experimental 
    !!
    !! Computes the Cholesky factorization \( A = L \cdot L^T \), or \( A = U^T \cdot U \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#cholesky-compute-the-cholesky-factorization-of-a-rank-2-square-array-matrix))
    !! 
    !!### Summary 
    !! Pure subroutine interface for computing the Cholesky triangular factors. 
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the lower- or upper- triangular matrix from the 
    !! Cholesky factorization of a `real` symmetric or `complex` Hermitian matrix.
    !! Supported data types include `real` and `complex`.    
    !! The factorization is computed in-place if only one matrix argument is present; or returned into 
    !! a second matrix argument, if present. The `lower` `logical` flag allows to select between upper or 
    !! lower factorization; the `other_zeroed` optional `logical` flag allows to choose whether the unused
    !! part of the triangular matrix should be filled with zeroes.
    !! 
    !!@note The solution is based on LAPACK's `*POTRF` methods.
    !!         
     pure module subroutine stdlib_linalg_s_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_s_cholesky_inplace
     
     pure module subroutine stdlib_linalg_s_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         real(sp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err     
     end subroutine stdlib_linalg_s_cholesky
     pure module subroutine stdlib_linalg_d_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_d_cholesky_inplace
     
     pure module subroutine stdlib_linalg_d_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         real(dp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err     
     end subroutine stdlib_linalg_d_cholesky
     pure module subroutine stdlib_linalg_c_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_c_cholesky_inplace
     
     pure module subroutine stdlib_linalg_c_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         complex(sp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err     
     end subroutine stdlib_linalg_c_cholesky
     pure module subroutine stdlib_linalg_z_cholesky_inplace(a,lower,other_zeroed,err) 
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_z_cholesky_inplace
     
     pure module subroutine stdlib_linalg_z_cholesky(a,c,lower,other_zeroed,err) 
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Output matrix with Cholesky factors c[n,n]
         complex(dp), intent(out) :: c(:,:)         
         !> [optional] is the lower or upper triangular factor required? Default = lower
         logical(lk), optional, intent(in) :: lower
         !> [optional] should the unused half of the return matrix be zeroed out? Default: yes
         logical(lk), optional, intent(in) :: other_zeroed
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err     
     end subroutine stdlib_linalg_z_cholesky
  end interface cholesky
     
  interface diag
    !! version: experimental
    !!
    !! Creates a diagonal array or extract the diagonal elements of an array
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! diag-create-a-diagonal-array-or-extract-the-diagonal-elements-of-an-array))
      !
      ! Vector to matrix
      !
      module pure function diag_rsp(v) result(res)
        real(sp), intent(in) :: v(:)
        real(sp) :: res(size(v),size(v))
      end function diag_rsp
      module pure function diag_rdp(v) result(res)
        real(dp), intent(in) :: v(:)
        real(dp) :: res(size(v),size(v))
      end function diag_rdp
      module pure function diag_csp(v) result(res)
        complex(sp), intent(in) :: v(:)
        complex(sp) :: res(size(v),size(v))
      end function diag_csp
      module pure function diag_cdp(v) result(res)
        complex(dp), intent(in) :: v(:)
        complex(dp) :: res(size(v),size(v))
      end function diag_cdp
      module pure function diag_iint8(v) result(res)
        integer(int8), intent(in) :: v(:)
        integer(int8) :: res(size(v),size(v))
      end function diag_iint8
      module pure function diag_iint16(v) result(res)
        integer(int16), intent(in) :: v(:)
        integer(int16) :: res(size(v),size(v))
      end function diag_iint16
      module pure function diag_iint32(v) result(res)
        integer(int32), intent(in) :: v(:)
        integer(int32) :: res(size(v),size(v))
      end function diag_iint32
      module pure function diag_iint64(v) result(res)
        integer(int64), intent(in) :: v(:)
        integer(int64) :: res(size(v),size(v))
      end function diag_iint64
      module pure function diag_rsp_k(v,k) result(res)
        real(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(sp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rsp_k
      module pure function diag_rdp_k(v,k) result(res)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(dp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rdp_k
      module pure function diag_csp_k(v,k) result(res)
        complex(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(sp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_csp_k
      module pure function diag_cdp_k(v,k) result(res)
        complex(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(dp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_cdp_k
      module pure function diag_iint8_k(v,k) result(res)
        integer(int8), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int8) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint8_k
      module pure function diag_iint16_k(v,k) result(res)
        integer(int16), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int16) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint16_k
      module pure function diag_iint32_k(v,k) result(res)
        integer(int32), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int32) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint32_k
      module pure function diag_iint64_k(v,k) result(res)
        integer(int64), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int64) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint64_k

      !
      ! Matrix to vector
      !
      module pure function diag_rsp_mat(A) result(res)
        real(sp), intent(in) :: A(:,:)
        real(sp) :: res(minval(shape(A)))
      end function diag_rsp_mat
      module pure function diag_rdp_mat(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res(minval(shape(A)))
      end function diag_rdp_mat
      module pure function diag_csp_mat(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        complex(sp) :: res(minval(shape(A)))
      end function diag_csp_mat
      module pure function diag_cdp_mat(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res(minval(shape(A)))
      end function diag_cdp_mat
      module pure function diag_iint8_mat(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer(int8) :: res(minval(shape(A)))
      end function diag_iint8_mat
      module pure function diag_iint16_mat(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer(int16) :: res(minval(shape(A)))
      end function diag_iint16_mat
      module pure function diag_iint32_mat(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res(minval(shape(A)))
      end function diag_iint32_mat
      module pure function diag_iint64_mat(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer(int64) :: res(minval(shape(A)))
      end function diag_iint64_mat
      module pure function diag_rsp_mat_k(A,k) result(res)
        real(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(sp) :: res(minval(shape(A))-abs(k))
      end function diag_rsp_mat_k
      module pure function diag_rdp_mat_k(A,k) result(res)
        real(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(dp) :: res(minval(shape(A))-abs(k))
      end function diag_rdp_mat_k
      module pure function diag_csp_mat_k(A,k) result(res)
        complex(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(sp) :: res(minval(shape(A))-abs(k))
      end function diag_csp_mat_k
      module pure function diag_cdp_mat_k(A,k) result(res)
        complex(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(dp) :: res(minval(shape(A))-abs(k))
      end function diag_cdp_mat_k
      module pure function diag_iint8_mat_k(A,k) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int8) :: res(minval(shape(A))-abs(k))
      end function diag_iint8_mat_k
      module pure function diag_iint16_mat_k(A,k) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int16) :: res(minval(shape(A))-abs(k))
      end function diag_iint16_mat_k
      module pure function diag_iint32_mat_k(A,k) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int32) :: res(minval(shape(A))-abs(k))
      end function diag_iint32_mat_k
      module pure function diag_iint64_mat_k(A,k) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int64) :: res(minval(shape(A))-abs(k))
      end function diag_iint64_mat_k
  end interface


  ! Matrix trace
  interface trace
    !! version: experimental
    !!
    !! Computes the trace of a matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! trace-trace-of-a-matrix))
      module procedure trace_rsp
      module procedure trace_rdp
      module procedure trace_csp
      module procedure trace_cdp
      module procedure trace_iint8
      module procedure trace_iint16
      module procedure trace_iint32
      module procedure trace_iint64
  end interface

  ! Identity matrix 
  interface eye
    !! version: experimental
    !!
    !! Constructs the identity matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#eye-construct-the-identity-matrix))    
      module procedure eye_rsp
      module procedure eye_rdp
      module procedure eye_csp
      module procedure eye_cdp
      module procedure eye_iint8
      module procedure eye_iint16
      module procedure eye_iint32
      module procedure eye_iint64
  end interface eye

  ! Outer product (of two vectors)
  interface outer_product
    !! version: experimental
    !!
    !! Computes the outer product of two vectors, returning a rank-2 array
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! outer_product-computes-the-outer-product-of-two-vectors))
      pure module function outer_product_rsp(u, v) result(res)
        real(sp), intent(in) :: u(:), v(:)
        real(sp) :: res(size(u),size(v))
      end function outer_product_rsp
      pure module function outer_product_rdp(u, v) result(res)
        real(dp), intent(in) :: u(:), v(:)
        real(dp) :: res(size(u),size(v))
      end function outer_product_rdp
      pure module function outer_product_csp(u, v) result(res)
        complex(sp), intent(in) :: u(:), v(:)
        complex(sp) :: res(size(u),size(v))
      end function outer_product_csp
      pure module function outer_product_cdp(u, v) result(res)
        complex(dp), intent(in) :: u(:), v(:)
        complex(dp) :: res(size(u),size(v))
      end function outer_product_cdp
      pure module function outer_product_iint8(u, v) result(res)
        integer(int8), intent(in) :: u(:), v(:)
        integer(int8) :: res(size(u),size(v))
      end function outer_product_iint8
      pure module function outer_product_iint16(u, v) result(res)
        integer(int16), intent(in) :: u(:), v(:)
        integer(int16) :: res(size(u),size(v))
      end function outer_product_iint16
      pure module function outer_product_iint32(u, v) result(res)
        integer(int32), intent(in) :: u(:), v(:)
        integer(int32) :: res(size(u),size(v))
      end function outer_product_iint32
      pure module function outer_product_iint64(u, v) result(res)
        integer(int64), intent(in) :: u(:), v(:)
        integer(int64) :: res(size(u),size(v))
      end function outer_product_iint64
  end interface outer_product

  interface kronecker_product
    !! version: experimental
    !!
    !! Computes the Kronecker product of two arrays of size M1xN1, and of M2xN2, returning an (M1*M2)x(N1*N2) array
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! kronecker_product-computes-the-kronecker-product-of-two-matrices))
      pure module function kronecker_product_rsp(A, B) result(C)
        real(sp), intent(in) :: A(:,:), B(:,:)
        real(sp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_rsp
      pure module function kronecker_product_rdp(A, B) result(C)
        real(dp), intent(in) :: A(:,:), B(:,:)
        real(dp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_rdp
      pure module function kronecker_product_csp(A, B) result(C)
        complex(sp), intent(in) :: A(:,:), B(:,:)
        complex(sp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_csp
      pure module function kronecker_product_cdp(A, B) result(C)
        complex(dp), intent(in) :: A(:,:), B(:,:)
        complex(dp) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_cdp
      pure module function kronecker_product_iint8(A, B) result(C)
        integer(int8), intent(in) :: A(:,:), B(:,:)
        integer(int8) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint8
      pure module function kronecker_product_iint16(A, B) result(C)
        integer(int16), intent(in) :: A(:,:), B(:,:)
        integer(int16) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint16
      pure module function kronecker_product_iint32(A, B) result(C)
        integer(int32), intent(in) :: A(:,:), B(:,:)
        integer(int32) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint32
      pure module function kronecker_product_iint64(A, B) result(C)
        integer(int64), intent(in) :: A(:,:), B(:,:)
        integer(int64) :: C(size(A,dim=1)*size(B,dim=1),size(A,dim=2)*size(B,dim=2))
      end function kronecker_product_iint64
  end interface kronecker_product


  ! Cross product (of two vectors)
  interface cross_product
    !! version: experimental
    !!
    !! Computes the cross product of two vectors, returning a rank-1 and size-3 array
    !! ([Specification](../page/specs/stdlib_linalg.html#cross_product-computes-the-cross-product-of-two-3-d-vectors))
      pure module function cross_product_rsp(a, b) result(res)
        real(sp), intent(in) :: a(3), b(3)
        real(sp) :: res(3)
      end function cross_product_rsp
      pure module function cross_product_rdp(a, b) result(res)
        real(dp), intent(in) :: a(3), b(3)
        real(dp) :: res(3)
      end function cross_product_rdp
      pure module function cross_product_csp(a, b) result(res)
        complex(sp), intent(in) :: a(3), b(3)
        complex(sp) :: res(3)
      end function cross_product_csp
      pure module function cross_product_cdp(a, b) result(res)
        complex(dp), intent(in) :: a(3), b(3)
        complex(dp) :: res(3)
      end function cross_product_cdp
      pure module function cross_product_iint8(a, b) result(res)
        integer(int8), intent(in) :: a(3), b(3)
        integer(int8) :: res(3)
      end function cross_product_iint8
      pure module function cross_product_iint16(a, b) result(res)
        integer(int16), intent(in) :: a(3), b(3)
        integer(int16) :: res(3)
      end function cross_product_iint16
      pure module function cross_product_iint32(a, b) result(res)
        integer(int32), intent(in) :: a(3), b(3)
        integer(int32) :: res(3)
      end function cross_product_iint32
      pure module function cross_product_iint64(a, b) result(res)
        integer(int64), intent(in) :: a(3), b(3)
        integer(int64) :: res(3)
      end function cross_product_iint64
  end interface cross_product


  ! Check for squareness
  interface is_square
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is square
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_square-checks-if-a-matrix-is-square))
      module procedure is_square_rsp
      module procedure is_square_rdp
      module procedure is_square_csp
      module procedure is_square_cdp
      module procedure is_square_iint8
      module procedure is_square_iint16
      module procedure is_square_iint32
      module procedure is_square_iint64
  end interface is_square


  ! Check for diagonality
  interface is_diagonal
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is diagonal
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_diagonal-checks-if-a-matrix-is-diagonal))
      module procedure is_diagonal_rsp
      module procedure is_diagonal_rdp
      module procedure is_diagonal_csp
      module procedure is_diagonal_cdp
      module procedure is_diagonal_iint8
      module procedure is_diagonal_iint16
      module procedure is_diagonal_iint32
      module procedure is_diagonal_iint64
  end interface is_diagonal


  ! Check for symmetry
  interface is_symmetric
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is symmetric
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_symmetric-checks-if-a-matrix-is-symmetric))
      module procedure is_symmetric_rsp
      module procedure is_symmetric_rdp
      module procedure is_symmetric_csp
      module procedure is_symmetric_cdp
      module procedure is_symmetric_iint8
      module procedure is_symmetric_iint16
      module procedure is_symmetric_iint32
      module procedure is_symmetric_iint64
  end interface is_symmetric


  ! Check for skew-symmetry
  interface is_skew_symmetric
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is skew-symmetric
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_skew_symmetric-checks-if-a-matrix-is-skew-symmetric))
      module procedure is_skew_symmetric_rsp
      module procedure is_skew_symmetric_rdp
      module procedure is_skew_symmetric_csp
      module procedure is_skew_symmetric_cdp
      module procedure is_skew_symmetric_iint8
      module procedure is_skew_symmetric_iint16
      module procedure is_skew_symmetric_iint32
      module procedure is_skew_symmetric_iint64
  end interface is_skew_symmetric


  ! Check for Hermiticity
  interface is_hermitian
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is Hermitian
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_hermitian-checks-if-a-matrix-is-hermitian))
      module procedure is_hermitian_rsp
      module procedure is_hermitian_rdp
      module procedure is_hermitian_csp
      module procedure is_hermitian_cdp
      module procedure is_hermitian_iint8
      module procedure is_hermitian_iint16
      module procedure is_hermitian_iint32
      module procedure is_hermitian_iint64
  end interface is_hermitian

  interface hermitian
    !! version: experimental
    !!
    !! Computes the Hermitian version of a rank-2 matrix.
    !! For complex matrices, this returns `conjg(transpose(a))`.
    !! For real or integer matrices, this returns `transpose(a)`.
    !!
    !! Usage:
    !! ```
    !! A  = reshape([(1, 2), (3, 4), (5, 6), (7, 8)], [2, 2])
    !! AH = hermitian(A)
    !! ```
    !!
    !! [Specification](../page/specs/stdlib_linalg.html#hermitian-compute-the-hermitian-version-of-a-rank-2-matrix)
    !!

    pure module function hermitian_rsp(a) result(ah)
        real(sp), intent(in) :: a(:,:)
        real(sp) :: ah(size(a, 2), size(a, 1))
    end function hermitian_rsp
    pure module function hermitian_rdp(a) result(ah)
        real(dp), intent(in) :: a(:,:)
        real(dp) :: ah(size(a, 2), size(a, 1))
    end function hermitian_rdp
    pure module function hermitian_csp(a) result(ah)
        complex(sp), intent(in) :: a(:,:)
        complex(sp) :: ah(size(a, 2), size(a, 1))
    end function hermitian_csp
    pure module function hermitian_cdp(a) result(ah)
        complex(dp), intent(in) :: a(:,:)
        complex(dp) :: ah(size(a, 2), size(a, 1))
    end function hermitian_cdp
    pure module function hermitian_iint8(a) result(ah)
        integer(int8), intent(in) :: a(:,:)
        integer(int8) :: ah(size(a, 2), size(a, 1))
    end function hermitian_iint8
    pure module function hermitian_iint16(a) result(ah)
        integer(int16), intent(in) :: a(:,:)
        integer(int16) :: ah(size(a, 2), size(a, 1))
    end function hermitian_iint16
    pure module function hermitian_iint32(a) result(ah)
        integer(int32), intent(in) :: a(:,:)
        integer(int32) :: ah(size(a, 2), size(a, 1))
    end function hermitian_iint32
    pure module function hermitian_iint64(a) result(ah)
        integer(int64), intent(in) :: a(:,:)
        integer(int64) :: ah(size(a, 2), size(a, 1))
    end function hermitian_iint64

  end interface hermitian


  ! Check for triangularity
  interface is_triangular
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is triangular
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_triangular-checks-if-a-matrix-is-triangular))
      module procedure is_triangular_rsp
      module procedure is_triangular_rdp
      module procedure is_triangular_csp
      module procedure is_triangular_cdp
      module procedure is_triangular_iint8
      module procedure is_triangular_iint16
      module procedure is_triangular_iint32
      module procedure is_triangular_iint64
  end interface is_triangular
  

  ! Check for matrix being Hessenberg
  interface is_hessenberg
    !! version: experimental
    !!
    !! Checks if a matrix (rank-2 array) is Hessenberg
    !! ([Specification](../page/specs/stdlib_linalg.html#
    !! is_hessenberg-checks-if-a-matrix-is-hessenberg))
      module procedure is_Hessenberg_rsp
      module procedure is_Hessenberg_rdp
      module procedure is_Hessenberg_csp
      module procedure is_Hessenberg_cdp
      module procedure is_Hessenberg_iint8
      module procedure is_Hessenberg_iint16
      module procedure is_Hessenberg_iint32
      module procedure is_Hessenberg_iint64
  end interface is_hessenberg

  ! Solve linear system system Ax=b.
  interface solve
     !! version: experimental 
     !!
     !! Solves the linear system \( A \cdot x = b \) for the unknown vector \( x \) from a square matrix \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#solve-solves-a-linear-matrix-equation-or-a-linear-system-of-equations))
     !!
     !!### Summary 
     !! Interface for solving a linear system arising from a general matrix.
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the solution of a linear matrix system.
     !! Supported data types include `real` and `complex`. No assumption is made on the matrix 
     !! structure. 
     !! The function can solve simultaneously either one (from a 1-d right-hand-side vector `b(:)`) 
     !! or several (from a 2-d right-hand-side vector `b(:,:)`) systems.
     !! 
     !!@note The solution is based on LAPACK's generic LU decomposition based solvers `*GESV`.
     !!    
     module function stdlib_linalg_s_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
     end function stdlib_linalg_s_solve_one
     pure module function stdlib_linalg_s_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
     end function stdlib_linalg_s_pure_solve_one
     module function stdlib_linalg_d_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
     end function stdlib_linalg_d_solve_one
     pure module function stdlib_linalg_d_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
     end function stdlib_linalg_d_pure_solve_one
     module function stdlib_linalg_c_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
     end function stdlib_linalg_c_solve_one
     pure module function stdlib_linalg_c_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
     end function stdlib_linalg_c_pure_solve_one
     module function stdlib_linalg_z_solve_one(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
     end function stdlib_linalg_z_solve_one
     pure module function stdlib_linalg_z_pure_solve_one(a,b) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
     end function stdlib_linalg_z_pure_solve_one
     module function stdlib_linalg_s_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_s_solve_many
     pure module function stdlib_linalg_s_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_s_pure_solve_many
     module function stdlib_linalg_d_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_d_solve_many
     pure module function stdlib_linalg_d_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_d_pure_solve_many
     module function stdlib_linalg_c_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_c_solve_many
     pure module function stdlib_linalg_c_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
     end function stdlib_linalg_c_pure_solve_many
     module function stdlib_linalg_z_solve_many(a,b,overwrite_a,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)     
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_z_solve_many
     pure module function stdlib_linalg_z_pure_solve_many(a,b) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
     end function stdlib_linalg_z_pure_solve_many
  end interface solve

  ! Solve linear system Ax = b using LU decomposition (subroutine interface).
  interface solve_lu
     !! version: experimental 
     !!
     !! Solves the linear system \( A \cdot x = b \) for the unknown vector \( x \) from a square matrix \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#solve-lu-solves-a-linear-matrix-equation-or-a-linear-system-of-equations-subroutine-interface))
     !!
     !!### Summary 
     !! Subroutine interface for solving a linear system using LU decomposition.
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the solution of a linear matrix system using
     !! a subroutine. Supported data types include `real` and `complex`. No assumption is made on the matrix 
     !! structure. Preallocated space for the solution vector `x` is user-provided, and it may be provided
     !! for the array of pivot indices, `pivot`. If all pre-allocated work spaces are provided, no internal 
     !! memory allocations take place when using this interface.     
     !! The function can solve simultaneously either one (from a 1-d right-hand-side vector `b(:)`) 
     !! or several (from a 2-d right-hand-side vector `b(:,:)`) systems.
     !! 
     !!@note The solution is based on LAPACK's generic LU decomposition based solvers `*GESV`.
     !!        
     pure module subroutine stdlib_linalg_s_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(sp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_s_solve_lu_one
     pure module subroutine stdlib_linalg_d_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(dp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_d_solve_lu_one
     pure module subroutine stdlib_linalg_c_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(sp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_c_solve_lu_one
     pure module subroutine stdlib_linalg_z_solve_lu_one(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(dp), intent(inout), contiguous, target :: x(:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_z_solve_lu_one
     pure module subroutine stdlib_linalg_s_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(sp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_s_solve_lu_many
     pure module subroutine stdlib_linalg_d_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         real(dp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_d_solve_lu_many
     pure module subroutine stdlib_linalg_c_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(sp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_c_solve_lu_many
     pure module subroutine stdlib_linalg_z_solve_lu_many(a,b,x,pivot,overwrite_a,err)     
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]     
         complex(dp), intent(inout), contiguous, target :: x(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
     end subroutine stdlib_linalg_z_solve_lu_many
  end interface solve_lu     
     
  ! Least squares solution to system Ax=b, i.e. such that the 2-norm abs(b-Ax) is minimized.
  interface lstsq
    !! version: experimental 
    !!
    !! Computes the squares solution to system \( A \cdot x = b \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#lstsq-computes-the-least-squares-solution-to-a-linear-matrix-equation))
    !! 
    !!### Summary 
    !! Interface for computing least squares, i.e. the 2-norm \( || (b-A \cdot x ||_2 \) minimizing solution.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the least squares of a linear matrix system.
    !! Supported data types include `real` and `complex`.
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GELSD` methods.
    !! 
      module function stdlib_linalg_s_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:)
      end function stdlib_linalg_s_lstsq_one
      module function stdlib_linalg_d_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:)
      end function stdlib_linalg_d_lstsq_one
      module function stdlib_linalg_c_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:)
      end function stdlib_linalg_c_lstsq_one
      module function stdlib_linalg_z_lstsq_one(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:)
      end function stdlib_linalg_z_lstsq_one
      module function stdlib_linalg_s_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), allocatable, target :: x(:,:)
      end function stdlib_linalg_s_lstsq_many
      module function stdlib_linalg_d_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), allocatable, target :: x(:,:)
      end function stdlib_linalg_d_lstsq_many
      module function stdlib_linalg_c_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), allocatable, target :: x(:,:)
      end function stdlib_linalg_c_lstsq_many
      module function stdlib_linalg_z_lstsq_many(a,b,cond,overwrite_a,rank,err) result(x)
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), allocatable, target :: x(:,:)
      end function stdlib_linalg_z_lstsq_many
  end interface lstsq

  ! Least squares solution to system Ax=b, i.e. such that the 2-norm abs(b-Ax) is minimized.
  interface solve_lstsq
    !! version: experimental 
    !!
    !! Computes the squares solution to system \( A \cdot x = b \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#solve-lstsq-compute-the-least-squares-solution-to-a-linear-matrix-equation-subroutine-interface))
    !! 
    !!### Summary 
    !! Subroutine interface for computing least squares, i.e. the 2-norm \( || (b-A \cdot x ||_2 \) minimizing solution.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the least squares of a linear matrix system using 
    !! a subroutine. Supported data types include `real` and `complex`. If pre-allocated work spaces 
    !! are provided, no internal memory allocations take place when using this interface.
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GELSD` methods.
    !! 
      module subroutine stdlib_linalg_s_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_s_solve_lstsq_one
      module subroutine stdlib_linalg_d_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_d_solve_lstsq_one
      module subroutine stdlib_linalg_c_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(sp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_c_solve_lstsq_one
      module subroutine stdlib_linalg_z_solve_lstsq_one(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(dp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_z_solve_lstsq_one
      module subroutine stdlib_linalg_s_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(sp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_s_solve_lstsq_many
      module subroutine stdlib_linalg_d_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         real(dp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_d_solve_lstsq_many
      module subroutine stdlib_linalg_c_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(sp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(sp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(sp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(sp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(sp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_c_solve_lstsq_many
      module subroutine stdlib_linalg_z_solve_lstsq_many(a,b,x,real_storage,int_storage,&
                        cmpl_storage,cond,singvals,overwrite_a,rank,err) 
         !> Input matrix a[n,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Result array/matrix x[n] or x[n,nrhs]
         complex(dp), intent(inout), contiguous, target :: x(:,:)     
         !> [optional] real working storage space
         real(dp), optional, intent(inout), target :: real_storage(:)
         !> [optional] integer working storage space
         integer(ilp), optional, intent(inout), target :: int_storage(:)
         !> [optional] complex working storage space
         complex(dp), optional, intent(inout), target :: cmpl_storage(:)                  
         !> [optional] cutoff for rank evaluation: singular values s(i)<=cond*maxval(s) are considered 0.
         real(dp), optional, intent(in) :: cond
         !> [optional] list of singular values [min(m,n)], in descending magnitude order, returned by the SVD
         real(dp), optional, intent(out), target :: singvals(:)                  
         !> [optional] Can A,b data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Return rank of A
         integer(ilp), optional, intent(out) :: rank
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_z_solve_lstsq_many
  end interface solve_lstsq

  ! Return the working array space required by the least squares solver
  interface lstsq_space
    !! version: experimental 
    !!
    !! Computes the integer, real [, complex] working space required by the least-squares solver
    !! ([Specification](../page/specs/stdlib_linalg.html#lstsq-space-compute-internal-working-space-requirements-for-the-least-squares-solver))
    !! 
    !!### Description
    !! 
    !! This interface provides sizes of integer, real [, complex] working spaces required by the 
    !! least-squares solver. These sizes can be used to pre-allocated working arrays in case several 
    !! repeated least-squares solutions to a same system are sought. If pre-allocated working arrays 
    !! are provided, no internal allocations will take place.
    !! 
      pure module subroutine stdlib_linalg_s_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_s_lstsq_space_one
      pure module subroutine stdlib_linalg_d_lstsq_space_one(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_d_lstsq_space_one
      pure module subroutine stdlib_linalg_c_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_c_lstsq_space_one
      pure module subroutine stdlib_linalg_z_lstsq_space_one(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_z_lstsq_space_one
      pure module subroutine stdlib_linalg_s_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(sp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_s_lstsq_space_many
      pure module subroutine stdlib_linalg_d_lstsq_space_many(a,b,lrwork,liwork)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         real(dp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork         
      end subroutine stdlib_linalg_d_lstsq_space_many
      pure module subroutine stdlib_linalg_c_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(sp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_c_lstsq_space_many
      pure module subroutine stdlib_linalg_z_lstsq_space_many(a,b,lrwork,liwork,lcwork)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Right hand side vector or array, b[n] or b[n,nrhs]
         complex(dp), intent(in) :: b(:,:)
         !> Size of the working space arrays                
         integer(ilp), intent(out) :: lrwork,liwork,lcwork         
      end subroutine stdlib_linalg_z_lstsq_space_many
  end interface lstsq_space

  ! QR factorization of rank-2 array A
  interface qr
    !! version: experimental 
    !!
    !! Computes the QR factorization of matrix \( A = Q R \). 
    !! ([Specification](../page/specs/stdlib_linalg.html#qr-compute-the-qr-factorization-of-a-matrix))
    !! 
    !!### Summary 
    !! Compute the QR factorization of a `real` or `complex` matrix: \( A = Q R \), where \( Q \)  is orthonormal 
    !! and \( R \) is upper-triangular. Matrix \( A \) has size `[m,n]`, with \( m\ge n \). 
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the QR factorization of a matrix. 
    !! Supported data types include `real` and `complex`. If a pre-allocated work space 
    !! is provided, no internal memory allocations take place when using this interface.
    !!
    !! Given `k = min(m,n)`, one can write \( A = \( Q_1  Q_2 \) \cdot \( \frac{R_1}{0}\) \). 
    !! The user may want the full problem (provide `shape(Q)==[m,m]`, `shape(R)==[m,n]`) or the reduced  
    !! problem only: \( A = Q_1 R_1 \) (provide `shape(Q)==[m,k]`, `shape(R)==[k,n]`).
    !! 
    !!@note The solution is based on LAPACK's QR factorization (`*GEQRF`) and ordered matrix output (`*ORGQR`, `*UNGQR`). 
    !!     
      pure module subroutine stdlib_linalg_s_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         real(sp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         real(sp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         real(sp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_s_qr
      pure module subroutine stdlib_linalg_d_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         real(dp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         real(dp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         real(dp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_d_qr
      pure module subroutine stdlib_linalg_c_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         complex(sp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         complex(sp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         complex(sp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_c_qr
      pure module subroutine stdlib_linalg_z_qr(a,q,r,overwrite_a,storage,err) 
         !> Input matrix a[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Orthogonal matrix Q ([m,m], or [m,k] if reduced)
         complex(dp), intent(out), contiguous, target :: q(:,:)
         !> Upper triangular matrix R ([m,n], or [k,n] if reduced)
         complex(dp), intent(out), contiguous, target :: r(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Provide pre-allocated workspace, size to be checked with qr_space
         complex(dp), intent(out), optional, target :: storage(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_z_qr
  end interface qr

  ! Return the working array space required by the QR factorization solver
  interface qr_space
    !! version: experimental 
    !!
    !! Computes the working array space required by the QR factorization solver
    !! ([Specification](../page/specs/stdlib_linalg.html#qr-space-compute-internal-working-space-requirements-for-the-qr-factorization))
    !! 
    !!### Description
    !! 
    !! This interface returns the size of the `real` or `complex` working storage required by the 
    !! QR factorization solver. The working size only depends on the kind (`real` or `complex`) and size of
    !! the matrix being factorized. Storage size can be used to pre-allocate a working array in case several 
    !! repeated QR factorizations to a same-size matrix are sought. If pre-allocated working arrays 
    !! are provided, no internal allocations will take place during the factorization.
    !!     
      pure module subroutine get_qr_s_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_qr_s_workspace
      pure module subroutine get_qr_d_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_qr_d_workspace
      pure module subroutine get_qr_c_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_qr_c_workspace
      pure module subroutine get_qr_z_workspace(a,lwork,err)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Minimum workspace size for both operations
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_qr_z_workspace
  end interface qr_space
 
  ! Schur decomposition of rank-2 array A
  interface schur
    !! version: experimental
    !!
    !! Computes the Schur decomposition of matrix \( A = Z T Z^H \).
    !! ([Specification](../page/specs/stdlib_linalg.html#schur-compute-the-schur-decomposition-of-a-matrix))
    !!
    !!### Summary
    !! Compute the Schur decomposition of a `real` or `complex` matrix: \( A = Z T Z^H \), where \( Z \) is
    !! orthonormal/unitary and \( T \) is upper-triangular or quasi-upper-triangular. Matrix \( A \) has size `[m,m]`.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the Schur decomposition of a matrix.
    !! Supported data types include `real` and `complex`. If a pre-allocated workspace is provided, no internal 
    !! memory allocations take place when using this interface.
    !!
    !! The output matrix \( T \) is upper-triangular for `complex` input matrices and quasi-upper-triangular
    !! for `real` input matrices (with possible \( 2 \times 2 \) blocks on the diagonal).
    !!
    !!@note The solution is based on LAPACK's Schur decomposition routines (`*GEES`). 
    !! 
      module subroutine stdlib_linalg_s_schur(a, t, z, eigvals, overwrite_a, storage, err)
         !> Input matrix a[m,m]
         real(sp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         real(sp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         real(sp), optional, intent(out), contiguous, target :: z(:,:)
         !> [optional] Output eigenvalues that appear on the diagonal of T
         complex(sp), optional, intent(out), contiguous, target :: eigvals(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a                 
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space         
         real(sp), optional, intent(inout), target :: storage(:)
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_s_schur
      
      ! Schur decomposition subroutine: real eigenvalue interface
      module subroutine stdlib_linalg_real_eig_s_schur(a,t,z,eigvals,overwrite_a,storage,err)
         !> Input matrix a[m,m]
         real(sp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         real(sp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         real(sp), optional, intent(out), contiguous, target :: z(:,:)
         !> Output real eigenvalues that appear on the diagonal of T
         real(sp), intent(out), contiguous, target :: eigvals(:)
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space
         real(sp), optional, intent(inout), target :: storage(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a        
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err        
      end subroutine stdlib_linalg_real_eig_s_schur 
      module subroutine stdlib_linalg_d_schur(a, t, z, eigvals, overwrite_a, storage, err)
         !> Input matrix a[m,m]
         real(dp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         real(dp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         real(dp), optional, intent(out), contiguous, target :: z(:,:)
         !> [optional] Output eigenvalues that appear on the diagonal of T
         complex(dp), optional, intent(out), contiguous, target :: eigvals(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a                 
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space         
         real(dp), optional, intent(inout), target :: storage(:)
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_d_schur
      
      ! Schur decomposition subroutine: real eigenvalue interface
      module subroutine stdlib_linalg_real_eig_d_schur(a,t,z,eigvals,overwrite_a,storage,err)
         !> Input matrix a[m,m]
         real(dp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         real(dp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         real(dp), optional, intent(out), contiguous, target :: z(:,:)
         !> Output real eigenvalues that appear on the diagonal of T
         real(dp), intent(out), contiguous, target :: eigvals(:)
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space
         real(dp), optional, intent(inout), target :: storage(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a        
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err        
      end subroutine stdlib_linalg_real_eig_d_schur 
      module subroutine stdlib_linalg_c_schur(a, t, z, eigvals, overwrite_a, storage, err)
         !> Input matrix a[m,m]
         complex(sp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         complex(sp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         complex(sp), optional, intent(out), contiguous, target :: z(:,:)
         !> [optional] Output eigenvalues that appear on the diagonal of T
         complex(sp), optional, intent(out), contiguous, target :: eigvals(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a                 
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space         
         complex(sp), optional, intent(inout), target :: storage(:)
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_c_schur
      
      ! Schur decomposition subroutine: real eigenvalue interface
      module subroutine stdlib_linalg_real_eig_c_schur(a,t,z,eigvals,overwrite_a,storage,err)
         !> Input matrix a[m,m]
         complex(sp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         complex(sp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         complex(sp), optional, intent(out), contiguous, target :: z(:,:)
         !> Output real eigenvalues that appear on the diagonal of T
         real(sp), intent(out), contiguous, target :: eigvals(:)
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space
         complex(sp), optional, intent(inout), target :: storage(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a        
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err        
      end subroutine stdlib_linalg_real_eig_c_schur 
      module subroutine stdlib_linalg_z_schur(a, t, z, eigvals, overwrite_a, storage, err)
         !> Input matrix a[m,m]
         complex(dp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         complex(dp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         complex(dp), optional, intent(out), contiguous, target :: z(:,:)
         !> [optional] Output eigenvalues that appear on the diagonal of T
         complex(dp), optional, intent(out), contiguous, target :: eigvals(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a                 
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space         
         complex(dp), optional, intent(inout), target :: storage(:)
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_z_schur
      
      ! Schur decomposition subroutine: real eigenvalue interface
      module subroutine stdlib_linalg_real_eig_z_schur(a,t,z,eigvals,overwrite_a,storage,err)
         !> Input matrix a[m,m]
         complex(dp), intent(inout), target :: a(:,:)
         !> Schur form of A: upper-triangular or quasi-upper-triangular matrix T
         complex(dp), intent(out), contiguous, target :: t(:,:)
         !> Unitary/orthonormal transformation matrix Z
         complex(dp), optional, intent(out), contiguous, target :: z(:,:)
         !> Output real eigenvalues that appear on the diagonal of T
         real(dp), intent(out), contiguous, target :: eigvals(:)
         !> [optional] Provide pre-allocated workspace, size to be checked with schur_space
         complex(dp), optional, intent(inout), target :: storage(:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a        
         !> [optional] State return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err        
      end subroutine stdlib_linalg_real_eig_z_schur 
  end interface schur

  ! Return the working array space required by the Schur decomposition solver
  interface schur_space
    !! version: experimental
    !!
    !! Computes the working array space required by the Schur decomposition solver
    !! ([Specification](../page/specs/stdlib_linalg.html#schur-space-compute-internal-working-space-requirements-for-the-schur-decomposition))
    !!
    !!### Description
    !! 
    !! This interface returns the size of the `real` or `complex` working storage required by the 
    !! Schur decomposition solver. The working size only depends on the kind (`real` or `complex`) and size of
    !! the matrix being decomposed. Storage size can be used to pre-allocate a working array in case several 
    !! repeated Schur decompositions of same-size matrices are sought. If pre-allocated working arrays 
    !! are provided, no internal allocations will take place during the decomposition.
    !!     
      module subroutine get_schur_s_workspace(a,lwork,err)
         !> Input matrix a[m,m]
         real(sp), intent(in), target :: a(:,:)
         !> Minimum workspace size for the decomposition operation
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_schur_s_workspace
      module subroutine get_schur_d_workspace(a,lwork,err)
         !> Input matrix a[m,m]
         real(dp), intent(in), target :: a(:,:)
         !> Minimum workspace size for the decomposition operation
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_schur_d_workspace
      module subroutine get_schur_c_workspace(a,lwork,err)
         !> Input matrix a[m,m]
         complex(sp), intent(in), target :: a(:,:)
         !> Minimum workspace size for the decomposition operation
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_schur_c_workspace
      module subroutine get_schur_z_workspace(a,lwork,err)
         !> Input matrix a[m,m]
         complex(dp), intent(in), target :: a(:,:)
         !> Minimum workspace size for the decomposition operation
         integer(ilp), intent(out) :: lwork
         !> State return flag. Returns an error if the query failed
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine get_schur_z_workspace
  end interface schur_space

  interface det
    !! version: experimental 
    !!
    !! Computes the determinant of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#det-computes-the-determinant-of-a-square-matrix))
    !! 
    !!### Summary 
    !! Interface for computing matrix determinant.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the determinant of a matrix.
    !! Supported data types include `real` and `complex`.
    !! 
    !!@note The provided functions are intended for square matrices only.          
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
    !! 
    !!### Example
    !!
    !!```fortran
    !!
    !!    real(sp) :: a(3,3), d
    !!    type(linalg_state_type) :: state  
    !!    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    !!
    !!    ! ...
    !!    d = det(a,err=state)
    !!    if (state%ok()) then 
    !!       print *, 'Success! det=',d
    !!    else
    !!       print *, state%print()
    !!    endif
    !!    ! ...
    !!```
    !!     
    module procedure stdlib_linalg_rspdeterminant
    module procedure stdlib_linalg_pure_rspdeterminant
    module procedure stdlib_linalg_rdpdeterminant
    module procedure stdlib_linalg_pure_rdpdeterminant
    module procedure stdlib_linalg_cspdeterminant
    module procedure stdlib_linalg_pure_cspdeterminant
    module procedure stdlib_linalg_cdpdeterminant
    module procedure stdlib_linalg_pure_cdpdeterminant
  end interface det

  interface operator(.det.)
    !! version: experimental 
    !!
    !! Determinant operator of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#det-determinant-operator-of-a-square-matrix))
    !!
    !!### Summary
    !! Pure operator interface for computing matrix determinant.
    !!
    !!### Description
    !! 
    !! This pure operator interface provides a convenient way to compute the determinant of a matrix.
    !! Supported data types include real and complex.
    !!
    !!@note The provided functions are intended for square matrices.
    !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
    !!
    !!### Example
    !!
    !!```fortran
    !!
    !!    ! ...
    !!    real(sp) :: matrix(3,3), d
    !!    matrix = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
    !!    d = .det.matrix
    !!    ! ...
    !! 
    !!```
    !     
    module procedure stdlib_linalg_pure_rspdeterminant
    module procedure stdlib_linalg_pure_rdpdeterminant
    module procedure stdlib_linalg_pure_cspdeterminant
    module procedure stdlib_linalg_pure_cdpdeterminant
  end interface operator(.det.)

  interface
    module function stdlib_linalg_rspdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        real(sp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        real(sp) :: det
    end function stdlib_linalg_rspdeterminant
    pure module function stdlib_linalg_pure_rspdeterminant(a) result(det)
        !> Input matrix a[m,n]
        real(sp), intent(in) :: a(:,:)
        !> Matrix determinant
        real(sp) :: det                
    end function stdlib_linalg_pure_rspdeterminant
    module function stdlib_linalg_rdpdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        real(dp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        real(dp) :: det
    end function stdlib_linalg_rdpdeterminant
    pure module function stdlib_linalg_pure_rdpdeterminant(a) result(det)
        !> Input matrix a[m,n]
        real(dp), intent(in) :: a(:,:)
        !> Matrix determinant
        real(dp) :: det                
    end function stdlib_linalg_pure_rdpdeterminant
    module function stdlib_linalg_cspdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        complex(sp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        complex(sp) :: det
    end function stdlib_linalg_cspdeterminant
    pure module function stdlib_linalg_pure_cspdeterminant(a) result(det)
        !> Input matrix a[m,n]
        complex(sp), intent(in) :: a(:,:)
        !> Matrix determinant
        complex(sp) :: det                
    end function stdlib_linalg_pure_cspdeterminant
    module function stdlib_linalg_cdpdeterminant(a,overwrite_a,err) result(det)
        !> Input matrix a[m,n]
        complex(dp), intent(inout), target :: a(:,:)
        !> [optional] Can A data be overwritten and destroyed?
        logical(lk), optional, intent(in) :: overwrite_a
        !> State return flag. 
        type(linalg_state_type), intent(out) :: err
        !> Matrix determinant
        complex(dp) :: det
    end function stdlib_linalg_cdpdeterminant
    pure module function stdlib_linalg_pure_cdpdeterminant(a) result(det)
        !> Input matrix a[m,n]
        complex(dp), intent(in) :: a(:,:)
        !> Matrix determinant
        complex(dp) :: det                
    end function stdlib_linalg_pure_cdpdeterminant
  end interface  

  ! Matrix Inverse: Function interface
  interface inv
    !! version: experimental 
    !!
    !! Inverse of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#inv-inverse-of-a-square-matrix))
    !!
    !!### Summary
    !! This interface provides methods for computing the inverse of a square `real` or `complex` matrix.
    !! The inverse \( A^{-1} \) is defined such that \( A \cdot A^{-1} = A^{-1} \cdot A = I_n \).
    !!
    !!### Description
    !!     
    !! This function interface provides methods that return the inverse of a square matrix.    
    !! Supported data types include `real` and `complex`. 
    !! The inverse matrix \( A^{-1} \) is returned as a function result. 
    !! Exceptions are raised in case of singular matrix or invalid size, and trigger an `error stop` if 
    !! the state flag `err` is not provided. 
    !!
    !!@note The provided functions are intended for square matrices.
    !!       
    module function stdlib_linalg_inverse_s(a,err) result(inva)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Output matrix inverse
         real(sp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end function stdlib_linalg_inverse_s
    module function stdlib_linalg_inverse_d(a,err) result(inva)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Output matrix inverse
         real(dp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end function stdlib_linalg_inverse_d
    module function stdlib_linalg_inverse_c(a,err) result(inva)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Output matrix inverse
         complex(sp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end function stdlib_linalg_inverse_c
    module function stdlib_linalg_inverse_z(a,err) result(inva)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Output matrix inverse
         complex(dp), allocatable :: inva(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end function stdlib_linalg_inverse_z
  end interface inv

  ! Matrix Inverse: Subroutine interface - in-place inversion
  interface invert
    !! version: experimental 
    !!
    !! Inversion of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#invert-inversion-of-a-square-matrix))
    !!
    !!### Summary
    !! This interface provides methods for inverting a square `real` or `complex` matrix in-place.
    !! The inverse \( A^{-1} \) is defined such that \( A \cdot A^{-1} = A^{-1} \cdot A = I_n \).
    !!
    !!### Description
    !!     
    !! This subroutine interface provides a way to compute the inverse of a matrix.    
    !! Supported data types include `real` and `complex`. 
    !! The user may provide a unique matrix argument `a`. In this case, `a` is replaced by the inverse matrix.
    !! on output. Otherwise, one may provide two separate arguments: an input matrix `a` and an output matrix 
    !! `inva`. In this case, `a` will not be modified, and the inverse is returned in `inva`.
    !! Pre-allocated storage may be provided for the array of pivot indices, `pivot`. If all pre-allocated 
    !! work spaces are provided, no internal memory allocations take place when using this interface.             
    !!
    !!@note The provided subroutines are intended for square matrices.
    !!      
    module subroutine stdlib_linalg_invert_inplace_s(a,pivot,err)
         !> Input matrix a[n,n]
         real(sp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_invert_inplace_s
    ! Compute the square matrix inverse of a
    module subroutine stdlib_linalg_invert_split_s(a,inva,pivot,err)
         !> Input matrix a[n,n].
         real(sp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         real(sp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err    
    end subroutine stdlib_linalg_invert_split_s
    module subroutine stdlib_linalg_invert_inplace_d(a,pivot,err)
         !> Input matrix a[n,n]
         real(dp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_invert_inplace_d
    ! Compute the square matrix inverse of a
    module subroutine stdlib_linalg_invert_split_d(a,inva,pivot,err)
         !> Input matrix a[n,n].
         real(dp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         real(dp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err    
    end subroutine stdlib_linalg_invert_split_d
    module subroutine stdlib_linalg_invert_inplace_c(a,pivot,err)
         !> Input matrix a[n,n]
         complex(sp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_invert_inplace_c
    ! Compute the square matrix inverse of a
    module subroutine stdlib_linalg_invert_split_c(a,inva,pivot,err)
         !> Input matrix a[n,n].
         complex(sp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         complex(sp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err    
    end subroutine stdlib_linalg_invert_split_c
    module subroutine stdlib_linalg_invert_inplace_z(a,pivot,err)
         !> Input matrix a[n,n]
         complex(dp), intent(inout) :: a(:,:)
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_invert_inplace_z
    ! Compute the square matrix inverse of a
    module subroutine stdlib_linalg_invert_split_z(a,inva,pivot,err)
         !> Input matrix a[n,n].
         complex(dp), intent(in) :: a(:,:)
         !> Inverse matrix a[n,n]. 
         complex(dp), intent(out) :: inva(:,:)         
         !> [optional] Storage array for the diagonal pivot indices
         integer(ilp), optional, intent(inout), target :: pivot(:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err    
    end subroutine stdlib_linalg_invert_split_z
  end interface invert

  ! Matrix Inverse: Operator interface
  interface operator(.inv.)
    !! version: experimental 
    !!
    !! Inverse operator of a square matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#inv-inverse-operator-of-a-square-matrix))
    !!
    !!### Summary
    !! Operator interface for computing the inverse of a square `real` or `complex` matrix.
    !!
    !!### Description
    !! 
    !! This operator interface provides a convenient way to compute the inverse of a matrix.
    !! Supported data types include `real` and `complex`. On input errors or singular matrix, 
    !! NaNs will be returned.
    !!
    !!@note The provided functions are intended for square matrices.
    !!
    module function stdlib_linalg_inverse_s_operator(a) result(inva)
         !> Input matrix a[n,n]
         real(sp), intent(in) :: a(:,:)
         !> Result matrix
         real(sp), allocatable :: inva(:,:)        
    end function stdlib_linalg_inverse_s_operator
    module function stdlib_linalg_inverse_d_operator(a) result(inva)
         !> Input matrix a[n,n]
         real(dp), intent(in) :: a(:,:)
         !> Result matrix
         real(dp), allocatable :: inva(:,:)        
    end function stdlib_linalg_inverse_d_operator
    module function stdlib_linalg_inverse_c_operator(a) result(inva)
         !> Input matrix a[n,n]
         complex(sp), intent(in) :: a(:,:)
         !> Result matrix
         complex(sp), allocatable :: inva(:,:)        
    end function stdlib_linalg_inverse_c_operator
    module function stdlib_linalg_inverse_z_operator(a) result(inva)
         !> Input matrix a[n,n]
         complex(dp), intent(in) :: a(:,:)
         !> Result matrix
         complex(dp), allocatable :: inva(:,:)        
    end function stdlib_linalg_inverse_z_operator
  end interface operator(.inv.)


  ! Moore-Penrose Pseudo-Inverse: Function interface
  interface pinv
    !! version: experimental 
    !!
    !! Pseudo-inverse of a matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#pinv-moore-penrose-pseudo-inverse-of-a-matrix))
    !!
    !!### Summary
    !! This interface provides methods for computing the Moore-Penrose pseudo-inverse of a matrix.
    !! The pseudo-inverse \( A^{+} \) is a generalization of the matrix inverse, computed for square, singular, 
    !! or rectangular matrices. It is defined such that it satisfies the conditions:
    !! - \( A \cdot A^{+} \cdot A = A \)
    !! - \( A^{+} \cdot A \cdot A^{+} = A^{+} \)
    !! - \( (A \cdot A^{+})^T = A \cdot A^{+} \)
    !! - \( (A^{+} \cdot A)^T = A^{+} \cdot A \)
    !!
    !!### Description
    !!     
    !! This function interface provides methods that return the Moore-Penrose pseudo-inverse of a matrix.    
    !! Supported data types include `real` and `complex`. 
    !! The pseudo-inverse \( A^{+} \) is returned as a function result. The computation is based on the 
    !! singular value decomposition (SVD). An optional relative tolerance `rtol` is provided to control the 
    !! inclusion of singular values during inversion. Singular values below \( \text{rtol} \cdot \sigma_{\max} \) 
    !! are treated as zero, where \( \sigma_{\max} \) is the largest singular value. If `rtol` is not provided, 
    !! a default threshold is applied.
    !! 
    !! Exceptions are raised in case of computational errors or invalid input, and trigger an `error stop` 
    !! if the state flag `err` is not provided. 
    !!
    !!@note The provided functions are intended for both rectangular and square matrices.
    !!       
    module function stdlib_linalg_pseudoinverse_s(a,rtol,err) result(pinva)
        !> Input matrix a[m,n]
        real(sp), intent(in), target :: a(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(sp), optional, intent(in) :: rtol         
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
        !> Output matrix pseudo-inverse [n,m]
        real(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
     end function stdlib_linalg_pseudoinverse_s
    module function stdlib_linalg_pseudoinverse_d(a,rtol,err) result(pinva)
        !> Input matrix a[m,n]
        real(dp), intent(in), target :: a(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(dp), optional, intent(in) :: rtol         
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
        !> Output matrix pseudo-inverse [n,m]
        real(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
     end function stdlib_linalg_pseudoinverse_d
    module function stdlib_linalg_pseudoinverse_c(a,rtol,err) result(pinva)
        !> Input matrix a[m,n]
        complex(sp), intent(in), target :: a(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(sp), optional, intent(in) :: rtol         
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
        !> Output matrix pseudo-inverse [n,m]
        complex(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
     end function stdlib_linalg_pseudoinverse_c
    module function stdlib_linalg_pseudoinverse_z(a,rtol,err) result(pinva)
        !> Input matrix a[m,n]
        complex(dp), intent(in), target :: a(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(dp), optional, intent(in) :: rtol         
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
        !> Output matrix pseudo-inverse [n,m]
        complex(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))         
     end function stdlib_linalg_pseudoinverse_z
  end interface pinv

  ! Moore-Penrose Pseudo-Inverse: Subroutine interface 
  interface pseudoinvert
    !! version: experimental 
    !!
    !! Computation of the Moore-Penrose pseudo-inverse
    !! ([Specification](../page/specs/stdlib_linalg.html#pseudoinvert-moore-penrose-pseudo-inverse-of-a-matrix))
    !!
    !!### Summary
    !! This interface provides methods for computing the Moore-Penrose pseudo-inverse of a rectangular 
    !! or square `real` or `complex` matrix.
    !! The pseudo-inverse \( A^{+} \) generalizes the matrix inverse and satisfies the properties:
    !! - \( A \cdot A^{+} \cdot A = A \)
    !! - \( A^{+} \cdot A \cdot A^{+} = A^{+} \)
    !! - \( (A \cdot A^{+})^T = A \cdot A^{+} \)
    !! - \( (A^{+} \cdot A)^T = A^{+} \cdot A \)
    !!
    !!### Description
    !!     
    !! This subroutine interface provides a way to compute the Moore-Penrose pseudo-inverse of a matrix.    
    !! Supported data types include `real` and `complex`. 
    !! Users must provide two matrices: the input matrix `a` [m,n] and the output pseudo-inverse `pinva` [n,m]. 
    !! The input matrix `a` is used to compute the pseudo-inverse and is not modified. The computed 
    !! pseudo-inverse is stored in `pinva`. The computation is based on the singular value decomposition (SVD).
    !! 
    !! An optional relative tolerance `rtol` is used to control the inclusion of singular values in the 
    !! computation. Singular values below \( \text{rtol} \cdot \sigma_{\max} \) are treated as zero, 
    !! where \( \sigma_{\max} \) is the largest singular value. If `rtol` is not provided, a default 
    !! threshold is applied. 
    !! 
    !! Exceptions are raised in case of computational errors or invalid input, and trigger an `error stop` 
    !! if the state flag `err` is not provided.
    !!
    !!@note The provided subroutines are intended for both rectangular and square matrices.
    !!       
    module subroutine stdlib_linalg_pseudoinvert_s(a,pinva,rtol,err)
        !> Input matrix a[m,n]
        real(sp), intent(inout) :: a(:,:)
        !> Output pseudo-inverse matrix [n,m]
        real(sp), intent(out) :: pinva(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(sp), optional, intent(in) :: rtol
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_pseudoinvert_s
    module subroutine stdlib_linalg_pseudoinvert_d(a,pinva,rtol,err)
        !> Input matrix a[m,n]
        real(dp), intent(inout) :: a(:,:)
        !> Output pseudo-inverse matrix [n,m]
        real(dp), intent(out) :: pinva(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(dp), optional, intent(in) :: rtol
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_pseudoinvert_d
    module subroutine stdlib_linalg_pseudoinvert_c(a,pinva,rtol,err)
        !> Input matrix a[m,n]
        complex(sp), intent(inout) :: a(:,:)
        !> Output pseudo-inverse matrix [n,m]
        complex(sp), intent(out) :: pinva(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(sp), optional, intent(in) :: rtol
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_pseudoinvert_c
    module subroutine stdlib_linalg_pseudoinvert_z(a,pinva,rtol,err)
        !> Input matrix a[m,n]
        complex(dp), intent(inout) :: a(:,:)
        !> Output pseudo-inverse matrix [n,m]
        complex(dp), intent(out) :: pinva(:,:)
        !> [optional] Relative tolerance for singular value cutoff
        real(dp), optional, intent(in) :: rtol
        !> [optional] State return flag. On error if not requested, the code will stop
        type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_pseudoinvert_z
  end interface pseudoinvert

  ! Moore-Penrose Pseudo-Inverse: Operator interface
  interface operator(.pinv.)
    !! version: experimental 
    !!
    !! Pseudo-inverse operator of a matrix
    !! ([Specification](../page/specs/stdlib_linalg.html#pinv-moore-penrose-pseudo-inverse-operator))
    !!
    !!### Summary
    !! Operator interface for computing the Moore-Penrose pseudo-inverse of a `real` or `complex` matrix.
    !!
    !!### Description
    !! 
    !! This operator interface provides a convenient way to compute the Moore-Penrose pseudo-inverse 
    !! of a matrix. Supported data types include `real` and `complex`. The pseudo-inverse \( A^{+} \) 
    !! is computed using singular value decomposition (SVD), with singular values below an internal 
    !! threshold treated as zero.
    !! 
    !! For computational errors or invalid input, the function may return a matrix filled with NaNs.
    !!
    !!@note The provided functions are intended for both rectangular and square matrices.
    !!
    module function stdlib_linalg_pinv_s_operator(a) result(pinva)
         !> Input matrix a[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         real(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
    end function stdlib_linalg_pinv_s_operator
    module function stdlib_linalg_pinv_d_operator(a) result(pinva)
         !> Input matrix a[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         real(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
    end function stdlib_linalg_pinv_d_operator
    module function stdlib_linalg_pinv_c_operator(a) result(pinva)
         !> Input matrix a[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         complex(sp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
    end function stdlib_linalg_pinv_c_operator
    module function stdlib_linalg_pinv_z_operator(a) result(pinva)
         !> Input matrix a[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> Result pseudo-inverse matrix
         complex(dp) :: pinva(size(a,2,kind=ilp),size(a,1,kind=ilp))
    end function stdlib_linalg_pinv_z_operator
  end interface operator(.pinv.)


  ! Eigendecomposition of a square matrix: eigenvalues, and optionally eigenvectors
  interface eig    
     !! version: experimental 
     !!
     !! Solves the eigendecomposition \( A \cdot \bar{v} - \lambda \cdot \bar{v} \) for square matrix \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#eig-eigenvalues-and-eigenvectors-of-a-square-matrix))
     !!
     !!### Summary 
     !! Subroutine interface for computing eigenvalues and eigenvectors of a square matrix.
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the eigenvalues, and optionally eigenvectors, 
     !! of a general square matrix. Supported data types include `real` and `complex`, and no assumption is 
     !! made on the matrix structure. The user may request either left, right, or both 
     !! eigenvectors to be returned. They are returned as columns of a square matrix with the same size as `A`. 
     !! Preallocated space for both eigenvalues `lambda` and the eigenvector matrices must be user-provided.      
     !! 
     !!@note The solution is based on LAPACK's general eigenproblem solvers `*GEEV`.
     !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
     !!       
    module subroutine stdlib_linalg_eig_standard_s(a,lambda,right,left, &
                                                      overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_standard_s

    module subroutine stdlib_linalg_real_eig_standard_s(a,lambda,right,left, &
                                                           overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of real eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_standard_s
    module subroutine stdlib_linalg_eig_generalized_s(a,b,lambda,right,left, &
                                                      overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         real(sp), intent(inout), target :: b(:,:)
         !> Array of eigenvalues
         complex(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_generalized_s

    module subroutine stdlib_linalg_real_eig_generalized_s(a,b,lambda,right,left, &
                                                           overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         real(sp), intent(inout), target :: b(:,:)  
         !> Array of real eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_generalized_s
    module subroutine stdlib_linalg_eig_standard_d(a,lambda,right,left, &
                                                      overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_standard_d

    module subroutine stdlib_linalg_real_eig_standard_d(a,lambda,right,left, &
                                                           overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of real eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_standard_d
    module subroutine stdlib_linalg_eig_generalized_d(a,b,lambda,right,left, &
                                                      overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         real(dp), intent(inout), target :: b(:,:)
         !> Array of eigenvalues
         complex(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_generalized_d

    module subroutine stdlib_linalg_real_eig_generalized_d(a,b,lambda,right,left, &
                                                           overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         real(dp), intent(inout), target :: b(:,:)  
         !> Array of real eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_generalized_d
    module subroutine stdlib_linalg_eig_standard_c(a,lambda,right,left, &
                                                      overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_standard_c

    module subroutine stdlib_linalg_real_eig_standard_c(a,lambda,right,left, &
                                                           overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of real eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_standard_c
    module subroutine stdlib_linalg_eig_generalized_c(a,b,lambda,right,left, &
                                                      overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         complex(sp), intent(inout), target :: b(:,:)
         !> Array of eigenvalues
         complex(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_generalized_c

    module subroutine stdlib_linalg_real_eig_generalized_c(a,b,lambda,right,left, &
                                                           overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         complex(sp), intent(inout), target :: b(:,:)  
         !> Array of real eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(sp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(sp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_generalized_c
    module subroutine stdlib_linalg_eig_standard_z(a,lambda,right,left, &
                                                      overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         complex(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_standard_z

    module subroutine stdlib_linalg_real_eig_standard_z(a,lambda,right,left, &
                                                           overwrite_a,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of real eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_standard_z
    module subroutine stdlib_linalg_eig_generalized_z(a,b,lambda,right,left, &
                                                      overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of eigenvalues, 
     !! and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         complex(dp), intent(inout), target :: b(:,:)
         !> Array of eigenvalues
         complex(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eig_generalized_z

    module subroutine stdlib_linalg_real_eig_generalized_z(a,b,lambda,right,left, &
                                                           overwrite_a,overwrite_b,err)
     !! Eigendecomposition of matrix A returning an array `lambda` of real eigenvalues, 
     !! and optionally right or left eigenvectors. Returns an error if the eigenvalues had
     !! non-trivial imaginary parts.
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Generalized problem matrix B[n,n]
         complex(dp), intent(inout), target :: b(:,:)  
         !> Array of real eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of RIGHT contain the right eigenvectors of A
         complex(dp), optional, intent(out), target :: right(:,:)
         !> The columns of LEFT contain the left eigenvectors of A
         complex(dp), optional, intent(out), target :: left(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Can B data be overwritten and destroyed? (default: no)
         logical(lk), optional, intent(in) :: overwrite_b
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_real_eig_generalized_z
  end interface eig

  ! Eigenvalues of a square matrix
  interface eigvals
     !! version: experimental 
     !!
     !! Returns the eigenvalues \( lambda \), \( A \cdot \bar{v} - \lambda \cdot \bar{v} \), for square matrix \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#eigvals-eigenvalues-of-a-square-matrix))
     !!
     !!### Summary 
     !! Function interface for computing the eigenvalues of a square matrix.
     !!
     !!### Description
     !! 
     !! This interface provides functions for returning the eigenvalues of a general square matrix. 
     !! Supported data types include `real` and `complex`, and no assumption is made on the matrix structure. 
     !! An `error stop` is thrown in case of failure; otherwise, error information can be returned 
     !! as an optional `type(linalg_state_type)` output flag. 
     !! 
     !!@note The solution is based on LAPACK's general eigenproblem solvers `*GEEV`.
     !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
     !!       
    module function stdlib_linalg_eigvals_standard_s(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(sp), intent(in), dimension(:,:), target :: a 
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_standard_s
    
    module function stdlib_linalg_eigvals_noerr_standard_s(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(sp), intent(in), dimension(:,:), target :: a 
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_standard_s
    module function stdlib_linalg_eigvals_generalized_s(a,b,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(sp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         real(sp), intent(inout), dimension(:,:), target :: b         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_generalized_s
    
    module function stdlib_linalg_eigvals_noerr_generalized_s(a,b) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(sp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         real(sp), intent(inout), dimension(:,:), target :: b         
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_generalized_s
    module function stdlib_linalg_eigvals_standard_d(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(dp), intent(in), dimension(:,:), target :: a 
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_standard_d
    
    module function stdlib_linalg_eigvals_noerr_standard_d(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(dp), intent(in), dimension(:,:), target :: a 
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_standard_d
    module function stdlib_linalg_eigvals_generalized_d(a,b,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(dp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         real(dp), intent(inout), dimension(:,:), target :: b         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_generalized_d
    
    module function stdlib_linalg_eigvals_noerr_generalized_d(a,b) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         real(dp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         real(dp), intent(inout), dimension(:,:), target :: b         
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_generalized_d
    module function stdlib_linalg_eigvals_standard_c(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(sp), intent(in), dimension(:,:), target :: a 
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_standard_c
    
    module function stdlib_linalg_eigvals_noerr_standard_c(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(sp), intent(in), dimension(:,:), target :: a 
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_standard_c
    module function stdlib_linalg_eigvals_generalized_c(a,b,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(sp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         complex(sp), intent(inout), dimension(:,:), target :: b         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_generalized_c
    
    module function stdlib_linalg_eigvals_noerr_generalized_c(a,b) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(sp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         complex(sp), intent(inout), dimension(:,:), target :: b         
         !> Array of singular values
         complex(sp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_generalized_c
    module function stdlib_linalg_eigvals_standard_z(a,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(dp), intent(in), dimension(:,:), target :: a 
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_standard_z
    
    module function stdlib_linalg_eigvals_noerr_standard_z(a) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(dp), intent(in), dimension(:,:), target :: a 
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_standard_z
    module function stdlib_linalg_eigvals_generalized_z(a,b,err) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(dp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         complex(dp), intent(inout), dimension(:,:), target :: b         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)        
    end function stdlib_linalg_eigvals_generalized_z
    
    module function stdlib_linalg_eigvals_noerr_generalized_z(a,b) result(lambda)
     !! Return an array of eigenvalues of matrix A.
         !> Input matrix A[m,n]
         complex(dp), intent(in), dimension(:,:), target :: a 
         !> Generalized problem matrix B[n,n]
         complex(dp), intent(inout), dimension(:,:), target :: b         
         !> Array of singular values
         complex(dp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvals_noerr_generalized_z
  end interface eigvals
     
  ! Eigendecomposition of a real symmetric or complex hermitian matrix
  interface eigh
     !! version: experimental 
     !!
     !! Solves the eigendecomposition \( A \cdot \bar{v} - \lambda \cdot \bar{v} \) for a real symmetric 
     !! \( A = A^T \) or complex Hermitian \( A = A^H \) square matrix. 
     !! ([Specification](../page/specs/stdlib_linalg.html#eigh-eigenvalues-and-eigenvectors-of-a-real-symmetric-or-complex-hermitian-square-matrix))
     !!
     !!### Summary 
     !! Subroutine interface for computing eigenvalues and eigenvectors of a real symmetric or complex Hermitian square matrix.
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the eigenvalues, and optionally eigenvectors, 
     !! of a real symmetric or complex Hermitian square matrix. Supported data types include `real` and `complex`. 
     !! The matrix must be symmetric (if `real`) or Hermitian (if `complex`). Only the lower or upper 
     !! half of the matrix is accessed, and the user can select which using the optional `upper_a` 
     !! flag (default: use lower half). The vectors are orthogonal, and may be returned as columns of an optional 
     !! matrix `vectors` with the same kind and size as `A`. 
     !! Preallocated space for both eigenvalues `lambda` and the eigenvector matrix must be user-provided.      
     !! 
     !!@note The solution is based on LAPACK's eigenproblem solvers `*SYEV`/`*HEEV`.
     !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
     !!      
    module subroutine stdlib_linalg_eigh_s(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         real(sp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eigh_s
    module subroutine stdlib_linalg_eigh_d(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         real(dp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eigh_d
    module subroutine stdlib_linalg_eigh_c(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(sp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         complex(sp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eigh_c
    module subroutine stdlib_linalg_eigh_z(a,lambda,vectors,upper_a,overwrite_a,err)
     !! Eigendecomposition of a real symmetric or complex Hermitian matrix A returning an array `lambda` 
     !! of eigenvalues, and optionally right or left eigenvectors.        
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of eigenvalues
         real(dp), intent(out) :: lambda(:)
         !> The columns of vectors contain the orthonormal eigenvectors of A
         complex(dp), optional, intent(out), target :: vectors(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
    end subroutine stdlib_linalg_eigh_z
  end interface eigh
     
  ! Eigenvalues of a real symmetric or complex hermitian matrix
  interface eigvalsh
     !! version: experimental 
     !!
     !! Returns the eigenvalues \( lambda \), \( A \cdot \bar{v} - \lambda \cdot \bar{v} \), for a real
     !! symmetric \( A = A^T \) or complex Hermitian \( A = A^H \) square matrix. 
     !! ([Specification](../page/specs/stdlib_linalg.html#eigvalsh-eigenvalues-of-a-real-symmetric-or-complex-hermitian-square-matrix))
     !!
     !!### Summary 
     !! Function interface for computing the eigenvalues of a real symmetric or complex hermitian square matrix.
     !!
     !!### Description
     !! 
     !! This interface provides functions for returning the eigenvalues of a real symmetric or complex Hermitian
     !! square matrix. Supported data types include `real` and `complex`. The matrix must be symmetric 
     !! (if `real`) or Hermitian (if `complex`). Only the lower or upper half of the matrix is accessed, 
     !! and the user can select which using the optional `upper_a` flag (default: use lower half). 
     !! An `error stop` is thrown in case of failure; otherwise, error information can be returned 
     !! as an optional `type(linalg_state_type)` output flag. 
     !! 
     !!@note The solution is based on LAPACK's eigenproblem solvers `*SYEV`/`*HEEV`.
     !!@note BLAS/LAPACK backends do not currently support extended precision (``xdp``).
     !!         
    module function stdlib_linalg_eigvalsh_s(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: lambda(:)    
    end function stdlib_linalg_eigvalsh_s
    
    module function stdlib_linalg_eigvalsh_noerr_s(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of singular values
         real(sp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvalsh_noerr_s
    module function stdlib_linalg_eigvalsh_d(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: lambda(:)    
    end function stdlib_linalg_eigvalsh_d
    
    module function stdlib_linalg_eigvalsh_noerr_d(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of singular values
         real(dp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvalsh_noerr_d
    module function stdlib_linalg_eigvalsh_c(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: lambda(:)    
    end function stdlib_linalg_eigvalsh_c
    
    module function stdlib_linalg_eigvalsh_noerr_c(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of singular values
         real(sp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvalsh_noerr_c
    module function stdlib_linalg_eigvalsh_z(a,upper_a,err) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: lambda(:)    
    end function stdlib_linalg_eigvalsh_z
    
    module function stdlib_linalg_eigvalsh_noerr_z(a,upper_a) result(lambda)
     !! Return an array of eigenvalues of real symmetric / complex hermitian A        
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] Should the upper/lower half of A be used? Default: lower
         logical(lk), optional, intent(in) :: upper_a         
         !> Array of singular values
         real(dp), allocatable :: lambda(:)
    end function stdlib_linalg_eigvalsh_noerr_z
  end interface eigvalsh

  ! Singular value decomposition  
  interface svd 
    !! version: experimental 
    !!
    !! Computes the singular value decomposition of a `real` or `complex` 2d matrix.
    !! ([Specification](../page/specs/stdlib_linalg.html#svd-compute-the-singular-value-decomposition-of-a-rank-2-array-matrix))
    !! 
    !!### Summary 
    !! Interface for computing the singular value decomposition of a `real` or `complex` 2d matrix.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the singular value decomposition of a matrix.
    !! Supported data types include `real` and `complex`. The subroutine returns a `real` array of 
    !! singular values, and optionally, left- and right- singular vector matrices, `U` and `V`. 
    !! For a matrix `A` with size [m,n], full matrix storage for `U` and `V` should be [m,m] and [n,n]. 
    !! It is possible to use partial storage [m,k] and [k,n], `k=min(m,n)`, choosing `full_matrices=.false.`.
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GESDD` methods.
    !! 
    !!### Example
    !!
    !!```fortran
    !!    real(sp) :: a(2,3), s(2), u(2,2), vt(3,3) 
    !!    a = reshape([3,2, 2,3, 2,-2],[2,3])
    !!
    !!    call svd(A,s,u,v)
    !!    print *, 'singular values = ',s
    !!```
    !!         
    module subroutine stdlib_linalg_svd_s(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         real(sp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(sp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         real(sp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(sp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_s
    module subroutine stdlib_linalg_svd_d(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         real(dp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(dp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         real(dp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         real(dp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_d
    module subroutine stdlib_linalg_svd_c(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         complex(sp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(sp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         complex(sp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(sp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_c
    module subroutine stdlib_linalg_svd_z(a,s,u,vt,overwrite_a,full_matrices,err)
     !!### Summary
     !! Compute singular value decomposition of a matrix \( A = U \cdot S \cdot \V^T \)
     !!
     !!### Description
     !!
     !! This function computes the singular value decomposition of a `real` or `complex` matrix \( A \), 
     !! and returns the array of singular values, and optionally the left matrix \( U \) containing the 
     !! left unitary singular vectors, and the right matrix \( V^T \), containing the right unitary 
     !! singular vectors.
     !!
     !! param: a Input matrix of size [m,n].
     !! param: s Output `real` array of size [min(m,n)] returning a list of singular values.
     !! param: u [optional] Output left singular matrix of size [m,m] or [m,min(m,n)] (.not.full_matrices). Contains singular vectors as columns.
     !! param: vt [optional] Output right singular matrix of size [n,n] or [min(m,n),n] (.not.full_matrices). Contains singular vectors as rows.     
     !! param: overwrite_a [optional] Flag indicating if the input matrix can be overwritten.
     !! param: full_matrices [optional] If `.true.` (default), matrices \( U \) and \( V^T \) have size [m,m], [n,n]. Otherwise, they are [m,k], [k,n] with `k=min(m,n)`.
     !! param: err [optional] State return flag.      
     !!            
         !> Input matrix A[m,n]
         complex(dp), intent(inout), target :: a(:,:)
         !> Array of singular values
         real(dp), intent(out) :: s(:)
         !> The columns of U contain the left singular vectors 
         complex(dp), optional, intent(out), target :: u(:,:)
         !> The rows of V^T contain the right singular vectors
         complex(dp), optional, intent(out), target :: vt(:,:)
         !> [optional] Can A data be overwritten and destroyed?
         logical(lk), optional, intent(in) :: overwrite_a
         !> [optional] full matrices have shape(u)==[m,m], shape(vh)==[n,n] (default); otherwise
         !> they are shape(u)==[m,k] and shape(vh)==[k,n] with k=min(m,n)
         logical(lk), optional, intent(in) :: full_matrices
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
      end subroutine stdlib_linalg_svd_z
  end interface svd

  ! Singular values
  interface svdvals
    !! version: experimental 
    !!
    !! Computes the singular values of a `real` or `complex` 2d matrix.
    !! ([Specification](../page/specs/stdlib_linalg.html#svdvals-compute-the-singular-values-of-a-rank-2-array-matrix))
    !! 
    !!### Summary 
    !! 
    !! Function interface for computing the array of singular values from the singular value decomposition 
    !! of a `real` or `complex` 2d matrix.
    !!
    !!### Description
    !! 
    !! This interface provides methods for computing the singular values a 2d matrix.
    !! Supported data types include `real` and `complex`. The function returns a `real` array of 
    !! singular values, with size [min(m,n)]. 
    !! 
    !!@note The solution is based on LAPACK's singular value decomposition `*GESDD` methods.
    !! 
    !!### Example
    !!
    !!```fortran
    !!    real(sp) :: a(2,3), s(2)
    !!    a = reshape([3,2, 2,3, 2,-2],[2,3])
    !!
    !!    s = svdvals(A)
    !!    print *, 'singular values = ',s
    !!```
    !!   
    module function stdlib_linalg_svdvals_s(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         real(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_s
    module function stdlib_linalg_svdvals_d(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         real(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_d
    module function stdlib_linalg_svdvals_c(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         complex(sp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(sp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_c
    module function stdlib_linalg_svdvals_z(a,err) result(s)
     !!### Summary
     !! Compute singular values \(S \) from the singular-value decomposition of a matrix \( A = U \cdot S \cdot \V^T \).
     !!
     !!### Description
     !!
     !! This function returns the array of singular values from the singular value decomposition of a `real` 
     !! or `complex` matrix \( A = U \cdot S \cdot V^T \).
     !!
     !! param: a Input matrix of size [m,n].
     !! param: err [optional] State return flag.      
     !!
     !!### Return value
     !! 
     !! param: s `real` array of size [min(m,n)] returning a list of singular values.
     !!           
         !> Input matrix A[m,n]
         complex(dp), intent(in), target :: a(:,:)
         !> [optional] state return flag. On error if not requested, the code will stop
         type(linalg_state_type), optional, intent(out) :: err
         !> Array of singular values
         real(dp), allocatable :: s(:)
      end function stdlib_linalg_svdvals_z
  end interface svdvals  


  ! Vector norms: function interface
  interface norm
     !! version: experimental 
     !!
     !! Computes the vector norm of a generic-rank array \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#norm-computes-the-vector-norm-of-a-generic-rank-array))
     !! 
     !!### Summary 
     !! Return one of several scalar norm metrics of a `real` or `complex` input array \( A \), 
     !! that can have any rank. For generic rank-n arrays, the scalar norm over the whole 
     !! array is returned by default. If `n>=2` and the optional input dimension `dim` is specified, 
     !! a rank `n-1` array is returned with dimension `dim` collapsed, containing all 1D array norms 
     !! evaluated along dimension `dim` only.
     !! 
     !!
     !!### Description
     !! 
     !! This interface provides methods for computing the vector norm(s) of an array.  
     !! Supported data types include `real` and `complex`. 
     !! Input arrays may have generic rank from 1 to 4.
     !!
     !! Norm type input is mandatory, and it is provided via the `order` argument. 
     !! This can be provided as either an `integer` value or a `character` string. 
     !! Allowed metrics are: 
     !! - 1-norm \( \sum_i{ \left|a_i\right| } \): `order` = 1 or '1'    
     !! - Euclidean norm \( \sqrt{\sum_i{ a_i^2 }} \): `order` = 2 or '2'
     !! - p-norm \( \left( \sum_i{ \left|a_i\right|^p }\right) ^{1/p} \): `integer` `order`, order>=3
     !! - Infinity norm \( \max_i{ \left|a_i\right| } \): order = huge(0) or 'inf'
     !! - Minus-infinity norm \( \min_i{ \left|a_i\right| } \): order = -huge(0) or '-inf'
     !! 
     !!### Example
     !!
     !!```fortran
     !!
     !!    real(sp) :: a(3,3), na, rown(3)
     !!    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !!
     !!    ! L2 norm: whole matrix
     !!    na = norm(a, 2)
     !!   
     !!    ! Infinity norm of each row
     !!    rown = norm(a, 'inf', dim=2)
     !!     
     !!```     
     !!
     !> Scalar norms: real(sp)
     pure module function stdlib_linalg_norm_1D_order_char_s(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_1D_order_char_s
     module function stdlib_linalg_norm_1D_order_err_char_s(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_char_s
     pure module function stdlib_linalg_norm_2D_order_char_s(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_2D_order_char_s
     module function stdlib_linalg_norm_2D_order_err_char_s(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_char_s
     pure module function stdlib_linalg_norm_3D_order_char_s(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_3D_order_char_s
     module function stdlib_linalg_norm_3D_order_err_char_s(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_char_s
     pure module function stdlib_linalg_norm_4D_order_char_s(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_4D_order_char_s
     module function stdlib_linalg_norm_4D_order_err_char_s(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_char_s
     !> Array norms: real(sp)
     pure module function stdlib_linalg_norm_2D_to_1D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_char_s
     module function stdlib_linalg_norm_2D_to_1D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_char_s
     pure module function stdlib_linalg_norm_3D_to_2D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_char_s
     module function stdlib_linalg_norm_3D_to_2D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_char_s
     pure module function stdlib_linalg_norm_4D_to_3D_char_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_char_s
     module function stdlib_linalg_norm_4D_to_3D_err_char_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_char_s
     !> Scalar norms: real(sp)
     pure module function stdlib_linalg_norm_1D_order_int_s(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_1D_order_int_s
     module function stdlib_linalg_norm_1D_order_err_int_s(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_int_s
     pure module function stdlib_linalg_norm_2D_order_int_s(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_2D_order_int_s
     module function stdlib_linalg_norm_2D_order_err_int_s(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_int_s
     pure module function stdlib_linalg_norm_3D_order_int_s(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_3D_order_int_s
     module function stdlib_linalg_norm_3D_order_err_int_s(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_int_s
     pure module function stdlib_linalg_norm_4D_order_int_s(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_4D_order_int_s
     module function stdlib_linalg_norm_4D_order_err_int_s(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_int_s
     !> Array norms: real(sp)
     pure module function stdlib_linalg_norm_2D_to_1D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_int_s
     module function stdlib_linalg_norm_2D_to_1D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_int_s
     pure module function stdlib_linalg_norm_3D_to_2D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_int_s
     module function stdlib_linalg_norm_3D_to_2D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_int_s
     pure module function stdlib_linalg_norm_4D_to_3D_int_s(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_int_s
     module function stdlib_linalg_norm_4D_to_3D_err_int_s(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_int_s
     !> Scalar norms: real(dp)
     pure module function stdlib_linalg_norm_1D_order_char_d(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_1D_order_char_d
     module function stdlib_linalg_norm_1D_order_err_char_d(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_char_d
     pure module function stdlib_linalg_norm_2D_order_char_d(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_2D_order_char_d
     module function stdlib_linalg_norm_2D_order_err_char_d(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_char_d
     pure module function stdlib_linalg_norm_3D_order_char_d(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_3D_order_char_d
     module function stdlib_linalg_norm_3D_order_err_char_d(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_char_d
     pure module function stdlib_linalg_norm_4D_order_char_d(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_4D_order_char_d
     module function stdlib_linalg_norm_4D_order_err_char_d(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_char_d
     !> Array norms: real(dp)
     pure module function stdlib_linalg_norm_2D_to_1D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_char_d
     module function stdlib_linalg_norm_2D_to_1D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_char_d
     pure module function stdlib_linalg_norm_3D_to_2D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_char_d
     module function stdlib_linalg_norm_3D_to_2D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_char_d
     pure module function stdlib_linalg_norm_4D_to_3D_char_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_char_d
     module function stdlib_linalg_norm_4D_to_3D_err_char_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_char_d
     !> Scalar norms: real(dp)
     pure module function stdlib_linalg_norm_1D_order_int_d(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_1D_order_int_d
     module function stdlib_linalg_norm_1D_order_err_int_d(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        real(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_int_d
     pure module function stdlib_linalg_norm_2D_order_int_d(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_2D_order_int_d
     module function stdlib_linalg_norm_2D_order_err_int_d(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        real(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_int_d
     pure module function stdlib_linalg_norm_3D_order_int_d(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_3D_order_int_d
     module function stdlib_linalg_norm_3D_order_err_int_d(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        real(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_int_d
     pure module function stdlib_linalg_norm_4D_order_int_d(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_4D_order_int_d
     module function stdlib_linalg_norm_4D_order_err_int_d(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        real(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_int_d
     !> Array norms: real(dp)
     pure module function stdlib_linalg_norm_2D_to_1D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_int_d
     module function stdlib_linalg_norm_2D_to_1D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_int_d
     pure module function stdlib_linalg_norm_3D_to_2D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_int_d
     module function stdlib_linalg_norm_3D_to_2D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_int_d
     pure module function stdlib_linalg_norm_4D_to_3D_int_d(a, order, dim) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_int_d
     module function stdlib_linalg_norm_4D_to_3D_err_int_d(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        real(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_int_d
     !> Scalar norms: complex(sp)
     pure module function stdlib_linalg_norm_1D_order_char_c(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_1D_order_char_c
     module function stdlib_linalg_norm_1D_order_err_char_c(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_char_c
     pure module function stdlib_linalg_norm_2D_order_char_c(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_2D_order_char_c
     module function stdlib_linalg_norm_2D_order_err_char_c(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_char_c
     pure module function stdlib_linalg_norm_3D_order_char_c(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_3D_order_char_c
     module function stdlib_linalg_norm_3D_order_err_char_c(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_char_c
     pure module function stdlib_linalg_norm_4D_order_char_c(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_4D_order_char_c
     module function stdlib_linalg_norm_4D_order_err_char_c(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_char_c
     !> Array norms: complex(sp)
     pure module function stdlib_linalg_norm_2D_to_1D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_char_c
     module function stdlib_linalg_norm_2D_to_1D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_char_c
     pure module function stdlib_linalg_norm_3D_to_2D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_char_c
     module function stdlib_linalg_norm_3D_to_2D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_char_c
     pure module function stdlib_linalg_norm_4D_to_3D_char_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_char_c
     module function stdlib_linalg_norm_4D_to_3D_err_char_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_char_c
     !> Scalar norms: complex(sp)
     pure module function stdlib_linalg_norm_1D_order_int_c(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_1D_order_int_c
     module function stdlib_linalg_norm_1D_order_err_int_c(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(sp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_int_c
     pure module function stdlib_linalg_norm_2D_order_int_c(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_2D_order_int_c
     module function stdlib_linalg_norm_2D_order_err_int_c(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(sp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_int_c
     pure module function stdlib_linalg_norm_3D_order_int_c(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_3D_order_int_c
     module function stdlib_linalg_norm_3D_order_err_int_c(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(sp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_int_c
     pure module function stdlib_linalg_norm_4D_order_int_c(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(sp) :: nrm       
     end function stdlib_linalg_norm_4D_order_int_c
     module function stdlib_linalg_norm_4D_order_err_int_c(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(sp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(sp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_int_c
     !> Array norms: complex(sp)
     pure module function stdlib_linalg_norm_2D_to_1D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_int_c
     module function stdlib_linalg_norm_2D_to_1D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_int_c
     pure module function stdlib_linalg_norm_3D_to_2D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_int_c
     module function stdlib_linalg_norm_3D_to_2D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_int_c
     pure module function stdlib_linalg_norm_4D_to_3D_int_c(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_int_c
     module function stdlib_linalg_norm_4D_to_3D_err_int_c(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(sp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(sp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_int_c
     !> Scalar norms: complex(dp)
     pure module function stdlib_linalg_norm_1D_order_char_z(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_1D_order_char_z
     module function stdlib_linalg_norm_1D_order_err_char_z(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_char_z
     pure module function stdlib_linalg_norm_2D_order_char_z(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_2D_order_char_z
     module function stdlib_linalg_norm_2D_order_err_char_z(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_char_z
     pure module function stdlib_linalg_norm_3D_order_char_z(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_3D_order_char_z
     module function stdlib_linalg_norm_3D_order_err_char_z(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_char_z
     pure module function stdlib_linalg_norm_4D_order_char_z(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_4D_order_char_z
     module function stdlib_linalg_norm_4D_order_err_char_z(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_char_z
     !> Array norms: complex(dp)
     pure module function stdlib_linalg_norm_2D_to_1D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_char_z
     module function stdlib_linalg_norm_2D_to_1D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_char_z
     pure module function stdlib_linalg_norm_3D_to_2D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_char_z
     module function stdlib_linalg_norm_3D_to_2D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_char_z
     pure module function stdlib_linalg_norm_4D_to_3D_char_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_char_z
     module function stdlib_linalg_norm_4D_to_3D_err_char_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_char_z
     !> Scalar norms: complex(dp)
     pure module function stdlib_linalg_norm_1D_order_int_z(a, order) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_1D_order_int_z
     module function stdlib_linalg_norm_1D_order_err_int_z(a, order, err) result(nrm)
        !> Input 1-d matrix a(:)
        complex(dp), intent(in) :: a(:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_1D_order_err_int_z
     pure module function stdlib_linalg_norm_2D_order_int_z(a, order) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_2D_order_int_z
     module function stdlib_linalg_norm_2D_order_err_int_z(a, order, err) result(nrm)
        !> Input 2-d matrix a(:,:)
        complex(dp), intent(in) :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_2D_order_err_int_z
     pure module function stdlib_linalg_norm_3D_order_int_z(a, order) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_3D_order_int_z
     module function stdlib_linalg_norm_3D_order_err_int_z(a, order, err) result(nrm)
        !> Input 3-d matrix a(:,:,:)
        complex(dp), intent(in) :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_3D_order_err_int_z
     pure module function stdlib_linalg_norm_4D_order_int_z(a, order) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Norm of the matrix.
        real(dp) :: nrm       
     end function stdlib_linalg_norm_4D_order_int_z
     module function stdlib_linalg_norm_4D_order_err_int_z(a, order, err) result(nrm)
        !> Input 4-d matrix a(:,:,:,:)
        complex(dp), intent(in) :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                         
        !> Norm of the matrix.
        real(dp) :: nrm               
     end function stdlib_linalg_norm_4D_order_err_int_z
     !> Array norms: complex(dp)
     pure module function stdlib_linalg_norm_2D_to_1D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))   
     end function stdlib_linalg_norm_2D_to_1D_int_z
     module function stdlib_linalg_norm_2D_to_1D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
     end function stdlib_linalg_norm_2D_to_1D_err_int_z
     pure module function stdlib_linalg_norm_3D_to_2D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))   
     end function stdlib_linalg_norm_3D_to_2D_int_z
     module function stdlib_linalg_norm_3D_to_2D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
     end function stdlib_linalg_norm_3D_to_2D_err_int_z
     pure module function stdlib_linalg_norm_4D_to_3D_int_z(a, order, dim) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along 
        integer(ilp), intent(in) :: dim
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))   
     end function stdlib_linalg_norm_4D_to_3D_int_z
     module function stdlib_linalg_norm_4D_to_3D_err_int_z(a, order, dim, err) result(nrm)
        !> Input matrix a[..]
        complex(dp), intent(in), target :: a(:,:,:,:)
        !> Order of the matrix norm being computed.
        integer(ilp), intent(in) :: order
        !> Dimension the norm is computed along
        integer(ilp), intent(in) :: dim
        !> Output state return flag. 
        type(linalg_state_type), intent(out) :: err                                 
        !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
        real(dp) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim), merge(size(a, 3),&
            & size(a, 4), mask=3<dim))     
     end function stdlib_linalg_norm_4D_to_3D_err_int_z
  end interface norm  

  !> Vector norm: subroutine interface
  interface get_norm
     !! version: experimental 
     !!
     !! Computes the vector norm of a generic-rank array \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#get-norm-computes-the-vector-norm-of-a-generic-rank-array))
     !! 
     !!### Summary 
     !! Subroutine interface that returns one of several scalar norm metrics of a `real` or `complex` 
     !! input array \( A \), that can have any rank. For generic rank-n arrays, the scalar norm over 
     !! the whole array is returned by default. If `n>=2` and the optional input dimension `dim` is 
     !! specified, a rank `n-1` array is returned with dimension `dim` collapsed, containing all 1D 
     !! array norms evaluated along dimension `dim` only.
     !! 
     !!
     !!### Description
     !! 
     !! This `pure subroutine `interface provides methods for computing the vector norm(s) of an array.  
     !! Supported data types include `real` and `complex`. 
     !! Input arrays may have generic rank from 1 to 4.
     !!
     !! Norm type input is mandatory, and it is provided via the `order` argument. 
     !! This can be provided as either an `integer` value or a `character` string. 
     !! Allowed metrics are: 
     !! - 1-norm \( \sum_i{ \left|a_i\right| } \): `order` = 1 or '1'    
     !! - Euclidean norm \( \sqrt{\sum_i{ a_i^2 }} \): `order` = 2 or '2'
     !! - p-norm \( \left( \sum_i{ \left|a_i\right|^p }\right) ^{1/p} \): `integer` `order`, order>=3
     !! - Infinity norm \( \max_i{ \left|a_i\right| } \): order = huge(0) or 'inf'
     !! - Minus-infinity norm \( \min_i{ \left|a_i\right| } \): order = -huge(0) or '-inf'
     !!     
     !!### Example
     !!
     !!```fortran
     !!
     !!    real(sp) :: a(3,3), na, rown(3)
     !!    type(linalg_state_type) :: err
     !!    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !!
     !!    ! L2 norm: whole matrix
     !!    call get_norm(a, na, 2)
     !!   
     !!    ! Infinity norms of each row, with error control
     !!    call get_norm(a, rown, 'inf', dim=2, err=err)     
     !!     
     !!```     
     !!     
        !> Scalar norms: real(sp)
        pure module subroutine norm_1D_char_s(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           real(sp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_char_s
        pure module subroutine norm_2D_char_s(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           real(sp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_char_s
        pure module subroutine norm_3D_char_s(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           real(sp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_char_s
        pure module subroutine norm_4D_char_s(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           real(sp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_char_s
        !> Array norms: real(sp)
        pure module subroutine norm_2D_to_1D_char_s(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(sp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_char_s
        pure module subroutine norm_3D_to_2D_char_s(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(sp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_char_s
        pure module subroutine norm_4D_to_3D_char_s(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(sp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_char_s
        !> Scalar norms: real(sp)
        pure module subroutine norm_1D_int_s(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           real(sp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_int_s
        pure module subroutine norm_2D_int_s(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           real(sp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_int_s
        pure module subroutine norm_3D_int_s(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           real(sp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_int_s
        pure module subroutine norm_4D_int_s(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           real(sp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_int_s
        !> Array norms: real(sp)
        pure module subroutine norm_2D_to_1D_int_s(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(sp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_int_s
        pure module subroutine norm_3D_to_2D_int_s(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(sp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_int_s
        pure module subroutine norm_4D_to_3D_int_s(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(sp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_int_s
        !> Scalar norms: real(dp)
        pure module subroutine norm_1D_char_d(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           real(dp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_char_d
        pure module subroutine norm_2D_char_d(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           real(dp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_char_d
        pure module subroutine norm_3D_char_d(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           real(dp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_char_d
        pure module subroutine norm_4D_char_d(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           real(dp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_char_d
        !> Array norms: real(dp)
        pure module subroutine norm_2D_to_1D_char_d(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(dp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_char_d
        pure module subroutine norm_3D_to_2D_char_d(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(dp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_char_d
        pure module subroutine norm_4D_to_3D_char_d(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(dp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_char_d
        !> Scalar norms: real(dp)
        pure module subroutine norm_1D_int_d(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           real(dp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_int_d
        pure module subroutine norm_2D_int_d(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           real(dp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_int_d
        pure module subroutine norm_3D_int_d(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           real(dp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_int_d
        pure module subroutine norm_4D_int_d(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           real(dp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_int_d
        !> Array norms: real(dp)
        pure module subroutine norm_2D_to_1D_int_d(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(dp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_int_d
        pure module subroutine norm_3D_to_2D_int_d(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(dp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_int_d
        pure module subroutine norm_4D_to_3D_int_d(a, nrm, order, dim, err)
           !> Input matrix a[..]
           real(dp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_int_d
        !> Scalar norms: complex(sp)
        pure module subroutine norm_1D_char_c(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           complex(sp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_char_c
        pure module subroutine norm_2D_char_c(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           complex(sp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_char_c
        pure module subroutine norm_3D_char_c(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           complex(sp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_char_c
        pure module subroutine norm_4D_char_c(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           complex(sp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_char_c
        !> Array norms: complex(sp)
        pure module subroutine norm_2D_to_1D_char_c(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(sp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_char_c
        pure module subroutine norm_3D_to_2D_char_c(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(sp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_char_c
        pure module subroutine norm_4D_to_3D_char_c(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(sp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_char_c
        !> Scalar norms: complex(sp)
        pure module subroutine norm_1D_int_c(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           complex(sp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_int_c
        pure module subroutine norm_2D_int_c(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           complex(sp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_int_c
        pure module subroutine norm_3D_int_c(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           complex(sp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_int_c
        pure module subroutine norm_4D_int_c(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           complex(sp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(sp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_int_c
        !> Array norms: complex(sp)
        pure module subroutine norm_2D_to_1D_int_c(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(sp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_int_c
        pure module subroutine norm_3D_to_2D_int_c(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(sp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_int_c
        pure module subroutine norm_4D_to_3D_int_c(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(sp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(sp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_int_c
        !> Scalar norms: complex(dp)
        pure module subroutine norm_1D_char_z(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           complex(dp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_char_z
        pure module subroutine norm_2D_char_z(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           complex(dp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_char_z
        pure module subroutine norm_3D_char_z(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           complex(dp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_char_z
        pure module subroutine norm_4D_char_z(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           complex(dp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_char_z
        !> Array norms: complex(dp)
        pure module subroutine norm_2D_to_1D_char_z(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(dp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_char_z
        pure module subroutine norm_3D_to_2D_char_z(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(dp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_char_z
        pure module subroutine norm_4D_to_3D_char_z(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(dp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           character(len=*), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_char_z
        !> Scalar norms: complex(dp)
        pure module subroutine norm_1D_int_z(a, nrm, order, err)
           !> Input 1-d matrix a(:)
           complex(dp), intent(in), target :: a(:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_1D_int_z
        pure module subroutine norm_2D_int_z(a, nrm, order, err)
           !> Input 2-d matrix a(:,:)
           complex(dp), intent(in), target :: a(:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_2D_int_z
        pure module subroutine norm_3D_int_z(a, nrm, order, err)
           !> Input 3-d matrix a(:,:,:)
           complex(dp), intent(in), target :: a(:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_3D_int_z
        pure module subroutine norm_4D_int_z(a, nrm, order, err)
           !> Input 4-d matrix a(:,:,:,:)
           complex(dp), intent(in), target :: a(:,:,:,:)
           !> Norm of the matrix.
           real(dp), intent(out) :: nrm
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err   
        end subroutine norm_4D_int_z
        !> Array norms: complex(dp)
        pure module subroutine norm_2D_to_1D_int_z(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(dp), intent(in) :: a(:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_2D_to_1D_int_z
        pure module subroutine norm_3D_to_2D_int_z(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(dp), intent(in) :: a(:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_3D_to_2D_int_z
        pure module subroutine norm_4D_to_3D_int_z(a, nrm, order, dim, err)
           !> Input matrix a[..]
           complex(dp), intent(in) :: a(:,:,:,:)
           !> Dimension the norm is computed along
           integer(ilp), intent(in) :: dim        
           !> Norm of the matrix. (Same shape as `a`, with `dim` dropped).
           real(dp), intent(out) :: nrm(merge(size(a, 1), size(a, 2), mask=1<dim), merge(size(a, 2), size(a, 3), mask=2<dim),&
               & merge(size(a, 3), size(a, 4), mask=3<dim))     
           !> Order of the matrix norm being computed.
           integer(ilp), intent(in) :: order
           !> [optional] state return flag. On error if not requested, the code will stop
           type(linalg_state_type), intent(out), optional :: err           
        end subroutine  norm_4D_to_3D_int_z
  end interface get_norm

  !> Matrix norms: function interface
  interface mnorm
     !! version: experimental 
     !!
     !! Computes the matrix norm of a generic-rank array \( A \). 
     !! ([Specification](../page/specs/stdlib_linalg.html#mnorm-computes-the-matrix-norm-of-a-generic-rank-array))
     !! 
     !!### Summary 
     !! Return one of several matrix norm metrics of a `real` or `complex` input array \( A \), 
     !! that can have rank 2 or higher. For rank-2 arrays, the matrix norm is returned.
     !! If rank>2 and the optional input dimensions `dim` are specified, 
     !! a rank `n-2` array is returned with dimensions `dim(1),dim(2)` collapsed, containing all 
     !! matrix norms evaluated over the specified dimensions only. `dim==[1,2]` are assumed as default
     !! dimensions if not specified.
     !! 
     !!### Description
     !! 
     !! This interface provides methods for computing the matrix norm(s) of an array.  
     !! Supported data types include `real` and `complex`. 
     !! Input arrays must have rank >= 2.
     !!
     !! Norm type input is optional, and it is provided via the `order` argument. 
     !! This can be provided as either an `integer` value or a `character` string. 
     !! Allowed metrics are: 
     !! - 1-norm: `order` = 1 or '1'    
     !! - 2-norm: `order` = 2 or '2'
     !! - Euclidean/Frobenius: `order` = 'Euclidean','Frobenius', or argument not specified
     !! - Infinity norm: `order` = huge(0) or 'Inf'
     !! 
     !! If an invalid norm type is provided, the routine returns an error state.
     !!
     !!### Example
     !!
     !!```fortran
     !!    real(sp) :: a(3,3), na
     !!    real(sp) :: b(3,3,4), nb(4)  ! Array of 4 3x3 matrices
     !!    a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9], [3, 3])
     !!    
     !!    ! Euclidean/Frobenius norm of single matrix
     !!    na = mnorm(a)
     !!    na = mnorm(a, 'Euclidean')
     !!   
     !!    ! 1-norm of each 3x3 matrix in b
     !!    nb = mnorm(b, 1, dim=[1,2])
     !!     
     !!    ! Infinity-norm 
     !!    na = mnorm(b, 'inf', dim=[3,2])
     !!```     
     !!
      
      !> Matrix norms: real(sp) rank-2 arrays
      module function matrix_norm_char_s(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_char_s
      
      !> Matrix norms: real(sp) higher rank arrays
      module function matrix_norm_3D_to_1D_char_s(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(sp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_char_s
      module function matrix_norm_4D_to_2D_char_s(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(sp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_char_s
      
      !> Matrix norms: real(sp) rank-2 arrays
      module function matrix_norm_int_s(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_int_s
      
      !> Matrix norms: real(sp) higher rank arrays
      module function matrix_norm_3D_to_1D_int_s(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(sp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_int_s
      module function matrix_norm_4D_to_2D_int_s(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(sp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_int_s
      
      !> Matrix norms: real(dp) rank-2 arrays
      module function matrix_norm_char_d(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_char_d
      
      !> Matrix norms: real(dp) higher rank arrays
      module function matrix_norm_3D_to_1D_char_d(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(dp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_char_d
      module function matrix_norm_4D_to_2D_char_d(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(dp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_char_d
      
      !> Matrix norms: real(dp) rank-2 arrays
      module function matrix_norm_int_d(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        real(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_int_d
      
      !> Matrix norms: real(dp) higher rank arrays
      module function matrix_norm_3D_to_1D_int_d(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(dp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_int_d
      module function matrix_norm_4D_to_2D_int_d(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          real(dp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_int_d
      
      !> Matrix norms: complex(sp) rank-2 arrays
      module function matrix_norm_char_c(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_char_c
      
      !> Matrix norms: complex(sp) higher rank arrays
      module function matrix_norm_3D_to_1D_char_c(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(sp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_char_c
      module function matrix_norm_4D_to_2D_char_c(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(sp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_char_c
      
      !> Matrix norms: complex(sp) rank-2 arrays
      module function matrix_norm_int_c(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(sp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(sp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_int_c
      
      !> Matrix norms: complex(sp) higher rank arrays
      module function matrix_norm_3D_to_1D_int_c(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(sp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_int_c
      module function matrix_norm_4D_to_2D_int_c(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(sp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(sp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_int_c
      
      !> Matrix norms: complex(dp) rank-2 arrays
      module function matrix_norm_char_z(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        character(len=*), intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_char_z
      
      !> Matrix norms: complex(dp) higher rank arrays
      module function matrix_norm_3D_to_1D_char_z(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(dp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_char_z
      module function matrix_norm_4D_to_2D_char_z(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(dp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          character(len=*), intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_char_z
      
      !> Matrix norms: complex(dp) rank-2 arrays
      module function matrix_norm_int_z(a, order, err) result(nrm)
        !> Input matrix a(m,n)
        complex(dp), intent(in), target :: a(:,:)
        !> Norm of the matrix.        
        real(dp) :: nrm
        !> Order of the matrix norm being computed.
        integer(ilp), optional, intent(in) :: order
        !> [optional] state return flag. On error if not requested, the code will stop
        type(linalg_state_type), intent(out), optional :: err      
      end function matrix_norm_int_z
      
      !> Matrix norms: complex(dp) higher rank arrays
      module function matrix_norm_3D_to_1D_int_z(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(dp), intent(in), contiguous, target :: a(:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_3D_to_1D_int_z
      module function matrix_norm_4D_to_2D_int_z(a, order, dim, err) result(nrm)
          !> Input matrix a(m,n)
          complex(dp), intent(in), contiguous, target :: a(:,:,:,:)
          !> Norm of the matrix.        
          real(dp), allocatable :: nrm(:,:)
          !> Order of the matrix norm being computed.
          integer(ilp), optional, intent(in) :: order
          !> [optional] dimensions of the sub-matrices the norms should be evaluated at (default = [1,2])
          integer(ilp), optional, intent(in) :: dim(2)
          !> [optional] state return flag. On error if not requested, the code will stop
          type(linalg_state_type), intent(out), optional :: err        
      end function matrix_norm_4D_to_2D_int_z
  end interface mnorm

contains


    !> Version: experimental
    !>
    !> Constructs the identity matrix.
    !> ([Specification](../page/specs/stdlib_linalg.html#eye-construct-the-identity-matrix))
    pure function eye_rsp(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        real(sp), intent(in) :: mold        
        real(sp), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_rsp
    pure function eye_rdp(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        real(dp), intent(in) , optional :: mold        
        real(dp), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_rdp
    pure function eye_csp(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        complex(sp), intent(in) :: mold        
        complex(sp), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_csp
    pure function eye_cdp(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        complex(dp), intent(in) :: mold        
        complex(dp), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_cdp
    pure function eye_iint8(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        integer(int8), intent(in) :: mold        
        integer(int8), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_iint8
    pure function eye_iint16(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        integer(int16), intent(in) :: mold        
        integer(int16), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_iint16
    pure function eye_iint32(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        integer(int32), intent(in) :: mold        
        integer(int32), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_iint32
    pure function eye_iint64(dim1, dim2, mold) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        integer(int64), intent(in) :: mold        
        integer(int64), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1
        end do

    end function eye_iint64

      function trace_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        real(sp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_rsp
      function trace_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_rdp
      function trace_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        complex(sp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_csp
      function trace_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_cdp
      function trace_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer(int8) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint8
      function trace_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer(int16) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint16
      function trace_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint32
      function trace_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer(int64) :: res
        integer :: i
        res = 0
        do i = 1, minval(shape(A))
          res = res + A(i,i)
        end do
      end function trace_iint64


      pure function is_square_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_rsp
      pure function is_square_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_rdp
      pure function is_square_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_csp
      pure function is_square_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_cdp
      pure function is_square_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint8
      pure function is_square_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint16
      pure function is_square_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint32
      pure function is_square_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        res = (size(A,1) == size(A,2))
      end function is_square_iint64


      pure function is_diagonal_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        real(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_rsp
      pure function is_diagonal_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        real(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_rdp
      pure function is_diagonal_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        complex(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_csp
      pure function is_diagonal_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        complex(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_cdp
      pure function is_diagonal_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        integer(int8), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint8
      pure function is_diagonal_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        integer(int16), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint16
      pure function is_diagonal_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        integer(int32), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint32
      pure function is_diagonal_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        integer(int64), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        do j = 1, n !loop over all columns
            o = min(j-1,m) !index of row above diagonal (or last row)
            do i = 1, o !loop over rows above diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
            do i = o+2, m !loop over rows below diagonal
                if (A(i,j) /= zero) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is diagonal
      end function is_diagonal_iint64


      pure function is_symmetric_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_rsp
      pure function is_symmetric_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_rdp
      pure function is_symmetric_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_csp
      pure function is_symmetric_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_cdp
      pure function is_symmetric_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint8
      pure function is_symmetric_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint16
      pure function is_symmetric_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint32
      pure function is_symmetric_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j-1 !loop over all rows above diagonal
                if (A(i,j) /= A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is symmetric
      end function is_symmetric_iint64


      pure function is_skew_symmetric_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_rsp
      pure function is_skew_symmetric_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_rdp
      pure function is_skew_symmetric_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_csp
      pure function is_skew_symmetric_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_cdp
      pure function is_skew_symmetric_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint8
      pure function is_skew_symmetric_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint16
      pure function is_skew_symmetric_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint32
      pure function is_skew_symmetric_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be skew-symmetric
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= -A(j,i)) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is skew-symmetric
      end function is_skew_symmetric_iint64


      pure function is_hermitian_rsp(A) result(res)
        real(sp), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_rsp
      pure function is_hermitian_rdp(A) result(res)
        real(dp), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_rdp
      pure function is_hermitian_iint8(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint8
      pure function is_hermitian_iint16(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint16
      pure function is_hermitian_iint32(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint32
      pure function is_hermitian_iint64(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        logical :: res
        res = is_symmetric(A) !symmetry and Hermiticity are equivalent for real matrices
      end function is_hermitian_iint64
      pure function is_hermitian_csp(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be Hermitian
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= conjg(A(j,i))) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is Hermitian
      end function is_hermitian_csp
      pure function is_hermitian_cdp(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        logical :: res
        integer :: n, i, j
        if (.not. is_square(A)) then
           res = .false.
           return !nonsquare matrices cannot be Hermitian
        end if
        n = size(A,1) !symmetric dimension of A
        do j = 1, n !loop over all columns
            do i = 1, j !loop over all rows above diagonal (and diagonal)
                if (A(i,j) /= conjg(A(j,i))) then
                  res = .false.
                  return
                end if
            end do
        end do
        res = .true. !otherwise A is Hermitian
      end function is_hermitian_cdp

      pure module function hermitian_rsp(a) result(ah)
        real(sp), intent(in) :: a(:,:)
        real(sp) :: ah(size(a, 2), size(a, 1))
        ah = transpose(a)
      end function hermitian_rsp
      pure module function hermitian_rdp(a) result(ah)
        real(dp), intent(in) :: a(:,:)
        real(dp) :: ah(size(a, 2), size(a, 1))
        ah = transpose(a)
      end function hermitian_rdp
      pure module function hermitian_csp(a) result(ah)
        complex(sp), intent(in) :: a(:,:)
        complex(sp) :: ah(size(a, 2), size(a, 1))
        ah = conjg(transpose(a))
      end function hermitian_csp
      pure module function hermitian_cdp(a) result(ah)
        complex(dp), intent(in) :: a(:,:)
        complex(dp) :: ah(size(a, 2), size(a, 1))
        ah = conjg(transpose(a))
      end function hermitian_cdp
      pure module function hermitian_iint8(a) result(ah)
        integer(int8), intent(in) :: a(:,:)
        integer(int8) :: ah(size(a, 2), size(a, 1))
        ah = transpose(a)
      end function hermitian_iint8
      pure module function hermitian_iint16(a) result(ah)
        integer(int16), intent(in) :: a(:,:)
        integer(int16) :: ah(size(a, 2), size(a, 1))
        ah = transpose(a)
      end function hermitian_iint16
      pure module function hermitian_iint32(a) result(ah)
        integer(int32), intent(in) :: a(:,:)
        integer(int32) :: ah(size(a, 2), size(a, 1))
        ah = transpose(a)
      end function hermitian_iint32
      pure module function hermitian_iint64(a) result(ah)
        integer(int64), intent(in) :: a(:,:)
        integer(int64) :: ah(size(a, 2), size(a, 1))
        ah = transpose(a)
      end function hermitian_iint64

      function is_triangular_rsp(A,uplo) result(res)
        real(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_rsp
      function is_triangular_rdp(A,uplo) result(res)
        real(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_rdp
      function is_triangular_csp(A,uplo) result(res)
        complex(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_csp
      function is_triangular_cdp(A,uplo) result(res)
        complex(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_cdp
      function is_triangular_iint8(A,uplo) result(res)
        integer(int8), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int8), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint8
      function is_triangular_iint16(A,uplo) result(res)
        integer(int16), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int16), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint16
      function is_triangular_iint32(A,uplo) result(res)
        integer(int32), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int32), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint32
      function is_triangular_iint64(A,uplo) result(res)
        integer(int64), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int64), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper triangularity
          do j = 1, n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i = o+2, m !loop over rows below diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower triangularity
          do j=1,n !loop over all columns
              o = min(j-1,m) !index of row above diagonal (or last row)
              do i=1,o !loop over rows above diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_triangular): second argument must be one of {'u','U','l','L'}")
        end if
     
        res = .true. !otherwise A is triangular of the requested type
      end function is_triangular_iint64


      function is_hessenberg_rsp(A,uplo) result(res)
        real(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_rsp
      function is_hessenberg_rdp(A,uplo) result(res)
        real(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        real(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_rdp
      function is_hessenberg_csp(A,uplo) result(res)
        complex(sp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(sp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_csp
      function is_hessenberg_cdp(A,uplo) result(res)
        complex(dp), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        complex(dp), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_cdp
      function is_hessenberg_iint8(A,uplo) result(res)
        integer(int8), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int8), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint8
      function is_hessenberg_iint16(A,uplo) result(res)
        integer(int16), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int16), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint16
      function is_hessenberg_iint32(A,uplo) result(res)
        integer(int32), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int32), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint32
      function is_hessenberg_iint64(A,uplo) result(res)
        integer(int64), intent(in) :: A(:,:)
        character, intent(in) :: uplo
        logical :: res
        integer(int64), parameter :: zero = 0 !zero of relevant type
        integer :: m, n, o, i, j
        m = size(A,1)
        n = size(A,2)
        if ((uplo == 'u') .or. (uplo == 'U')) then !check for upper Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = o+4, m !loop over rows two or more below main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
          end do
        else if ((uplo == 'l') .or. (uplo == 'L')) then !check for lower Hessenberg
          do j = 1, n !loop over all columns
              o = min(j-2,m) !index of row two above diagonal (or last row)
              do i = 1, o !loop over rows one or more above main diagonal
                  if (A(i,j) /= zero) then
                    res = .false.
                    return
                  end if
              end do
           end do
        else
           call error_stop("ERROR (is_hessenberg): second argument must be one of {'u','U','l','L'}")
        end if
        res = .true. !otherwise A is Hessenberg of the requested type
      end function is_hessenberg_iint64
    
end module stdlib_linalg
