module stdlib_linalg
  !!Provides a support for various linear algebra procedures
  !! ([Specification](../page/specs/stdlib_linalg.html))
  use stdlib_kinds, only: sp, dp, xdp, qp, &
    int8, int16, int32, int64
  use stdlib_optval, only: optval
  implicit none
  private

  public :: diag
  public :: eye
  public :: trace
  public :: outer_product

  interface diag
    !! version: experimental
    !!
    !! Creates a diagonal array or extract the diagonal elements of an array
    !! ([Specification](../page/specs/stdlib_linalg.html#description))
      !
      ! Vector to matrix
      !
      module function diag_rsp(v) result(res)
        real(sp), intent(in) :: v(:)
        real(sp) :: res(size(v),size(v))
      end function diag_rsp
      module function diag_rdp(v) result(res)
        real(dp), intent(in) :: v(:)
        real(dp) :: res(size(v),size(v))
      end function diag_rdp
      module function diag_csp(v) result(res)
        complex(sp), intent(in) :: v(:)
        complex(sp) :: res(size(v),size(v))
      end function diag_csp
      module function diag_cdp(v) result(res)
        complex(dp), intent(in) :: v(:)
        complex(dp) :: res(size(v),size(v))
      end function diag_cdp
      module function diag_iint8(v) result(res)
        integer(int8), intent(in) :: v(:)
        integer(int8) :: res(size(v),size(v))
      end function diag_iint8
      module function diag_iint16(v) result(res)
        integer(int16), intent(in) :: v(:)
        integer(int16) :: res(size(v),size(v))
      end function diag_iint16
      module function diag_iint32(v) result(res)
        integer(int32), intent(in) :: v(:)
        integer(int32) :: res(size(v),size(v))
      end function diag_iint32
      module function diag_iint64(v) result(res)
        integer(int64), intent(in) :: v(:)
        integer(int64) :: res(size(v),size(v))
      end function diag_iint64
      module function diag_rsp_k(v,k) result(res)
        real(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(sp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rsp_k
      module function diag_rdp_k(v,k) result(res)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(dp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_rdp_k
      module function diag_csp_k(v,k) result(res)
        complex(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(sp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_csp_k
      module function diag_cdp_k(v,k) result(res)
        complex(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(dp) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_cdp_k
      module function diag_iint8_k(v,k) result(res)
        integer(int8), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int8) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint8_k
      module function diag_iint16_k(v,k) result(res)
        integer(int16), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int16) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint16_k
      module function diag_iint32_k(v,k) result(res)
        integer(int32), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int32) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint32_k
      module function diag_iint64_k(v,k) result(res)
        integer(int64), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int64) :: res(size(v)+abs(k),size(v)+abs(k))
      end function diag_iint64_k

      !
      ! Matrix to vector
      !
      module function diag_rsp_mat(A) result(res)
        real(sp), intent(in) :: A(:,:)
        real(sp) :: res(minval(shape(A)))
      end function diag_rsp_mat
      module function diag_rdp_mat(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res(minval(shape(A)))
      end function diag_rdp_mat
      module function diag_csp_mat(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        complex(sp) :: res(minval(shape(A)))
      end function diag_csp_mat
      module function diag_cdp_mat(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res(minval(shape(A)))
      end function diag_cdp_mat
      module function diag_iint8_mat(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer(int8) :: res(minval(shape(A)))
      end function diag_iint8_mat
      module function diag_iint16_mat(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer(int16) :: res(minval(shape(A)))
      end function diag_iint16_mat
      module function diag_iint32_mat(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res(minval(shape(A)))
      end function diag_iint32_mat
      module function diag_iint64_mat(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer(int64) :: res(minval(shape(A)))
      end function diag_iint64_mat
      module function diag_rsp_mat_k(A,k) result(res)
        real(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(sp) :: res(minval(shape(A))-abs(k))
      end function diag_rsp_mat_k
      module function diag_rdp_mat_k(A,k) result(res)
        real(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(dp) :: res(minval(shape(A))-abs(k))
      end function diag_rdp_mat_k
      module function diag_csp_mat_k(A,k) result(res)
        complex(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(sp) :: res(minval(shape(A))-abs(k))
      end function diag_csp_mat_k
      module function diag_cdp_mat_k(A,k) result(res)
        complex(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(dp) :: res(minval(shape(A))-abs(k))
      end function diag_cdp_mat_k
      module function diag_iint8_mat_k(A,k) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int8) :: res(minval(shape(A))-abs(k))
      end function diag_iint8_mat_k
      module function diag_iint16_mat_k(A,k) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int16) :: res(minval(shape(A))-abs(k))
      end function diag_iint16_mat_k
      module function diag_iint32_mat_k(A,k) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int32) :: res(minval(shape(A))-abs(k))
      end function diag_iint32_mat_k
      module function diag_iint64_mat_k(A,k) result(res)
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
    !! ([Specification](../page/specs/stdlib_linalg.html#description_2))
      module procedure trace_rsp
      module procedure trace_rdp
      module procedure trace_csp
      module procedure trace_cdp
      module procedure trace_iint8
      module procedure trace_iint16
      module procedure trace_iint32
      module procedure trace_iint64
  end interface


  ! Outer product (of two vectors)
  interface outer_product
    !! version: experimental
    !!
    !! Computes the outer product of two vectors, returning a rank-2 array
    !! ([Specification](../page/specs/stdlib_linalg.html#description_3))
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

contains

    !> Version: experimental
    !>
    !> Constructs the identity matrix.
    !> ([Specification](../page/specs/stdlib_linalg.html#eye-construct-the-identity-matrix))
    pure function eye(dim1, dim2) result(result)

        integer, intent(in) :: dim1
        integer, intent(in), optional :: dim2
        integer(int8), allocatable :: result(:, :)

        integer :: dim2_
        integer :: i

        dim2_ = optval(dim2, dim1)
        allocate(result(dim1, dim2_))
        
        result = 0_int8
        do i = 1, min(dim1, dim2_)
            result(i, i) = 1_int8
        end do

    end function eye

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

end module stdlib_linalg
