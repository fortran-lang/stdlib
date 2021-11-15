submodule (stdlib_linalg) stdlib_linalg_outer_product

  implicit none

contains

    pure module function outer_product_rsp(u, v) result(res)
      real(sp), intent(in) :: u(:), v(:)
      real(sp) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_rsp
    pure module function outer_product_rdp(u, v) result(res)
      real(dp), intent(in) :: u(:), v(:)
      real(dp) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_rdp
    pure module function outer_product_csp(u, v) result(res)
      complex(sp), intent(in) :: u(:), v(:)
      complex(sp) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_csp
    pure module function outer_product_cdp(u, v) result(res)
      complex(dp), intent(in) :: u(:), v(:)
      complex(dp) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_cdp
    pure module function outer_product_iint8(u, v) result(res)
      integer(int8), intent(in) :: u(:), v(:)
      integer(int8) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_iint8
    pure module function outer_product_iint16(u, v) result(res)
      integer(int16), intent(in) :: u(:), v(:)
      integer(int16) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_iint16
    pure module function outer_product_iint32(u, v) result(res)
      integer(int32), intent(in) :: u(:), v(:)
      integer(int32) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_iint32
    pure module function outer_product_iint64(u, v) result(res)
      integer(int64), intent(in) :: u(:), v(:)
      integer(int64) :: res(size(u),size(v))
      integer :: col
      do col = 1, size(v)
        res(:,col) = v(col) * u
      end do
    end function outer_product_iint64

end submodule
