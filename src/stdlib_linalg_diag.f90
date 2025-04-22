submodule (stdlib_linalg) stdlib_linalg_diag

  implicit none

contains

      module pure function diag_rsp(v) result(res)
        real(sp), intent(in) :: v(:)
        real(sp) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_rsp
      module pure function diag_rdp(v) result(res)
        real(dp), intent(in) :: v(:)
        real(dp) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_rdp
      module pure function diag_csp(v) result(res)
        complex(sp), intent(in) :: v(:)
        complex(sp) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_csp
      module pure function diag_cdp(v) result(res)
        complex(dp), intent(in) :: v(:)
        complex(dp) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_cdp
      module pure function diag_iint8(v) result(res)
        integer(int8), intent(in) :: v(:)
        integer(int8) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_iint8
      module pure function diag_iint16(v) result(res)
        integer(int16), intent(in) :: v(:)
        integer(int16) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_iint16
      module pure function diag_iint32(v) result(res)
        integer(int32), intent(in) :: v(:)
        integer(int32) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_iint32
      module pure function diag_iint64(v) result(res)
        integer(int64), intent(in) :: v(:)
        integer(int64) :: res(size(v),size(v))
        integer :: i
        res = 0
        do i = 1, size(v)
          res(i,i) = v(i)
        end do
      end function diag_iint64


      module pure function diag_rsp_k(v,k) result(res)
        real(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(sp) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_rsp_k
      module pure function diag_rdp_k(v,k) result(res)
        real(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        real(dp) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_rdp_k
      module pure function diag_csp_k(v,k) result(res)
        complex(sp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(sp) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_csp_k
      module pure function diag_cdp_k(v,k) result(res)
        complex(dp), intent(in) :: v(:)
        integer, intent(in) :: k
        complex(dp) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_cdp_k
      module pure function diag_iint8_k(v,k) result(res)
        integer(int8), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int8) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_iint8_k
      module pure function diag_iint16_k(v,k) result(res)
        integer(int16), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int16) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_iint16_k
      module pure function diag_iint32_k(v,k) result(res)
        integer(int32), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int32) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_iint32_k
      module pure function diag_iint64_k(v,k) result(res)
        integer(int64), intent(in) :: v(:)
        integer, intent(in) :: k
        integer(int64) :: res(size(v)+abs(k),size(v)+abs(k))
        integer :: i, sz
        sz = size(v)
        res = 0
        if (k > 0) then
          do i = 1, sz
              res(i,k+i) = v(i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i+abs(k),i) = v(i)
          end do
        else
          do i = 1, sz
              res(i,i) = v(i)
          end do
        end if
      end function diag_iint64_k

      module pure function diag_rsp_mat(A) result(res)
        real(sp), intent(in) :: A(:,:)
        real(sp) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_rsp_mat
      module pure function diag_rdp_mat(A) result(res)
        real(dp), intent(in) :: A(:,:)
        real(dp) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_rdp_mat
      module pure function diag_csp_mat(A) result(res)
        complex(sp), intent(in) :: A(:,:)
        complex(sp) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_csp_mat
      module pure function diag_cdp_mat(A) result(res)
        complex(dp), intent(in) :: A(:,:)
        complex(dp) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_cdp_mat
      module pure function diag_iint8_mat(A) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer(int8) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_iint8_mat
      module pure function diag_iint16_mat(A) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer(int16) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_iint16_mat
      module pure function diag_iint32_mat(A) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer(int32) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_iint32_mat
      module pure function diag_iint64_mat(A) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer(int64) :: res(minval(shape(A)))
        integer :: i
        do i = 1, minval(shape(A))
          res(i) = A(i,i)
        end do
      end function diag_iint64_mat

      module pure function diag_rsp_mat_k(A,k) result(res)
        real(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(sp) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_rsp_mat_k
      module pure function diag_rdp_mat_k(A,k) result(res)
        real(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        real(dp) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_rdp_mat_k
      module pure function diag_csp_mat_k(A,k) result(res)
        complex(sp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(sp) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_csp_mat_k
      module pure function diag_cdp_mat_k(A,k) result(res)
        complex(dp), intent(in) :: A(:,:)
        integer, intent(in) :: k
        complex(dp) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_cdp_mat_k
      module pure function diag_iint8_mat_k(A,k) result(res)
        integer(int8), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int8) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_iint8_mat_k
      module pure function diag_iint16_mat_k(A,k) result(res)
        integer(int16), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int16) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_iint16_mat_k
      module pure function diag_iint32_mat_k(A,k) result(res)
        integer(int32), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int32) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_iint32_mat_k
      module pure function diag_iint64_mat_k(A,k) result(res)
        integer(int64), intent(in) :: A(:,:)
        integer, intent(in) :: k
        integer(int64) :: res(minval(shape(A))-abs(k))
        integer :: i, sz
        sz = minval(shape(A))-abs(k)
        if (k > 0) then
          do i = 1, sz
              res(i) = A(i,k+i)
          end do
        else if (k < 0) then
          do i = 1, sz
              res(i) = A(i+abs(k),i)
          end do
        else
          do i = 1, sz
              res(i) = A(i,i)
          end do
        end if
      end function diag_iint64_mat_k

end submodule
