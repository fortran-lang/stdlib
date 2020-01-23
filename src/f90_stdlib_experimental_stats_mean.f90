submodule (stdlib_experimental_stats) stdlib_experimental_stats_mean


use stdlib_experimental_error, only: error_stop
implicit none

contains

module function mean_1_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:)
    real(sp) :: res

    integer :: i

    res = 0.0_sp
    do i = 1, size(x)
        res = res + x(i)
    end do
    res = res / real(size(x), sp)

end function mean_1_sp_sp
module function mean_1_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:)
    real(dp) :: res

    integer :: i

    res = 0.0_dp
    do i = 1, size(x)
        res = res + x(i)
    end do
    res = res / real(size(x), dp)

end function mean_1_dp_dp
module function mean_1_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:)
    real(qp) :: res

    integer :: i

    res = 0.0_qp
    do i = 1, size(x)
        res = res + x(i)
    end do
    res = res / real(size(x), qp)

end function mean_1_qp_qp

module function mean_1_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:)
    real(dp) :: res

    integer :: i

    res = 0.0_dp
    do i = 1, size(x)
        res = res + real(x(i), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int8_dp
module function mean_1_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:)
    real(dp) :: res

    integer :: i

    res = 0.0_dp
    do i = 1, size(x)
        res = res + real(x(i), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int16_dp
module function mean_1_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:)
    real(dp) :: res

    integer :: i

    res = 0.0_dp
    do i = 1, size(x)
        res = res + real(x(i), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int32_dp
module function mean_1_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:)
    real(dp) :: res

    integer :: i

    res = 0.0_dp
    do i = 1, size(x)
        res = res + real(x(i), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int64_dp


module function mean_2_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:)
    real(sp) :: res

    integer :: i, i_

    res = 0.0_sp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + x(i, i_)
      end do
    end do
    res = res / real(size(x), sp)

end function mean_2_all_sp_sp
module function mean_2_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i, i_

    res = 0.0_dp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + x(i, i_)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_dp_dp
module function mean_2_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:)
    real(qp) :: res

    integer :: i, i_

    res = 0.0_qp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + x(i, i_)
      end do
    end do
    res = res / real(size(x), qp)

end function mean_2_all_qp_qp

module function mean_2_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i, i_

    res = 0.0_dp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + real(x(i, i_), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int8_dp
module function mean_2_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i, i_

    res = 0.0_dp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + real(x(i, i_), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int16_dp
module function mean_2_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i, i_

    res = 0.0_dp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + real(x(i, i_), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int32_dp
module function mean_2_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i, i_

    res = 0.0_dp
    do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
        res = res + real(x(i, i_), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int64_dp

module function mean_2_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(sp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_sp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + x(i, i_)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + x(i, i_)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), sp)

end function mean_2_sp_sp
module function mean_2_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(dp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + x(i, i_)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + x(i, i_)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_2_dp_dp
module function mean_2_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(qp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_qp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + x(i, i_)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + x(i, i_)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), qp)

end function mean_2_qp_qp

module function mean_2_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(dp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + real(x(i, i_), dp)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + real(x(i, i_), dp)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_2_int8_dp
module function mean_2_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(dp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + real(x(i, i_), dp)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + real(x(i, i_), dp)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_2_int16_dp
module function mean_2_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(dp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + real(x(i, i_), dp)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + real(x(i, i_), dp)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_2_int32_dp
module function mean_2_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(dp) :: res(size(x)/size(x, dim))

    integer :: i, i_

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i_) = res(i_) + real(x(i, i_), dp)
         end do
       end do
     case(2)
       do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
           res(i) = res(i) + real(x(i, i_), dp)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_2_int64_dp







module function mean_3_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:)
    real(sp) :: res

    integer :: i,i_,i__

    res = 0.0_sp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + x(i,i_,i__)
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_3_all_sp_sp
module function mean_4_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_sp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + x(i,i_,i__,i___)
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_4_all_sp_sp
module function mean_5_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_sp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + x(i,i_,i__,i___,i____)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_5_all_sp_sp
module function mean_6_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_sp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + x(i,i_,i__,i___,i____,i_____)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_6_all_sp_sp
module function mean_7_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_sp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + x(i,i_,i__,i___,i____,i_____,i______)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_7_all_sp_sp
module function mean_3_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:)
    real(dp) :: res

    integer :: i,i_,i__

    res = 0.0_dp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + x(i,i_,i__)
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_dp_dp
module function mean_4_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_dp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + x(i,i_,i__,i___)
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_dp_dp
module function mean_5_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + x(i,i_,i__,i___,i____)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_5_all_dp_dp
module function mean_6_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + x(i,i_,i__,i___,i____,i_____)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_6_all_dp_dp
module function mean_7_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + x(i,i_,i__,i___,i____,i_____,i______)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_7_all_dp_dp
module function mean_3_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:)
    real(qp) :: res

    integer :: i,i_,i__

    res = 0.0_qp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + x(i,i_,i__)
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_3_all_qp_qp
module function mean_4_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_qp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + x(i,i_,i__,i___)
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_4_all_qp_qp
module function mean_5_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_qp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + x(i,i_,i__,i___,i____)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_5_all_qp_qp
module function mean_6_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_qp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + x(i,i_,i__,i___,i____,i_____)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_6_all_qp_qp
module function mean_7_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_qp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + x(i,i_,i__,i___,i____,i_____,i______)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_7_all_qp_qp

module function mean_3_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:)
    real(dp) :: res

    integer :: i,i_,i__

    res = 0.0_dp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + real(x(i,i_,i__), dp)
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int8_dp
module function mean_4_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_dp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + real(x(i,i_,i__,i___), dp)
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int8_dp
module function mean_5_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + real(x(i,i_,i__,i___,i____), dp)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_5_all_int8_dp
module function mean_6_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + real(x(i,i_,i__,i___,i____,i_____), dp)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_6_all_int8_dp
module function mean_7_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_7_all_int8_dp
module function mean_3_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:)
    real(dp) :: res

    integer :: i,i_,i__

    res = 0.0_dp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + real(x(i,i_,i__), dp)
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int16_dp
module function mean_4_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_dp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + real(x(i,i_,i__,i___), dp)
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int16_dp
module function mean_5_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + real(x(i,i_,i__,i___,i____), dp)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_5_all_int16_dp
module function mean_6_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + real(x(i,i_,i__,i___,i____,i_____), dp)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_6_all_int16_dp
module function mean_7_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_7_all_int16_dp
module function mean_3_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:)
    real(dp) :: res

    integer :: i,i_,i__

    res = 0.0_dp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + real(x(i,i_,i__), dp)
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int32_dp
module function mean_4_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_dp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + real(x(i,i_,i__,i___), dp)
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int32_dp
module function mean_5_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + real(x(i,i_,i__,i___,i____), dp)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_5_all_int32_dp
module function mean_6_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + real(x(i,i_,i__,i___,i____,i_____), dp)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_6_all_int32_dp
module function mean_7_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_7_all_int32_dp
module function mean_3_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:)
    real(dp) :: res

    integer :: i,i_,i__

    res = 0.0_dp

    do i__ = 1, size(x, 3)
     do i_ = 1, size(x, 2)
      do i = 1, size(x, 1)
       res = res + real(x(i,i_,i__), dp)
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int64_dp
module function mean_4_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___

    res = 0.0_dp

    do i___ = 1, size(x, 4)
     do i__ = 1, size(x, 3)
      do i_ = 1, size(x, 2)
       do i = 1, size(x, 1)
        res = res + real(x(i,i_,i__,i___), dp)
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int64_dp
module function mean_5_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    do i____ = 1, size(x, 5)
     do i___ = 1, size(x, 4)
      do i__ = 1, size(x, 3)
       do i_ = 1, size(x, 2)
        do i = 1, size(x, 1)
         res = res + real(x(i,i_,i__,i___,i____), dp)
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_5_all_int64_dp
module function mean_6_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    do i_____ = 1, size(x, 6)
     do i____ = 1, size(x, 5)
      do i___ = 1, size(x, 4)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res = res + real(x(i,i_,i__,i___,i____,i_____), dp)
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_6_all_int64_dp
module function mean_7_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    do i______ = 1, size(x, 7)
     do i_____ = 1, size(x, 6)
      do i____ = 1, size(x, 5)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res = res + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_7_all_int64_dp

module function mean_3_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_sp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + x(i,i_,i__)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + x(i,i_,i__)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + x(i,i_,i__)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), sp)

end function mean_3_sp_sp
module function mean_4_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_sp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), sp)

end function mean_4_sp_sp
module function mean_5_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_sp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), sp)

end function mean_5_sp_sp
module function mean_6_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_sp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), sp)

end function mean_6_sp_sp
module function mean_7_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_sp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), sp)

end function mean_7_sp_sp
module function mean_3_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + x(i,i_,i__)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + x(i,i_,i__)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + x(i,i_,i__)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_3_dp_dp
module function mean_4_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_4_dp_dp
module function mean_5_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_5_dp_dp
module function mean_6_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_6_dp_dp
module function mean_7_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_7_dp_dp
module function mean_3_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_qp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + x(i,i_,i__)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + x(i,i_,i__)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + x(i,i_,i__)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), qp)

end function mean_3_qp_qp
module function mean_4_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_qp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + x(i,i_,i__,i___)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), qp)

end function mean_4_qp_qp
module function mean_5_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_qp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + x(i,i_,i__,i___,i____)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), qp)

end function mean_5_qp_qp
module function mean_6_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_qp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + x(i,i_,i__,i___,i____,i_____)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), qp)

end function mean_6_qp_qp
module function mean_7_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_qp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + x(i,i_,i__,i___,i____,i_____,i______)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), qp)

end function mean_7_qp_qp

module function mean_3_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_3_int8_dp
module function mean_4_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_4_int8_dp
module function mean_5_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_5_int8_dp
module function mean_6_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_6_int8_dp
module function mean_7_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______),&
                  & dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_7_int8_dp
module function mean_3_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_3_int16_dp
module function mean_4_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_4_int16_dp
module function mean_5_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_5_int16_dp
module function mean_6_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_6_int16_dp
module function mean_7_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______),&
                  & dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_7_int16_dp
module function mean_3_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_3_int32_dp
module function mean_4_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_4_int32_dp
module function mean_5_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_5_int32_dp
module function mean_6_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_6_int32_dp
module function mean_7_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______),&
                  & dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_7_int32_dp
module function mean_3_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ) )

    integer :: i,i_,i__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i_,i__) = res(i_,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(2)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i__) = res(i,i__) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case(3)
       do i__ = 1, size(x, 3)
        do i_ = 1, size(x, 2)
         do i = 1, size(x, 1)
          res(i,i_) = res(i,i_) + real(x(i,i_,i__), dp)
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_3_int64_dp
module function mean_4_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ) )

    integer :: i,i_,i__,i___

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i_,i__,i___) = res(i_,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(2)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i__,i___) = res(i,i__,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(3)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i___) = res(i,i_,i___) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case(4)
       do i___ = 1, size(x, 4)
        do i__ = 1, size(x, 3)
         do i_ = 1, size(x, 2)
          do i = 1, size(x, 1)
           res(i,i_,i__) = res(i,i_,i__) + real(x(i,i_,i__,i___), dp)
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_4_int64_dp
module function mean_5_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ) )

    integer :: i,i_,i__,i___,i____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i_,i__,i___,i____) = res(i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i__,i___,i____) = res(i,i__,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i___,i____) = res(i,i_,i___,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i____) = res(i,i_,i__,i____) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res(i,i_,i__,i___) = res(i,i_,i__,i___) + real(x(i,i_,i__,i___,i____), dp)
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_5_int64_dp
module function mean_6_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i_,i__,i___,i____,i_____) = res(i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i__,i___,i____,i_____) = res(i,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i___,i____,i_____) = res(i,i_,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i____,i_____) = res(i,i_,i__,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i_____) = res(i,i_,i__,i___,i_____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res(i,i_,i__,i___,i____) = res(i,i_,i__,i___,i____) + real(x(i,i_,i__,i___,i____,i_____), dp)
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_6_int64_dp
module function mean_7_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i_,i__,i___,i____,i_____,i______) = res(i_,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i__,i___,i____,i_____,i______) = res(i,i__,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i___,i____,i_____,i______) = res(i,i_,i___,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i____,i_____,i______) = res(i,i_,i__,i____,i_____,i______) +&
                  & real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i_____,i______) = res(i,i_,i__,i___,i_____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______),&
                  & dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i______) = res(i,i_,i__,i___,i____,i______) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res(i,i_,i__,i___,i____,i_____) = res(i,i_,i__,i___,i____,i_____) + real(x(i,i_,i__,i___,i____,i_____,i______), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_7_int64_dp

end submodule
