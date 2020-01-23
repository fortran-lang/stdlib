submodule (stdlib_experimental_stats) stdlib_experimental_stats_mean


use stdlib_experimental_error, only: error_stop
implicit none

contains

module function mean_1_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:)
    real(sp) :: res

    integer :: i1

    res = 0.0_sp
    do i1 = 1, size(x)
        res = res + x(i1)
    end do
    res = res / real(size(x), sp)

end function mean_1_sp_sp
module function mean_1_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:)
    real(dp) :: res

    integer :: i1

    res = 0.0_dp
    do i1 = 1, size(x)
        res = res + x(i1)
    end do
    res = res / real(size(x), dp)

end function mean_1_dp_dp
module function mean_1_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:)
    real(qp) :: res

    integer :: i1

    res = 0.0_qp
    do i1 = 1, size(x)
        res = res + x(i1)
    end do
    res = res / real(size(x), qp)

end function mean_1_qp_qp

module function mean_1_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:)
    real(dp) :: res

    integer :: i1

    res = 0.0_dp
    do i1 = 1, size(x)
        res = res + real(x(i1), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int8_dp
module function mean_1_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:)
    real(dp) :: res

    integer :: i1

    res = 0.0_dp
    do i1 = 1, size(x)
        res = res + real(x(i1), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int16_dp
module function mean_1_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:)
    real(dp) :: res

    integer :: i1

    res = 0.0_dp
    do i1 = 1, size(x)
        res = res + real(x(i1), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int32_dp
module function mean_1_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:)
    real(dp) :: res

    integer :: i1

    res = 0.0_dp
    do i1 = 1, size(x)
        res = res + real(x(i1), dp)
    end do
    res = res / real(size(x), dp)

end function mean_1_int64_dp


module function mean_2_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:)
    real(sp) :: res

    integer :: i1, i2

    res = 0.0_sp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + x(i1, i2)
      end do
    end do
    res = res / real(size(x), sp)

end function mean_2_all_sp_sp
module function mean_2_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i1, i2

    res = 0.0_dp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + x(i1, i2)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_dp_dp
module function mean_2_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:)
    real(qp) :: res

    integer :: i1, i2

    res = 0.0_qp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + x(i1, i2)
      end do
    end do
    res = res / real(size(x), qp)

end function mean_2_all_qp_qp

module function mean_2_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i1, i2

    res = 0.0_dp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + real(x(i1, i2), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int8_dp
module function mean_2_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i1, i2

    res = 0.0_dp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + real(x(i1, i2), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int16_dp
module function mean_2_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i1, i2

    res = 0.0_dp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + real(x(i1, i2), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int32_dp
module function mean_2_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:)
    real(dp) :: res

    integer :: i1, i2

    res = 0.0_dp
    do i2 = 1, size(x, 2)
      do i1 = 1, size(x, 1)
        res = res + real(x(i1, i2), dp)
      end do
    end do
    res = res / real(size(x), dp)

end function mean_2_all_int64_dp

module function mean_2_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:)
    integer, intent(in) :: dim
    real(sp) :: res(size(x)/size(x, dim))

    integer :: i1, i2

    res = 0.0_sp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + x(i1, i2)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + x(i1, i2)
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

    integer :: i1, i2

    res = 0.0_dp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + x(i1, i2)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + x(i1, i2)
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

    integer :: i1, i2

    res = 0.0_qp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + x(i1, i2)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + x(i1, i2)
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

    integer :: i1, i2

    res = 0.0_dp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + real(x(i1, i2), dp)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + real(x(i1, i2), dp)
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

    integer :: i1, i2

    res = 0.0_dp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + real(x(i1, i2), dp)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + real(x(i1, i2), dp)
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

    integer :: i1, i2

    res = 0.0_dp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + real(x(i1, i2), dp)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + real(x(i1, i2), dp)
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

    integer :: i1, i2

    res = 0.0_dp

    select case(dim)
     case(1)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i2) = res(i2) + real(x(i1, i2), dp)
         end do
       end do
     case(2)
       do i2 = 1, size(x, 2)
         do i1 = 1, size(x, 1)
           res(i1) = res(i1) + real(x(i1, i2), dp)
         end do
       end do
     case default
       call error_stop("ERROR (mean): wrong dimension")
    end select

    res = res / real(size(x, dim), dp)

end function mean_2_int64_dp



!As proposed by @arady




module function mean_3_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:)
    real(sp) :: res

    integer :: i1,i2,i3

    res = 0.0_sp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + x(i1,i2,i3)
        end do
      end do
    end do

    res = res / real(size(x), sp)

end function mean_3_all_sp_sp
module function mean_4_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:)
    real(sp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_sp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + x(i1,i2,i3,i4)
          end do
        end do
      end do
    end do

    res = res / real(size(x), sp)

end function mean_4_all_sp_sp
module function mean_5_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:)
    real(sp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_sp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + x(i1,i2,i3,i4,i5)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_sp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + x(i1,i2,i3,i4,i5,i6)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_sp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + x(i1,i2,i3,i4,i5,i6,i7)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + x(i1,i2,i3)
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_dp_dp
module function mean_4_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + x(i1,i2,i3,i4)
          end do
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_dp_dp
module function mean_5_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + x(i1,i2,i3,i4,i5)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + x(i1,i2,i3,i4,i5,i6)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + x(i1,i2,i3,i4,i5,i6,i7)
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

    integer :: i1,i2,i3

    res = 0.0_qp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + x(i1,i2,i3)
        end do
      end do
    end do

    res = res / real(size(x), qp)

end function mean_3_all_qp_qp
module function mean_4_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:)
    real(qp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_qp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + x(i1,i2,i3,i4)
          end do
        end do
      end do
    end do

    res = res / real(size(x), qp)

end function mean_4_all_qp_qp
module function mean_5_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:)
    real(qp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_qp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + x(i1,i2,i3,i4,i5)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_qp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + x(i1,i2,i3,i4,i5,i6)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_qp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + x(i1,i2,i3,i4,i5,i6,i7)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + real(x(i1,i2,i3), dp)
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int8_dp
module function mean_4_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + real(x(i1,i2,i3,i4), dp)
          end do
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int8_dp
module function mean_5_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + real(x(i1,i2,i3), dp)
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int16_dp
module function mean_4_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + real(x(i1,i2,i3,i4), dp)
          end do
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int16_dp
module function mean_5_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + real(x(i1,i2,i3), dp)
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int32_dp
module function mean_4_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + real(x(i1,i2,i3,i4), dp)
          end do
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int32_dp
module function mean_5_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    do i3 = 1, size(x, 3)
      do i2 = 1, size(x, 2)
        do i1 = 1, size(x, 1)
          res = res + real(x(i1,i2,i3), dp)
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_3_all_int64_dp
module function mean_4_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    do i4 = 1, size(x, 4)
      do i3 = 1, size(x, 3)
        do i2 = 1, size(x, 2)
          do i1 = 1, size(x, 1)
            res = res + real(x(i1,i2,i3,i4), dp)
          end do
        end do
      end do
    end do

    res = res / real(size(x), dp)

end function mean_4_all_int64_dp
module function mean_5_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    do i5 = 1, size(x, 5)
      do i4 = 1, size(x, 4)
        do i3 = 1, size(x, 3)
          do i2 = 1, size(x, 2)
            do i1 = 1, size(x, 1)
              res = res + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    do i6 = 1, size(x, 6)
      do i5 = 1, size(x, 5)
        do i4 = 1, size(x, 4)
          do i3 = 1, size(x, 3)
            do i2 = 1, size(x, 2)
              do i1 = 1, size(x, 1)
                res = res + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    do i7 = 1, size(x, 7)
      do i6 = 1, size(x, 6)
        do i5 = 1, size(x, 5)
          do i4 = 1, size(x, 4)
            do i3 = 1, size(x, 3)
              do i2 = 1, size(x, 2)
                do i1 = 1, size(x, 1)
                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_sp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + x(i1,i2,i3)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + x(i1,i2,i3)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + x(i1,i2,i3)
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

    integer :: i1,i2,i3,i4

    res = 0.0_sp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + x(i1,i2,i3,i4)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_sp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + x(i1,i2,i3,i4,i5)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_sp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + x(i1,i2,i3,i4,i5,i6)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_sp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6,i7)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + x(i1,i2,i3)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + x(i1,i2,i3)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + x(i1,i2,i3)
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

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + x(i1,i2,i3,i4)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + x(i1,i2,i3,i4,i5)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + x(i1,i2,i3,i4,i5,i6)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6,i7)
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

    integer :: i1,i2,i3

    res = 0.0_qp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + x(i1,i2,i3)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + x(i1,i2,i3)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + x(i1,i2,i3)
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

    integer :: i1,i2,i3,i4

    res = 0.0_qp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + x(i1,i2,i3,i4)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + x(i1,i2,i3,i4)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_qp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + x(i1,i2,i3,i4,i5)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + x(i1,i2,i3,i4,i5)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_qp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + x(i1,i2,i3,i4,i5,i6)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + x(i1,i2,i3,i4,i5,i6)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_qp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + x(i1,i2,i3,i4,i5,i6,i7)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + x(i1,i2,i3,i4,i5,i6,i7)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + real(x(i1,i2,i3), dp)
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

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + real(x(i1,i2,i3,i4), dp)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + real(x(i1,i2,i3), dp)
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

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + real(x(i1,i2,i3,i4), dp)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + real(x(i1,i2,i3), dp)
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

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + real(x(i1,i2,i3,i4), dp)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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

    integer :: i1,i2,i3

    res = 0.0_dp

    select case(dim)
     case(1)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i2,i3) = res(i2,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(2)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i3) = res(i1,i3) + real(x(i1,i2,i3), dp)
           end do
         end do
       end do
     case(3)
       do i3 = 1, size(x, 3)
         do i2 = 1, size(x, 2)
           do i1 = 1, size(x, 1)
             res(i1,i2) = res(i1,i2) + real(x(i1,i2,i3), dp)
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

    integer :: i1,i2,i3,i4

    res = 0.0_dp

    select case(dim)
     case(1)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i2,i3,i4) = res(i2,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(2)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i3,i4) = res(i1,i3,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(3)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i4) = res(i1,i2,i4) + real(x(i1,i2,i3,i4), dp)
             end do
           end do
         end do
       end do
     case(4)
       do i4 = 1, size(x, 4)
         do i3 = 1, size(x, 3)
           do i2 = 1, size(x, 2)
             do i1 = 1, size(x, 1)
               res(i1,i2,i3) = res(i1,i2,i3) + real(x(i1,i2,i3,i4), dp)
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

    integer :: i1,i2,i3,i4,i5

    res = 0.0_dp

    select case(dim)
     case(1)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i2,i3,i4,i5) = res(i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i3,i4,i5) = res(i1,i3,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i4,i5) = res(i1,i2,i4,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i5) = res(i1,i2,i3,i5) + real(x(i1,i2,i3,i4,i5), dp)
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i5 = 1, size(x, 5)
         do i4 = 1, size(x, 4)
           do i3 = 1, size(x, 3)
             do i2 = 1, size(x, 2)
               do i1 = 1, size(x, 1)
                 res(i1,i2,i3,i4) = res(i1,i2,i3,i4) + real(x(i1,i2,i3,i4,i5), dp)
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

    integer :: i1,i2,i3,i4,i5,i6

    res = 0.0_dp

    select case(dim)
     case(1)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i2,i3,i4,i5,i6) = res(i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i3,i4,i5,i6) = res(i1,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i4,i5,i6) = res(i1,i2,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i5,i6) = res(i1,i2,i3,i5,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i6) = res(i1,i2,i3,i4,i6) + real(x(i1,i2,i3,i4,i5,i6), dp)
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i6 = 1, size(x, 6)
         do i5 = 1, size(x, 5)
           do i4 = 1, size(x, 4)
             do i3 = 1, size(x, 3)
               do i2 = 1, size(x, 2)
                 do i1 = 1, size(x, 1)
                   res(i1,i2,i3,i4,i5) = res(i1,i2,i3,i4,i5) + real(x(i1,i2,i3,i4,i5,i6), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7

    res = 0.0_dp

    select case(dim)
     case(1)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i2,i3,i4,i5,i6,i7) = res(i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i3,i4,i5,i6,i7) = res(i1,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i4,i5,i6,i7) = res(i1,i2,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i5,i6,i7) = res(i1,i2,i3,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i6,i7) = res(i1,i2,i3,i4,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i7) = res(i1,i2,i3,i4,i5,i7) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i7 = 1, size(x, 7)
         do i6 = 1, size(x, 6)
           do i5 = 1, size(x, 5)
             do i4 = 1, size(x, 4)
               do i3 = 1, size(x, 3)
                 do i2 = 1, size(x, 2)
                   do i1 = 1, size(x, 1)
                     res(i1,i2,i3,i4,i5,i6) = res(i1,i2,i3,i4,i5,i6) + real(x(i1,i2,i3,i4,i5,i6,i7), dp)
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
