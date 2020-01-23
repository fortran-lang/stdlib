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
module function mean_8_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_sp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_8_all_sp_sp
module function mean_9_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_sp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_9_all_sp_sp
module function mean_10_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_sp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_10_all_sp_sp
module function mean_11_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_sp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_11_all_sp_sp
module function mean_12_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_sp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_12_all_sp_sp
module function mean_13_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_sp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_________&
                     &___)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_13_all_sp_sp
module function mean_14_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_sp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i________&
                      &____,i_____________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_14_all_sp_sp
module function mean_15_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_sp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                       &_____,i_____________,i______________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), sp)

end function mean_15_all_sp_sp
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
module function mean_8_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_8_all_dp_dp
module function mean_9_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_9_all_dp_dp
module function mean_10_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_10_all_dp_dp
module function mean_11_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_11_all_dp_dp
module function mean_12_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_12_all_dp_dp
module function mean_13_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_________&
                     &___)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_13_all_dp_dp
module function mean_14_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i________&
                      &____,i_____________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_14_all_dp_dp
module function mean_15_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                       &_____,i_____________,i______________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_15_all_dp_dp
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
module function mean_8_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_qp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_8_all_qp_qp
module function mean_9_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_qp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_9_all_qp_qp
module function mean_10_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_qp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_10_all_qp_qp
module function mean_11_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_qp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_11_all_qp_qp
module function mean_12_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_qp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_12_all_qp_qp
module function mean_13_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_qp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_________&
                     &___)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_13_all_qp_qp
module function mean_14_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_qp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i________&
                      &____,i_____________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_14_all_qp_qp
module function mean_15_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_qp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                       &_____,i_____________,i______________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), qp)

end function mean_15_all_qp_qp

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
module function mean_8_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_8_all_int8_dp
module function mean_9_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_9_all_int8_dp
module function mean_10_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_10_all_int8_dp
module function mean_11_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_11_all_int8_dp
module function mean_12_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_12_all_int8_dp
module function mean_13_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                     &________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_13_all_int8_dp
module function mean_14_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                      &_________,i_____________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_14_all_int8_dp
module function mean_15_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                       &__________,i_____________,i______________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_15_all_int8_dp
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
module function mean_8_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_8_all_int16_dp
module function mean_9_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_9_all_int16_dp
module function mean_10_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_10_all_int16_dp
module function mean_11_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_11_all_int16_dp
module function mean_12_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_12_all_int16_dp
module function mean_13_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                     &________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_13_all_int16_dp
module function mean_14_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                      &_________,i_____________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_14_all_int16_dp
module function mean_15_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                       &__________,i_____________,i______________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_15_all_int16_dp
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
module function mean_8_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_8_all_int32_dp
module function mean_9_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_9_all_int32_dp
module function mean_10_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_10_all_int32_dp
module function mean_11_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_11_all_int32_dp
module function mean_12_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_12_all_int32_dp
module function mean_13_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                     &________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_13_all_int32_dp
module function mean_14_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                      &_________,i_____________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_14_all_int32_dp
module function mean_15_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                       &__________,i_____________,i______________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_15_all_int32_dp
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
module function mean_8_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    do i_______ = 1, size(x, 8)
     do i______ = 1, size(x, 7)
      do i_____ = 1, size(x, 6)
       do i____ = 1, size(x, 5)
        do i___ = 1, size(x, 4)
         do i__ = 1, size(x, 3)
          do i_ = 1, size(x, 2)
           do i = 1, size(x, 1)
            res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_8_all_int64_dp
module function mean_9_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    do i________ = 1, size(x, 9)
     do i_______ = 1, size(x, 8)
      do i______ = 1, size(x, 7)
       do i_____ = 1, size(x, 6)
        do i____ = 1, size(x, 5)
         do i___ = 1, size(x, 4)
          do i__ = 1, size(x, 3)
           do i_ = 1, size(x, 2)
            do i = 1, size(x, 1)
             res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_9_all_int64_dp
module function mean_10_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    do i_________ = 1, size(x, 10)
     do i________ = 1, size(x, 9)
      do i_______ = 1, size(x, 8)
       do i______ = 1, size(x, 7)
        do i_____ = 1, size(x, 6)
         do i____ = 1, size(x, 5)
          do i___ = 1, size(x, 4)
           do i__ = 1, size(x, 3)
            do i_ = 1, size(x, 2)
             do i = 1, size(x, 1)
              res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_10_all_int64_dp
module function mean_11_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    do i__________ = 1, size(x, 11)
     do i_________ = 1, size(x, 10)
      do i________ = 1, size(x, 9)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_11_all_int64_dp
module function mean_12_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    do i___________ = 1, size(x, 12)
     do i__________ = 1, size(x, 11)
      do i_________ = 1, size(x, 10)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_12_all_int64_dp
module function mean_13_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    do i____________ = 1, size(x, 13)
     do i___________ = 1, size(x, 12)
      do i__________ = 1, size(x, 11)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                     &________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_13_all_int64_dp
module function mean_14_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    do i_____________ = 1, size(x, 14)
     do i____________ = 1, size(x, 13)
      do i___________ = 1, size(x, 12)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                      &_________,i_____________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_14_all_int64_dp
module function mean_15_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    do i______________ = 1, size(x, 15)
     do i_____________ = 1, size(x, 14)
      do i____________ = 1, size(x, 13)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res = res + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                       &__________,i_____________,i______________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
      end do
     end do
    end do

    res = res / real(size(x), dp)

end function mean_15_all_int64_dp

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
module function mean_8_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_sp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
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

end function mean_8_sp_sp
module function mean_9_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_sp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
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

end function mean_9_sp_sp
module function mean_10_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_sp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
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

end function mean_10_sp_sp
module function mean_11_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_sp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
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

end function mean_11_sp_sp
module function mean_12_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_sp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_sp_sp
module function mean_13_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_sp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_________&
                        &___)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_sp_sp
module function mean_14_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_sp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i____&
                         &______,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i______&
                         &____,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i________&
                         &__,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________&
                         &,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i&
                         &___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                         &_________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                         &_______,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i______&
                         &_____,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i________&
                         &___,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,&
                         &i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_&
                         &___________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                         &_________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_sp_sp
module function mean_15_sp_sp(x, dim) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(sp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_sp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,&
                          &i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_&
                          &______,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i___&
                          &____,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_____&
                          &__,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______&
                          &,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i&
                          &________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i__&
                          &______,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                          &____,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i______&
                          &__,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________&
                          &,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i&
                          &_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__&
                          &_______,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i____&
                          &_____,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i______&
                          &___,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i________&
                          &_,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_sp_sp
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
module function mean_8_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
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

end function mean_8_dp_dp
module function mean_9_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
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

end function mean_9_dp_dp
module function mean_10_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
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

end function mean_10_dp_dp
module function mean_11_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
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

end function mean_11_dp_dp
module function mean_12_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_dp_dp
module function mean_13_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_________&
                        &___)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_dp_dp
module function mean_14_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i____&
                         &______,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i______&
                         &____,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i________&
                         &__,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________&
                         &,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i&
                         &___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                         &_________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                         &_______,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i______&
                         &_____,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i________&
                         &___,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,&
                         &i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_&
                         &___________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                         &_________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_dp_dp
module function mean_15_dp_dp(x, dim) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,&
                          &i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_&
                          &______,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i___&
                          &____,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_____&
                          &__,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______&
                          &,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i&
                          &________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i__&
                          &______,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                          &____,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i______&
                          &__,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________&
                          &,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i&
                          &_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__&
                          &_______,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i____&
                          &_____,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i______&
                          &___,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i________&
                          &_,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_dp_dp
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
module function mean_8_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_qp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & x(i,i_,i__,i___,i____,i_____,i______,i_______)
              end do
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

end function mean_8_qp_qp
module function mean_9_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_qp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________)
               end do
              end do
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

end function mean_9_qp_qp
module function mean_10_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_qp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________)
                end do
               end do
              end do
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

end function mean_10_qp_qp
module function mean_11_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_qp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________)
                 end do
                end do
               end do
              end do
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

end function mean_11_qp_qp
module function mean_12_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_qp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_qp_qp
module function mean_13_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_qp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_________&
                        &___)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__________&
                        &__)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_qp_qp
module function mean_14_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_qp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i____&
                         &______,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i______&
                         &____,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i________&
                         &__,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________&
                         &,i___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i&
                         &___________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                         &_________,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                         &_______,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i______&
                         &_____,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i________&
                         &___,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,&
                         &i____________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_&
                         &___________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i___&
                         &_________,i_____________)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_qp_qp
module function mean_15_qp_qp(x, dim) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(qp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_qp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,&
                          &i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_&
                          &______,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i___&
                          &____,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_____&
                          &__,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______&
                          &,i________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i&
                          &________,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i__&
                          &______,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                          &____,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i______&
                          &__,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________&
                          &,i_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i&
                          &_________,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__&
                          &_______,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i____&
                          &_____,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i______&
                          &___,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i________&
                          &_,i__________,i___________,i____________,i_____________,i______________)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_qp_qp

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
module function mean_8_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
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

end function mean_8_int8_dp
module function mean_9_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
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

end function mean_9_int8_dp
module function mean_10_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
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

end function mean_10_int8_dp
module function mean_11_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
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

end function mean_11_int8_dp
module function mean_12_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_int8_dp
module function mean_13_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                        &________), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_int8_dp
module function mean_14_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,&
                         &i__________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_&
                         &_________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___&
                         &_______,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                         &_____,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                         &___,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                         &_,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,&
                         &i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_&
                         &__________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                         &________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                         &______,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_______&
                         &____,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_________&
                         &__,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_int8_dp
module function mean_15_int8_dp(x, dim) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i__&
                          &____,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________&
                          &), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i____&
                          &__,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______&
                          &,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i&
                          &_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i__&
                          &_____,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i____&
                          &___,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i______&
                          &_,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,&
                          &i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_&
                          &_______,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                          &_____,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_____&
                          &___,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_______&
                          &_,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,&
                          &i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_&
                          &________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i___&
                          &______,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_int8_dp
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
module function mean_8_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
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

end function mean_8_int16_dp
module function mean_9_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
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

end function mean_9_int16_dp
module function mean_10_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
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

end function mean_10_int16_dp
module function mean_11_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
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

end function mean_11_int16_dp
module function mean_12_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_int16_dp
module function mean_13_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                        &________), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_int16_dp
module function mean_14_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,&
                         &i__________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_&
                         &_________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___&
                         &_______,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                         &_____,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                         &___,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                         &_,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,&
                         &i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_&
                         &__________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                         &________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                         &______,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_______&
                         &____,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_________&
                         &__,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_int16_dp
module function mean_15_int16_dp(x, dim) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i__&
                          &____,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________&
                          &), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i____&
                          &__,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______&
                          &,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i&
                          &_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i__&
                          &_____,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i____&
                          &___,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i______&
                          &_,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,&
                          &i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_&
                          &_______,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                          &_____,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_____&
                          &___,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_______&
                          &_,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,&
                          &i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_&
                          &________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i___&
                          &______,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_int16_dp
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
module function mean_8_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
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

end function mean_8_int32_dp
module function mean_9_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
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

end function mean_9_int32_dp
module function mean_10_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
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

end function mean_10_int32_dp
module function mean_11_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
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

end function mean_11_int32_dp
module function mean_12_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_int32_dp
module function mean_13_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                        &________), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_int32_dp
module function mean_14_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,&
                         &i__________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_&
                         &_________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___&
                         &_______,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                         &_____,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                         &___,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                         &_,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,&
                         &i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_&
                         &__________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                         &________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                         &______,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_______&
                         &____,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_________&
                         &__,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_int32_dp
module function mean_15_int32_dp(x, dim) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i__&
                          &____,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________&
                          &), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i____&
                          &__,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______&
                          &,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i&
                          &_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i__&
                          &_____,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i____&
                          &___,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i______&
                          &_,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,&
                          &i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_&
                          &_______,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                          &_____,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_____&
                          &___,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_______&
                          &_,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,&
                          &i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_&
                          &________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i___&
                          &______,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_int32_dp
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
module function mean_8_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i_,i__,i___,i____,i_____,i______,i_______) = res(i_,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i__,i___,i____,i_____,i______,i_______) = res(i,i__,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i___,i____,i_____,i______,i_______) = res(i,i_,i___,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i____,i_____,i______,i_______) = res(i,i_,i__,i____,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i_____,i______,i_______) = res(i,i_,i__,i___,i_____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i______,i_______) = res(i,i_,i__,i___,i____,i______,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i_______) = res(i,i_,i__,i___,i____,i_____,i_______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_______ = 1, size(x, 8)
        do i______ = 1, size(x, 7)
         do i_____ = 1, size(x, 6)
          do i____ = 1, size(x, 5)
           do i___ = 1, size(x, 4)
            do i__ = 1, size(x, 3)
             do i_ = 1, size(x, 2)
              do i = 1, size(x, 1)
               res(i,i_,i__,i___,i____,i_____,i______) = res(i,i_,i__,i___,i____,i_____,i______) +&
                   & real(x(i,i_,i__,i___,i____,i_____,i______,i_______), dp)
              end do
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

end function mean_8_int64_dp
module function mean_9_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i__,i___,i____,i_____,i______,i_______,i________)&
                    & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i___,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i____,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i_____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i______,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i_______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i________) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i________ = 1, size(x, 9)
        do i_______ = 1, size(x, 8)
         do i______ = 1, size(x, 7)
          do i_____ = 1, size(x, 6)
           do i____ = 1, size(x, 5)
            do i___ = 1, size(x, 4)
             do i__ = 1, size(x, 3)
              do i_ = 1, size(x, 2)
               do i = 1, size(x, 1)
                res(i,i_,i__,i___,i____,i_____,i______,i_______) = res(i,i_,i__,i___,i____,i_____,i______,i_______) +&
                    & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________), dp)
               end do
              end do
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

end function mean_9_int64_dp
module function mean_10_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i_,i__,i___,i____,i_____,i______,i______&
                     &_,i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i__,i___,i____,i_____,i______,i_______,&
                     &i________,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i___,i____,i_____,i______,i_______,i_&
                     &_______,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i____,i_____,i______,i_______,i___&
                     &_____,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i_____,i______,i_______,i_____&
                     &___,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i______,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i______,i_______,i_______&
                     &_,i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,&
                     &i_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i________,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_&
                     &________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                     &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_________ = 1, size(x, 10)
        do i________ = 1, size(x, 9)
         do i_______ = 1, size(x, 8)
          do i______ = 1, size(x, 7)
           do i_____ = 1, size(x, 6)
            do i____ = 1, size(x, 5)
             do i___ = 1, size(x, 4)
              do i__ = 1, size(x, 3)
               do i_ = 1, size(x, 2)
                do i = 1, size(x, 1)
                 res(i,i_,i__,i___,i____,i_____,i______,i_______,i________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i____&
                     &____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________), dp)
                end do
               end do
              end do
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

end function mean_10_int64_dp
module function mean_11_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i__________ = 1, size(x, 11)
        do i_________ = 1, size(x, 10)
         do i________ = 1, size(x, 9)
          do i_______ = 1, size(x, 8)
           do i______ = 1, size(x, 7)
            do i_____ = 1, size(x, 6)
             do i____ = 1, size(x, 5)
              do i___ = 1, size(x, 4)
               do i__ = 1, size(x, 3)
                do i_ = 1, size(x, 2)
                 do i = 1, size(x, 1)
                  res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) =&
                      & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________) +&
                      & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________), dp)
                 end do
                end do
               end do
              end do
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

end function mean_11_int64_dp
module function mean_12_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i___________ = 1, size(x, 12)
        do i__________ = 1, size(x, 11)
         do i_________ = 1, size(x, 10)
          do i________ = 1, size(x, 9)
           do i_______ = 1, size(x, 8)
            do i______ = 1, size(x, 7)
             do i_____ = 1, size(x, 6)
              do i____ = 1, size(x, 5)
               do i___ = 1, size(x, 4)
                do i__ = 1, size(x, 3)
                 do i_ = 1, size(x, 2)
                  do i = 1, size(x, 1)
                   res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) =&
                       & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________) +&
                       & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________), dp)
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_12_int64_dp
module function mean_13_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________&
                        &) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                        &________), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                        & + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____&
                        &_______), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i____________ = 1, size(x, 13)
        do i___________ = 1, size(x, 12)
         do i__________ = 1, size(x, 11)
          do i_________ = 1, size(x, 10)
           do i________ = 1, size(x, 9)
            do i_______ = 1, size(x, 8)
             do i______ = 1, size(x, 7)
              do i_____ = 1, size(x, 6)
               do i____ = 1, size(x, 5)
                do i___ = 1, size(x, 4)
                 do i__ = 1, size(x, 3)
                  do i_ = 1, size(x, 2)
                   do i = 1, size(x, 1)
                    res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) =&
                        & res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________) +&
                        & real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                        &_____), dp)
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_13_int64_dp
module function mean_14_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__

    res = 0.0_dp

    select case(dim)
     case(1)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                         &__________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,&
                         &i__________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                         &_________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i&
                         &____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_&
                         &_________,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                         &________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i__&
                         &__________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___&
                         &_______,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                         &_______) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____&
                         &________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                         &_____,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                         &______) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i______&
                         &______,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                         &___,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i________&
                         &_____) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i________&
                         &____,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                         &_,i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i_________&
                         &____) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i__________&
                         &__,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,&
                         &i___________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i__________&
                         &___) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________&
                         &,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_&
                         &__________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i___________&
                         &__) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i&
                         &_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                         &________,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i____________&
                         &_) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i__&
                         &___________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                         &______,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i_____________&
                         &) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____&
                         &_________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_______&
                         &____,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i______&
                         &_______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_________&
                         &__,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &______) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__________&
                         &_,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i_____________ = 1, size(x, 14)
        do i____________ = 1, size(x, 13)
         do i___________ = 1, size(x, 12)
          do i__________ = 1, size(x, 11)
           do i_________ = 1, size(x, 10)
            do i________ = 1, size(x, 9)
             do i_______ = 1, size(x, 8)
              do i______ = 1, size(x, 7)
               do i_____ = 1, size(x, 6)
                do i____ = 1, size(x, 5)
                 do i___ = 1, size(x, 4)
                  do i__ = 1, size(x, 3)
                   do i_ = 1, size(x, 2)
                    do i = 1, size(x, 1)
                     res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________)&
                         & = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_______&
                         &_____) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________&
                         &,i____________,i_____________), dp)
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_14_int64_dp
module function mean_15_int64_dp(x, dim) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
    integer, intent(in) :: dim
    real(dp) :: res( &
                  merge(size(x,1),size(x,2),mask = 1 < dim ), &
                  merge(size(x,2),size(x,3),mask = 2 < dim ), &
                  merge(size(x,3),size(x,4),mask = 3 < dim ), &
                  merge(size(x,4),size(x,5),mask = 4 < dim ), &
                  merge(size(x,5),size(x,6),mask = 5 < dim ), &
                  merge(size(x,6),size(x,7),mask = 6 < dim ), &
                  merge(size(x,7),size(x,8),mask = 7 < dim ), &
                  merge(size(x,8),size(x,9),mask = 8 < dim ), &
                  merge(size(x,9),size(x,10),mask = 9 < dim ), &
                  merge(size(x,10),size(x,11),mask = 10 < dim ), &
                  merge(size(x,11),size(x,12),mask = 11 < dim ), &
                  merge(size(x,12),size(x,13),mask = 12 < dim ), &
                  merge(size(x,13),size(x,14),mask = 13 < dim ), &
                  merge(size(x,14),size(x,15),mask = 14 < dim ) )

    integer :: i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___________&
        &__,i______________

    res = 0.0_dp

    select case(dim)
     case(1)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i__&
                          &___________,i______________) = res(i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_____&
                          &_____,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i__&
                          &____,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________&
                          &), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(2)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i___&
                          &__________,i______________) = res(i,i__,i___,i____,i_____,i______,i_______,i________,i_________,i_______&
                          &___,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i____&
                          &__,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(3)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i____&
                          &_________,i______________) = res(i,i_,i___,i____,i_____,i______,i_______,i________,i_________,i_________&
                          &_,i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______&
                          &,i_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________),&
                          & dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(4)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_____&
                          &________,i______________) = res(i,i_,i__,i____,i_____,i______,i_______,i________,i_________,i__________,&
                          &i___________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i&
                          &_______,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(5)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i______&
                          &_______,i______________) = res(i,i_,i__,i___,i_____,i______,i_______,i________,i_________,i__________,i_&
                          &__________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i__&
                          &_____,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(6)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___________,i____________,i_______&
                          &______,i______________) = res(i,i_,i__,i___,i____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i____&
                          &___,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(7)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i___________,i____________,i________&
                          &_____,i______________) = res(i,i_,i__,i___,i____,i_____,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i______&
                          &_,i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(8)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i___________,i____________,i_________&
                          &____,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i________,i_________,i__________,i_______&
                          &____,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,&
                          &i________,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(9)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i___________,i____________,i__________&
                          &___,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i_________,i__________,i_________&
                          &__,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_&
                          &_______,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(10)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________,i____________,i___________&
                          &__,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i__________,i___________&
                          &,i____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i___&
                          &_____,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(11)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i____________,i____________&
                          &_,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i___________,i&
                          &____________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_____&
                          &___,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(12)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____________,i_____________&
                          &,i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i__&
                          &__________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i_______&
                          &_,i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(13)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i_____________,&
                          &i______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___&
                          &________,i_____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,&
                          &i_________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(14)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &______________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i____&
                          &_______,i____________,i______________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_&
                          &________,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
             end do
            end do
           end do
          end do
         end do
        end do
       end do
     case(15)
       do i______________ = 1, size(x, 15)
        do i_____________ = 1, size(x, 14)
         do i____________ = 1, size(x, 13)
          do i___________ = 1, size(x, 12)
           do i__________ = 1, size(x, 11)
            do i_________ = 1, size(x, 10)
             do i________ = 1, size(x, 9)
              do i_______ = 1, size(x, 8)
               do i______ = 1, size(x, 7)
                do i_____ = 1, size(x, 6)
                 do i____ = 1, size(x, 5)
                  do i___ = 1, size(x, 4)
                   do i__ = 1, size(x, 3)
                    do i_ = 1, size(x, 2)
                     do i = 1, size(x, 1)
                      res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i___________,i____________,i&
                          &_____________) = res(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i_________,i__________,i_____&
                          &______,i____________,i_____________) + real(x(i,i_,i__,i___,i____,i_____,i______,i_______,i________,i___&
                          &______,i__________,i___________,i____________,i_____________,i______________), dp)
                     end do
                    end do
                   end do
                  end do
                 end do
                end do
               end do
              end do
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

end function mean_15_int64_dp

end submodule
