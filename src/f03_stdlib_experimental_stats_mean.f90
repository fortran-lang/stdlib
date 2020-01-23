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
module function mean_8_all_sp_sp(x) result(res)
    real(sp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(sp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_sp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + x(i1,i2,i3,i4,i5,i6,i7,i8)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_sp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_sp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_sp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_sp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_sp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_sp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_sp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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
module function mean_8_all_dp_dp(x) result(res)
    real(dp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + x(i1,i2,i3,i4,i5,i6,i7,i8)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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
module function mean_8_all_qp_qp(x) result(res)
    real(qp), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(qp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_qp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + x(i1,i2,i3,i4,i5,i6,i7,i8)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_qp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_qp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_qp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_qp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_qp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_qp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_qp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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
module function mean_8_all_int8_dp(x) result(res)
    integer(int8), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15), dp)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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
module function mean_8_all_int16_dp(x) result(res)
    integer(int16), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15), dp)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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
module function mean_8_all_int32_dp(x) result(res)
    integer(int32), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15), dp)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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
module function mean_8_all_int64_dp(x) result(res)
    integer(int64), intent(in) :: x(:,:,:,:,:,:,:,:)
    real(dp) :: res

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    do i8 = 1, size(x, 8)
      do i7 = 1, size(x, 7)
        do i6 = 1, size(x, 6)
          do i5 = 1, size(x, 5)
            do i4 = 1, size(x, 4)
              do i3 = 1, size(x, 3)
                do i2 = 1, size(x, 2)
                  do i1 = 1, size(x, 1)
                    res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    do i9 = 1, size(x, 9)
      do i8 = 1, size(x, 8)
        do i7 = 1, size(x, 7)
          do i6 = 1, size(x, 6)
            do i5 = 1, size(x, 5)
              do i4 = 1, size(x, 4)
                do i3 = 1, size(x, 3)
                  do i2 = 1, size(x, 2)
                    do i1 = 1, size(x, 1)
                      res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    do i10 = 1, size(x, 10)
      do i9 = 1, size(x, 9)
        do i8 = 1, size(x, 8)
          do i7 = 1, size(x, 7)
            do i6 = 1, size(x, 6)
              do i5 = 1, size(x, 5)
                do i4 = 1, size(x, 4)
                  do i3 = 1, size(x, 3)
                    do i2 = 1, size(x, 2)
                      do i1 = 1, size(x, 1)
                        res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    do i11 = 1, size(x, 11)
      do i10 = 1, size(x, 10)
        do i9 = 1, size(x, 9)
          do i8 = 1, size(x, 8)
            do i7 = 1, size(x, 7)
              do i6 = 1, size(x, 6)
                do i5 = 1, size(x, 5)
                  do i4 = 1, size(x, 4)
                    do i3 = 1, size(x, 3)
                      do i2 = 1, size(x, 2)
                        do i1 = 1, size(x, 1)
                          res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    do i12 = 1, size(x, 12)
      do i11 = 1, size(x, 11)
        do i10 = 1, size(x, 10)
          do i9 = 1, size(x, 9)
            do i8 = 1, size(x, 8)
              do i7 = 1, size(x, 7)
                do i6 = 1, size(x, 6)
                  do i5 = 1, size(x, 5)
                    do i4 = 1, size(x, 4)
                      do i3 = 1, size(x, 3)
                        do i2 = 1, size(x, 2)
                          do i1 = 1, size(x, 1)
                            res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    do i13 = 1, size(x, 13)
      do i12 = 1, size(x, 12)
        do i11 = 1, size(x, 11)
          do i10 = 1, size(x, 10)
            do i9 = 1, size(x, 9)
              do i8 = 1, size(x, 8)
                do i7 = 1, size(x, 7)
                  do i6 = 1, size(x, 6)
                    do i5 = 1, size(x, 5)
                      do i4 = 1, size(x, 4)
                        do i3 = 1, size(x, 3)
                          do i2 = 1, size(x, 2)
                            do i1 = 1, size(x, 1)
                              res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    do i14 = 1, size(x, 14)
      do i13 = 1, size(x, 13)
        do i12 = 1, size(x, 12)
          do i11 = 1, size(x, 11)
            do i10 = 1, size(x, 10)
              do i9 = 1, size(x, 9)
                do i8 = 1, size(x, 8)
                  do i7 = 1, size(x, 7)
                    do i6 = 1, size(x, 6)
                      do i5 = 1, size(x, 5)
                        do i4 = 1, size(x, 4)
                          do i3 = 1, size(x, 3)
                            do i2 = 1, size(x, 2)
                              do i1 = 1, size(x, 1)
                                res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    do i15 = 1, size(x, 15)
      do i14 = 1, size(x, 14)
        do i13 = 1, size(x, 13)
          do i12 = 1, size(x, 12)
            do i11 = 1, size(x, 11)
              do i10 = 1, size(x, 10)
                do i9 = 1, size(x, 9)
                  do i8 = 1, size(x, 8)
                    do i7 = 1, size(x, 7)
                      do i6 = 1, size(x, 6)
                        do i5 = 1, size(x, 5)
                          do i4 = 1, size(x, 4)
                            do i3 = 1, size(x, 3)
                              do i2 = 1, size(x, 2)
                                do i1 = 1, size(x, 1)
                                  res = res + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15), dp)
                                end do
                              end do
                            end do
                          end do
                        end do
                      end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_sp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7,i8)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_sp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_sp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_sp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_sp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_sp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_sp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_sp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7,i8)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_qp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + x(i1,i2,i3,i4,i5,i6,i7,i8)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_qp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_qp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_qp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_qp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_qp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_qp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_qp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,&
                                         &i11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i&
                                         &11,i12,i13,i14,i15)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8

    res = 0.0_dp

    select case(dim)
     case(1)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i2,i3,i4,i5,i6,i7,i8) = res(i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(2)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i3,i4,i5,i6,i7,i8) = res(i1,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(3)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i4,i5,i6,i7,i8) = res(i1,i2,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(4)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i5,i6,i7,i8) = res(i1,i2,i3,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(5)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i6,i7,i8) = res(i1,i2,i3,i4,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(6)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i7,i8) = res(i1,i2,i3,i4,i5,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(7)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i8) = res(i1,i2,i3,i4,i5,i6,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
                     end do
                   end do
                 end do
               end do
             end do
           end do
         end do
       end do
     case(8)
       do i8 = 1, size(x, 8)
         do i7 = 1, size(x, 7)
           do i6 = 1, size(x, 6)
             do i5 = 1, size(x, 5)
               do i4 = 1, size(x, 4)
                 do i3 = 1, size(x, 3)
                   do i2 = 1, size(x, 2)
                     do i1 = 1, size(x, 1)
                       res(i1,i2,i3,i4,i5,i6,i7) = res(i1,i2,i3,i4,i5,i6,i7) + real(x(i1,i2,i3,i4,i5,i6,i7,i8), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9

    res = 0.0_dp

    select case(dim)
     case(1)
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i2,i3,i4,i5,i6,i7,i8,i9) = res(i2,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i3,i4,i5,i6,i7,i8,i9) = res(i1,i3,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i4,i5,i6,i7,i8,i9) = res(i1,i2,i4,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i5,i6,i7,i8,i9) = res(i1,i2,i3,i5,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i6,i7,i8,i9) = res(i1,i2,i3,i4,i6,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i7,i8,i9) = res(i1,i2,i3,i4,i5,i7,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i8,i9) = res(i1,i2,i3,i4,i5,i6,i8,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i9) = res(i1,i2,i3,i4,i5,i6,i7,i9) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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
       do i9 = 1, size(x, 9)
         do i8 = 1, size(x, 8)
           do i7 = 1, size(x, 7)
             do i6 = 1, size(x, 6)
               do i5 = 1, size(x, 5)
                 do i4 = 1, size(x, 4)
                   do i3 = 1, size(x, 3)
                     do i2 = 1, size(x, 2)
                       do i1 = 1, size(x, 1)
                         res(i1,i2,i3,i4,i5,i6,i7,i8) = res(i1,i2,i3,i4,i5,i6,i7,i8) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9), dp)
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10

    res = 0.0_dp

    select case(dim)
     case(1)
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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
       do i10 = 1, size(x, 10)
         do i9 = 1, size(x, 9)
           do i8 = 1, size(x, 8)
             do i7 = 1, size(x, 7)
               do i6 = 1, size(x, 6)
                 do i5 = 1, size(x, 5)
                   do i4 = 1, size(x, 4)
                     do i3 = 1, size(x, 3)
                       do i2 = 1, size(x, 2)
                         do i1 = 1, size(x, 1)
                           res(i1,i2,i3,i4,i5,i6,i7,i8,i9) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9) +&
                               & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10), dp)
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11

    res = 0.0_dp

    select case(dim)
     case(1)
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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
       do i11 = 1, size(x, 11)
         do i10 = 1, size(x, 10)
           do i9 = 1, size(x, 9)
             do i8 = 1, size(x, 8)
               do i7 = 1, size(x, 7)
                 do i6 = 1, size(x, 6)
                   do i5 = 1, size(x, 5)
                     do i4 = 1, size(x, 4)
                       do i3 = 1, size(x, 3)
                         do i2 = 1, size(x, 2)
                           do i1 = 1, size(x, 1)
                             res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10) +&
                                 & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11), dp)
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12

    res = 0.0_dp

    select case(dim)
     case(1)
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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
       do i12 = 1, size(x, 12)
         do i11 = 1, size(x, 11)
           do i10 = 1, size(x, 10)
             do i9 = 1, size(x, 9)
               do i8 = 1, size(x, 8)
                 do i7 = 1, size(x, 7)
                   do i6 = 1, size(x, 6)
                     do i5 = 1, size(x, 5)
                       do i4 = 1, size(x, 4)
                         do i3 = 1, size(x, 3)
                           do i2 = 1, size(x, 2)
                             do i1 = 1, size(x, 1)
                               res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11) +&
                                   & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12), dp)
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13

    res = 0.0_dp

    select case(dim)
     case(1)
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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
       do i13 = 1, size(x, 13)
         do i12 = 1, size(x, 12)
           do i11 = 1, size(x, 11)
             do i10 = 1, size(x, 10)
               do i9 = 1, size(x, 9)
                 do i8 = 1, size(x, 8)
                   do i7 = 1, size(x, 7)
                     do i6 = 1, size(x, 6)
                       do i5 = 1, size(x, 5)
                         do i4 = 1, size(x, 4)
                           do i3 = 1, size(x, 3)
                             do i2 = 1, size(x, 2)
                               do i1 = 1, size(x, 1)
                                 res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12) +&
                                     & real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13), dp)
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14

    res = 0.0_dp

    select case(dim)
     case(1)
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i&
                                       &14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i14 = 1, size(x, 14)
         do i13 = 1, size(x, 13)
           do i12 = 1, size(x, 12)
             do i11 = 1, size(x, 11)
               do i10 = 1, size(x, 10)
                 do i9 = 1, size(x, 9)
                   do i8 = 1, size(x, 8)
                     do i7 = 1, size(x, 7)
                       do i6 = 1, size(x, 6)
                         do i5 = 1, size(x, 5)
                           do i4 = 1, size(x, 4)
                             do i3 = 1, size(x, 3)
                               do i2 = 1, size(x, 2)
                                 do i1 = 1, size(x, 1)
                                   res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13) = res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13&
                                       &) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14), dp)
                                 end do
                               end do
                             end do
                           end do
                         end do
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

    integer :: i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15

    res = 0.0_dp

    select case(dim)
     case(1)
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i7,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i8,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i9,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i10,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9&
                                         &,i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i11,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i12,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i13,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i14,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i15) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
       do i15 = 1, size(x, 15)
         do i14 = 1, size(x, 14)
           do i13 = 1, size(x, 13)
             do i12 = 1, size(x, 12)
               do i11 = 1, size(x, 11)
                 do i10 = 1, size(x, 10)
                   do i9 = 1, size(x, 9)
                     do i8 = 1, size(x, 8)
                       do i7 = 1, size(x, 7)
                         do i6 = 1, size(x, 6)
                           do i5 = 1, size(x, 5)
                             do i4 = 1, size(x, 4)
                               do i3 = 1, size(x, 3)
                                 do i2 = 1, size(x, 2)
                                   do i1 = 1, size(x, 1)
                                     res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) =&
                                         & res(i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14) + real(x(i1,i2,i3,i4,i5,i6,i7,i8,i9,&
                                         &i10,i11,i12,i13,i14,i15), dp)
                                   end do
                                 end do
                               end do
                             end do
                           end do
                         end do
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
