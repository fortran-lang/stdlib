! SPDX-Identifier: MIT


module test_math
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, qp
    use stdlib_error, only : check
    use stdlib_math

    implicit none

    contains
        subroutine test_clip_integer_int8(x, xmin, xmax, compare)
            integer(int8), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_integer_int8

        subroutine test_clip_integer_int16(x, xmin, xmax, compare)
            integer(int16), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_integer_int16

        subroutine test_clip_integer_int32(x, xmin, xmax, compare)
            integer(int32), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_integer_int32

        subroutine test_clip_integer_int64(x, xmin, xmax, compare)
            integer(int64), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_integer_int64

        subroutine test_clip_real_sp(x, xmin, xmax, compare)
            real(sp), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_real_sp

        subroutine test_clip_real_dp(x, xmin, xmax, compare)
            real(dp), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_real_dp

        subroutine test_clip_real_qp(x, xmin, xmax, compare)
            real(qp), intent(in) :: x, xmin, xmax, compare

            call check(clip(x, xmin, xmax) == compare)
        
        end subroutine test_clip_real_qp


end module test_math


program tester
    use test_math
    implicit none

    ! test case format: (x, xmin, xmax, correct answer)
    ! valid case: xmin is not greater than xmax
    ! invalid case: xmin is greater than xmax


    ! data declaration
        integer(int8) :: x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8
        integer(int16) :: x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16
        integer(int32) :: x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32
        integer(int64) :: x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64
        real(sp) :: x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp
        real(dp) :: x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp
        real(qp) :: x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp

        ! type: integer, kind: int8
            ! valid test cases
            x_integer_int8 = 2
            xmin_integer_int8 = -2
            xmax_integer_int8 = 5
            compare_integer_int8 = 2
            call test_clip_integer_int8(x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8)

            x_integer_int8 = 127
            xmin_integer_int8 = -127
            xmax_integer_int8 = 0
            compare_integer_int8 = 0
            call test_clip_integer_int8(x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8)

            x_integer_int8 = -57
            xmin_integer_int8 = -57
            xmax_integer_int8 = 57
            compare_integer_int8 = -57
            call test_clip_integer_int8(x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8)

            ! invalid test cases
            x_integer_int8 = 2
            xmin_integer_int8 = 5
            xmax_integer_int8 = -2
            compare_integer_int8 = 5
            call test_clip_integer_int8(x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8)

            x_integer_int8 = 127
            xmin_integer_int8 = 0
            xmax_integer_int8 = -127
            compare_integer_int8 = 0
            call test_clip_integer_int8(x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8)

            x_integer_int8 = -57
            xmin_integer_int8 = 57
            xmax_integer_int8 = -57
            compare_integer_int8 = 57
            call test_clip_integer_int8(x_integer_int8, xmin_integer_int8, xmax_integer_int8, compare_integer_int8)

        ! type: integer, kind: int16
            ! valid test cases
            x_integer_int16 = 2
            xmin_integer_int16 = -2
            xmax_integer_int16 = 5
            compare_integer_int16 = 2
            call test_clip_integer_int16(x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16)

            x_integer_int16 = 32767
            xmin_integer_int16 = -32767
            xmax_integer_int16 = 0
            compare_integer_int16 = 0
            call test_clip_integer_int16(x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16)

            x_integer_int16 = -598
            xmin_integer_int16 = -32
            xmax_integer_int16 = 676
            compare_integer_int16 = -32
            call test_clip_integer_int16(x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16)

            ! invalid test cases
            x_integer_int16 = 2
            xmin_integer_int16 = 5
            xmax_integer_int16 = -2
            compare_integer_int16 = 5
            call test_clip_integer_int16(x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16)

            x_integer_int16 = 32767
            xmin_integer_int16 = 0
            xmax_integer_int16 = -32767
            compare_integer_int16 = 0
            call test_clip_integer_int16(x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16)

            x_integer_int16 = -598
            xmin_integer_int16 = 676
            xmax_integer_int16 = -32
            compare_integer_int16 = 676
            call test_clip_integer_int16(x_integer_int16, xmin_integer_int16, xmax_integer_int16, compare_integer_int16)

        ! type: integer, kind: int32
            ! valid test cases
            x_integer_int32 = 2
            xmin_integer_int32 = -2
            xmax_integer_int32 = 5
            compare_integer_int32 = 2
            call test_clip_integer_int32(x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32)

            x_integer_int32 = -2147483647
            xmin_integer_int32 = 0
            xmax_integer_int32 = 2147483647
            compare_integer_int32 = 0
            call test_clip_integer_int32(x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32)

            x_integer_int32 = 45732
            xmin_integer_int32 = -385769
            xmax_integer_int32 = 57642
            compare_integer_int32 = 45732
            call test_clip_integer_int32(x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32)

            ! invalid test cases
            x_integer_int32 = 2
            xmin_integer_int32 = 5
            xmax_integer_int32 = -2
            compare_integer_int32 = 5
            call test_clip_integer_int32(x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32)

            x_integer_int32 = -2147483647
            xmin_integer_int32 = 2147483647
            xmax_integer_int32 = 0
            compare_integer_int32 = 2147483647
            call test_clip_integer_int32(x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32)

            x_integer_int32 = 45732
            xmin_integer_int32 = 57642
            xmax_integer_int32 = -385769
            compare_integer_int32 = 57642
            call test_clip_integer_int32(x_integer_int32, xmin_integer_int32, xmax_integer_int32, compare_integer_int32)

        ! type: integer, kind: int64
            ! valid test cases
            x_integer_int64 = 2
            xmin_integer_int64 = -2
            xmax_integer_int64 = 5
            compare_integer_int64 = 2
            call test_clip_integer_int64(x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64)

            x_integer_int64 = -922337203
            xmin_integer_int64 = -10
            xmax_integer_int64 = 25
            compare_integer_int64 = -10
            call test_clip_integer_int64(x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64)

            x_integer_int64 = 97683
            xmin_integer_int64 = -200
            xmax_integer_int64 = 513788324
            compare_integer_int64 = 97683
            call test_clip_integer_int64(x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64)

            ! invalid test cases
            x_integer_int64 = 2
            xmin_integer_int64 = 5
            xmax_integer_int64 = -2
            compare_integer_int64 = 5
            call test_clip_integer_int64(x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64)

            x_integer_int64 = -922337203
            xmin_integer_int64 = 25
            xmax_integer_int64 = -10
            compare_integer_int64 = 25
            call test_clip_integer_int64(x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64)

            x_integer_int64 = 97683
            xmin_integer_int64 = 513788324
            xmax_integer_int64 = -200
            compare_integer_int64 = 513788324
            call test_clip_integer_int64(x_integer_int64, xmin_integer_int64, xmax_integer_int64, compare_integer_int64)

        ! type: real, kind: sp
            ! valid test cases
            x_real_sp = 3.025
            xmin_real_sp = -5.77
            xmax_real_sp = 3.025
            compare_real_sp = 3.025
            call test_clip_real_sp(x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp)

            x_real_sp = 0.0
            xmin_real_sp = -1578.025
            xmax_real_sp = -59.68
            compare_real_sp = -59.68
            call test_clip_real_sp(x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp)

            x_real_sp = 5.6
            xmin_real_sp = -97854.25
            xmax_real_sp = 2.3666
            compare_real_sp = 2.3666
            call test_clip_real_sp(x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp)

            ! invalid test cases
            x_real_sp = 3.025
            xmin_real_sp = 3.025
            xmax_real_sp = -5.77
            compare_real_sp = 3.025
            call test_clip_real_sp(x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp)

            x_real_sp = 0.0
            xmin_real_sp = -59.68
            xmax_real_sp = -1578.025
            compare_real_sp = -59.68
            call test_clip_real_sp(x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp)

            x_real_sp = 5.6
            xmin_real_sp = 2.3666
            xmax_real_sp = -97854.25
            compare_real_sp = 2.3666
            call test_clip_real_sp(x_real_sp, xmin_real_sp, xmax_real_sp, compare_real_sp)

        ! type: real, kind: dp
            ! valid test cases
            x_real_dp = 3.025
            xmin_real_dp = -5.77
            xmax_real_dp = 3.025
            compare_real_dp = 3.025
            call test_clip_real_dp(x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp)

            x_real_dp = -7.0
            xmin_real_dp = 0.059668
            xmax_real_dp = 1.00268
            compare_real_dp = 0.059668
            call test_clip_real_dp(x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp)

            x_real_dp = -12.3358
            xmin_real_dp = 8.55759
            xmax_real_dp = 8.55759
            compare_real_dp = 8.55759
            call test_clip_real_dp(x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp)

            ! invalid test cases
            x_real_dp = 3.025
            xmin_real_dp = 3.025
            xmax_real_dp = -5.77
            compare_real_dp = 3.025
            call test_clip_real_dp(x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp)

            x_real_dp = -7.0
            xmin_real_dp = 1.00268
            xmax_real_dp = 0.059668
            compare_real_dp = 1.00268
            call test_clip_real_dp(x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp)

            x_real_dp = -12.3358
            xmin_real_dp = 8.55759
            xmax_real_dp = 8.55759
            compare_real_dp = 8.55759
            call test_clip_real_dp(x_real_dp, xmin_real_dp, xmax_real_dp, compare_real_dp)

        ! type: real, kind: qp
            ! valid test cases
            x_real_qp = 3.025
            xmin_real_qp = -5.77
            xmax_real_qp = 3.025
            compare_real_qp = 3.025
            call test_clip_real_qp(x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp)

            x_real_qp = -55891546.2
            xmin_real_qp = -8958133457.23
            xmax_real_qp = -689712245.23
            compare_real_qp = -689712245.23
            call test_clip_real_qp(x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp)

            x_real_qp = 58.7
            xmin_real_qp = -2352.335
            xmax_real_qp = -189.58
            compare_real_qp = -189.58
            call test_clip_real_qp(x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp)

            ! invalid test cases
            x_real_qp = 3.025
            xmin_real_qp = 3.025
            xmax_real_qp = -5.77
            compare_real_qp = 3.025
            call test_clip_real_qp(x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp)

            x_real_qp = -55891546.2
            xmin_real_qp = -689712245.23
            xmax_real_qp = -8958133457.23
            compare_real_qp = -689712245.23
            call test_clip_real_qp(x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp)

            x_real_qp = 58.7
            xmin_real_qp = -189.58
            xmax_real_qp = -2352.335
            compare_real_qp = -189.58
            call test_clip_real_qp(x_real_qp, xmin_real_qp, xmax_real_qp, compare_real_qp)


end program tester
