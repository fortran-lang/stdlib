! SPDX-Identifier: MIT

module test_math
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, qp
    use stdlib_error, only : check
    use stdlib_math

    implicit none

contains

    subroutine test_clip_int8(x, xmin, xmax, answer)
        integer(int8), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_int8 failed.', warn=.true.)
    
    end subroutine test_clip_int8

    subroutine test_clip_int16(x, xmin, xmax, answer)
        integer(int16), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_int16 failed.', warn=.true.)
    
    end subroutine test_clip_int16

    subroutine test_clip_int32(x, xmin, xmax, answer)
        integer(int32), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_int32 failed.', warn=.true.)
    
    end subroutine test_clip_int32

    subroutine test_clip_int64(x, xmin, xmax, answer)
        integer(int64), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_int64 failed.', warn=.true.)
    
    end subroutine test_clip_int64

    subroutine test_clip_sp(x, xmin, xmax, answer)
        real(sp), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_sp failed.', warn=.true.)
    
    end subroutine test_clip_sp

    subroutine test_clip_dp(x, xmin, xmax, answer)
        real(dp), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_dp failed.', warn=.true.)
    
    end subroutine test_clip_dp

    subroutine test_clip_qp(x, xmin, xmax, answer)
        real(qp), intent(in) :: x, xmin, xmax, answer

        call check(clip(x, xmin, xmax) == answer, 'test_clip_qp failed.', warn=.true.)
    
    end subroutine test_clip_qp

end module test_math


program tester
    use test_math
    implicit none

    ! test case format: (x, xmin, xmax, correct answer)
    ! valid case: xmin is not greater than xmax
    ! invalid case: xmin is greater than xmax


    ! type: integer(int8), kind: int8
        ! valid test cases
        call test_clip_int8(2_int8, -2_int8, 5_int8, 2_int8)
        call test_clip_int8(127_int8, -127_int8, 0_int8, 0_int8)
        ! invalid test cases
        call test_clip_int8(2_int8, 5_int8, -2_int8, 5_int8)
        call test_clip_int8(127_int8, 0_int8, -127_int8, 0_int8)

    ! type: integer(int16), kind: int16
        ! valid test cases
        call test_clip_int16(2_int16, -2_int16, 5_int16, 2_int16)
        call test_clip_int16(32767_int16, -32767_int16, 0_int16, 0_int16)
        ! invalid test cases
        call test_clip_int16(2_int16, 5_int16, -2_int16, 5_int16)
        call test_clip_int16(32767_int16, 0_int16, -32767_int16, 0_int16)

    ! type: integer(int32), kind: int32
        ! valid test cases
        call test_clip_int32(2_int32, -2_int32, 5_int32, 2_int32)
        call test_clip_int32(-2147483647_int32, 0_int32, 2147483647_int32, 0_int32)
        ! invalid test cases
        call test_clip_int32(2_int32, 5_int32, -2_int32, 5_int32)
        call test_clip_int32(-2147483647_int32, 2147483647_int32, 0_int32, 2147483647_int32)

    ! type: integer(int64), kind: int64
        ! valid test cases
        call test_clip_int64(2_int64, -2_int64, 5_int64, 2_int64)
        call test_clip_int64(-922337203_int64, -10_int64, 25_int64, -10_int64)
        ! invalid test cases
        call test_clip_int64(2_int64, 5_int64, -2_int64, 5_int64)
        call test_clip_int64(-922337203_int64, 25_int64, -10_int64, 25_int64)

    ! type: real(sp), kind: sp
        ! valid test cases
        call test_clip_sp(3.025_sp, -5.77_sp, 3.025_sp, 3.025_sp)
        call test_clip_sp(0.0_sp, -1578.025_sp, -59.68_sp, -59.68_sp)
        ! invalid test cases
        call test_clip_sp(3.025_sp, 3.025_sp, -5.77_sp, 3.025_sp)
        call test_clip_sp(0.0_sp, -59.68_sp, -1578.025_sp, -59.68_sp)

    ! type: real(dp), kind: dp
        ! valid test cases
        call test_clip_dp(3.025_dp, -5.77_dp, 3.025_dp, 3.025_dp)
        call test_clip_dp(-7.0_dp, 0.059668_dp, 1.00268_dp, 0.059668_dp)
        ! invalid test cases
        call test_clip_dp(3.025_dp, 3.025_dp, -5.77_dp, 3.025_dp)
        call test_clip_dp(-7.0_dp, 1.00268_dp, 0.059668_dp, 1.00268_dp)

    ! type: real(qp), kind: qp
        ! valid test cases
        call test_clip_qp(3.025_qp, -5.77_qp, 3.025_qp, 3.025_qp)
        call test_clip_qp(-55891546.2_qp, -8958133457.23_qp, -689712245.23_qp, -689712245.23_qp)
        ! invalid test cases
        call test_clip_qp(3.025_qp, 3.025_qp, -5.77_qp, 3.025_qp)
        call test_clip_qp(-55891546.2_qp, -689712245.23_qp, -8958133457.23_qp, -689712245.23_qp)

end program tester
