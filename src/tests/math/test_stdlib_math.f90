! SPDX-Identifier: MIT

program test_stdlib_math
    use stdlib_math, only: clip
    use stdlib_error, only: check
    use stdlib_kinds, only: int8, int16, int32, int64, sp, dp, qp
    implicit none

    ! clip function
    ! testing format: check(clip(x, xmin, xmax) == correct answer)
    ! valid case: xmin is not greater than xmax
    ! invalid case: xmin is greater than xmax

    ! type: integer(int8), kind: int8
    ! valid test case
    call check(clip(2_int8, -2_int8, 5_int8) == 2_int8, &
               'clip_int8 failed for valid case', warn=.true.)
    call check(clip(127_int8, -127_int8, 0_int8) == 0_int8, &
               'clip_int8 failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(2_int8, 5_int8, -2_int8) == 5_int8, &
               'clip_int8 failed for invalid case', warn=.true.)
    call check(clip(127_int8, 0_int8, -127_int8) == 0_int8, &
               'clip_int8 failed for invalid case', warn=.true.)

    ! type: integer(int16), kind: int16
    ! valid test case
    call check(clip(2_int16, -2_int16, 5_int16) == 2_int16, &
               'clip_int16 failed for valid case', warn=.true.)
    call check(clip(32767_int16, -32767_int16, 0_int16) == 0_int16, &
               'clip_int16 failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(2_int16, 5_int16, -2_int16) == 5_int16, &
               'clip_int16 failed for invalid case', warn=.true.)
    call check(clip(32767_int16, 0_int16, -32767_int16) == 0_int16, &
               'clip_int16 failed for invalid case', warn=.true.)

    ! type: integer(int32), kind: int32
    ! valid test case
    call check(clip(2_int32, -2_int32, 5_int32) == 2_int32, &
               'clip_int32 failed for valid case', warn=.true.)
    call check(clip(-2147483647_int32, 0_int32, 2147483647_int32) == 0_int32, &
               'clip_int32 failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(2_int32, 5_int32, -2_int32) == 5_int32, &
               'clip_int32 failed for invalid case', warn=.true.)
    call check(clip(-2147483647_int32, 2147483647_int32, 0_int32) == 2147483647_int32, &
               'clip_int32 failed for invalid case', warn=.true.)

    ! type: integer(int64), kind: int64
    ! valid test case
    call check(clip(2_int64, -2_int64, 5_int64) == 2_int64, &
               'clip_int64 failed for valid case', warn=.true.)
    call check(clip(-922337203_int64, -10_int64, 25_int64) == -10_int64, &
               'clip_int64 failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(2_int64, 5_int64, -2_int64) == 5_int64, &
               'clip_int64 failed for invalid case', warn=.true.)
    call check(clip(-922337203_int64, 25_int64, -10_int64) == 25_int64, &
               'clip_int64 failed for invalid case', warn=.true.)

    ! type: real(sp), kind: sp
    ! valid test case
    call check(clip(3.025_sp, -5.77_sp, 3.025_sp) == 3.025_sp, &
               'clip_sp failed for valid case', warn=.true.)
    call check(clip(0.0_sp, -1578.025_sp, -59.68_sp) == -59.68_sp, &
               'clip_sp failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(3.025_sp, 3.025_sp, -5.77_sp) == 3.025_sp, &
               'clip_sp failed for invalid case', warn=.true.)
    call check(clip(0.0_sp, -59.68_sp, -1578.025_sp) == -59.68_sp, &
               'clip_sp failed for invalid case', warn=.true.)

    ! type: real(dp), kind: dp
    ! valid test case
    call check(clip(3.025_dp, -5.77_dp, 3.025_dp) == 3.025_dp, &
               'clip_dp failed for valid case', warn=.true.)
    call check(clip(-7.0_dp, 0.059668_dp, 1.00268_dp) == 0.059668_dp, &
               'clip_dp failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(3.025_dp, 3.025_dp, -5.77_dp) == 3.025_dp, &
               'clip_dp failed for invalid case', warn=.true.)
    call check(clip(-7.0_dp, 1.00268_dp, 0.059668_dp) == 1.00268_dp, &
               'clip_dp failed for invalid case', warn=.true.)

    ! type: real(qp), kind: qp
    ! valid test case
    call check(clip(3.025_qp, -5.77_qp, 3.025_qp) == 3.025_qp, &
               'clip_qp failed for valid case', warn=.true.)
    call check(clip(-55891546.2_qp, -8958133457.23_qp, -689712245.23_qp) == -689712245.23_qp, &
               'clip_qp failed for valid case', warn=.true.)
    ! invalid test case
    call check(clip(3.025_qp, 3.025_qp, -5.77_qp) == 3.025_qp, &
               'clip_qp failed for invalid case', warn=.true.)
    call check(clip(-55891546.2_qp, -689712245.23_qp, -8958133457.23_qp) == -689712245.23_qp, &
               'clip_qp failed for invalid case', warn=.true.)

end program test_stdlib_math
