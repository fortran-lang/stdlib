! SPDX-Identifier: MIT

program test_stdlib_math
    use stdlib_math, only: clip, gcd
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


    ! gcd function
    ! testing format: check(gcd(a, b) == correct answer)
    call check(gcd(0, 0) == 0, 'gcd(0, 0) failed.', warn=.true.)
    call check(gcd(2, 0) == 2, 'gcd(2, 0) failed.', warn=.true.)
    call check(gcd(0, -2) == 2, 'gcd(0, -2) failed.', warn=.true.)
    call check(gcd(3, 3) == 3, 'gcd(3, 3) failed.', warn=.true.)
    call check(gcd(9, 6) == 3, 'gcd(9, 6) failed.', warn=.true.)
    call check(gcd(6, 9) == 3, 'gcd(6, 9) failed.', warn=.true.)
    call check(gcd(-9, 6) == 3, 'gcd(-9, 6) failed.', warn=.true.)
    call check(gcd(9, -6) == 3, 'gcd(9, -6) failed.', warn=.true.)
    call check(gcd(-9, -6) == 3, 'gcd(-9, -6) failed.', warn=.true.)
    call check(gcd(97, 91) == 1, 'gcd(97, 91) failed.', warn=.true.)

    call check(gcd(48_int8, 18_int8) == 6_int8, 'gcd(48, 18) failed for int8.', warn=.true.)
    call check(gcd(48_int16, 18_int16) == 6_int16, 'gcd(48, 18) failed for int16', warn=.true.)
    call check(gcd(48_int32, 18_int32) == 6_int32, 'gcd(48, 18) failed for int32', warn=.true.)
    call check(gcd(48_int64, 18_int64) == 6_int64, 'gcd(48, 18) failed for int64', warn=.true.)

end program test_stdlib_math
