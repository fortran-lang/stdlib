program test_optval
  use, intrinsic :: iso_fortran_env, only: &
       sp => real32, dp => real64, qp => real128, &
       int8, int16, int32, int64
  use stdlib_experimental_error, only: assert
  use stdlib_experimental_optval, only: optval

  implicit none

  call test_optval_sp
  call test_optval_dp
  call test_optval_qp

  call test_optval_cdp

  call test_optval_int8
  call test_optval_int16
  call test_optval_int32
  call test_optval_int64

  call test_optval_logical

  call test_optval_character


  call test_optval_sp_arr
  call test_optval_dp_arr
  call test_optval_qp_arr

  call test_optval_int8_arr
  call test_optval_int16_arr
  call test_optval_int32_arr
  call test_optval_int64_arr

contains

  subroutine test_optval_sp
    print *, "test_optval_sp"
    call assert(foo_sp(1.0_sp) == 1.0_sp)
    call assert(foo_sp() == 2.0_sp)
  end subroutine test_optval_sp


  function foo_sp(x) result(z)
    real(sp), intent(in), optional :: x
    real(sp) :: z
    z = optval(x, 2.0_sp)
  endfunction foo_sp


  subroutine test_optval_dp
    print *, "test_optval_dp"
    call assert(foo_dp(1.0_dp) == 1.0_dp)
    call assert(foo_dp() == 2.0_dp)
  end subroutine test_optval_dp


  function foo_dp(x) result(z)
    real(dp), intent(in), optional :: x
    real(dp) :: z
    z = optval(x, 2.0_dp)
  endfunction foo_dp


  subroutine test_optval_sdp
    complex(sp) :: z1
    print *, "test_optval_dp"
    z1 = cmplx(1.0_sp, 2.0_sp)
    call assert(foo_sdp(z1) == z1)
    call assert(foo_sdp() == z1)
  end subroutine test_optval_sdp

  function foo_sdp(x) result(z)
    complex(sp), intent(in), optional :: x
    complex(sp) :: z
    z = optval(x, cmplx(1.0_sp, 2.0_sp, kind=sp))
  endfunction foo_sdp


  subroutine test_optval_cdp
    complex(dp) :: z1
    print *, "test_optval_dp"
    z1 = cmplx(1.0_dp, 2.0_dp)
    call assert(foo_cdp(z1) == z1)
    call assert(foo_cdp() == z1)
  end subroutine test_optval_cdp

  function foo_cdp(x) result(z)
    complex(dp), intent(in), optional :: x
    complex(dp) :: z
    z = optval(x, cmplx(1.0_dp, 2.0_dp, kind=dp))
  endfunction foo_cdp


  subroutine test_optval_qp
    print *, "test_optval_qp"
    call assert(foo_qp(1.0_qp) == 1.0_qp)
    call assert(foo_qp() == 2.0_qp)
  end subroutine test_optval_qp


  function foo_qp(x) result(z)
    real(qp), intent(in), optional :: x
    real(qp) :: z
    z = optval(x, 2.0_qp)
  endfunction foo_qp


  subroutine test_optval_int8
    print *, "test_optval_int8"
    call assert(foo_int8(1_int8) == 1_int8)
    call assert(foo_int8() == 2_int8)
  end subroutine test_optval_int8


  function foo_int8(x) result(z)
    integer(int8), intent(in), optional :: x
    integer(int8) :: z
    z = optval(x, 2_int8)
  endfunction foo_int8


  subroutine test_optval_int16
    print *, "test_optval_int16"
    call assert(foo_int16(1_int16) == 1_int16)
    call assert(foo_int16() == 2_int16)
  end subroutine test_optval_int16


  function foo_int16(x) result(z)
    integer(int16), intent(in), optional :: x
    integer(int16) :: z
    z = optval(x, 2_int16)
  endfunction foo_int16


  subroutine test_optval_int32
    print *, "test_optval_int32"
    call assert(foo_int32(1_int32) == 1_int32)
    call assert(foo_int32() == 2_int32)
  end subroutine test_optval_int32


  function foo_int32(x) result(z)
    integer(int32), intent(in), optional :: x
    integer(int32) :: z
    z = optval(x, 2_int32)
  endfunction foo_int32


  subroutine test_optval_int64
    print *, "test_optval_int64"
    call assert(foo_int64(1_int64) == 1_int64)
    call assert(foo_int64() == 2_int64)
  end subroutine test_optval_int64


  function foo_int64(x) result(z)
    integer(int64), intent(in), optional :: x
    integer(int64) :: z
    z = optval(x, 2_int64)
  endfunction foo_int64


  subroutine test_optval_logical
    print *, "test_optval_logical"
    call assert(foo_logical(.true.))
    call assert(.not.foo_logical())
  end subroutine test_optval_logical


  function foo_logical(x) result(z)
    logical, intent(in), optional :: x
    logical :: z
    z = optval(x, .false.)
  endfunction foo_logical


  subroutine test_optval_character
    print *, "test_optval_character"
    call assert(foo_character("x") == "x")
    call assert(foo_character() == "y")
  end subroutine test_optval_character


  function foo_character(x) result(z)
    character(len=*), intent(in), optional :: x
    character(len=:), allocatable :: z
    z = optval(x, "y")
  endfunction foo_character


  subroutine test_optval_sp_arr
    print *, "test_optval_sp_arr"
    call assert(all(foo_sp_arr([1.0_sp, -1.0_sp]) == [1.0_sp, -1.0_sp]))
    call assert(all(foo_sp_arr() == [2.0_sp, -2.0_sp]))
  end subroutine test_optval_sp_arr


  function foo_sp_arr(x) result(z)
    real(sp), dimension(2), intent(in), optional :: x
    real(sp), dimension(2) :: z
    z = optval(x, [2.0_sp, -2.0_sp])
  end function foo_sp_arr


  subroutine test_optval_dp_arr
    print *, "test_optval_dp_arr"
    call assert(all(foo_dp_arr([1.0_dp, -1.0_dp]) == [1.0_dp, -1.0_dp]))
    call assert(all(foo_dp_arr() == [2.0_dp, -2.0_dp]))
  end subroutine test_optval_dp_arr


  function foo_dp_arr(x) result(z)
    real(dp), dimension(2), intent(in), optional :: x
    real(dp), dimension(2) :: z
    z = optval(x, [2.0_dp, -2.0_dp])
  end function foo_dp_arr


  subroutine test_optval_qp_arr
    print *, "test_optval_qp_arr"
    call assert(all(foo_qp_arr([1.0_qp, -1.0_qp]) == [1.0_qp, -1.0_qp]))
    call assert(all(foo_qp_arr() == [2.0_qp, -2.0_qp]))
  end subroutine test_optval_qp_arr


  function foo_qp_arr(x) result(z)
    real(qp), dimension(2), intent(in), optional :: x
    real(qp), dimension(2) :: z
    z = optval(x, [2.0_qp, -2.0_qp])
  end function foo_qp_arr


  subroutine test_optval_int8_arr
    print *, "test_optval_int8_arr"
    call assert(all(foo_int8_arr([1_int8, -1_int8]) == [1_int8, -1_int8]))
    call assert(all(foo_int8_arr() == [2_int8, -2_int8]))
  end subroutine test_optval_int8_arr


  function foo_int8_arr(x) result(z)
    integer(int8), dimension(2), intent(in), optional :: x
    integer(int8), dimension(2) :: z
    z = optval(x, [2_int8, -2_int8])
  end function foo_int8_arr


  subroutine test_optval_int16_arr
    print *, "test_optval_int16_arr"
    call assert(all(foo_int16_arr([1_int16, -1_int16]) == [1_int16, -1_int16]))
    call assert(all(foo_int16_arr() == [2_int16, -2_int16]))
  end subroutine test_optval_int16_arr


  function foo_int16_arr(x) result(z)
    integer(int16), dimension(2), intent(in), optional :: x
    integer(int16), dimension(2) :: z
    z = optval(x, [2_int16, -2_int16])
  end function foo_int16_arr


  subroutine test_optval_int32_arr
    print *, "test_optval_int32_arr"
    call assert(all(foo_int32_arr([1_int32, -1_int32]) == [1_int32, -1_int32]))
    call assert(all(foo_int32_arr() == [2_int32, -2_int32]))
  end subroutine test_optval_int32_arr


  function foo_int32_arr(x) result(z)
    integer(int32), dimension(2), intent(in), optional :: x
    integer(int32), dimension(2) :: z
    z = optval(x, [2_int32, -2_int32])
  end function foo_int32_arr


  subroutine test_optval_int64_arr
    print *, "test_optval_int64_arr"
    call assert(all(foo_int64_arr([1_int64, -1_int64]) == [1_int64, -1_int64]))
    call assert(all(foo_int64_arr() == [2_int64, -2_int64]))
  end subroutine test_optval_int64_arr


  function foo_int64_arr(x) result(z)
    integer(int64), dimension(2), intent(in), optional :: x
    integer(int64), dimension(2) :: z
    z = optval(x, [2_int64, -2_int64])
  end function foo_int64_arr


  subroutine test_optval_logical_arr
    print *, "test_optval_logical_arr"
    call assert(all(foo_logical_arr()))
    call assert(all(.not.foo_logical_arr()))
  end subroutine test_optval_logical_arr


  function foo_logical_arr(x) result(z)
    logical, dimension(2), intent(in), optional :: x
    logical, dimension(2) :: z
    z = optval(x, [.false., .false.])
  end function foo_logical_arr

end program test_optval
