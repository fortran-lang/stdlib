program test_optval
  use, intrinsic :: iso_fortran_env, only: &
       sp => real32, dp => real64, qp => real128, &
       int8, int16, int32, int64
  use stdlib_error, only: check
  use stdlib_optval, only: optval

  implicit none

  call test_optval_rsp
  call test_optval_rdp
  call test_optval_rqp

  call test_optval_csp
  call test_optval_cdp
  call test_optval_cqp

  call test_optval_iint8
  call test_optval_iint16
  call test_optval_iint32
  call test_optval_iint64

  call test_optval_logical

  call test_optval_character


  call test_optval_rsp_arr
  call test_optval_rdp_arr
  call test_optval_rqp_arr

  call test_optval_csp_arr
  call test_optval_cdp_arr
  call test_optval_cqp_arr

  call test_optval_iint8_arr
  call test_optval_iint16_arr
  call test_optval_iint32_arr
  call test_optval_iint64_arr

contains

  subroutine test_optval_rsp
    print *, "test_optval_rsp"
    call check(foo_sp(1.0_sp) == 1.0_sp)
    call check(foo_sp() == 2.0_sp)
  end subroutine test_optval_rsp


  function foo_sp(x) result(z)
    real(sp), intent(in), optional :: x
    real(sp) :: z
    z = optval(x, 2.0_sp)
  endfunction foo_sp


  subroutine test_optval_rdp
    print *, "test_optval_rdp"
    call check(foo_dp(1.0_dp) == 1.0_dp)
    call check(foo_dp() == 2.0_dp)
  end subroutine test_optval_rdp


  function foo_dp(x) result(z)
    real(dp), intent(in), optional :: x
    real(dp) :: z
    z = optval(x, 2.0_dp)
  endfunction foo_dp


  subroutine test_optval_rqp
    print *, "test_optval_rqp"
    call check(foo_qp(1.0_qp) == 1.0_qp)
    call check(foo_qp() == 2.0_qp)
  end subroutine test_optval_rqp


  function foo_qp(x) result(z)
    real(qp), intent(in), optional :: x
    real(qp) :: z
    z = optval(x, 2.0_qp)
  endfunction foo_qp


  subroutine test_optval_csp
    complex(sp) :: z1
    print *, "test_optval_csp"
    z1 = cmplx(1.0_sp, 2.0_sp, kind=sp)
    call check(foo_csp(z1) == z1)
    call check(foo_csp() == z1)
  end subroutine test_optval_csp

  function foo_csp(x) result(z)
    complex(sp), intent(in), optional :: x
    complex(sp) :: z
    z = optval(x, cmplx(1.0_sp, 2.0_sp, kind=sp))
  endfunction foo_csp


  subroutine test_optval_cdp
    complex(dp) :: z1
    print *, "test_optval_cdp"
    z1 = cmplx(1.0_dp, 2.0_dp,kind=dp)
    call check(foo_cdp(z1) == z1)
    call check(foo_cdp() == z1)
  end subroutine test_optval_cdp

  function foo_cdp(x) result(z)
    complex(dp), intent(in), optional :: x
    complex(dp) :: z
    z = optval(x, cmplx(1.0_dp, 2.0_dp, kind=dp))
  endfunction foo_cdp


  subroutine test_optval_cqp
    complex(qp) :: z1
    print *, "test_optval_cqp"
    z1 = cmplx(1.0_qp, 2.0_qp, kind=qp)
    call check(foo_cqp(z1) == z1)
    call check(foo_cqp() == z1)
  end subroutine test_optval_cqp

  function foo_cqp(x) result(z)
    complex(qp), intent(in), optional :: x
    complex(qp) :: z
    z = optval(x, cmplx(1.0_qp, 2.0_qp, kind=qp))
  endfunction foo_cqp


  subroutine test_optval_iint8
    print *, "test_optval_iint8"
    call check(foo_int8(1_int8) == 1_int8)
    call check(foo_int8() == 2_int8)
  end subroutine test_optval_iint8


  function foo_int8(x) result(z)
    integer(int8), intent(in), optional :: x
    integer(int8) :: z
    z = optval(x, 2_int8)
  endfunction foo_int8


  subroutine test_optval_iint16
    print *, "test_optval_iint16"
    call check(foo_int16(1_int16) == 1_int16)
    call check(foo_int16() == 2_int16)
  end subroutine test_optval_iint16


  function foo_int16(x) result(z)
    integer(int16), intent(in), optional :: x
    integer(int16) :: z
    z = optval(x, 2_int16)
  endfunction foo_int16


  subroutine test_optval_iint32
    print *, "test_optval_iint32"
    call check(foo_int32(1_int32) == 1_int32)
    call check(foo_int32() == 2_int32)
  end subroutine test_optval_iint32


  function foo_int32(x) result(z)
    integer(int32), intent(in), optional :: x
    integer(int32) :: z
    z = optval(x, 2_int32)
  endfunction foo_int32


  subroutine test_optval_iint64
    print *, "test_optval_int64"
    call check(foo_int64(1_int64) == 1_int64)
    call check(foo_int64() == 2_int64)
  end subroutine test_optval_iint64


  function foo_int64(x) result(z)
    integer(int64), intent(in), optional :: x
    integer(int64) :: z
    z = optval(x, 2_int64)
  endfunction foo_int64


  subroutine test_optval_logical
    print *, "test_optval_logical"
    call check(foo_logical(.true.))
    call check(.not.foo_logical())
  end subroutine test_optval_logical


  function foo_logical(x) result(z)
    logical, intent(in), optional :: x
    logical :: z
    z = optval(x, .false.)
  endfunction foo_logical


  subroutine test_optval_character
    print *, "test_optval_character"
    call check(foo_character("x") == "x")
    call check(foo_character() == "y")
  end subroutine test_optval_character


  function foo_character(x) result(z)
    character(len=*), intent(in), optional :: x
    character(len=:), allocatable :: z
    z = optval(x, "y")
  endfunction foo_character


  subroutine test_optval_rsp_arr
    print *, "test_optval_rsp_arr"
    call check(all(foo_sp_arr([1.0_sp, -1.0_sp]) == [1.0_sp, -1.0_sp]))
    call check(all(foo_sp_arr() == [2.0_sp, -2.0_sp]))
  end subroutine test_optval_rsp_arr


  function foo_sp_arr(x) result(z)
    real(sp), dimension(2), intent(in), optional :: x
    real(sp), dimension(2) :: z
    z = optval(x, [2.0_sp, -2.0_sp])
  end function foo_sp_arr


  subroutine test_optval_rdp_arr
    print *, "test_optval_rdp_arr"
    call check(all(foo_dp_arr([1.0_dp, -1.0_dp]) == [1.0_dp, -1.0_dp]))
    call check(all(foo_dp_arr() == [2.0_dp, -2.0_dp]))
  end subroutine test_optval_rdp_arr


  function foo_dp_arr(x) result(z)
    real(dp), dimension(2), intent(in), optional :: x
    real(dp), dimension(2) :: z
    z = optval(x, [2.0_dp, -2.0_dp])
  end function foo_dp_arr


  subroutine test_optval_rqp_arr
    print *, "test_optval_qp_arr"
    call check(all(foo_qp_arr([1.0_qp, -1.0_qp]) == [1.0_qp, -1.0_qp]))
    call check(all(foo_qp_arr() == [2.0_qp, -2.0_qp]))
  end subroutine test_optval_rqp_arr


  function foo_qp_arr(x) result(z)
    real(qp), dimension(2), intent(in), optional :: x
    real(qp), dimension(2) :: z
    z = optval(x, [2.0_qp, -2.0_qp])
  end function foo_qp_arr


  subroutine test_optval_csp_arr
    complex(sp), dimension(2) :: z1, z2
    print *, "test_optval_csp_arr"
    z1 = cmplx(1.0_sp, 2.0_sp, kind=sp)*[1.0_sp, -1.0_sp]
    z2 = cmplx(2.0_sp, 2.0_sp, kind=sp)*[1.0_sp, -1.0_sp]
    call check(all(foo_csp_arr(z1) == z1))
    call check(all(foo_csp_arr() == z2))
  end subroutine test_optval_csp_arr


  function foo_csp_arr(x) result(z)
    complex(sp), dimension(2), intent(in), optional :: x
    complex(sp), dimension(2) :: z
    z = optval(x, cmplx(2.0_sp, 2.0_sp, kind=sp)*[1.0_sp, -1.0_sp])
  end function foo_csp_arr


  subroutine test_optval_cdp_arr
    complex(dp), dimension(2) :: z1, z2
    print *, "test_optval_cdp_arr"
    z1 = cmplx(1.0_dp, 2.0_dp, kind=dp)*[1.0_dp, -1.0_dp]
    z2 = cmplx(2.0_dp, 2.0_dp, kind=dp)*[1.0_dp, -1.0_dp]
    call check(all(foo_cdp_arr(z1) == z1))
    call check(all(foo_cdp_arr() == z2))
  end subroutine test_optval_cdp_arr


  function foo_cdp_arr(x) result(z)
    complex(dp), dimension(2), intent(in), optional :: x
    complex(dp), dimension(2) :: z
    z = optval(x, cmplx(2.0_dp, 2.0_dp, kind=dp)*[1.0_dp, -1.0_dp])
  end function foo_cdp_arr


  subroutine test_optval_cqp_arr
    complex(qp), dimension(2) :: z1, z2
    print *, "test_optval_cqp_arr"
    z1 = cmplx(1.0_qp, 2.0_qp, kind=qp)*[1.0_qp, -1.0_qp]
    z2 = cmplx(2.0_qp, 2.0_qp, kind=qp)*[1.0_qp, -1.0_qp]
    call check(all(foo_cqp_arr(z1) == z1))
    call check(all(foo_cqp_arr() == z2))
  end subroutine test_optval_cqp_arr


  function foo_cqp_arr(x) result(z)
    complex(qp), dimension(2), intent(in), optional :: x
    complex(qp), dimension(2) :: z
    z = optval(x, cmplx(2.0_qp, 2.0_qp, kind=qp)*[1.0_qp, -1.0_qp])
  end function foo_cqp_arr


  subroutine test_optval_iint8_arr
    print *, "test_optval_int8_arr"
    call check(all(foo_int8_arr([1_int8, -1_int8]) == [1_int8, -1_int8]))
    call check(all(foo_int8_arr() == [2_int8, -2_int8]))
  end subroutine test_optval_iint8_arr


  function foo_int8_arr(x) result(z)
    integer(int8), dimension(2), intent(in), optional :: x
    integer(int8), dimension(2) :: z
    z = optval(x, [2_int8, -2_int8])
  end function foo_int8_arr


  subroutine test_optval_iint16_arr
    print *, "test_optval_int16_arr"
    call check(all(foo_int16_arr([1_int16, -1_int16]) == [1_int16, -1_int16]))
    call check(all(foo_int16_arr() == [2_int16, -2_int16]))
  end subroutine test_optval_iint16_arr


  function foo_int16_arr(x) result(z)
    integer(int16), dimension(2), intent(in), optional :: x
    integer(int16), dimension(2) :: z
    z = optval(x, [2_int16, -2_int16])
  end function foo_int16_arr


  subroutine test_optval_iint32_arr
    print *, "test_optval_int32_arr"
    call check(all(foo_int32_arr([1_int32, -1_int32]) == [1_int32, -1_int32]))
    call check(all(foo_int32_arr() == [2_int32, -2_int32]))
  end subroutine test_optval_iint32_arr


  function foo_int32_arr(x) result(z)
    integer(int32), dimension(2), intent(in), optional :: x
    integer(int32), dimension(2) :: z
    z = optval(x, [2_int32, -2_int32])
  end function foo_int32_arr


  subroutine test_optval_iint64_arr
    print *, "test_optval_int64_arr"
    call check(all(foo_int64_arr([1_int64, -1_int64]) == [1_int64, -1_int64]))
    call check(all(foo_int64_arr() == [2_int64, -2_int64]))
  end subroutine test_optval_iint64_arr


  function foo_int64_arr(x) result(z)
    integer(int64), dimension(2), intent(in), optional :: x
    integer(int64), dimension(2) :: z
    z = optval(x, [2_int64, -2_int64])
  end function foo_int64_arr


  subroutine test_optval_logical_arr
    print *, "test_optval_logical_arr"
    call check(all(foo_logical_arr()))
    call check(all(.not.foo_logical_arr()))
  end subroutine test_optval_logical_arr


  function foo_logical_arr(x) result(z)
    logical, dimension(2), intent(in), optional :: x
    logical, dimension(2) :: z
    z = optval(x, [.false., .false.])
  end function foo_logical_arr

end program test_optval
