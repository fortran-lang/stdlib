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

  call test_optval_int8
  call test_optval_int16
  call test_optval_int32
  call test_optval_int64

  call test_optval_logical

  call test_optval_character

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
  
end program test_optval
